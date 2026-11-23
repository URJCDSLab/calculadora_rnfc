#' @title Funciones Clínicas y Lógica Predictiva (HF-Prognosis)
#' @description Colección de funciones auxiliares para traducción de inputs clínicos,
#'   inferencia con el modelo de regresión logística, asignación de grupos de riesgo,
#'   generación de gráficos de distribución y análisis univariante de factores modificables.
#'
#' @name fct_prediccion
NULL

#' @title Traducir Entradas de la Interfaz a Valores Numéricos del Modelo
#' @description Convierte las etiquetas legibles seleccionadas por el clínico en los
#'   valores numéricos codificados en el modelo logístico original del RNFC.
#'
#' @param variable Nombre de la variable clínica (e.g., "Demencia_NA", "Movilidad_basal_FAC_agrup").
#' @param input Cadena de texto seleccionada en la interfaz de usuario.
#'
#' @return Valor numérico (o factorizable) para el data frame de inferencia.
#' @export
#'
#' @examples
#' traduccion_newdata("Movilidad_basal_FAC_agrup", "Independiente (FAC 4-5)") # 0
#' traduccion_newdata("Demencia_NA", "Sí") # 1
traduccion_newdata <- function(variable, input) {
  if (is.null(input) || is.na(input)) return(NA)

  ### VARIABLE MOVILIDAD BASAL
  if (input %in% c("Dependiente (FAC 1-3)", "Dependent (FAC 1-3)")) {
    valor <- 1
  } else if (input %in% c("Independiente (FAC 4-5)", "Independent (FAC 4-5)")) {
    valor <- 0
  }

  ### VARIABLES BINARIAS: OPCIÓN SÍ / YES
  if (input %in% c("Sí", "Yes")) {
    if (variable == "Demencia_NA") {
      valor <- 1
    } else if (variable == "Sedestacion_precoz") {
      valor <- 0
    } else if (variable == "Ulceras_por_presion_NA_agrup") {
      valor <- 1
    }
  }

  ### VARIABLES BINARIAS: OPCIÓN NO
  if (input %in% c("No")) {
    if (variable == "Demencia_NA") {
      valor <- 0
    } else if (variable == "Sedestacion_precoz") {
      valor <- 1
    }
  }

  ### VARIABLES CON OPCIÓN DESCONOCIDO / UNKNOWN
  if (input %in% c("Desconocido", "Unknown")) {
    if (variable == "Demencia_NA") {
      valor <- 9
    } else if (variable == "ASA_I_IV_NA") {
      valor <- 9
    }
  }

  ### VARIABLE ASA
  if (input == "I") {
    valor <- 3
  } else if (input == "II") {
    valor <- 2
  } else if (input == "III") {
    valor <- 1
  } else if (input == "IV-V") {
    valor <- 4
  }

  ### VARIABLE DEMORA QUIRÚRGICA
  if (input %in% c("Menor o igual que 24h", "Less than or equal to 24h")) {
    valor <- 2
  } else if (input %in% c("Mayor que 24h", "Greater than 24h")) {
    valor <- 1
  }

  ### VARIABLE UBICACIÓN AL ALTA
  if (input %in% c("Domicilio", "Home")) {
    valor <- 1
  } else if (input %in% c("Residencia", "Nursing home")) {
    valor <- 2
  } else if (input %in% c("Unidad de Recuperación Funcional", "Functional Recovery Unit")) {
    valor <- 3
  } else if (input %in% c("Otros", "Other")) {
    valor <- 4
  }

  ### VARIABLE TIPO DE FRACTURA
  if (input %in% c("Subtrocantérea", "Subtrochanteric")) {
    valor <- 1
  } else if (input %in% c("Pertrocantérea o subcapital", "Pertrochanteric or subcapital")) {
    valor <- 0
  }

  ### VARIABLE DESCARGA
  if (input %in% c("Carga autorizada o desconocido", "Weight-bearing permitted or unknown")) {
    valor <- 0
  } else if (input %in% c("Descarga", "Non-weight-bearing")) {
    valor <- 1
  }

  ### VARIABLE ÚLCERAS POR PRESIÓN
  if (input %in% c("No o desconocido", "No or unknown")) {
    valor <- 0
  }

  return(valor)
}

#' @title Inferencia del Modelo Predictivo con Intervalo de Confianza
#' @description Calcula la probabilidad esperada de recuperación de marcha al mes
#'   y el margen del intervalo de confianza al 90% (critval = 1.645).
#'
#' @param model Objeto `glm` entrenado.
#' @param new_data Data frame de una fila con las 10 covariables requeridas.
#' @param critval Valor crítico normal (por defecto 1.645 para 90% IC).
#'
#' @return Lista con elementos:
#'   \item{prob}{Probabilidad ajustada (entre 0 y 1).}
#'   \item{ic}{Margen de error del intervalo de confianza (+/-).}
#' @export
prediccion_modelo <- function(model, new_data, critval = 1.645) {
  preds <- predict(model, newdata = new_data, type = "response", se.fit = TRUE)
  prob <- preds$fit
  ic_pred <- preds$se.fit * critval

  res <- list(
    prob = as.numeric(prob),
    ic = as.numeric(ic_pred)
  )
  return(res)
}

#' @title Asignación a Grupo de Probabilidad y Métricas de Cohorte
#' @description Clasifica al paciente en uno de los 5 estratos clínicos del RNFC
#'   (muy baja, baja, media, alta, muy alta) y recupera la proporción de pacientes
#'   en la base de datos nacional y la tasa de recuperación observada.
#'
#' @param prob Probabilidad numérica (0 a 1).
#'
#' @return Lista con:
#'   \item{grupo}{Categoría ("muy baja", "baja", "media", "alta", "muy alta").}
#'   \item{porc_base_datos}{Porcentaje de la cohorte RNFC en este grupo.}
#'   \item{prob_recuperacion_grupo}{Porcentaje de recuperación observado en el grupo.}
#'   \item{color_hex}{Código de color semántico para interfaces modernas.}
#' @export
asignacion_grupo <- function(prob, i18n = getOption("app.translator")) {
  # Registro explícito de literales para el auditor estático i18n
  if (!is.null(i18n)) {
    dummy_keys <- c(
      i18n$t("muy baja"),
      i18n$t("baja"),
      i18n$t("media"),
      i18n$t("alta"),
      i18n$t("muy alta")
    )
  }

  if (prob <= 0.2) {
    grupo <- "muy baja"
    porc_base_datos <- 1.8
    prob_recuperacion_grupo <- 22.1
    color_hex <- "#ef4444" # Danger
  } else if (prob > 0.2 && prob <= 0.4) {
    grupo <- "baja"
    porc_base_datos <- 8.6
    prob_recuperacion_grupo <- 35.4
    color_hex <- "#f97316" # Orange
  } else if (prob > 0.4 && prob <= 0.6) {
    grupo <- "media"
    porc_base_datos <- 23.2
    prob_recuperacion_grupo <- 50.0
    color_hex <- "#eab308" # Warning
  } else if (prob > 0.6 && prob <= 0.8) {
    grupo <- "alta"
    porc_base_datos <- 39.1
    prob_recuperacion_grupo <- 69.2
    color_hex <- "#0ea5e9" # Info/Teal
  } else {
    grupo <- "muy alta"
    porc_base_datos <- 27.3
    prob_recuperacion_grupo <- 86.8
    color_hex <- "#10b981" # Success
  }

  res <- list(
    grupo = grupo,
    porc_base_datos = porc_base_datos,
    prob_recuperacion_grupo = prob_recuperacion_grupo,
    color_hex = color_hex
  )
  return(res)
}

#' @title Gráfico Interactivo de Distribución y Posición del Paciente
#' @description Genera un gráfico `plotly` con la muestra de validación del RNFC
#'   y superpone el punto estimado del paciente con su intervalo de confianza.
#'
#' @param datos Data frame con datos de validación (`prob_validation`, `prob_validation_cat`).
#' @param grupo_pac Etiqueta del grupo de recuperación del paciente.
#' @param prob_paciente Probabilidad porcentual (0 - 100).
#' @param ic Margen porcentual del intervalo de confianza (+/-).
#'
#' @param color_primario Color primario institucional (por defecto "#95A1C0").
#' @param color_secundario Color secundario institucional (por defecto "#D0F0C0").
#' @param i18n Objeto traductor opcional (por defecto getOption("app.translator")).
#' @param dark Booleano que indica si se debe aplicar la paleta de modo oscuro (por defecto FALSE).
#'
#' @return Objeto `plotly`.
#' @export
grafico_grupo <- function(datos, grupo_pac, prob_paciente, ic, color_primario = "#95A1C0", color_secundario = "#D0F0C0", i18n = getOption("app.translator"), dark = FALSE) {
  if (is.null(i18n)) {
    i18n <- list(t = function(x) x)
  }

  # Configuración cromática sincronizada con brand.yml
  brand <- getOption("app.brand")
  col_prim <- if (!is.null(brand$color$primary)) brand$color$primary else color_primario
  col_sec <- if (!is.null(brand$color$secondary)) brand$color$secondary else color_secundario
  col_fg <- if (isTRUE(dark)) {
    if (!is.null(brand$color$background)) brand$color$background else "#ffffff"
  } else {
    if (!is.null(brand$color$foreground)) brand$color$foreground else "#2c3e50"
  }
  col_grid <- if (isTRUE(dark)) "rgba(255, 255, 255, 0.12)" else "rgba(0, 0, 0, 0.08)"
  col_border <- if (isTRUE(dark)) "#ffffff" else col_fg

  x_label <- c(
    i18n$t("Muy baja"),
    i18n$t("Baja"),
    i18n$t("Media"),
    i18n$t("Alta"),
    i18n$t("Muy alta")
  )
  x <- c(1, 2, 3, 4, 5)

  xform <- list(
    title = list(
      text = i18n$t("Grupos dado el nivel de recuperación"),
      font = list(size = 14, family = "Inter, sans-serif", color = col_fg)
    ),
    showticklabels = TRUE,
    tickmode = "array",
    ticktext = as.character(x_label),
    tickvals = x,
    tickfont = list(size = 13, family = "Inter, sans-serif", color = col_fg),
    gridcolor = col_grid,
    zerolinecolor = col_grid
  )

  grupos_es <- c("muy baja", "baja", "media", "alta", "muy alta")
  grupo_pac_num <- which(tolower(grupos_es) == tolower(grupo_pac))
  if (length(grupo_pac_num) == 0) {
    grupo_pac_num <- which(tolower(x_label) == tolower(grupo_pac))
  }
  if (length(grupo_pac_num) == 0) grupo_pac_num <- 3

  res_grupos <- data.frame(
    x = c(grupo_pac_num),
    y = c(prob_paciente),
    sd = c(ic)
  )

  fig <- datos %>%
    plotly::plot_ly() %>%
    plotly::add_trace(
      x = ~prob_validation_cat,
      y = ~prob_validation,
      hoverinfo = "text",
      text = ~paste0(i18n$t("Probabilidad en cohorte: "), round(prob_validation, 1), "\u202f%"),
      name = i18n$t("Muestra de validación (RNFC)"),
      type = "scatter",
      mode = "markers",
      marker = list(color = "rgba(149, 161, 192, 0.45)", size = 6)
    ) %>%
    plotly::add_trace(
      data = res_grupos,
      x = ~x,
      y = ~y,
      error_y = list(array = ~sd, color = col_sec, width = 5, thickness = 2.5),
      type = "scatter",
      mode = "markers",
      marker = list(color = col_sec, size = 14, symbol = "diamond", line = list(color = col_border, width = 1.5)),
      name = i18n$t("Predicción del paciente"),
      hoverinfo = "text",
      text = ~paste0(i18n$t("Predicción: "), round(y, 1), "\u202f% \u00b1\u202f", round(sd, 1), "\u202f%")
    ) %>%
    plotly::layout(
      xaxis = xform,
      yaxis = list(
        range = c(0, 110),
        title = list(
          text = i18n$t("Probabilidad de recuperación (%)"),
          font = list(size = 14, family = "Inter, sans-serif", color = col_fg)
        ),
        tickfont = list(size = 12, family = "Inter, sans-serif", color = col_fg),
        gridcolor = col_grid,
        zerolinecolor = col_grid
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = 1.1, font = list(size = 12, family = "Inter, sans-serif", color = col_fg)),
      margin = list(l = 50, r = 20, t = 40, b = 50),
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      hoverlabel = list(
        bgcolor = if (isTRUE(dark)) "#1e293b" else "#ffffff",
        bordercolor = col_prim,
        font = list(family = "Inter, sans-serif", size = 12, color = col_fg)
      ),
      modebar = list(
        bgcolor = "transparent",
        color = if (isTRUE(dark)) "#cbd5e1" else col_prim,
        activecolor = col_prim
      )
    ) %>%
    {
      if (requireNamespace("brand.yml", quietly = TRUE) && file.exists("_brand.yml")) {
        brand.yml::theme_brand_plotly(
          .,
          brand = "_brand.yml",
          background = "transparent",
          foreground = if (isTRUE(dark)) "background" else "foreground"
        )
      } else {
        .
      }
    } %>%
    plotly::config(
      displayModeBar = "hover",
      displaylogo = FALSE
    )

  return(fig)
}

#' @title Análisis Univariante de Factores Clínicos Modificables
#' @description Evalúa el impacto de alterar individualmente los 4 factores
#'   clínicamente modificables (demora quirúrgica, sedestación precoz, carga postoperatoria,
#'   ubicación al alta) y devuelve aquellos niveles que alcanzan o superan una meta deseada.
#'
#' @param model Modelo `glm`.
#' @param new_data Data frame con los valores actuales del paciente.
#' @param prob_deseada Probabilidad umbral buscada (0 a 1).
#' @param critval Valor crítico (1.645 por defecto).
#'
#' @return Lista con los factores modificables y las probabilidades alcanzables.
#' @export
explicatividad_univariante <- function(model, new_data, prob_deseada, critval = 1.645) {
  new_data_pruebas <- new_data
  vector_variables_cambio <- c(
    "Demora_quirurgica_cat",
    "Sedestacion_precoz",
    "Ubicacion_al_alta",
    "Descarga_NA_agrup"
  )

  lista_variables_mejora_prob <- list()

  for (variable_cambio in vector_variables_cambio) {
    niveles <- model$xlevels[[variable_cambio]][model$xlevels[[variable_cambio]] != as.character(new_data[[variable_cambio]])]
    niveles_cambio <- c()
    prob_alcanzadas <- c()

    for (nivel in niveles) {
      new_data_pruebas[[variable_cambio]] <- as.factor(nivel)
      prob_cambio <- prediccion_modelo(model, new_data_pruebas, critval)$prob

      if (prob_cambio >= prob_deseada) {
        niveles_cambio <- c(niveles_cambio, nivel)
        prob_alcanzadas <- c(prob_alcanzadas, round(prob_cambio, 2))
      }
    }

    if (length(niveles_cambio) > 0) {
      lista_variables_mejora_prob[[variable_cambio]] <- prob_alcanzadas
      names(lista_variables_mejora_prob[[variable_cambio]]) <- niveles_cambio
    }
  }

  return(lista_variables_mejora_prob)
}

#' @title Construcción de Narrativa Clínica Resumida
#' @description Redacta un párrafo en lenguaje clínico natural que sintetiza
#'   el estado basal y perioperatorio del paciente.
#'
#' @param input Lista de entradas clínicas.
#' @param prob Probabilidad predicha (0 a 1).
#' @param i18n Objeto traductor opcional (por defecto getOption("app.translator")).
#'
#' @return Cadena de texto con el resumen estructurado.
#' @export
construir_resumen_clinico <- function(input, prob, i18n = getOption("app.translator")) {
  if (is.null(i18n)) {
    i18n <- list(t = function(x) x)
  }

  txt <- paste0(i18n$t("Paciente de "), input$Edad, i18n$t(" años, "))

  # Movilidad basal
  if (input$Movilidad_basal_FAC_agrup %in% c("Dependiente (FAC 1-3)", "Dependent (FAC 1-3)")) {
    txt <- paste0(txt, i18n$t("con movilidad basal dependiente (FAC 1-3), "))
  } else {
    txt <- paste0(txt, i18n$t("con movilidad basal independiente (FAC 4-5), "))
  }

  # Deterioro cognitivo
  if (input$Demencia_NA %in% c("Sí", "Yes")) {
    txt <- paste0(txt, i18n$t("con deterioro cognitivo, "))
  } else if (input$Demencia_NA %in% c("No")) {
    txt <- paste0(txt, i18n$t("sin deterioro cognitivo, "))
  } else {
    txt <- paste0(txt, i18n$t("con deterioro cognitivo desconocido, "))
  }

  # ASA
  if (input$ASA_I_IV_NA == "I") {
    txt <- paste0(txt, i18n$t("nivel I de ASA, "))
  } else if (input$ASA_I_IV_NA == "II") {
    txt <- paste0(txt, i18n$t("nivel II de ASA, "))
  } else if (input$ASA_I_IV_NA == "III") {
    txt <- paste0(txt, i18n$t("nivel III de ASA, "))
  } else if (input$ASA_I_IV_NA == "IV-V") {
    txt <- paste0(txt, i18n$t("nivel IV-V de ASA, "))
  } else {
    txt <- paste0(txt, i18n$t("nivel ASA no consignado, "))
  }

  # Tipo fractura
  if (input$Tipo_fractura_3cat_agrup %in% c("Subtrocantérea", "Subtrochanteric")) {
    txt <- paste0(txt, i18n$t("fractura subtrocantérea, "))
  } else {
    txt <- paste0(txt, i18n$t("fractura pertrocantérea o subcapital, "))
  }

  # Demora quirúrgica
  if (input$Demora_quirurgica_cat %in% c("Menor o igual que 24h", "Less than or equal to 24h")) {
    txt <- paste0(txt, i18n$t("demora quirúrgica ≤ 24h, "))
  } else {
    txt <- paste0(txt, i18n$t("demora quirúrgica > 24h, "))
  }

  # Sedestación precoz
  if (input$Sedestacion_precoz %in% c("Sí", "Yes")) {
    txt <- paste0(txt, i18n$t("con sedestación precoz en las primeras 24h, "))
  } else {
    txt <- paste0(txt, i18n$t("sin sedestación precoz, "))
  }

  # Carga
  if (input$Descarga_NA_agrup %in% c("Carga autorizada o desconocido", "Weight-bearing permitted or unknown")) {
    txt <- paste0(txt, i18n$t("carga postoperatoria autorizada, "))
  } else {
    txt <- paste0(txt, i18n$t("en descarga articular, "))
  }

  # Úlceras
  if (input$Ulceras_por_presion_NA_agrup %in% c("Sí", "Yes")) {
    txt <- paste0(txt, i18n$t("con presencia de úlceras por presión, "))
  } else {
    txt <- paste0(txt, i18n$t("sin úlceras por presión, "))
  }

  # Ubicación al alta
  ubic_trans <- tolower(i18n$t(input$Ubicacion_al_alta))
  txt <- paste0(txt, i18n$t("y destino al alta previsto a "), ubic_trans, ".")

  return(txt)
}
