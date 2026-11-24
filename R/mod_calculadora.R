#' @title Módulo UI y Server: Calculadora de Riesgo de Marcha (HF-Prognosis)
#' @description Módulo de Shiny que proporciona el formulario clínico estructurado,
#'   el cálculo de la probabilidad esperada con intervalos de confianza al 90%,
#'   tarjetas compactas de valor métrico (value boxes) y la visualización interactiva de distribución.
#'
#' @param id Identificador del módulo.
#' @param model Modelo `glm` serializado cargado en memoria.
#' @param datos_grafico Data frame con la muestra de validación del RNFC.
#'
#' @return En el servidor, devuelve una lista reactiva con:
#'   \item{paciente_df}{Data frame reactivo con la fila de covariables procesadas.}
#'   \item{resultado}{Lista reactiva con probabilidad, IC y grupo.}
#'   \item{inputs_raw}{Lista reactiva con los valores crudos seleccionados.}
#'
#' @examples
#' \dontrun{
#'   # Probar el módulo de forma independiente:
#'   mod_calculadora_demo()
#' }
#' @name mod_calculadora
NULL

#' @rdname mod_calculadora
#' @export
mod_calculadora_ui <- function(id) {
  ns <- NS(id)
  i18n <- getOption("app.translator")

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 380,
      title = uiOutput(ns("sidebar_title_ui")),

      bslib::accordion(
        id = ns("acc_inputs"),
        open = c("sec_basales", "sec_quirurgicos"),
        multiple = TRUE,

        # SECCIÓN 1: FACTORES BASALES
        bslib::accordion_panel(
          title = uiOutput(ns("acc_sec1_title")),
          value = "sec_basales",
          icon = bsicons::bs_icon("heart-pulse"),

          sliderInput(
            inputId = ns("Edad"),
            label = i18n$t("Edad del paciente (años):"),
            min = 75,
            max = 110,
            value = 80,
            step = 1,
            ticks = TRUE
          ),

          shinyWidgets::pickerInput(
            inputId = ns("Movilidad_basal_FAC_agrup"),
            label = i18n$t("Movilidad basal previa (FAC):"),
            choices = c("Independiente (FAC 4-5)", "Dependiente (FAC 1-3)"),
            selected = "Independiente (FAC 4-5)",
            options = shinyWidgets::pickerOptions(
              size = 5
            )
          ),

          shinyWidgets::pickerInput(
            inputId = ns("Demencia_NA"),
            label = i18n$t("Deterioro cognitivo (Pfeiffer ≥ 4):"),
            choices = c("No", "Sí", "Desconocido"),
            selected = "No"
          ),

          shinyWidgets::pickerInput(
            inputId = ns("ASA_I_IV_NA"),
            label = i18n$t("Clasificación anestésica ASA:"),
            choices = c("I", "II", "III", "IV-V", "Desconocido"),
            selected = "II"
          )
        ),

        # SECCIÓN 2: FACTORES QUIRÚRGICOS Y POSTOPERATORIOS
        bslib::accordion_panel(
          title = uiOutput(ns("acc_sec2_title")),
          value = "sec_quirurgicos",
          icon = bsicons::bs_icon("bandaid"),

          shinyWidgets::pickerInput(
            inputId = ns("Tipo_fractura_3cat_agrup"),
            label = i18n$t("Tipo de fractura:"),
            choices = c("Pertrocantérea o subcapital", "Subtrocantérea"),
            selected = "Pertrocantérea o subcapital"
          ),

          shinyWidgets::pickerInput(
            inputId = ns("Demora_quirurgica_cat"),
            label = i18n$t("Demora quirúrgica:"),
            choices = c("Menor o igual que 24h", "Mayor que 24h"),
            selected = "Menor o igual que 24h"
          ),

          shinyWidgets::radioGroupButtons(
            inputId = ns("Sedestacion_precoz"),
            label = i18n$t("Sedestación precoz (< 24h):"),
            choices = c("Sí", "No"),
            selected = "Sí",
            justified = TRUE
          ),

          shinyWidgets::pickerInput(
            inputId = ns("Descarga_NA_agrup"),
            label = i18n$t("Manejo postoperatorio de carga:"),
            choices = c("Carga autorizada o desconocido", "Descarga"),
            selected = "Carga autorizada o desconocido"
          ),

          shinyWidgets::radioGroupButtons(
            inputId = ns("Ulceras_por_presion_NA_agrup"),
            label = i18n$t("Úlceras por presión durante el ingreso:"),
            choices = c("No o desconocido", "Sí"),
            selected = "No o desconocido",
            justified = TRUE
          ),

          shinyWidgets::pickerInput(
            inputId = ns("Ubicacion_al_alta"),
            label = i18n$t("Ubicación al alta hospitalaria:"),
            choices = c("Domicilio", "Residencia", "Unidad de Recuperación Funcional", "Otros"),
            selected = "Domicilio"
          )
        )
      ),

      actionButton(
        inputId = ns("calcular_pred"),
        label = i18n$t("Calcular probabilidad"),
        icon = bsicons::bs_icon("calculator"),
        class = "btn-primary w-100 py-2 fs-6 shadow-sm"
      )
    ),

    # PANEL PRINCIPAL
    # Distribución en 2 columnas: PLOT a la izquierda | 4 tarjetas KPI a la derecha
    bslib::layout_columns(
      col_widths = c(9, 3),
      fill = FALSE,

      # COLUMNA IZQUIERDA: Gráfico interactivo Plotly nativo en card
      bslib::card(
        full_screen = TRUE,
        class = "shadow-sm mb-0",
        bslib::card_header(
          uiOutput(ns("header_grafico_ui"), inline = TRUE)
        ),
        plotly::plotlyOutput(ns("grafico_grupos"), height = "100%")
      ),

      # COLUMNA DERECHA: 4 tarjetas KPI agrupadas por color institucional
      div(
        class = "d-flex flex-column gap-3",
        bslib::card(
          class = "text-bg-primary border-0 shadow-sm mb-0",
          bslib::card_body(
            class = "p-3",
            tags$div(
              class = "small fw-semibold opacity-75 d-flex align-items-center gap-2 mb-1",
              bsicons::bs_icon("person-walking", class = "flex-shrink-0"),
              textOutput(ns("vb_prob_title"), inline = TRUE)
            ),
            tags$div(class = "fs-2 fw-bold lh-1", textOutput(ns("vb_prob"), inline = TRUE))
          )
        ),
        bslib::card(
          class = "text-bg-primary border-0 shadow-sm mb-0",
          bslib::card_body(
            class = "p-3",
            tags$div(
              class = "small fw-semibold opacity-75 d-flex align-items-center gap-2 mb-1",
              bsicons::bs_icon("arrows-expand", class = "flex-shrink-0"),
              textOutput(ns("vb_ic_title"), inline = TRUE)
            ),
            tags$div(class = "fs-2 fw-bold lh-1", textOutput(ns("vb_ic"), inline = TRUE)),
            tags$div(class = "small opacity-75 mt-1", textOutput(ns("vb_ic_rango"), inline = TRUE))
          )
        ),
        bslib::card(
          class = "text-bg-secondary border-0 shadow-sm mb-0",
          bslib::card_body(
            class = "p-3",
            tags$div(
              class = "small fw-semibold opacity-75 d-flex align-items-center gap-2 mb-1",
              bsicons::bs_icon("graph-up", class = "flex-shrink-0"),
              textOutput(ns("vb_grupo_title"), inline = TRUE)
            ),
            tags$div(class = "fs-2 fw-bold lh-1", textOutput(ns("vb_grupo"), inline = TRUE))
          )
        ),
        bslib::card(
          class = "text-bg-secondary border-0 shadow-sm mb-0",
          bslib::card_body(
            class = "p-3",
            tags$div(
              class = "small fw-semibold opacity-75 d-flex align-items-center gap-2 mb-1",
              bsicons::bs_icon("people", class = "flex-shrink-0"),
              textOutput(ns("vb_cohorte_title"), inline = TRUE)
            ),
            tags$div(class = "fs-2 fw-bold lh-1", textOutput(ns("vb_cohorte"), inline = TRUE)),
            tags$div(class = "small opacity-75 mt-1", textOutput(ns("vb_cohorte_recup"), inline = TRUE))
          )
        )
      )
    ),

    # Interpretación clínica: el texto y las explicaciones aparecen directamente en el card
    bslib::card(
      class = "shadow-sm",
      bslib::card_header(
        uiOutput(ns("header_resumen_ui"), inline = TRUE)
      ),
      bslib::card_body(
        uiOutput(ns("resumen_clinico_ui"))
      )
    )
  )
}

#' @param lang Expresión reactiva opcional con el idioma activo ("es", "en").
#' @param dark_mode Expresión reactiva opcional con el modo de tema ("light", "dark").
#'
#' @rdname mod_calculadora
#' @export
mod_calculadora_server <- function(id, model, datos_grafico, lang = NULL, dark_mode = NULL) {
  moduleServer(id, function(input, output, session) {
    i18n <- getOption("app.translator")

    # Títulos y encabezados reactivos a nivel de módulo
    output$sidebar_title_ui <- renderUI({
      if (!is.null(lang)) lang()
      tags$span(
        bsicons::bs_icon("person-vcard", class = "me-2 text-primary"),
        i18n$t("Variables del paciente")
      )
    })

    output$acc_sec1_title <- renderUI({
      if (!is.null(lang)) lang()
      tags$strong(i18n$t("1. Factores basales y demográficos"))
    })

    output$acc_sec2_title <- renderUI({
      if (!is.null(lang)) lang()
      tags$strong(i18n$t("2. Cirugía y postoperatorio"))
    })

    output$header_grafico_ui <- renderUI({
      if (!is.null(lang)) lang()
      tags$span(
        bsicons::bs_icon("bar-chart-line", class = "me-2 text-primary"),
        i18n$t("Distribución en la cohorte del Registro Nacional de Fractura de Cadera (RNFC)")
      )
    })

    output$header_resumen_ui <- renderUI({
      if (!is.null(lang)) lang()
      tags$span(
        bsicons::bs_icon("journal-medical", class = "me-2 text-primary"),
        i18n$t("Interpretación clínica del paciente")
      )
    })

    output$vb_prob_title <- renderText({
      if (!is.null(lang)) lang()
      i18n$t("Probabilidad estimada de recuperación")
    })

    output$vb_ic_title <- renderText({
      if (!is.null(lang)) lang()
      i18n$t("Intervalo de confianza (90%)")
    })

    output$vb_grupo_title <- renderText({
      if (!is.null(lang)) lang()
      i18n$t("Grupo de riesgo pronóstico")
    })

    output$vb_cohorte_title <- renderText({
      if (!is.null(lang)) lang()
      i18n$t("Frecuencia en cohorte RNFC")
    })

    # Actualización reactiva de las opciones y etiquetas de los controles al cambiar de idioma
    observe({
      if (!is.null(lang)) lang()

      shiny::updateSliderInput(
        session,
        "Edad",
        label = i18n$t("Edad del paciente (años):")
      )

      shinyWidgets::updatePickerInput(
        session,
        "Movilidad_basal_FAC_agrup",
        label = i18n$t("Movilidad basal previa (FAC):"),
        choices = setNames(
          c("Independiente (FAC 4-5)", "Dependiente (FAC 1-3)"),
          c(i18n$t("Independiente (FAC 4-5)"), i18n$t("Dependiente (FAC 1-3)"))
        ),
        selected = input$Movilidad_basal_FAC_agrup
      )
      shinyWidgets::updatePickerInput(
        session,
        "Demencia_NA",
        label = i18n$t("Deterioro cognitivo (Pfeiffer ≥ 4):"),
        choices = setNames(
          c("No", "Sí", "Desconocido"),
          c(i18n$t("No"), i18n$t("Sí"), i18n$t("Desconocido"))
        ),
        selected = input$Demencia_NA
      )
      shinyWidgets::updatePickerInput(
        session,
        "ASA_I_IV_NA",
        label = i18n$t("Clasificación anestésica ASA:"),
        choices = setNames(
          c("I", "II", "III", "IV-V", "Desconocido"),
          c(i18n$t("I"), i18n$t("II"), i18n$t("III"), i18n$t("IV-V"), i18n$t("Desconocido"))
        ),
        selected = input$ASA_I_IV_NA
      )
      shinyWidgets::updatePickerInput(
        session,
        "Tipo_fractura_3cat_agrup",
        label = i18n$t("Tipo de fractura:"),
        choices = setNames(
          c("Pertrocantérea o subcapital", "Subtrocantérea"),
          c(i18n$t("Pertrocantérea o subcapital"), i18n$t("Subtrocantérea"))
        ),
        selected = input$Tipo_fractura_3cat_agrup
      )
      shinyWidgets::updatePickerInput(
        session,
        "Demora_quirurgica_cat",
        label = i18n$t("Demora quirúrgica:"),
        choices = setNames(
          c("Menor o igual que 24h", "Mayor que 24h"),
          c(i18n$t("Menor o igual que 24h"), i18n$t("Mayor que 24h"))
        ),
        selected = input$Demora_quirurgica_cat
      )
      shinyWidgets::updateRadioGroupButtons(
        session,
        "Sedestacion_precoz",
        label = i18n$t("Sedestación precoz (< 24h):"),
        choices = setNames(
          c("Sí", "No"),
          c(i18n$t("Sí"), i18n$t("No"))
        ),
        selected = input$Sedestacion_precoz
      )
      shinyWidgets::updatePickerInput(
        session,
        "Descarga_NA_agrup",
        label = i18n$t("Manejo postoperatorio de carga:"),
        choices = setNames(
          c("Carga autorizada o desconocido", "Descarga"),
          c(i18n$t("Carga autorizada o desconocido"), i18n$t("Descarga"))
        ),
        selected = input$Descarga_NA_agrup
      )
      shinyWidgets::updateRadioGroupButtons(
        session,
        "Ulceras_por_presion_NA_agrup",
        label = i18n$t("Úlceras por presión durante el ingreso:"),
        choices = setNames(
          c("No o desconocido", "Sí"),
          c(i18n$t("No o desconocido"), i18n$t("Sí"))
        ),
        selected = input$Ulceras_por_presion_NA_agrup
      )
      shinyWidgets::updatePickerInput(
        session,
        "Ubicacion_al_alta",
        label = i18n$t("Ubicación al alta hospitalaria:"),
        choices = setNames(
          c("Domicilio", "Residencia", "Unidad de Recuperación Funcional", "Otros"),
          c(i18n$t("Domicilio"), i18n$t("Residencia"), i18n$t("Unidad de Recuperación Funcional"), i18n$t("Otros"))
        ),
        selected = input$Ubicacion_al_alta
      )
      shiny::updateActionButton(
        session,
        "calcular_pred",
        label = i18n$t("Calcular probabilidad")
      )
    })

    # Estado reactivo del paciente procesado
    paciente_df <- eventReactive(input$calcular_pred, {
      df <- data.frame(
        Edad = as.numeric(input$Edad),
        Movilidad_basal_FAC_agrup = factor(traduccion_newdata("Movilidad_basal_FAC_agrup", input$Movilidad_basal_FAC_agrup)),
        Demencia_NA = factor(traduccion_newdata("Demencia_NA", input$Demencia_NA)),
        ASA_I_IV_NA = factor(traduccion_newdata("ASA_I_IV_NA", input$ASA_I_IV_NA)),
        Tipo_fractura_3cat_agrup = factor(traduccion_newdata("Tipo_fractura_3cat_agrup", input$Tipo_fractura_3cat_agrup)),
        Demora_quirurgica_cat = factor(traduccion_newdata("Demora_quirurgica_cat", input$Demora_quirurgica_cat)),
        Sedestacion_precoz = factor(traduccion_newdata("Sedestacion_precoz", input$Sedestacion_precoz)),
        Descarga_NA_agrup = factor(traduccion_newdata("Descarga_NA_agrup", input$Descarga_NA_agrup)),
        Ulceras_por_presion_NA_agrup = factor(traduccion_newdata("Ulceras_por_presion_NA_agrup", input$Ulceras_por_presion_NA_agrup)),
        Ubicacion_al_alta = factor(traduccion_newdata("Ubicacion_al_alta", input$Ubicacion_al_alta))
      )
      return(df)
    }, ignoreNULL = FALSE)

    # Cálculo inferencial
    resultado_calc <- eventReactive(input$calcular_pred, {
      req(paciente_df())
      p_res <- prediccion_modelo(model, paciente_df(), critval = 1.645)
      g_res <- asignacion_grupo(p_res$prob)
      list(
        prob = p_res$prob,
        ic = p_res$ic,
        grupo = g_res$grupo,
        porc_bd = g_res$porc_base_datos,
        recup_grupo = g_res$prob_recuperacion_grupo,
        color = g_res$color_hex
      )
    }, ignoreNULL = FALSE)

    # Value Box 1: Probabilidad (solo valor)
    output$vb_prob <- renderText({
      res <- resultado_calc()
      paste0(round(res$prob * 100, 1), "\u202f%")
    })

    # Value Box 2: Intervalo de confianza
    output$vb_ic <- renderText({
      res <- resultado_calc()
      paste0("\u00b1\u202f", round(res$ic * 100, 1), "\u202f%")
    })
    output$vb_ic_rango <- renderText({
      if (!is.null(lang)) lang()
      res <- resultado_calc()
      ic_min <- max(0, round((res$prob - res$ic) * 100, 1))
      ic_max <- min(100, round((res$prob + res$ic) * 100, 1))
      paste0(ic_min, "\u202f%", i18n$t(" a "), ic_max, "\u202f%")
    })

    # Value Box 3: Grupo de riesgo pronóstico
    output$vb_grupo <- renderText({
      if (!is.null(lang)) lang()
      res <- resultado_calc()
      grupo_trad <- i18n$t(res$grupo)
      paste0(toupper(substring(grupo_trad, 1, 1)), substring(grupo_trad, 2))
    })

    # Value Box 4: Frecuencia en cohorte RNFC
    output$vb_cohorte <- renderText({
      res <- resultado_calc()
      paste0(res$porc_bd, "\u202f%")
    })
    output$vb_cohorte_recup <- renderText({
      if (!is.null(lang)) lang()
      res <- resultado_calc()
      paste0(res$recup_grupo, "\u202f% ", i18n$t("recuperación en cohorte"))
    })

    # Gráfico Plotly interactivo
    output$grafico_grupos <- plotly::renderPlotly({
      if (!is.null(lang)) lang()
      is_dark <- if (!is.null(dark_mode)) isTRUE(identical(dark_mode(), "dark")) else FALSE
      res <- resultado_calc()
      grafico_grupo(
        datos = datos_grafico,
        grupo_pac = res$grupo,
        prob_paciente = res$prob * 100,
        ic = res$ic * 100,
        i18n = i18n,
        dark = is_dark
      )
    })

    # Resumen clínico: texto directo en el card con detalles de IC y cohorte
    output$resumen_clinico_ui <- renderUI({
      if (!is.null(lang)) lang()
      res <- resultado_calc()
      txt_narrativo <- construir_resumen_clinico(input, res$prob, i18n = i18n)
      tags$p(class = "mb-0", HTML(txt_narrativo))
    })

    # Retorno reactivo para conectar con el módulo de explicabilidad
    return(list(
      paciente_df = paciente_df,
      resultado = resultado_calc,
      inputs_raw = reactive({
        list(
          Edad = input$Edad,
          Movilidad_basal_FAC_agrup = input$Movilidad_basal_FAC_agrup,
          Demencia_NA = input$Demencia_NA,
          ASA_I_IV_NA = input$ASA_I_IV_NA,
          Tipo_fractura_3cat_agrup = input$Tipo_fractura_3cat_agrup,
          Demora_quirurgica_cat = input$Demora_quirurgica_cat,
          Sedestacion_precoz = input$Sedestacion_precoz,
          Descarga_NA_agrup = input$Descarga_NA_agrup,
          Ulceras_por_presion_NA_agrup = input$Ulceras_por_presion_NA_agrup,
          Ubicacion_al_alta = input$Ubicacion_al_alta
        )
      })
    ))
  })
}

#' @title Demo Independiente del Módulo de Calculadora
#' @description Lanza una aplicación Shiny autónoma para probar y validar el módulo
#'   de calculadora de forma aislada sin cargar el resto de la aplicación.
#'
#' @export
#' @examples
#' \dontrun{
#'   mod_calculadora_demo()
#' }
mod_calculadora_demo <- function() {
  library(shiny)
  library(bslib)
  library(plotly)
  library(shinyWidgets)

  source("R/fct_prediccion.R")
  model <- readRDS("data/model_final.rds")
  datos_grafico <- read.csv("www/datos_grafico_para_APP_formato.csv")

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(brand = TRUE),
    title = "Demo módulo calculadora",
    div(class = "container py-4", mod_calculadora_ui("demo_calc"))
  )

  server <- function(input, output, session) {
    mod_calculadora_server("demo_calc", model = model, datos_grafico = datos_grafico)
  }

  shinyApp(ui, server)
}
