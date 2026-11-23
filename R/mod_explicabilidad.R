#' @title Módulo UI y Server: Factores Modificables y Explicabilidad Clínica
#' @description Módulo de Shiny para simular el impacto en la probabilidad de recuperación
#'   al alterar los 4 factores perioperatorios potencialmente modificables:
#'   demora quirúrgica, sedestación precoz, carga postoperatoria y destino al alta.
#'   Se sincroniza automáticamente con el paciente introducido en la calculadora.
#'
#' @param id Identificador de espacio de nombres (namespace).
#' @param model Modelo predictivo `glm`.
#' @param paciente_reactivo Lista reactiva devuelta por `mod_calculadora_server`.
#'
#' @examples
#' \dontrun{
#'   # Probar el módulo de forma independiente:
#'   mod_explicabilidad_demo()
#' }
#' @name mod_explicabilidad
NULL

#' @rdname mod_explicabilidad
#' @export
mod_explicabilidad_ui <- function(id) {
  ns <- NS(id)
  i18n <- getOption("app.translator")

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      title = uiOutput(ns("expl_sidebar_title_ui")),

      uiOutput(ns("expl_sidebar_desc_ui")),

      sliderInput(
        inputId = ns("prob_deseada"),
        label = i18n$t("Probabilidad deseada (%):"),
        min = 0,
        max = 100,
        value = 50,
        step = 1,
        post = "%",
        ticks = TRUE
      ),

      actionButton(
        inputId = ns("calcular_expl"),
        label = i18n$t("Analizar mejoras"),
        icon = bsicons::bs_icon("magic"),
        class = "btn-primary w-100 py-2 fs-6 shadow-sm"
      )
    ),

    # PANEL PRINCIPAL (Tarjetas nativas directas sin divs envoltorios)
    # Tarjeta 1: Estado del paciente sincronizado
    bslib::card(
      class = "mb-3",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        uiOutput(ns("header_paciente_ui"), inline = TRUE),
        uiOutput(ns("badge_prob_actual"))
      ),
      bslib::card_body(
        uiOutput(ns("texto_resumen_paciente_ui"))
      )
    ),

    # Tarjeta 2: Resultados y recomendaciones asistenciales
    bslib::card(
      bslib::card_header(
        uiOutput(ns("header_mejoras_ui"), inline = TRUE)
      ),
      bslib::card_body(
        uiOutput(ns("resultados_explicabilidad_ui"))
      )
    )
  )
}

#' @param lang Expresión reactiva opcional con el idioma seleccionado.
#' @rdname mod_explicabilidad
#' @export
mod_explicabilidad_server <- function(id, model, paciente_reactivo, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    i18n <- getOption("app.translator")

    # Títulos y descripciones reactivas a nivel de módulo
    output$expl_sidebar_title_ui <- renderUI({
      if (!is.null(lang)) lang()
      tags$span(
        bsicons::bs_icon("sliders", class = "me-2 text-primary"),
        i18n$t("Objetivo de recuperación")
      )
    })

    output$expl_sidebar_desc_ui <- renderUI({
      if (!is.null(lang)) lang()
      p(
        class = "text-muted small",
        i18n$t("Seleccione la meta de probabilidad que desea alcanzar para este paciente. El sistema identificará qué decisiones asistenciales permiten conseguirla o superarla.")
      )
    })

    output$header_paciente_ui <- renderUI({
      if (!is.null(lang)) lang()
      tags$span(
        bsicons::bs_icon("person-check", class = "me-2 text-primary"),
        i18n$t("Perfil del paciente analizado")
      )
    })

    output$header_mejoras_ui <- renderUI({
      if (!is.null(lang)) lang()
      tags$span(
        bsicons::bs_icon("lightbulb", class = "me-2 text-primary"),
        i18n$t("Intervenciones asistenciales que permiten alcanzar el objetivo")
      )
    })

    # Actualización reactiva de etiquetas al cambiar de idioma
    observe({
      if (!is.null(lang)) lang()
      updateSliderInput(
        session,
        "prob_deseada",
        label = i18n$t("Probabilidad deseada (%):")
      )
      updateActionButton(
        session,
        "calcular_expl",
        label = i18n$t("Analizar mejoras")
      )
    })

    # Sincronización reactiva del paciente y actualización del slider
    observe({
      req(paciente_reactivo$resultado())
      prob_actual_pct <- round(paciente_reactivo$resultado()$prob * 100)

      meta_sugerida <- min(100, prob_actual_pct + 10)
      updateSliderInput(
        session,
        "prob_deseada",
        min = min(prob_actual_pct, 95),
        value = meta_sugerida
      )
    })

    # Badge con la probabilidad actual en el header de la tarjeta
    output$badge_prob_actual <- renderUI({
      if (!is.null(lang)) lang()
      req(paciente_reactivo$resultado())
      p_pct <- round(paciente_reactivo$resultado()$prob * 100, 1)
      tags$span(
        class = "badge text-bg-secondary fs-6",
        paste0(i18n$t("Probabilidad basal: "), p_pct, "\u202f%")
      )
    })

    # Resumen estructurado del paciente
    output$texto_resumen_paciente_ui <- renderUI({
      if (!is.null(lang)) lang()
      req(paciente_reactivo$resultado(), paciente_reactivo$inputs_raw())
      inputs <- paciente_reactivo$inputs_raw()
      prob <- paciente_reactivo$resultado()$prob
      narrativa <- construir_resumen_clinico(inputs, prob, i18n = i18n)

      tagList(
        tags$p(class = "mb-1", HTML(narrativa)),
        tags$p(
          class = "text-muted small mt-2 mb-0",
          tags$em(
            i18n$t("Cualquier modificación introducida en la pestaña de 'Calculadora de riesgo' se refleja automáticamente en esta sección.")
          )
        )
      )
    })

    # Cálculo y presentación de explicatividad
    explicatividad_res <- eventReactive(input$calcular_expl, {
      req(paciente_reactivo$paciente_df())
      prob_meta <- input$prob_deseada / 100
      paciente <- paciente_reactivo$paciente_df()

      vars_mejora <- explicatividad_univariante(
        model = model,
        new_data = paciente,
        prob_deseada = prob_meta,
        critval = 1.645
      )

      list(
        vars = vars_mejora,
        prob_meta = prob_meta,
        prob_actual = paciente_reactivo$resultado()$prob
      )
    }, ignoreNULL = FALSE)

    output$resultados_explicabilidad_ui <- renderUI({
      if (!is.null(lang)) lang()
      req(explicatividad_res())
      res <- explicatividad_res()
      vars <- res$vars
      meta_pct <- round(res$prob_meta * 100)
      actual_pct <- round(res$prob_actual * 100, 1)

      if (length(vars) == 0) {
        return(
          div(
            class = "alert alert-light border d-flex align-items-center gap-2",
            bsicons::bs_icon("info-circle", class = "fs-5"),
            div(
              tags$strong(paste0(i18n$t("Ninguna intervención univariante aislada alcanza la meta seleccionada ("), meta_pct, "\u202f%).")),
              tags$p(class = "mb-0 text-muted small",
                     i18n$t("Para pacientes con este perfil clínico basal, se requeriría una combinación de varios factores o un objetivo de probabilidad más gradual."))
            )
          )
        )
      }

      # Si hay factores que mejoran la probabilidad
      tarjetas_factores <- list()

      tarjetas_factores[[1]] <- div(
        class = "alert alert-light border d-flex align-items-center gap-2 mb-3",
        bsicons::bs_icon("check-circle-fill", class = "fs-5 text-success"),
        div(
          tags$strong(
            paste0(i18n$t("Se han detectado factores asistenciales que permiten superar la meta del "), meta_pct, "\u202f%:")
          ),
          tags$div(class = "text-muted small", paste0(i18n$t("Situación actual de partida: "), actual_pct, "\u202f%."))
        )
      )

      items_ui <- list()

      # Factor 1: Sedestación precoz
      if ("Sedestacion_precoz" %in% names(vars)) {
        prob_val <- vars[["Sedestacion_precoz"]] * 100
        dif <- round(prob_val - actual_pct, 1)
        items_ui[[length(items_ui) + 1]] <- div(
          class = "d-flex justify-content-between align-items-center p-3 mb-2 rounded border bg-body-tertiary",
          div(
            tags$strong(i18n$t("Movilización y sedestación precoz (< 24h)")),
            tags$div(class = "text-muted small", i18n$t("Levantar al paciente en las primeras 24 horas tras la cirugía."))
          ),
          div(
            class = "text-end",
            tags$span(class = "badge text-bg-secondary fs-6 me-1", paste0(prob_val, "\u202f%")),
            if (dif > 0) tags$span(class = "badge bg-success", paste0("+", dif, "\u202f%"))
          )
        )
      }

      # Factor 2: Demora quirúrgica
      if ("Demora_quirurgica_cat" %in% names(vars)) {
        prob_val <- vars[["Demora_quirurgica_cat"]] * 100
        dif <- round(prob_val - actual_pct, 1)
        items_ui[[length(items_ui) + 1]] <- div(
          class = "d-flex justify-content-between align-items-center p-3 mb-2 rounded border bg-body-tertiary",
          div(
            tags$strong(i18n$t("Cirugía temprana (≤ 24h)")),
            tags$div(class = "text-muted small", i18n$t("Intervención realizada antes de las 24 horas desde el ingreso hospitalario."))
          ),
          div(
            class = "text-end",
            tags$span(class = "badge text-bg-secondary fs-6 me-1", paste0(prob_val, "\u202f%")),
            if (dif > 0) tags$span(class = "badge bg-success", paste0("+", dif, "\u202f%"))
          )
        )
      }

      # Factor 3: Descarga
      if ("Descarga_NA_agrup" %in% names(vars)) {
        prob_val <- vars[["Descarga_NA_agrup"]] * 100
        dif <- round(prob_val - actual_pct, 1)
        items_ui[[length(items_ui) + 1]] <- div(
          class = "d-flex justify-content-between align-items-center p-3 mb-2 rounded border bg-body-tertiary",
          div(
            tags$strong(i18n$t("Carga precoz autorizada")),
            tags$div(class = "text-muted small", i18n$t("Permitir apoyo y carga articular inmediata en lugar de descarga completa."))
          ),
          div(
            class = "text-end",
            tags$span(class = "badge text-bg-secondary fs-6 me-1", paste0(prob_val, "\u202f%")),
            if (dif > 0) tags$span(class = "badge bg-success", paste0("+", dif, "\u202f%"))
          )
        )
      }

      # Factor 4: Ubicación al alta
      if ("Ubicacion_al_alta" %in% names(vars)) {
        ubics <- vars[["Ubicacion_al_alta"]]
        nombres_ubics <- c(
          "1" = i18n$t("Traslado a domicilio habitual"),
          "2" = i18n$t("Traslado a residencia sociosanitaria"),
          "3" = i18n$t("Traslado a unidad de recuperación funcional (URF)"),
          "4" = i18n$t("Traslado a otra unidad especializada")
        )

        for (u_cod in names(ubics)) {
          p_ub <- ubics[u_cod] * 100
          d_ub <- round(p_ub - actual_pct, 1)
          nom <- nombres_ubics[u_cod]

          items_ui[[length(items_ui) + 1]] <- div(
            class = "d-flex justify-content-between align-items-center p-3 mb-2 rounded border bg-body-tertiary",
            div(
              tags$strong(nom),
              tags$div(class = "text-muted small", i18n$t("Destino asistencial asignado tras el alta de agudos."))
            ),
            div(
              class = "text-end",
              tags$span(class = "badge text-bg-secondary fs-6 me-1", paste0(p_ub, "\u202f%")),
              if (d_ub > 0) tags$span(class = "badge bg-success", paste0("+", d_ub, "\u202f%"))
            )
          )
        }
      }

      tagList(tarjetas_factores, items_ui)
    })
  })
}

#' @title Demo Independiente del Módulo de Explicabilidad
#' @description Lanza una aplicación Shiny autónoma con un paciente sintético de prueba
#'   para validar el módulo de explicabilidad de manera independiente.
#'
#' @export
#' @examples
#' \dontrun{
#'   mod_explicabilidad_demo()
#' }
mod_explicabilidad_demo <- function() {
  library(shiny)
  library(bslib)
  library(plotly)
  library(shinyWidgets)

  source("R/fct_prediccion.R")
  model <- readRDS("data/model_final.rds")

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(brand = TRUE),
    title = "Demo módulo explicabilidad",
    div(class = "container py-4", mod_explicabilidad_ui("demo_expl"))
  )

  server <- function(input, output, session) {
    # Paciente de prueba simulado
    paciente_simulado <- reactive({
      data.frame(
        Edad = 82,
        Movilidad_basal_FAC_agrup = factor(0),
        Demencia_NA = factor(0),
        ASA_I_IV_NA = factor(2),
        Tipo_fractura_3cat_agrup = factor(0),
        Demora_quirurgica_cat = factor(1), # > 24h
        Sedestacion_precoz = factor(1),    # No
        Descarga_NA_agrup = factor(1),     # Descarga
        Ulceras_por_presion_NA_agrup = factor(0),
        Ubicacion_al_alta = factor(2)      # Residencia
      )
    })

    inputs_raw_simulado <- reactive({
      list(
        Edad = 82,
        Movilidad_basal_FAC_agrup = "Independiente (FAC 4-5)",
        Demencia_NA = "No",
        ASA_I_IV_NA = "II",
        Tipo_fractura_3cat_agrup = "Pertrocantérea o subcapital",
        Demora_quirurgica_cat = "Mayor que 24h",
        Sedestacion_precoz = "No",
        Descarga_NA_agrup = "Descarga",
        Ulceras_por_presion_NA_agrup = "No o desconocido",
        Ubicacion_al_alta = "Residencia"
      )
    })

    resultado_simulado <- reactive({
      p_res <- prediccion_modelo(model, paciente_simulado(), 1.645)
      g_res <- asignacion_grupo(p_res$prob)
      list(
        prob = p_res$prob,
        ic = p_res$ic,
        grupo = g_res$grupo,
        porc_bd = g_res$porc_base_datos,
        recup_grupo = g_res$prob_recuperacion_grupo,
        color = g_res$color_hex
      )
    })

    paciente_wrapper <- list(
      paciente_df = paciente_simulado,
      resultado = resultado_simulado,
      inputs_raw = inputs_raw_simulado
    )

    mod_explicabilidad_server("demo_expl", model = model, paciente_reactivo = paciente_wrapper)
  }

  shinyApp(ui, server)
}
