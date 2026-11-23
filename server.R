####################################################################################
########            HF-PROGNOSIS: LÓGICA DEL SERVIDOR (MODULAR)             ########
####################################################################################

server <- function(input, output, session) {

  # Manejo reactivo del cambio de idioma (i18n)
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(session = session, language = input$selected_language)
    i18n$set_translation_language(input$selected_language)

    # Actualizar dinámicamente el título de la pestaña del navegador y metadatos
    session$sendCustomMessage("update_page_metadata", list(
      title = i18n$t("HF-Prognosis: Recuperación tras fractura de cadera"),
      description = i18n$t("Calculadora clínica de probabilidad de recuperación de la marcha un mes después de fractura de cadera.")
    ))
  })

  lang <- reactive(input$selected_language)

  # Traducciones reactivas de la barra de navegación superior
  output$navbar_subtitulo <- renderText({
    lang()
    i18n$t("Recuperación tras fractura de cadera")
  })
  output$tab_calc_title <- renderText({
    lang()
    i18n$t("Calculadora de riesgo")
  })
  output$tab_expl_title <- renderText({
    lang()
    i18n$t("Factores modificables")
  })
  output$tab_vars_title <- renderText({
    lang()
    i18n$t("Guía de variables")
  })
  output$tab_info_title <- renderText({
    lang()
    i18n$t("Acerca del estudio")
  })

  # 1. Módulo Principal: Calculadora de Riesgo
  # Procesa las covariables clínicas y calcula la probabilidad con IC al 90%
  paciente_calc <- mod_calculadora_server(
    id = "calc",
    model = model,
    datos_grafico = datos_grafico,
    lang = lang,
    dark_mode = reactive(input$dark_mode)
  )

  # 2. Módulo de Explicabilidad Asistencial (Factores Modificables)
  # Recibe el objeto reactivo del paciente calculado para mantener la sincronización automática
  mod_explicabilidad_server(
    id = "expl",
    model = model,
    paciente_reactivo = paciente_calc,
    lang = lang
  )

  # 3. Módulo de Consulta: Guía de Variables
  mod_variables_server(id = "vars", lang = lang)

  # 4. Módulo Informativo: Estudio, Cohorte y Logotipos
  mod_info_server(id = "info", lang = lang)

}