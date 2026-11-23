#' @title Módulo UI y Server: Información, Validación Científica y Logos
#' @description Módulo que presenta los detalles del estudio científico, las instituciones
#'   participantes, la cohorte nacional del RNFC, los logotipos institucionales y el aviso legal clínico.
#'
#' @param id Identificador de espacio de nombres.
#'
#' @examples
#' \dontrun{
#'   mod_info_demo()
#' }
#' @name mod_info
NULL

#' @rdname mod_info
#' @export
mod_info_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("info_content"))
}

#' @param lang Expresión reactiva opcional con el idioma seleccionado.
#' @rdname mod_info
#' @export
mod_info_server <- function(id, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    i18n <- getOption("app.translator")

    output$info_content <- renderUI({
      if (!is.null(lang)) lang()

      bslib::page_fluid(
        class = "py-3",

        # TARJETA ÚNICA COHESIONADA: ACERCA DEL ESTUDIO (sin anidamiento excesivo de divs)
        bslib::card(
          class = "shadow-sm mb-4",
          style = "max-width: 980px; margin: 0 auto;",
          bslib::card_header(
            tags$span(
              bsicons::bs_icon("info-circle", class = "me-2 text-primary"),
              i18n$t("Acerca del estudio")
            )
          ),
          bslib::card_body(
            # Cabecera con logotipos institucionales colaboradores
            tags$section(
              class = "d-flex flex-wrap justify-content-around align-items-center gap-4 py-2 mb-4 border-bottom",
              tags$figure(
                class = "text-center mb-0",
                tags$img(
                  src = "RNFC.png",
                  alt = "Registro Nacional de Fractura de Cadera",
                  style = "max-height: 65px; max-width: 240px; object-fit: contain;"
                ),
                tags$figcaption(class = "text-muted small mt-1", i18n$t("Registro Nacional de Fractura de Cadera (RNFC)"))
              ),
              tags$figure(
                class = "text-center mb-0",
                tags$img(
                  src = "DSLab_logo.svg",
                  alt = "Data Science Lab URJC",
                  style = "max-height: 60px; max-width: 220px; object-fit: contain;"
                ),
                tags$figcaption(class = "text-muted small mt-1", i18n$t("Data Science Lab - URJC"))
              )
            ),

            # Descripción de la herramienta e instituciones participantes
            tags$h5(class = "fw-semibold mb-2", i18n$t("Base científica del modelo predictivo")),
            tags$p(
              class = "fs-6",
              i18n$t("Esta herramienta clínica estima la "),
              tags$strong(i18n$t("probabilidad de recuperación de la marcha un mes después")),
              i18n$t(" de la intervención quirúrgica de fractura de cadera en pacientes geriátricos. El modelo ha sido desarrollado conjuntamente por el "),
              tags$strong(i18n$t("Hospital Universitario Rey Juan Carlos"), .noWS = "after"), ", el ",
              tags$strong(i18n$t("Hospital Universitario Fundación Alcorcón"), .noWS = "after"), ", el ",
              tags$strong(i18n$t("Data Science Lab de la Universidad Rey Juan Carlos (DSLab URJC)")), " y el ",
              tags$strong(i18n$t("Hospital Universitario 12 de Octubre"), .noWS = "after"), "."
            ),

            # Cohorte del estudio
            tags$h6(class = "fw-semibold mt-3 mb-1", i18n$t("Cohorte nacional de validación")),
            tags$p(
              i18n$t("Los datos proceden de una cohorte nacional de "),
              tags$strong(i18n$t("25.607 pacientes")),
              i18n$t(" incluidos en el "), tags$strong(i18n$t("Registro Nacional de Fractura de Cadera (RNFC)")),
              i18n$t(" de España entre 2017 y 2019. El resultado primario analizado fue la recuperación del nivel basal de capacidad ambulatoria. El modelo de regresión logística se desarrolló en una muestra de entrenamiento y fue rigurosamente validado sobre la cohorte restante.")
            ),

            # Publicación científica de referencia
            tags$blockquote(
              class = "bg-body-tertiary border rounded p-3 my-3",
              tags$strong(i18n$t("Publicación de referencia:")),
              tags$p(
                class = "mb-1 mt-1 font-monospace small",
                "González de Villaumbrosia, Cristina; Sáez López, Pilar; Martín de Diego, Isaac; ",
                "Lancho Martín, Carmen; Cuesta Santa Teresa, Marina; Alarcón, Teresa; Ojeda Thies, Cristina; ",
                "Queipo Matas, Rocío; González-Montalvo, Juan I.; on behalf of the Participants in the Spanish National Hip Fracture Registry. 2021. ",
                em("''Predictive Model of Gait Recovery at One Month after Hip Fracture from a National Cohort of 25,607 Patients: The Hip Fracture Prognosis (HF-Prognosis) Tool''. "),
                "Int. J. Environ. Res. Public Health 18, no. 7: 3809."
              ),
              tags$a(
                href = "https://doi.org/10.3390/ijerph18073809",
                target = "_blank",
                class = "btn btn-sm btn-primary mt-2",
                bsicons::bs_icon("box-arrow-up-right", class = "me-1"),
                i18n$t("Acceder al artículo (DOI: 10.3390/ijerph18073809)")
              )
            ),

            # Aviso clínico y limitaciones integrado de forma semántica al pie
            tags$footer(
              class = "mt-4 pt-3 border-top",
              tags$h6(
                class = "fw-semibold mb-2",
                bsicons::bs_icon("exclamation-triangle", class = "me-1 text-warning"),
                i18n$t("Aviso clínico y limitaciones de uso")
              ),
              tags$p(
                class = "small text-muted mb-0",
                tags$em(
                  i18n$t("Las calculadoras de riesgo son instrumentos de apoyo probabilístico y no determinan de forma absoluta el pronóstico individual de ningún paciente. Proporcionan una estimación basada en la frecuencia observada en pacientes con factores basales y asistenciales similares dentro de la cohorte nacional, pero no sustituyen el juicio del equipo médico multidisciplinar. Al derivar de un estudio observacional de cohorte, las correlaciones estadísticamente significativas no implican necesariamente relación de causalidad directa; para demostrar el impacto independiente de cada factor se requieren ensayos clínicos controlados.")
                )
              )
            )
          )
        )
      )
    })
  })
}

#' @title Demo Independiente del Módulo de Información
#' @description Permite visualizar y probar la pantalla de información de forma autónoma.
#'
#' @export
#' @examples
#' \dontrun{
#'   mod_info_demo()
#' }
mod_info_demo <- function() {
  library(shiny)
  library(bslib)

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(brand = TRUE),
    title = "Demo módulo información",
    mod_info_ui("demo_info")
  )

  server <- function(input, output, session) {
    mod_info_server("demo_info")
  }

  shinyApp(ui, server)
}
