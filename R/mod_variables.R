#' @title Módulo UI y Server: Guía Clínica de Variables
#' @description Módulo que detalla la definición operativa, escalas validadas
#'   (FAC, Pfeiffer, ASA) y categorías de cada una de las variables clínicas involucradas.
#'
#' @param id Identificador de espacio de nombres.
#'
#' @examples
#' \dontrun{
#'   mod_variables_demo()
#' }
#' @name mod_variables
NULL

#' @rdname mod_variables
#' @export
mod_variables_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("vars_content"))
}

#' @param lang Expresión reactiva opcional con el idioma activo.
#' @rdname mod_variables
#' @export
mod_variables_server <- function(id, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    i18n <- getOption("app.translator")

    output$vars_content <- renderUI({
      if (!is.null(lang)) lang()

      bslib::page_fluid(
        class = "py-3",

        tags$header(
          class = "mb-4",
          tags$h4(
            class = "fw-bold",
            bsicons::bs_icon("book-half", class = "me-2 text-primary"),
            i18n$t("Definición operativa de las variables clínicas")
          ),
          tags$p(
            class = "text-muted",
            i18n$t("A continuación se detallan los criterios de codificación y las escalas validadas empleadas en el Registro Nacional de Fractura de Cadera (RNFC) para este modelo predictivo.")
          )
        ),

        bslib::accordion(
          id = session$ns("acc_variables"),
          multiple = TRUE,
          open = c("fac", "pfeiffer", "asa"),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Edad")),
            value = "edad",
            icon = bsicons::bs_icon("calendar3"),
            tags$p(
              i18n$t("Edad cronológica del paciente en el momento del ingreso. El modelo fue diseñado y validado para pacientes de edad geriátrica a partir de 75 años.")
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Movilidad basal previa (escala FAC)")),
            value = "fac",
            icon = bsicons::bs_icon("person-walking"),
            tags$p(
              i18n$t("Capacidad del paciente para deambular antes de sufrir la fractura de cadera, evaluada mediante la escala "),
              tags$strong(i18n$t("Functional Ambulation Classification (FAC)")), ":"
            ),
            tags$ul(
              tags$li(tags$strong(i18n$t("FAC 0:")), i18n$t(" No deambula o requiere ayuda de dos personas.")),
              tags$li(tags$strong(i18n$t("FAC 1:")), i18n$t(" Deambula con asistencia continua y firme de una persona.")),
              tags$li(tags$strong(i18n$t("FAC 2:")), i18n$t(" Deambula con asistencia ligera o intermitente de una persona.")),
              tags$li(tags$strong(i18n$t("FAC 3:")), i18n$t(" Deambula de forma independiente pero requiere supervisión verbal o presencia cercana.")),
              tags$li(tags$strong(i18n$t("FAC 4:")), i18n$t(" Deambula de forma independiente en terreno llano, pero requiere ayuda para escaleras.")),
              tags$li(tags$strong(i18n$t("FAC 5:")), i18n$t(" Deambula de forma completamente independiente en cualquier superficie y escaleras."))
            ),
            tags$p(
              class = "alert alert-light border py-2 small mb-0",
              i18n$t("Para el modelo se agrupan en: "),
              tags$strong(i18n$t("Dependiente (FAC 1-3)")),
              i18n$t(" frente a "),
              tags$strong(i18n$t("Independiente (FAC 4-5)"))
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Deterioro cognitivo (cuestionario de Pfeiffer)")),
            value = "pfeiffer",
            icon = bsicons::bs_icon("clipboard2-pulse"),
            tags$p(
              i18n$t("Presencia de deterioro cognitivo durante el ingreso hospitalario, medido a través del "),
              tags$strong(i18n$t("cuestionario de Pfeiffer (Short Portable Mental State Questionnaire, SPMSQ)")), "."
            ),
            tags$p(
              i18n$t("Se define la presencia de deterioro cognitivo cuando el paciente comete "),
              tags$strong(i18n$t("4 o más errores")),
              i18n$t(" en el cuestionario de 10 preguntas.")
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Clasificación del estado físico ASA")),
            value = "asa",
            icon = bsicons::bs_icon("shield-shaded"),
            tags$p(
              i18n$t("Evaluación preoperatoria de la comorbilidad física según la clasificación de la "),
              tags$strong(i18n$t("American Society of Anesthesiologists (ASA)")), ":"
            ),
            tags$ul(
              tags$li(tags$strong(i18n$t("ASA I:")), i18n$t(" Paciente sano sin enfermedad sistémica.")),
              tags$li(tags$strong(i18n$t("ASA II:")), i18n$t(" Paciente con enfermedad sistémica leve controlada sin limitación funcional.")),
              tags$li(tags$strong(i18n$t("ASA III:")), i18n$t(" Paciente con enfermedad sistémica grave que limita la actividad pero no es incapacitante.")),
              tags$li(tags$strong(i18n$t("ASA IV-V:")), i18n$t(" Enfermedad sistémica grave e incapacitante que es una amenaza constante para la vida o paciente moribundo."))
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Demora quirúrgica")),
            value = "demora",
            icon = bsicons::bs_icon("clock-history"),
            tags$p(
              i18n$t("Tiempo transcurrido desde el momento del ingreso en el hospital hasta la incisión quirúrgica. "),
              i18n$t("Se categoriza en "),
              tags$strong(i18n$t("menor o igual a 24 horas (≤ 24h)")),
              i18n$t(" frente a "),
              tags$strong(i18n$t("mayor de 24 horas (> 24h)")), "."
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Sedestación precoz")),
            value = "sedestacion",
            icon = bsicons::bs_icon("arrow-up-circle"),
            tags$p(
              i18n$t("Indica si se ha logrado levantar y sentar al paciente en un sillón dentro de las "),
              tags$strong(i18n$t("primeras 24 horas")),
              i18n$t(" posteriores a la intervención quirúrgica.")
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Manejo de carga postoperatoria (descarga)")),
            value = "descarga",
            icon = bsicons::bs_icon("universal-access"),
            tags$p(
              i18n$t("Indica si el cirujano autoriza la carga y apoyo de la extremidad desde los primeros días del postoperatorio, o si por el contrario, debido a inestabilidad de la osteosíntesis o características óseas, se prescribe "),
              tags$strong(i18n$t("descarga completa")),
              i18n$t(" (sin apoyar el pie durante 4-6 semanas).")
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Úlceras por presión")),
            value = "ulceras",
            icon = bsicons::bs_icon("activity"),
            tags$p(
              i18n$t("Desarrollo de úlceras por presión intrahospitalarias de "),
              tags$strong(i18n$t("grado 2 o superior")),
              i18n$t(" durante la estancia aguda.")
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Tipo de fractura")),
            value = "tipo_fractura",
            icon = bsicons::bs_icon("diagram-3"),
            tags$p(
              i18n$t("Localización anatómica de la fractura:"),
              tags$ul(
                tags$li(tags$strong(i18n$t("Pertrocantérea o subcapital")), i18n$t(" (engloba fracturas extracapsulares trocantéreas e intracapsulares de cuello femoral).")),
                tags$li(tags$strong(i18n$t("Subtrocantérea")), i18n$t(" (localizadas en la región inmediatamente distal al trocánter menor)."))
              )
            )
          ),

          bslib::accordion_panel(
            title = tags$strong(i18n$t("Ubicación al alta")),
            value = "ubicacion",
            icon = bsicons::bs_icon("hospital"),
            tags$p(
              i18n$t("Destino asignado al paciente al recibir el alta de la unidad de hospitalización de agudos:"),
              tags$ul(
                tags$li(tags$strong(i18n$t("Domicilio habitual"))),
                tags$li(tags$strong(i18n$t("Residencia sociosanitaria"))),
                tags$li(tags$strong(i18n$t("Unidad de Recuperación Funcional (URF) / Media estancia"))),
                tags$li(tags$strong(i18n$t("Otros: Traslado a unidades de agudos de otros centros o centros de larga estancia.")))
              )
            )
          )
        )
      )
    })
  })
}

#' @title Demo Independiente del Módulo de Variables
#' @description Permite visualizar y probar el acordeón clínico de variables de forma autónoma.
#'
#' @export
#' @examples
#' \dontrun{
#'   mod_variables_demo()
#' }
mod_variables_demo <- function() {
  library(shiny)
  library(bslib)

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(brand = TRUE),
    title = "Demo módulo variables",
    div(class = "container py-4", mod_variables_ui("demo_vars"))
  )

  server <- function(input, output, session) {
    mod_variables_server("demo_vars")
  }

  shinyApp(ui, server)
}
