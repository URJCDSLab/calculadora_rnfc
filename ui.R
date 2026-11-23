####################################################################################
########            HF-PROGNOSIS: INTERFAZ DE USUARIO (BSLIB)               ########
####################################################################################

ui <- bslib::page_navbar(
  theme = app_theme,
  title = tags$span(
    class = "d-flex align-items-center gap-3 py-0 my-0 pe-2",
    tags$img(
      src = "RNFC.png",
      alt = "Registro Nacional de Fractura de Cadera",
      height = "30px",
      style = "object-fit: contain; display: block;"
    ),
    tags$img(
      src = "DSLab_logo.svg",
      alt = "Data Science Lab URJC",
      height = "25px",
      style = "object-fit: contain; display: block; margin-top: 4px;"
    ),
    tags$span(
      class = "fw-bold fs-6 ms-2 text-nowrap",
      "HF-Prognosis",
      tags$span(
        class = "d-none d-xl-inline",
        tags$span(class = "opacity-50 me-1", ":"),
        textOutput("navbar_subtitulo", inline = TRUE)
      )
    ),
    # Separador vertical nativo de Bootstrap (.vr) para delimitar título de pestañas
    tags$span(
      class = "vr mx-2 opacity-50",
      style = "align-self: center; height: 22px;"
    )
  ),
  window_title = "HF-Prognosis: Recuperación tras fractura de cadera",
  id = "nav_principal",
  navbar_options = bslib::navbar_options(collapsible = TRUE, underline = TRUE),
  fillable = FALSE,

  header = tags$head(
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$link(rel = "shortcut icon", type = "image/png", href = "favicon.png"),
    shiny.i18n::usei18n(i18n),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
    tags$meta(name = "description", content = "Calculadora clínica de probabilidad de recuperación de la marcha un mes después de fractura de cadera."),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('update_page_metadata', function(msg) {
          if (msg && msg.title) document.title = msg.title;
          var meta = document.querySelector('meta[name=\"description\"]');
          if (meta && msg && msg.description) meta.setAttribute('content', msg.description);
        });
      });
    "))
  ),

  # Pestaña 1: Calculadora principal
  bslib::nav_panel(
    title = textOutput("tab_calc_title", inline = TRUE),
    value = "calc",
    mod_calculadora_ui("calc")
  ),

  # Pestaña 2: Factores modificables
  bslib::nav_panel(
    title = textOutput("tab_expl_title", inline = TRUE),
    value = "expl",
    mod_explicabilidad_ui("expl")
  ),

  # Pestaña 3: Guía de variables
  bslib::nav_panel(
    title = textOutput("tab_vars_title", inline = TRUE),
    value = "vars",
    mod_variables_ui("vars")
  ),

  # Pestaña 4: Información científica y cohorte
  bslib::nav_panel(
    title = textOutput("tab_info_title", inline = TRUE),
    value = "info",
    mod_info_ui("info")
  ),

  # Selector de idioma y modo claro/oscuro en el extremo del navbar
  bslib::nav_spacer(),
  bslib::nav_item(
    shinyWidgets::radioGroupButtons(
      inputId = "selected_language",
      label = NULL,
      choices = c("ES" = "es", "EN" = "en"),
      selected = "es",
      size = "sm",
      status = "outline-primary",
      checkIcon = list(
        yes = bsicons::bs_icon("check-lg")
      )
    )
  ),
  bslib::nav_item(
    bslib::input_dark_mode(id = "dark_mode")
  )
)
