####################################################################################
########         HF-PROGNOSIS: CONFIGURACIÓN GLOBAL Y MODELO PREDICTIVO      ########
####################################################################################

# 1. Carga de paquetes requeridos
source(file = "carga_paquetes.R")

# 2. Carga de funciones auxiliares y módulos de Shiny
archivos_r <- list.files(path = "R", pattern = "\\.[rR]$", full.names = TRUE)
for (archivo in archivos_r) {
  source(file = archivo)
}

# 3. Inicialización del servicio de internacionalización (shiny.i18n)
ruta_traducciones <- file.path("inst", "app", "translations")
i18n <- suppressWarnings(
  shiny.i18n::Translator$new(
    translation_csvs_path = ruta_traducciones
  )
)
i18n$set_translation_language("es")
options(app.translator = i18n, app.default.lang = "es")

# 3. Configuración del tema con Posit brand.yml y fallback robusto
brand_config <- if (file.exists("_brand.yml") && requireNamespace("brand.yml", quietly = TRUE)) {
  brand.yml::read_brand_yml("_brand.yml")
} else {
  NULL
}
options(app.brand = brand_config)

app_theme <- if (!is.null(brand_config)) {
  bslib::bs_theme(brand = TRUE)
} else {
  bslib::bs_theme(
    version = 5,
    primary = "#95a1c0",
    secondary = "#D0F0C0",
    success = "#10b981",
    info = "#0284c7",
    warning = "#f59e0b",
    danger = "#ef4444",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  )
}

# 4. Carga del modelo predictivo serializado
ruta_modelo <- file.path("data", "model_final.rds")
if (!file.exists(ruta_modelo)) {
  message(">> data/model_final.rds no encontrado. Ejecutando scripts/entrenar_modelo.R...")
  source(file = file.path("scripts", "entrenar_modelo.R"))
}
model <- readRDS(ruta_modelo)

# 5. Carga de datos para visualización de cohorte
ruta_datos_grafico <- file.path("www", "datos_grafico_para_APP_formato.csv")
datos_grafico <- read.csv(
  file = ruta_datos_grafico,
  header = TRUE,
  sep = ",",
  dec = ".",
  na.strings = c("", "NA")
)