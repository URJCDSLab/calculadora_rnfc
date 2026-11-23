#' @title Entrenamiento y Serialización del Modelo Predictivo (HF-Prognosis)
#' @description Script para entrenar el modelo de regresión logística binaria
#'   de recuperación de marcha al mes de la intervención de fractura de cadera
#'   según los criterios del Registro Nacional de Fractura de Cadera (RNFC).
#'   Genera el archivo serializado `data/model_final.rds` y un conjunto de
#'   predicciones de referencia `data/baseline_predictions.rds` para pruebas de regresión.
#'
#' @author Antigravity / Data Science Lab URJC
#' @details
#' El modelo utiliza una muestra del 20% estratificada por el desenlace primario
#' (`Recuperacion_mismo_grado_aproximado_marcha`) con semilla fija 3456.
#'
#' @examples
#' \dontrun{
#'   # Ejecutar desde la raíz del proyecto:
#'   source("scripts/entrenar_modelo.R")
#' }

suppressPackageStartupMessages({
  library(dplyr)
  library(caret)
})

message(">> [1/4] Leyendo datos crudos desde www/datos_sin_na.csv...")
ruta_datos <- file.path("www", "datos_sin_na.csv")
if (!file.exists(ruta_datos)) {
  stop("No se encuentra el archivo de datos: ", ruta_datos)
}

datos_csv <- read.csv(
  file = ruta_datos,
  header = TRUE,
  sep = ",",
  dec = ".",
  na.strings = c("", "NA")
)

message(">> [2/4] Preprocesando variables clínicas...")
preprocesado_datos <- function(datos_csv) {
  datos <- datos_csv %>%
    mutate(
      Edad = as.numeric(Edad),
      Sexo = factor(Sexo),
      Movilidad_basal_FAC = factor(Movilidad_basal_FAC),
      Recuperacion_mismo_grado_aproximado_marcha = factor(Recuperacion_mismo_grado_aproximado_marcha),
      Residencia_previa = factor(Residencia_previa),
      Demencia_NA = factor(Demencia_NA),
      ASA_I_IV_NA = factor(ASA_I_IV_NA),
      Tipo_fractura_3cat = factor(Tipo_fractura_3cat),
      Tipo_cirugia = factor(Tipo_cirugia),
      Bloqueoperif_NA = factor(Bloqueoperif_NA),
      Demora_quirurgica_en_horas = as.numeric(Demora_quirurgica_en_horas),
      Demora_quirurgica_cat = factor(Demora_quirurgica_cat),
      Sedestacion_precoz = factor(Sedestacion_precoz),
      Descarga_NA = factor(Descarga_NA),
      Ulceras_por_presion_NA = factor(Ulceras_por_presion_NA),
      Ubicacion_al_alta = factor(Ubicacion_al_alta),
      Traslado_URF = factor(Traslado_URF)
    )

  # Agrupaciones clínicas originales del estudio
  # Tipo_fractura_3cat agrupada: 0 = {0,1} (pertrocanterea o subcapital), 1 = {2} (subcanterea)
  datos$Tipo_fractura_3cat_agrup <- datos$Tipo_fractura_3cat
  datos$Tipo_fractura_3cat_agrup[datos$Tipo_fractura_3cat == 1] <- 0
  datos$Tipo_fractura_3cat_agrup[datos$Tipo_fractura_3cat == 2] <- 1
  datos$Tipo_fractura_3cat_agrup <- factor(as.numeric(datos$Tipo_fractura_3cat_agrup) - 1)

  # Descarga_NA agrupada: 0 = {0,9} (carga autorizada o desconocida), 1 = {1} (descarga)
  datos$Descarga_NA_agrup <- datos$Descarga_NA
  datos$Descarga_NA_agrup[datos$Descarga_NA == 9] <- 0
  datos$Descarga_NA_agrup <- factor(as.numeric(datos$Descarga_NA_agrup) - 1)

  # Ulceras_por_presion_NA agrupada: 0 = {0,9} (no o desconocido), 1 = {1} (si)
  datos$Ulceras_por_presion_NA_agrup <- datos$Ulceras_por_presion_NA
  datos$Ulceras_por_presion_NA_agrup[datos$Ulceras_por_presion_NA == 9] <- 0
  datos$Ulceras_por_presion_NA_agrup <- factor(as.numeric(datos$Ulceras_por_presion_NA_agrup) - 1)

  # FAC 1-3 // FAC 4-5: 0 = {0,1} (FAC 4-5 independiente), 1 = {2,3,4} (FAC 1-3 dependiente)
  datos$Movilidad_basal_FAC_agrup <- datos$Movilidad_basal_FAC
  datos$Movilidad_basal_FAC_agrup[datos$Movilidad_basal_FAC == 1] <- 0
  datos$Movilidad_basal_FAC_agrup[datos$Movilidad_basal_FAC == 2] <- 1
  datos$Movilidad_basal_FAC_agrup[datos$Movilidad_basal_FAC == 3] <- 1
  datos$Movilidad_basal_FAC_agrup[datos$Movilidad_basal_FAC == 4] <- 1
  datos$Movilidad_basal_FAC_agrup <- factor(as.numeric(datos$Movilidad_basal_FAC_agrup) - 1)

  # Selección final de variables involucradas en el modelo
  datos <- datos %>%
    select(
      Recuperacion_mismo_grado_aproximado_marcha,
      Edad,
      Movilidad_basal_FAC_agrup,
      Demencia_NA,
      ASA_I_IV_NA,
      Tipo_fractura_3cat_agrup,
      Demora_quirurgica_cat,
      Sedestacion_precoz,
      Descarga_NA_agrup,
      Ulceras_por_presion_NA_agrup,
      Ubicacion_al_alta
    )

  return(datos)
}

datos <- preprocesado_datos(datos_csv)

message(">> [3/4] Muestreando partición train (20%) y ajustando modelo glm...")
set.seed(3456)
trainIndex <- caret::createDataPartition(
  datos$Recuperacion_mismo_grado_aproximado_marcha,
  p = 0.2,
  list = FALSE,
  times = 1
)

datos_train <- datos[trainIndex, ]

model <- glm(
  Recuperacion_mismo_grado_aproximado_marcha ~
    Edad +
    Movilidad_basal_FAC_agrup +
    Demencia_NA +
    ASA_I_IV_NA +
    Tipo_fractura_3cat_agrup +
    Demora_quirurgica_cat +
    Sedestacion_precoz +
    Descarga_NA_agrup +
    Ulceras_por_presion_NA_agrup +
    Ubicacion_al_alta,
  data = datos_train,
  family = binomial(link = "logit")
)

# Guardar modelo serializado en data/
if (!dir.exists("data")) dir.create("data", recursive = TRUE)
ruta_modelo <- file.path("data", "model_final.rds")
saveRDS(model, file = ruta_modelo)
message("   -> Modelo guardado exitosamente en: ", ruta_modelo)

message(">> [4/4] Generando pacientes benchmark y guardando predicciones de referencia...")

# Función de traducción idéntica a la original para validar benchmarks
traduccion_benchmark <- function(variable, input) {
  if (input == "Dependiente (FAC 1-3)") valor <- 1
  else if (input == "Independiente (FAC 4-5)") valor <- 0
  else if (input == "Sí") {
    if (variable == "Demencia_NA") valor <- 1
    else if (variable == "Sedestacion_precoz") valor <- 0
    else if (variable == "Ulceras_por_presion_NA_agrup") valor <- 1
  } else if (input == "No") {
    if (variable == "Demencia_NA") valor <- 0
    else if (variable == "Sedestacion_precoz") valor <- 1
  } else if (input == "Desconocido") {
    if (variable %in% c("Demencia_NA", "ASA_I_IV_NA")) valor <- 9
  } else if (input == "I") valor <- 3
  else if (input == "II") valor <- 2
  else if (input == "III") valor <- 1
  else if (input == "IV-V") valor <- 4
  else if (input == "Menor o igual que 24h") valor <- 2
  else if (input == "Mayor que 24h") valor <- 1
  else if (input == "Domicilio") valor <- 1
  else if (input == "Residencia") valor <- 2
  else if (input == "Unidad de Recuperación Funcional") valor <- 3
  else if (input == "Otros") valor <- 4
  else if (input == "Subtrocantérea") valor <- 1
  else if (input == "Pertrocantérea o subcapital") valor <- 0
  else if (input == "Carga autorizada o desconocido") valor <- 0
  else if (input == "Descarga") valor <- 1
  else if (input == "No o desconocido") valor <- 0
  return(valor)
}

crear_paciente_df <- function(Edad, Movilidad, Demencia, ASA, TipoFrac, Demora, Sedestacion, Descarga, Ulceras, Ubicacion) {
  df <- data.frame(
    Edad = as.numeric(Edad),
    Movilidad_basal_FAC_agrup = factor(traduccion_benchmark("Movilidad_basal_FAC_agrup", Movilidad)),
    Demencia_NA = factor(traduccion_benchmark("Demencia_NA", Demencia)),
    ASA_I_IV_NA = factor(traduccion_benchmark("ASA_I_IV_NA", ASA)),
    Tipo_fractura_3cat_agrup = factor(traduccion_benchmark("Tipo_fractura_3cat_agrup", TipoFrac)),
    Demora_quirurgica_cat = factor(traduccion_benchmark("Demora_quirurgica_cat", Demora)),
    Sedestacion_precoz = factor(traduccion_benchmark("Sedestacion_precoz", Sedestacion)),
    Descarga_NA_agrup = factor(traduccion_benchmark("Descarga_NA_agrup", Descarga)),
    Ulceras_por_presion_NA_agrup = factor(traduccion_benchmark("Ulceras_por_presion_NA_agrup", Ulceras)),
    Ubicacion_al_alta = factor(traduccion_benchmark("Ubicacion_al_alta", Ubicacion))
  )
  return(df)
}

# 5 casos de prueba diversos:
casos_prueba <- list(
  caso1 = crear_paciente_df(80, "Independiente (FAC 4-5)", "No", "II", "Pertrocantérea o subcapital", "Menor o igual que 24h", "Sí", "Carga autorizada o desconocido", "No o desconocido", "Domicilio"),
  caso2 = crear_paciente_df(88, "Dependiente (FAC 1-3)", "Sí", "III", "Pertrocantérea o subcapital", "Mayor que 24h", "No", "Descarga", "Sí", "Residencia"),
  caso3 = crear_paciente_df(75, "Independiente (FAC 4-5)", "No", "I", "Subtrocantérea", "Menor o igual que 24h", "Sí", "Carga autorizada o desconocido", "No o desconocido", "Domicilio"),
  caso4 = crear_paciente_df(95, "Dependiente (FAC 1-3)", "Sí", "IV-V", "Subtrocantérea", "Mayor que 24h", "No", "Descarga", "Sí", "Otros"),
  caso5 = crear_paciente_df(83, "Independiente (FAC 4-5)", "Desconocido", "Desconocido", "Pertrocantérea o subcapital", "Menor o igual que 24h", "Sí", "Carga autorizada o desconocido", "No o desconocido", "Unidad de Recuperación Funcional")
)

baseline_preds <- lapply(casos_prueba, function(p) {
  preds <- predict(model, newdata = p, type = "response", se.fit = TRUE)
  list(prob = round(preds$fit, 2), ic = round(preds$se.fit * 1.645, 2))
})

ruta_baseline <- file.path("data", "baseline_predictions.rds")
saveRDS(baseline_preds, file = ruta_baseline)
message("   -> Predicciones baseline guardadas en: ", ruta_baseline)
print(baseline_preds)
message(">> [OK] Proceso de entrenamiento finalizado con exito.")
