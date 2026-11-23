####################################################################################
########              CARGA DE PAQUETES (MODERNIZADO PARA SHINY/BSLIB)       ########
####################################################################################

# Nota: Para compatibilidad con shinyapps.io y despliegues productivos,
# las dependencias se gestionan de forma declarativa sin llamadas a install.packages().

library(shiny)
library(bslib)
library(shinyWidgets)
library(bsicons)
library(plotly)
library(ggplot2)
library(dplyr)
library(shiny.i18n)

# Soporte opcional para brand.yml si está instalado en el entorno
if (requireNamespace("brand.yml", quietly = TRUE)) {
  suppressPackageStartupMessages(library(brand.yml))
}
