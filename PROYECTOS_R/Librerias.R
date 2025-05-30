# Funcion para cargar e instalar librerias en caso de que haga falta
load_libraries <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List your required packages here
required_packages <- c(
    "dplyr",
    "lubridate",
    "ggplot2",
    "plotly",
    "factoextra",
    "RColorBrewer",
    "readr",
    "cluster",
    "tidyr",
    "corrplot",
    "ggpubr",
    "reshape2",
    "visdat",
    "ggdendro",
    "readxl",
    "discretization",
    "dendextend",
    "recommenderlab",
    "rsparse",
    "data.table",
    "Matrix",
    "stringr",
    "shiny",
    "DT",
    "tibble",
    "tidyverse"
)

# Load all required packages
load_libraries(required_packages)
