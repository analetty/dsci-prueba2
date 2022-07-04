################################################
####              Prueba 2                  ####
####           Ana Leticia Ibarra           ####
####           Ana Gabriela Ibarra          ####
################################################

# Objetivo: 

# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggplot2)


# Parte 1: Importar Datos ----------------------------------------------------------

# Importamos el archivo desde el directorio donde se encuentra

getwd()
#setwd("prueba2") #Directorio donde tenemos el archivo
messy_data <- read_excel("ds-abpolicies.xlsx")
messy_data2 <- read_excel("ds-abrate.xlsx")
messy_data3 <- read_excel("ds-wntdprg.xlsx")



# Parte 2: Limpieza de datos -------------------------------------------------------

# Parte 3: Resumen y Graficos ------------------------------------------------------
