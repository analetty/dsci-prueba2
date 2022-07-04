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
#setwd("prueba2/dsci-prueba2/datasets") #Directorio donde tenemos el archivo

dirty_abpolicies <- read_excel("ds-abpolicies.xlsx")

dirty_abrate <- read_excel("ds-abrate.xlsx")

dirty_wntdprg <- read_excel("ds-wntdprg.xlsx")

names(dirty_abpolicies)[names(dirty_abpolicies) == 'Member State (States/Entities)'] <- 'Country'

a = anti_join(dirty_abpolicies, dirty_abrate)



# Parte 2: Limpieza de datos -------------------------------------------------------

# Paso 1 identificar los países que estén diferentes en dirty_abpolicies y 
# ponerlos como en los otros ds

# Definir con qué observaciones nos vamos a quedar de dirty_abpolicies

# Eliminar filas innecesarias dirty_abrate y dirty_wntdprg

# Cambiar u por NA en dirty_abrate y dirty_wntdprg
dirty_abrate[dirty_abrate == 'u'] <- NA
dirty_wntdprg[dirty_wntdprg == 'u'] <- NA

# Parte 3: Resumen y Graficos ------------------------------------------------------
