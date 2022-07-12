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

#install.packages("rmarkdown")


# Parte 1: Importar Datos ----------------------------------------------------------

# Importamos el archivo desde el directorio donde se encuentra

getwd()
setwd("prueba2/dsci-prueba2/datasets") #Directorio donde tenemos el archivo

dirty_abrate <- read_excel("ds-abrate.xlsx")

dirty_wntdprg <- read_excel("ds-wntdprg.xlsx")

regions <- read.csv("all.csv")

names(regions)[names(regions) == 'name'] <- 'Country'


# Parte 2: Limpieza de datos -------------------------------------------------------

# Cambiar los nombres de los países

regions$Country <-  gsub('Congo, Democratic Republic of the', 'Democratic Republic of Congo', 
                            gsub('Hong Kong', 'Hong Kong (China)', 
                                 gsub('Micronesia (Federated States of)', 'Micronesia', 
                                      gsub('Moldova, Republic of', 'Republic of Moldova',
                                           gsub('Korea, Republic of', 'Republic of South Korea (South Korea)',
                                                gsub('Réunion', 'Reunion', 
                                                     gsub('Palestine, State of', 'State of Palestine', 
                                                          gsub('United Kingdom of Great Britain and Northern Ireland', 'United Kingdom', 
                                                               gsub('Tanzania, United Republic of', 'United Republic of Tanzania', 
                                                                    gsub('United States of America', 'United States', 
                                                                        regions$Country))))))))))



regions$Country[regions$Country == "Bolivia (Plurinational State of)"] <- "Bolivia"
regions$Country[regions$Country == "Korea (Democratic People's Republic of)"] <- "Democratic People's Republic of Korea (North Korea)"
regions$Country[regions$Country == "Iran (Islamic Republic of)"] <- "Iran"
regions$Country[regions$Country == "Micronesia (Federated States of)"] <- "Micronesia"
regions$Country[regions$Country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"

a = anti_join(dirty_abrate, regions)
b = anti_join(regions, dirty_abrate)
# Eliminar filas innecesarias dirty_abrate y dirty_wntdprg


# Cambiar u por NA en dirty_abrate y dirty_wntdprg
dirty_abrate[dirty_abrate == 'u'] <- NA
dirty_wntdprg[dirty_wntdprg == 'u'] <- NA

# Parte 3: Resumen y Graficos ------------------------------------------------------

# Gráfica de embarazos %rural vs %urbana no quieren evadir el embarazo Países o regiones
# Gráfica de embarazos %rural vs %urbana no quieren evadir el embarazo Países o regiones
# Abortion rate por regiones y entre regiones ver algunos países
# El porcentaje no deseados es muy grande, hay pa'ises que no tienen regulaciones
# Tasa de abortos no deseados

