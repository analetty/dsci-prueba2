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
#setwd("prueba2/dsci-prueba2/datasets") #Directorio donde tenemos el archivo

dirty_abpolicies <- read_excel("ds-abpolicies.xlsx")

dirty_abrate <- read_excel("ds-abrate.xlsx")

dirty_wntdprg <- read_excel("ds-wntdprg.xlsx")

names(dirty_abpolicies)[names(dirty_abpolicies) == 'Member State (States/Entities)'] <- 'Country'

a = anti_join(dirty_abpolicies, dirty_abrate)


# Parte 2: Limpieza de datos -------------------------------------------------------

# Paso 1 identificar los países que estén diferentes en dirty_abpolicies y 
# ponerlos como en los otros ds

prueba = c(
  "Australia",
  "Bosnia and Herzegovina",
  "Canada",
  "Congo",
  "Côte d'Ivoire",
  "Hong Kong (China)",
  "India",
  "Iran",
  "Mexico",
  "Micronesia",
  "Nigeria",
  "Pakistan",
  "Republic of South Korea (South Korea)",
  "Switzerland",
  "United Kingdom",
  "United States",
  "Venezuela",
  "Viet Nam",
  a$Country
)

print(prueba)



a <- a %>% separate(Country, c('Country', 'State'), sep=" -")

a$Country <- gsub('Congo (Republic of the)', 'Congo',
                  gsub("Côte d’Ivoire", "Côte d'Ivoire",
                       gsub('Hong Kong', 'Hong Kong (China)', 
                            gsub('Iran (Islamic Republic of)', 'Iran', 
                                 gsub('Micronesia (Federated States of)', 'Micronesia', 
                                      gsub('Republic of Korea', 'Republic of South Korea (South Korea)',
                                           gsub('United Kingdom of Great Britain and Northern Ireland', 'United Kingdom',
                                                gsub('United States of America', 'United States', 
                                                     gsub('Venezuela (Bolivarian Republic of)', 'Venezuela', 
                                                          gsub('Vietnam', 'Viet Nam', 
                                                               a$Country))))))))))

b = anti_join(a, dirty_abrate)




# Definir con qué observaciones nos vamos a quedar de dirty_abpolicies

# Eliminar filas innecesarias dirty_abrate y dirty_wntdprg
# Create sample data frame.
id <- c(rep('Participant 1', 2), rep('Participant 2', 2))
value1 <- rep('A', 4)
value2 <- rep('B', 4)
value3 <- rep('C', 4)
value4 <- c('x', NA, NA, 'x')
value5 <- c('x', NA, 'x', NA)
value6 <- c(NA, 'x', NA, 'x')

df <- data.frame(id, value1, value2, value3, value4, value5, value6, stringsAsFactors = F)

# Use dplyr to group the data and keep the non-NA value from the other columns.
df %>% group_by(id, value1, value2, value3) %>%
  summarise(value4 = max(value4, na.rm = T),
            value5 = max(value5, na.rm = T),
            value6 = max(value6, na.rm = T))


# Cambiar u por NA en dirty_abrate y dirty_wntdprg
dirty_abrate[dirty_abrate == 'u'] <- NA
dirty_wntdprg[dirty_wntdprg == 'u'] <- NA

# Parte 3: Resumen y Graficos ------------------------------------------------------
