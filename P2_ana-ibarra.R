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
library(ggthemes)

# Parte 1: Importar Datos ----------------------------------------------------------

# Cambiamos al directorio donde tenemos los datasets

getwd()
setwd("prueba2/dsci-prueba2/datasets") #Directorio donde tenemos el archivo

# Importamos los datasets a usar

dirty_abrate <- read_excel("ds-abrate.xlsx")

dirty_wntdprg <- read_excel("ds-wntdprg.xlsx")

regions <- read.csv("all.csv")

# Renombramos la columna de nombre a Country
names(regions)[names(regions) == 'name'] <- 'Country'


# Parte 2: Limpieza de datos -------------------------------------------------------

# Se desea unir la base de datos regions con las de embarazos y abortos por paises
# Sin embargo, notamos que los paises tienen nombres diferentes. 

# Se modifican los paises en la base de datos regions para que coincidan

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


# Eliminar filas innecesarias dirty_abrate y dirty_wntdprg y renombramos las columnas
dirty_abrate <- dirty_abrate[,1:3] %>% 
  rename('avg_perc_preg_eia' = 'Average annual % of all pregnancies ending in abortion, 2015-2019', 
         'avg_perc_uw_preg_eia' = 'Average annual % of unintended pregnancies ending in abortion, 2015-2019')

dirty_wntdprg <- dirty_wntdprg[,c(1:5, 12:15)] %>% 
  rename('perc_rural_w_nwap' = '% of rural women aged 15–49 not wanting to avoid pregnancy, 2019 [1]',
         'perc_rural_w_wap' = '% of rural women aged 15–49 who want to avoid pregnancy, 2019 [2]', 
         'perc_urban_w_nwap' = '% of urban women aged 15–49 not wanting to avoid pregnancy, 2019 [1]', 
         'perc_urban_w_wap' = '% of urban women aged 15–49 who want to avoid pregnancy, 2019 [2]',
         'perc_poor_w_nwap' = 'Among women aged 15–49 in the poorest wealth quintile, % not wanting to avoid pregnancy, 2019 [1]', 
         'perc_poor_w_wap' = 'Among women aged 15–49 in the poorest wealth quintile, % who want to avoid pregnancy, 2019 [2]', 
         'perc_rich_w_nwap' = 'Among women aged 15–49 in the richest wealth quintile, % not wanting to avoid pregnancy, 2019 [1]', 
         'perc_rich_w_wap' = 'Among women aged 15–49 in the richest wealth quintile, % who want to avoid pregnancy, 2019 [2]')

# Cambiar u por NA en dirty_abrate y dirty_wntdprg
dirty_abrate[dirty_abrate == 'u'] <- NA
dirty_wntdprg[dirty_wntdprg == 'u'] <- NA

# Subset de países con valores NA 
na_ab_rate <- dirty_abrate[!complete.cases(dirty_abrate),] 
na_ab_rate <- na_ab_rate[-(26:28),]


na_wntd_prg <- dirty_wntdprg[!complete.cases(dirty_wntdprg),]
na_wntd_prg <- na_wntd_prg[-(44:46),]


# Transformamos los valores a numericos
dirty_wntdprg <- dirty_wntdprg %>% 
  transform(perc_rural_w_nwap = as.numeric(perc_rural_w_nwap),
            perc_rural_w_wap = as.numeric(perc_rural_w_wap),
            perc_urban_w_nwap = as.numeric(perc_urban_w_nwap),
            perc_urban_w_wap = as.numeric(perc_urban_w_wap),
            perc_poor_w_nwap = as.numeric(perc_poor_w_nwap),
            perc_poor_w_wap = as.numeric(perc_poor_w_wap),
            perc_rich_w_nwap = as.numeric(perc_rich_w_nwap),
            perc_rich_w_wap = as.numeric(perc_rich_w_wap))

dirty_abrate <- dirty_abrate %>% 
  transform(avg_perc_preg_eia = as.numeric(avg_perc_preg_eia),
            avg_perc_uw_preg_eia = as.numeric(avg_perc_uw_preg_eia))

# Bases de datos con casos completos 
ab_rate <-  dirty_abrate[complete.cases(dirty_abrate),]
wntd_prg <- dirty_wntdprg[complete.cases(dirty_wntdprg),]
regions <- regions %>% select(Country, region)

# Obtenemos las bases de datos a usar limpias, uniendo por regiones
ab_data <- inner_join(ab_rate, regions)
preg_data <- inner_join(wntd_prg, regions)

# Exportamos las bases de datos a archivos .csv
write.csv(ab_data,"ab_data.csv", row.names = FALSE)
write.csv(preg_data,"preg_data.csv", row.names = FALSE)
write.csv(regions,"regions.csv", row.names = FALSE)


# Parte 3: Resumen y Graficos ------------------------------------------------------

# Gráfica de barra % embarazos deseados por región en áreas rurales
preg_data %>% 
  select(Country, region, perc_rural_w_nwap, perc_rural_w_wap) %>% 
  group_by(region) %>% 
  summarise(perc_rural_reg = mean(perc_rural_w_wap)) %>% 
  ggplot( aes(x = region, y= perc_rural_reg,fill=region)) + 
  geom_bar(stat='identity') +
  labs(title="Porcentaje de embarazos deseados por región en áreas rurales", 
       x = "Región", y ="Porcentaje de embarazos") +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfica de barra % embarazos no deseados por región en áreas rurales

preg_data %>% 
  select(Country, region, perc_rural_w_nwap, perc_rural_w_wap) %>% 
  group_by(region) %>% 
  summarise(perc_rural_reg = mean(perc_rural_w_nwap)) %>% 
  ggplot( aes(x = region, y= perc_rural_reg,fill=region)) + 
  geom_bar(stat='identity') +
  labs(title="Porcentaje de embarazos no deseados por región en áreas rurales", 
       x = "Región", y ="Porcentaje de embarazos no deseados") +
  theme(legend.position = "none")

# Gráfica de barra % embarazos deseados por región en áreas urbanas
preg_data %>% 
  select(Country, region, perc_urban_w_nwap, perc_urban_w_wap) %>% 
  group_by(region) %>% 
  summarise(perc_urban_reg = mean(perc_urban_w_wap)) %>% 
  ggplot( aes(x = region, y= perc_urban_reg,fill=region)) + 
  geom_bar(stat='identity') +
  labs(title="Porcentaje de embarazos deseados por región en áreas urbanas", 
       x = "Región", y ="Porcentaje de embarazos") +
  theme(legend.position = "none")

# Gráfica de barra % embarazos no deseados por región en áreas urbanas

preg_data %>% 
  select(Country, region, perc_urban_w_nwap, perc_urban_w_wap) %>% 
  group_by(region) %>% 
  summarise(perc_urban_reg = mean(perc_urban_w_nwap)) %>% 
  ggplot( aes(x = region, y= perc_urban_reg,fill=region)) + 
  geom_bar(stat='identity') +
  labs(title="Porcentaje de embarazos no deseados por región en áreas urbanas", 
       x = "Región", y ="Porcentaje de embarazos") +
  theme(legend.position = "none")

# Gráfico de tasa de embarazos que terminan en aborto por región
ab_data %>% 
  select(Country, region, avg_perc_preg_eia, avg_perc_uw_preg_eia) %>% 
  group_by(region) %>% 
  summarise(perc_urban_reg = mean(avg_perc_preg_eia)) %>% 
  ggplot( aes(x = region, y= perc_urban_reg, fill=region)) + 
  geom_bar(stat='identity') +
  labs(title="Tasa de embarazos que terminan en aborto por región", 
       x = "Región", y ="Porcentaje de embarazos") +
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5))

# Gráfico de tasa de embarazos no deseados que terminan en aborto por región

ab_data %>% 
  select(Country, region, avg_perc_preg_eia, avg_perc_uw_preg_eia) %>% 
  group_by(region) %>% 
  summarise(perc_urban_reg = mean(avg_perc_uw_preg_eia)) %>% 
  ggplot( aes(x = region, y= perc_urban_reg, fill=region)) + 
  geom_bar(stat='identity') +
  labs(title="Tasa embarazos no deseados que terminan en aborto por región", 
       x = "Región", y ="Porcentaje de embarazos") +
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5))

# Comparación entre embarazos no deseados áreas urbanas vs rurales por región

longer_urban_rural_nwap <- preg_data %>% 
  select(Country, region, perc_urban_w_wap, perc_rural_w_wap)  %>% 
  pivot_longer(cols = c("perc_urban_w_wap", "perc_rural_w_wap"),
               names_to = "series" ,
               values_to = "value")  


urban_rural_nwap <- longer_urban_rural_nwap %>% 
  mutate(region = reorder(region, value, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(hjust = 0.5), 
        plot.title=element_text(hjust=0.5)) +
  xlab("") 

  urban_rural_nwap + geom_boxplot(aes(region, value, fill = factor(series))) + 
    labs(title = "Tasa de mujeres que desean evitar el embarazo: Áreas rurales vs Urbanas", 
         y = "Porcentaje de mujeres") + 
    guides(fill=guide_legend(title="Área")) + 
    scale_fill_discrete(labels=c('Rural', 'Urbana')) 

# Comparación entre embarazos no deseados percentil rico vs pobre
  
  longer_rich_poor_nwap <- preg_data %>% 
    select(Country, region, perc_poor_w_wap, perc_rich_w_wap)  %>% 
    pivot_longer(cols = c("perc_poor_w_wap", "perc_rich_w_wap"),
                 names_to = "series" ,
                 values_to = "value")  
  
  
  rich_poor_nwap <- longer_rich_poor_nwap %>% 
    mutate(region = reorder(region, value, FUN = median)) %>%
    ggplot() +
    theme(axis.text.x = element_text(hjust = 0.5), 
          plot.title=element_text(hjust=0.5)) +
    xlab("") 
  
  rich_poor_nwap + geom_boxplot(aes(region, value, fill = factor(series))) + 
    labs(title = "Tasa de mujeres que desean evitar el embarazo: Percentil ingresos altos vs bajos", 
         y = "Porcentaje de mujeres") + 
    guides(fill=guide_legend(title="Percentil de ingresos")) + 
    scale_fill_discrete(labels=c('Bajo', 'Alto')) 
  
