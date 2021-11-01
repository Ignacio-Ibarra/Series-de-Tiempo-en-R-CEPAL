#### Herramientas para el Análisis de Series de Tiempo en R - Ejercitación 1 ####

# Procesamiento de datos y función de autocorrelación ----


## Carga de datos ----

##  Limpiar la memoria
rm(list=ls())

## Apertura de librerias
library(tidyverse)  #Manejo de bases de datos y otros
library(openxlsx)   #Lectura de xlsx
library(lubridate)  #Manejo de variables en formato fecha
library(zoo)        #Series de tiempo
library(forecast)   #Pronosticos
library(ggfortify)  #Graficos
library(seasonal)   #Descomposición X11 y otras

## Establecemos el directorio, recordar cambiar las \ por /
setwd("C:/Users/Garlan/Desktop/CEPAL/Curso Series")

options(scipen=9999) #Evita que los resultados se impriman en notación científica


tasas <- read.xlsx("Clase 1.xlsx", sheet = "Tasas")


#View(tasas)              #No recomendable si el df es muy pesado
head(tasas)
summary(tasas$tasa_polmon)
summary(tasas$prestamos_personales)

## Agregación y exportación ----

tasas$Fecha <- as.Date(tasas$Fecha, origin = "1899-12-30")
tasas$year <- year(tasas$Fecha)
tasas$quarter <- quarter(tasas$Fecha)
tasas$month <- month(tasas$Fecha)
tasas$day <- day(tasas$Fecha)

# Podríamos hacer esto más eficientemente usando la sintaxis de tidyverse

# tasas <- tasas %>% 
#   mutate(Fecha = as.Date(Fecha, origin = "1899-12-30"),
#          year = year(Fecha),
#          quarter = quarter(Fecha),
#          month = month(Fecha),
#          day = day(Fecha))

tasas_mensual <- tasas %>% 
  group_by(year,month) %>% 
  summarise(tasa_polmon = mean(tasa_polmon),
            prestamos_personales = mean(prestamos_personales)) %>% 
  dplyr::filter(year>2015) %>%
  ungroup() 

# Reconstruimos Fecha
tasas_mensual <- tasas_mensual %>% 
  mutate(Fecha = paste0(year,"-",month))

write.xlsx(tasas_mensual, "Tasas mensualizadas.xlsx", overwrite = TRUE)

## Cálculos y gráficos ----

tasas_mensual <- tasas_mensual %>% 
  mutate(dtasa_polmon_pb =round((tasa_polmon - dplyr::lag(tasa_polmon))*100,0),
         dprestamos_personales_pb=
           round((prestamos_personales-dplyr::lag(prestamos_personales))*100,0),
         prestamos_personales_ma=
           rollmean(prestamos_personales, k=3, fill = NA, align = "center"))

# tasas_mensual <- tasas_mensual %>%
#   mutate(tasa_polmon = ts(tasa_polmon, start=c(2016,1), frequency = 12),
#          prestamos_personales = ts(prestamos_personales, start=c(2016,1), frequency = 12),
#          dtasa_polmon_pb = ts(dtasa_polmon_pb, start=c(2016,1), frequency = 12),
#          dprestamos_personales_pb = ts(dprestamos_personales_pb, start=c(2016,1), frequency = 12),
#          prestamos_personales_ma = ts(prestamos_personales_ma, start=c(2016,1), frequency = 12))

tasas_mensual <- tasas_mensual %>% 
  mutate(across(everything(),.fns = ts, start=c(2016,1), frequency = 12))


attach(tasas_mensual)
autoplot(cbind(tasa_polmon,prestamos_personales), facets=FALSE)+
  ylab("TNA")+
  ggtitle("Tasas de interés")

## Función de autocorrelación y descomposición ----

#Variables en niveles
Acf(tasa_polmon) #Si no usamos attach, habría que especificar tasa_mensual$tasa_polmon
Acf(prestamos_personales)
#Variaciones
Acf(dtasa_polmon_pb, main = "Tasa de política monetaria (variación en pb)") #Título
Acf(dprestamos_personales_pb, main = "Tasa de préstamos personales (variación en pb)")

# Descomposición
prestamos_personales.x11 <- seas(prestamos_personales, x11="")
autoplot(prestamos_personales.x11)



