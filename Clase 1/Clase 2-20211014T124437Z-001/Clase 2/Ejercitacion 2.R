#### Herramientas para el Análisis de Series de Tiempo en R - Ejercitación 2 ####

##  Limpiar la memoria
rm(list=ls())

## Apertura de librerias
library(tidyverse)  #Manejo de bases de datos y otros
library(openxlsx)   #Lectura de xlsx
library(lubridate)  #Manejo de variables en formato fecha
library(zoo)        #Series de tiempo
library(forecast)   #Pronosticos
library(ggfortify)  #Graficos
library(kableExtra) #Formato tablas

## Establecemos el directorio, recordar cambiar las \ por /
setwd("C:/Users/Garlan/Desktop/CEPAL/Curso Series")

options(scipen=9999) #Evita que los resultados se impriman en notación científica

## Carga de datos ----

scn <- read.xlsx("Clase 2.xlsx",sheet = 'PIB') %>% 
  mutate(Fecha = as.Date(Fecha, origin = "1899-12-30"),
         year = year(Fecha),
         quarter = quarter(Fecha))
tcr <- read.xlsx("Clase 2.xlsx",sheet = 'TCR') %>% 
  mutate(Fecha = as.Date(Fecha, origin = "1899-12-30"),
         year = year(Fecha),
         quarter = quarter(Fecha)) %>% 
  group_by(year, quarter) %>% 
  summarise(TCR = mean(TCR)) %>% 
  ungroup() 

df <- scn %>% 
  left_join(tcr, by = c("year", "quarter"))


## Estimación ----

df <- df %>% 
  mutate(lPIB = log(PIB),
         lM = log(M),
         lTCR = log(TCR),
         across(everything(), .fns = ts, start = c(2004,1), frequency = 4))

attach(df)
autoplot(cbind(lPIB,lM), facets=TRUE)+
  labs(y = "", x = "Año",
       title = "PIB e importaciones de Argentina 2004-2021", 
       subtitle = "Series desestacionalizadas en niveles logarítmicos")

modelo1 <- tslm(lM ~ lPIB)          #Para escribir ~ podemos usar Alt+126
modelo1 <- tslm(log(M) ~ log(PIB))  #Idéntico a la linea anterior

modelo2 <- tslm(lM ~ lPIB + lTCR)

summary(modelo1)
summary(modelo2)

names(modelo2)
modelo2$coefficients
coef(modelo2)

names(summary(modelo1))
summary(modelo2)$coefficients
p_value_pib_mod2 <- summary(modelo2)$coefficients[2,4]
p_value_pib_mod2

CV(modelo1)
CV(modelo2)

bondad_ajuste <- data.frame(c(1,1),c(0,1)) %>% 
  rename("PIB"=1,"TCR"=2) %>% 
  cbind(rbind(CV(modelo1),CV(modelo2)))
row.names(bondad_ajuste) <- c("Modelo 1", "Modelo 2")

kable(bondad_ajuste, align=rep('c', 7)) %>%  #Centrar las 7 columnas
  kable_styling(bootstrap_options = c("striped", "hover"), #Formato de filas
                font_size = 14) #Tamaño de letra




