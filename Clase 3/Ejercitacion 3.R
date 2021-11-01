#### Herramientas para el Análisis de Series de Tiempo en R - Ejercitación 3 ####

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
# install.packages("fUnitRoots")
library(fUnitRoots) #adfTest y otros

## Establecemos el directorio
setwd("~/Escritorio/CEP/CursoCEPAL/Series de Tiempo en R - CEPAL")

options(scipen=9999) #Evita que los resultados se impriman en notación científica

## Primeros pasos ----

tasas <- read.xlsx("./Clase 3/Clase 3.xlsx", sheet="tasas_mensual")
head(tasas)

class(tasas$tasa_polmon) #Las variables son numéricas
tasas <- tasas %>% 
  mutate(tasa_polmon = ts(tasa_polmon, start = c(2016,1),frequency = 12),
         prestamos_personales =ts(prestamos_personales,start=c(2016,1),frequency=12))
class(tasas$tasa_polmon) #Ahora son time series

attach(tasas)
autoplot(cbind(tasa_polmon,prestamos_personales))+
  ylab("TNA")+
  ggtitle("Tasas de interés")

tasas <- tasas %>% 
  dplyr::filter(year<2020) %>% 
  mutate(across(everything(), .fns = ts, start = c(2016,1), frequency = 12))

detach(tasas)
attach(tasas)
autoplot(cbind(tasa_polmon,prestamos_personales))+
  ylab("TNA")+
  ggtitle("Tasas de interés a dic-2019")

## Estacionariedad de las series ----

Acf(tasa_polmon)
Acf(prestamos_personales)

adfTest(tasa_polmon, type="c") #Constant
adfTest(prestamos_personales, type="c")
adfTest(tasa_polmon, type="ct") #Constant & trend
adfTest(prestamos_personales, type="ct")

dtasa_polmon <-  tasa_polmon - stats::lag(tasa_polmon,-1)
autoplot(dtasa_polmon)
Acf(dtasa_polmon)
adfTest(dtasa_polmon,type = "nc") #No constant

dprestamos_personales <- prestamos_personales - stats::lag(prestamos_personales,-1)
autoplot(dprestamos_personales)
Acf(dprestamos_personales)
adfTest(dprestamos_personales, type="nc")


## Cointegracion ----

modelo1 <- tslm(prestamos_personales ~ tasa_polmon)
res <- ts(modelo1$res, start = c(2016,1), frequency = 12)
tsdisplay(res)
adfTest(res, type = "nc")

## Estimación del MCE ----

mlp <- tslm(prestamos_personales ~ tasa_polmon)
L.res <- ts(stats::lag(res,-1)[-48], start = c(2016,2), frequency=12)
mce <- tslm(dprestamos_personales ~ dtasa_polmon + L.res)
summary(mlp)
summary(mce)

res_mce <- ts(mce$res, start = c(2016,2), frequency = 12)
tsdisplay(res_mce)


L.dprestamos_personales <- ts(c(NA,stats::lag(dprestamos_personales,-1)[-47]),
                              start = c(2016,3), frequency=12)
mce <- tslm(dprestamos_personales ~ dtasa_polmon+L.res+L.dprestamos_personales)
summary(mce)

res_mce <- ts(mce$res, start = c(2016,2), frequency = 12)
tsdisplay(res_mce)

lp <- mlp$coefficients
ce <- mce$coefficients

tabla <- data.frame(c(lp,"",""), ce) %>%  #Espacios vacíos para completar la columna
  rename("Modelo LP"=1,"MCE"=2)           #Nombres columnas
rownames(tabla) <-  c("Intercepto",       #Nombres filas
                      "Tasa polmon",
                      "Corrección de errores",
                      "Autorregresivo")  

kbl(tabla, align=rep('c', 3)) %>%  #Centrar las 7 columnas
  kable_styling(bootstrap_options = c("striped", "hover"), #Formato de filas
                font_size = 14) #Tamaño de letra

## Pronostico ----

tasa_polmon <- ts(c(tasa_polmon,rep(NA,6)),start = c(2016,1),frequency=12)
prestamos_personales <- ts(c(prestamos_personales,rep(NA,6)),
                           start= c(2016,1),frequency=12)
res <- ts(c(res,rep(NA,6)), start = c(2016,1), frequency = 12)
prestamos_personales_lp <- c(rep(NA,54))

tasa_polmon[49:54] <- 38      #Shock: fija en 38.
for (t in 1:48) {
  prestamos_personales_lp[t] <- lp[1]+lp[2]*tasa_polmon[t]
}

for (t in 49:54) {
  prestamos_personales[t] <- prestamos_personales[t-1]+ce[1]+
    ce[2]*(tasa_polmon[t]-tasa_polmon[t-1])+ce[3]*res[t-1]+
    ce[4]*(prestamos_personales[t-1]-prestamos_personales[t-2])
  
  prestamos_personales_lp[t] <- lp[1]+lp[2]*tasa_polmon[t] #Relacion de LP
  
  res[t] <- prestamos_personales[t] - prestamos_personales_lp[t] #Res pronosticado
}

tasas_efectivas <- read.xlsx("./Clase 3/Clase 3.xlsx", sheet="tasas_mensual")
pp_efectiva <- ts(tasas_efectivas$prestamos_personales[1:54],
                  start = c(2016,1), frequency = 12)

autoplot(cbind(tasa_polmon,prestamos_personales,prestamos_personales_lp,pp_efectiva))+
  geom_vline(xintercept = 2019.92, linetype="dashed")+ #2019.92 = dic-19
  ylab("TNA")+
  ggtitle("Tasas de interés: MCE vs tasa efectiva")

## Otras posibilidades para la exógena

# Cayendo 30pb por mes
for (t in 49:54) {
  tasa_polmon[t] <- tasa_polmon[t-1] - 0.3
}

# Autorregresiva
modelo_univariado <- ar(tasa_polmon, method = 'ols')

mu <- c(modelo_univariado$x.intercept, modelo_univariado$ar)

for (t in 4:54) {
  tasa_polmon[t] <- mu[1]+mu[2]*tasa_polmon[t-1]+
    mu[3]*tasa_polmon[t-2]+mu[4]*tasa_polmon[t-3]
}







