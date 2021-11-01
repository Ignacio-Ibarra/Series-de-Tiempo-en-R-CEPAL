#### Herramientas para el Análisis de Series de Tiempo en R - Ejercitación 4 ####

##  Limpiar la memoria
rm(list=ls())

## Apertura de librerias
library(tidyverse)  #Manejo de bases de datos y otros
library(openxlsx)   #Lectura de xlsx
library(lubridate)  #Manejo de variables en formato fecha
library(zoo)        #Series de tiempo
library(forecast)   #Pronosticos
library(ggfortify)  #Graficos
library(fUnitRoots) #adfTest y otros
library(kableExtra) #Formato tablas
library(vars)       #Modelos VAR

## Establecemos el directorio
setwd("C:/Users/Garlan/Desktop/CEPAL/Curso Series")


options(scipen=9999) #Evita que los resultados se impriman en notación científica


## Estimación VAR ----

tasas <- read.xlsx("Clase 4.xlsx", sheet="tasas_mensual") %>% 
  dplyr::filter(year<2020) %>% 
  mutate(across(everything(), .fns = ts, start = c(2016,1), frequency = 12))
attach(tasas)


dtasa_polmon <-  tasa_polmon - stats::lag(tasa_polmon,-1)
dprestamos_personales <-  prestamos_personales - stats::lag(prestamos_personales,-1)

autoplot(cbind(dtasa_polmon,dprestamos_personales), facets=TRUE)+
  labs(x="Período", y="")


VARselect(cbind(dtasa_polmon,dprestamos_personales), type="const")[["selection"]]

var <- VAR(cbind(dtasa_polmon,dprestamos_personales), p=1, type="const")
summary(var)


class(coef(var))
coef_polmon <- coef(var)[[1]]
coef_prestamos <- coef(var)[[2]]
coef_polmon

autoplot(forecast(var, h=6)) + xlab("Year")


forecast(var)$forecast
dtasa_polmon_p <- forecast(var)$forecast$dtasa_polmon$mean
dprestamos_personales_p <- forecast(var)$forecast$dprestamos_personales$mean
dtasa_polmon_p


dtasa_polmon_p <- c(dtasa_polmon, dtasa_polmon_p)
dprestamos_personales_p <- c(dprestamos_personales, dprestamos_personales_p)

tasa_polmon_p <- tasa_polmon
prestamos_personales_p <- prestamos_personales


for (t in 49:54) {
  tasa_polmon_p <- c(tasa_polmon_p,
                     tasa_polmon_p[t-1]+dtasa_polmon_p[t])
  prestamos_personales_p <- c(prestamos_personales_p,
                              prestamos_personales_p[t-1]+dprestamos_personales_p[t])
}

# Otra opción es calcular manualmente el pronóstico mediante los coeficientes

# for (t in 49:54) {
#   tasa_polmon_p <- c(tasa_polmon_p,
#                      tasa_polmon_p[t-1]+
#                        coef_polmon[3]+
#                        coef_polmon[1]*dtasa_polmon_p[t-1]+
#                        coef_polmon[2]*dprestamos_personales_p[t-1])
#   prestamos_personales_p <- c(prestamos_personales_p,
#                               prestamos_personales_p[t-1]+
#                                 coef_prestamos[3]+
#                                 coef_prestamos[1]*dtasa_polmon_p[t-1]+
#                                 coef_prestamos[2]*dprestamos_personales_p[t])
# }


tasa_polmon_p <- ts(tasa_polmon_p, start = c(2016,1), frequency = 12)
prestamos_personales_p <- ts(prestamos_personales_p, start = c(2016,1), frequency = 12)

autoplot(cbind(tasa_polmon_p,prestamos_personales_p))+
  labs(x="Período", y="TNA")+
  geom_vline(xintercept = 2019.92, linetype="dashed") #2019.92 = dic-19


#Forecast error impulse response
feir <- irf(var, impulse = "dtasa_polmon", response = "dprestamos_personales",
            n.ahead = 8, ortho = FALSE, runs = 1000)

plot(feir)


## Selección de modelos ----

autoplot(liquidez)

tsdisplay(liquidez)
adfTest(liquidez, lags=1, type = "ct")

dliquidez <- liquidez - stats::lag(liquidez,-1)
autoplot(dliquidez)
tsdisplay(dliquidez)
adfTest(dliquidez, type = "nc")


entrenamiento1 <- window(cbind(dprestamos_personales,dtasa_polmon), 
                         start = c(2016,2), end = c(2019,4))
entrenamiento2 <- window(cbind(dprestamos_personales,dtasa_polmon,dliquidez), 
                         start = c(2016,2), end = c(2019,4))
prueba <- window(dprestamos_personales, 
                 start = c(2019,5), end = c(2019,12), frequency = 12)


VARselect(entrenamiento1, lag.max = 3, type = "const")[["selection"]] 
VARselect(entrenamiento2, lag.max = 2, type = "const")[["selection"]] 

var1 <- forecast(VAR(entrenamiento1, p=1,type="const"),h=8)
var2 <- forecast(VAR(entrenamiento2, p=1,type="const"),h=8)


dprestamos_personales_adj1 <- var1$forecast$dprestamos_personales$mean
dprestamos_personales_adj2 <- var2$forecast$dprestamos_personales$mean


a1 <- accuracy(dprestamos_personales_adj1,prueba)
a2 <- accuracy(dprestamos_personales_adj2,prueba)

# Armamos y presentamos una tabla con los resultados
comp<-rbind(a1,a2)
rownames(comp)[1] <- "Modelo 1"
rownames(comp)[2] <- "Modelo 2"

kable(comp, align=rep('c', 4), digits=4, 
      format.args = list(big.mark = ".", dec=",")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 14)


autoplot(cbind(prueba,dprestamos_personales_adj1,dprestamos_personales_adj2))+
  xlab("") + ylab("")+
  ggtitle("Proyección de la tasa de préstamos personales")


prestamos_personales_adj1 <- prestamos_personales
prestamos_personales_adj2 <- prestamos_personales

for (t in 41:48) {
  prestamos_personales_adj1[t] <- prestamos_personales_adj1[t-1]+dprestamos_personales_adj1[t-40]
  
  prestamos_personales_adj2[t] <- prestamos_personales_adj2[t-1]+dprestamos_personales_adj2[t-40]
}

#Graficamos
prestamos_personales_adj1 <- ts(prestamos_personales_adj1, 
                                start = c(2016,1), frequency = 12)
prestamos_personales_adj2 <- ts(prestamos_personales_adj2, 
                                start = c(2016,1), frequency = 12)
autoplot(cbind(prestamos_personales_adj1,
               prestamos_personales_adj2,
               prestamos_personales))+
  labs(x="Período", y="TNA", 
       title="Selección de pronósticos para la tasa de préstamos personales",
       subtitle = "Metodología VAR")

var <- VAR(cbind(dprestamos_personales,dtasa_polmon,dliquidez),
           p=1, type = "const")

pronostico <- forecast(var,h=6)
dprestamos_personales_p <- pronostico$forecast$dprestamos_personales$mean


# En un loop recuperamos la variable en niveles
prestamos_personales_p <- c(prestamos_personales,rep(NA,6))

for (t in 49:54) {
  prestamos_personales_p[t] <- prestamos_personales_p[t-1]+dprestamos_personales_p[t-48]
}


# Graficamos
prestamos_personales_p <- ts(prestamos_personales_p,start = c(2016,1), frequency = 12)
autoplot(cbind(prestamos_personales_p,prestamos_personales))+
  labs(x="Período", y="TNA", title="Pronóstico de la tasa de préstamos personales",
       subtitle="Metodología VAR")




