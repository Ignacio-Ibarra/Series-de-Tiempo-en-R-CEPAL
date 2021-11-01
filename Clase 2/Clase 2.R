

# 1. Limpiamos la memoria y cargamos las librer?as

rm(list=ls())

library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(lubridate)
library(forecast)
library(gdata)
library(ggfortify)
library(kableExtra)

# 2. Definimos el directorio y cargamos el archivo

setwd("~/Escritorio/CEP/CursoCEPAL/Series de Tiempo en R - CEPAL/Clase 2")

df <- read.xlsx("Clase 2.xlsx",sheet = "Base")


# 3. Inspeccionamos los datos

head(df,5)


# 4.Definimos la variable tiempo


df$Fecha<-as.Date(df$Fecha, origin = "1899-12-30")


# 5.Aplicamos transformaciones a las series

df<-df %>% mutate(dlTCN = log(TCN) - lag(log(TCN)),
                  dlIPC = log(IPC) - lag(log(IPC)))


# 6. Creamos variables de series de tiempo

df <- df %>%
        filter(Fecha > "2004-01-01")

dlTCN <- ts((df$dlTCN), start=c(2004,1), frequency = 4)
dlIPC <- ts((df$dlIPC), start=c(2004,1), frequency = 4)
l.dlIPC <- ts(lag((df$dlIPC)), start=c(2004,1), frequency = 4)


# 7.Graficamos las series

autoplot(cbind(dlTCN,dlIPC), facets = T)+
  xlab("Fecha") +
  ylab("Diferencia logarítmica") +
  ggtitle("Inflacion y tipo de cambio") +
  guides(colour=guide_legend(title="Variables"))


# 8.Estimamos un modelo por MCO

modelo <- tslm(dlIPC ~ dlTCN+l.dlIPC)

summary(modelo)

# 9. Obtenemos intervalos de confianza
passthrough_puntual<-coef(summary(modelo))[2,1]
passthrough_superior<-coef(summary(modelo))[2,1]+1.96*coef(summary(modelo))[2,2]
passthrough_inferior<-coef(summary(modelo))[2,1]-1.96*coef(summary(modelo))[2,2]


# 10. Selecci?n de variables para mejorar la proyecci?n
modelo1 <- tslm(dlIPC ~ dlTCN)
modelo2 <- tslm(dlIPC ~ l.dlIPC)
modelo3 <- tslm(dlIPC ~ dlTCN+l.dlIPC)

# Cálculo detallado
t=length(dlIPC)
k=length(coefficients(modelo1))-1
resid=dlIPC-fitted.values(modelo1) # Alternativa 1
resid=residuals(modelo1) # Alternativa 2

SCR=sum((resid)^2)
R2<-sum((fitted.values(modelo1)-mean(dlIPC))^2)/sum((dlIPC-mean(dlIPC))^2)
R2A<-1-(1-R2)*(t-1)/(t-k-1)
AIC=t*log(SCR/t)+2*(k+2)
AICc=AIC+(2*(k+2)*(k+3)/(t-k-3))
BIC=t*log(SCR/t)+(k+2)*log(t)


# C?lculo con el c?digo CV
tabla<-data.frame(cbind(c(1,0,1),c(0,1,1))) %>%
  rename("TCN"=1,"Inercia"=2) %>%
  cbind(rbind(CV(modelo1),CV(modelo2),CV(modelo3)))

kable(tabla, align=rep('c', 5), format.args = list(big.mark = ".")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 14)

