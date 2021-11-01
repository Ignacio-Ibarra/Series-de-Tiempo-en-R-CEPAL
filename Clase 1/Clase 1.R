

### Clase 1 - Series de tiempo - CEPAL

## Paso 1: Limpiamos la memoria y cargamos las librer?as

#  Limpiar la memoria
rm(list=ls())

# Instalaci?n de librerias
#install.packages("tidyverse")  #Manejo de bases de datos y otros
#install.packages("openxlsx")   #Lectura de xlsx
#install.packages("lubridate")  #Manejo de variables en formato fecha
#install.packages("zoo")        #Series de tiempo
#install.packages("forecast")   #Pron?sticos
#install.packages("ggfortify")  #Gr?ficos


## Apertura de librerias
library(tidyverse)  #Manejo de bases de datos y otros
library(openxlsx)   #Lectura de xlsx
library(lubridate)  #Manejo de variables en formato fecha
library(zoo)        #Series de tiempo
library(forecast)   #Pron?sticos
library(ggfortify)  #Gr?ficos
library(seasonal)

## Paso 2: Definimos el directorio y cargamos el archivo

# Establecemos directorio
setwd("~/Escritorio/CEP/CursoCEPAL/Series de Tiempo en R - CEPAL/Clase 1")

# Levantamos archivo desde Excel 
tcn <- read.xlsx("Clase 1.xlsx",sheet = "TCN")



## Paso 3. Inspeccionamos los datos

# Observamos las primeras 10 observaciones
head(tcn,10)

# Consultamos la clase de objeto
class(tcn$TCN)

# Convertimos a variable num?rica
tcn$TCN <- as.numeric(tcn$TCN) 

# Obtenemos la media del tipo de cambio
mean(tcn$TCN)

# Resumen de estad?stica descriptiva
summary(tcn$TCN)

# Guardamos el resumen de estad?stica descriptiva en una matriz
summary<-as.matrix(summary(tcn$TCN))



## Paso 4: Definimos la variable tiempo

# Declaramos fecha y aclaramos el origen
tcn$Fecha<-as.Date(tcn$Fecha, origin = "1899-12-30")

# Levantamos el archivo con la funci?n read_excel
library(readxl)

tcn<-read_excel("Clase 1.xlsx",sheet = "TCN")

# Definimos la variable tiempo
tcn$Fecha<-as.Date(tcn$Fecha)

# Extraemos el a?o
tcn$year<-year(tcn$Fecha)

# Extraemos el semestre
tcn$semester<-semester(tcn$Fecha)

# Extraemos el trimestre
tcn$quarter<-quarter(tcn$Fecha)

# Extraemos el mes
tcn$month<-month(tcn$Fecha)

# Extraemos el d?a
tcn$day<-day(tcn$Fecha)

# Construimos variable fecha
tcn$Fecha<-paste(tcn$month, tcn$year, sep="-")

# La declaramos como variable tiempo pero con lubridate
tcn$Fecha<-my(tcn$Fecha)

# Contabilizar lapso de d?as
difftime(last(tcn$Fecha), first(tcn$Fecha), unit="days")

## Paso 5. Ajustamos la frecuencia de las variables

# Mensualizamos
tcn_mensual<- tcn %>%
  select(year,month,TCN) %>% 
  group_by(year,month) %>% 
  summarise(TCN=mean(TCN)) %>% 
  ungroup()

# Reconstruimos la variable fecha
tcn_mensual<- tcn_mensual %>%
  mutate(Fecha=ym(paste0(year,"-",month))) %>%
  select(Fecha,TCN)



## Paso 6. Unimos dos bases

# Levantamos una serie de IPC
ipc<-read_excel("Clase 1.xlsx",sheet = "IPC")

# Unimos las dos bases
df_m<- left_join(tcn_mensual,ipc, by="Fecha")

# Tambi?n lo podemos realizar con el pipe de la siguiente forma
df_m<- tcn_mensual %>% left_join(.,ipc, by="Fecha") %>% 
  mutate(Fecha=as.Date(Fecha))



## Paso 7. Aplicamos transformaciones a las series

# C?lculo de la variaci?n interanual del TCN y del IPC
df_m<- df_m %>%
  mutate(va.tcn=round(100*(TCN/lag(TCN,12)-1),1),
         va.ipc=round(100*(IPC/lag(IPC,12)-1),1),
         year=year(Fecha))

# C?lculo de la variaci?n anual del tipo de cambio y del IPC
df_m<- df_m %>%
  mutate(TCN_roll=rollmean(TCN, k=3, fill = NA, align = "right"))



## Paso 8. Creamos variables de series de tiempo

# Creamos series de tiempo
TCN <- ts(df_m$va.tcn[df_m$year >= 2005], start=c(2005,1), frequency = 12)
IPC <- ts(df_m$va.ipc[df_m$year >= 2005], start=c(2005,1), frequency = 12)



## Paso 9. Graficamos las series

library(ggfortify)

# Gr?fico
autoplot(cbind(TCN,IPC), facets = T)+
  xlab("Fecha") +
  ylab("Variacion interanual") +
  ggtitle("Inflacion y tipo de cambio") +
  guides(colour=guide_legend(title="Variables"))

## Paso 10. Graficamos correlograma y X11

# Graficamos el correlograma
autoplot(Acf(TCN, plot = FALSE))

#Correlacion parcial
Pacf(TCN, plot=T)

# Descomponemos por X11
fit<-IPC %>% seas(x11="")

# Graficamos descomposici?n por X11
autoplot(fit) +
  ggtitle("IPC de Argentina: descomposici?n por el m?todo X11")

# Graficamos descomposici?n por X11
trendcycle <- ts(trendcycle(fit), start=c(2005,1), frequency = 12)
seasadj <- ts(seasadj(fit), start=c(2005,1), frequency = 12)

autoplot(cbind(IPC,trendcycle,seasadj), facets = FALSE)+
  ggtitle("IPC de Argentina: descomposici?n por el m?todo X11") +
  labs(x = "", y = "", subtitle="Variaci?n interanual") +
  guides(colour=guide_legend(title="Variables"))+
  scale_color_manual(labels = c("Original","Tendencia-ciclo","Desestacionalizado"), values = c(1, 2,3))

