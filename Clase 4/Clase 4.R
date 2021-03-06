
### Clase 4 - Series de tiempo - CEPAL


## Paso 1. Limpiamos la memoria y cargamos las librer�as

#  Limpiamos memoria
rm(list=ls())

# Instalamos librer�as
#install.packages("vars")
#install.packages("zoo")
#install.packages("forecast")

# Apertura de librerias
library(tidyverse)
library(openxlsx)
library(lubridate)
library(zoo)
library(forecast)
library(ggfortify)
library(vars)
library(seasonal)


## Paso 2. Definimos el directorio y cargamos el archivo

# Establecemos directorio
setwd("G:/My Drive/CEPAL/Curso series de tiempo/Clase 4")

# Levantamos archivo desde Excel 
df <- read.xlsx("Clase 4.xlsx",sheet = "Base")


## Paso 3. Inspeccionamos los datos

# Guardamos el resumen de estad�stica descriptiva en una matriz
summary<-as.matrix(summary(df))

kable(summary, align=rep('c', 4), format.args = list(big.mark = ".")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 14)


## Paso 4. Definimos la variable tiempo

# Declaramos fecha y aclaramos el origen
df$Fecha<-yq(paste(df$y, df$q, sep="-"))


## Paso 5. Aplicamos transformaciones a las series

# Desestacionalizamos el RIPTE
df$RIPTE<- seasadj(seas(ts(df[,"RIPTE"],start=2005,frequency=4), x11=""))
df$RIPTE<-as.numeric(df$RIPTE)

# Creamos variable en diferencia logar�tmica
df<-df %>% mutate(dlTCN = log(TCN) - lag(log(TCN)),
                  dlIPC = log(IPC) - lag(log(IPC)),
                  dlRIPTE = log(RIPTE) - lag(log(RIPTE))) %>% 
  filter(y>2004)

# Creamos series de tiempo
df<-ts(df, start=2005,frequency=4)


## Paso 6. Selecci�n de cantidad de rezagos del VAR

# Realizamos test para seleccionar la cantidad de rezagos
VARselect(df[2:65,7:8], lag.max=8,
          type="const")[["selection"]]


## Paso 7. Estimaci�n del VAR

# Estimaci�n de modelo VAR
var <- VAR(df[,7:8], p=1, type="const")

# Resultados de la estimaci�n
summary(var)


## Paso 8. Proyecci�n

# "Chart fan" de la proyecci�n con el modelo VAR 
forecast(var) %>%
  autoplot() +
  ggtitle("Proyecci�n del IPC y el TCN") +
  labs(x = "", y = "")

# Funci�n impulso-respuesta
feir <-irf(var, impulse = "dlTCN", response = "dlIPC",
           n.ahead = 8, ortho = FALSE, runs = 1000)

plot(feir )

## Paso 9. Partici�n de la muestra y estimaci�n de los modelos

# Muestras de entrenamiento
df1<-window(df[,7:8], start=2006,end=c(2018,4))
df2<-window(df[,7:9], start=2006,end=c(2018,4))

# Muestra de prueba
IPC_test <- window(df[,8], start=2019,end=c(2019,4))

# Estimaci�n del modelo sobre el conjunto de entrenamiento
var1 <- forecast(VAR(df1, p=1, type="const"),h=9)
var2 <- forecast(VAR(df2, p=1, type="const"),h=9)

# Proyecci�n sobre el per�odo correspondiente a la muestra de prueba
IPC_ajustado1<-var1$forecast$dlIPC$mean
IPC_ajustado2<-var2$forecast$dlIPC$mean


## Paso 10. Comparaci�n de los modelos

# C�lculo de m�tricas
a1<-accuracy(IPC_ajustado1, IPC_test)
a2<-accuracy(IPC_ajustado2, IPC_test)

# Armamos y presentamos una tabla con los resultados
comp<-rbind(a1,a2)
rownames(comp)[1] <- "Modelo 1"
rownames(comp)[2] <- "Modelo 2"

kable(comp, align=rep('c', 4), format.args = list(big.mark = ".")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 14)

# Graficamos
autoplot(cbind(df[,8],IPC_ajustado1,IPC_ajustado2))+
  xlab("") + ylab("") +
  ylim(0,0.2)+
  ggtitle("Proyecci�n de la inflaci�n") +
  guides(colour=guide_legend(title="Proyecci�n"))

