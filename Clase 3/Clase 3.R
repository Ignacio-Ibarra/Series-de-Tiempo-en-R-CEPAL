
## Clase 3 - Series de tiempo - CEPAL

# Paso 1. Limpiamos la memoria y cargamos las librer?as

rm(list=ls())

library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(lubridate)
library(forecast)
library(dynlm)
library(fUnitRoots)
library(kableExtra)


## Paso 2. Definimos el directorio y cargamos el archivo

setwd("~/Escritorio/CEP/CursoCEPAL/Series de Tiempo en R - CEPAL")

df <- read.xlsx("./Clase 3/Clase 3.xlsx",sheet = "Base1")


## Paso 3. Inspeccionamos los datos

head(df,5)

tail(df,5)


# Paso 4. Definimos la variable tiempo

df$Trimestre<-as.Date(df$Trimestre, origin = "1899-12-30")


# Paso 5. Creamos las variables de series de tiempo

PIB <- ts(df$PIB, start=c(2004,1), frequency = 4)
M <- ts(df$M,start=c(2004,1), frequency = 4)
TCRM <- ts(df$TCRM,start=c(2004,1), frequency = 4)


# Paso 6. Graficamos las series

library(ggfortify)
autoplot(cbind(PIB,M,TCRM), facets = TRUE)+
  ggtitle("Argentina: PIB, importaciones y tipo de cambio real") +
  guides(colour=guide_legend(title="Variables"))+
  labs(x = "A?o", y = "Variables") 


# Paso 7. Evaluamos si las series son estacionarias

ggAcf(PIB)
ggAcf(M)
ggAcf(TCRM)

adfTest(PIB, type = "c")
adfTest(M, type = "c")
adfTest(TCRM, type = "c")


# Paso 8. Test de cointegraci?n

res <- ts(dynlm(log(M) ~ log(PIB)+log(TCRM), data = df)$res, start = 2004, frequency = 4)

adfTest(res)

# Paso 9. Modelo de correcci?n de errores

mlp <- dynlm(log(M) ~ log(PIB)+log(TCRM))
res <- ts(dynlm(log(M) ~ log(PIB)+log(TCRM), data = df)$res, start = 2004, frequency = 4)

mcp <- dynlm(d(log(M)) ~ d(log(PIB))+d(log(TCRM)))
mce <- dynlm(d(log(M)) ~ d(log(PIB))+d(log(TCRM))+L(res))


# Recupero los coeficientes
mlp<-round(data.frame(coef(summary(mlp))[,1]),2) %>% rbind(.,"")
mcp<-round(data.frame(coef(summary(mcp))[,1]),2) %>% rbind(.,"")
mce<-round(data.frame(coef(summary(mce))[,1]),2)

# Presentaci?n de los coeficientes en una ?nica tabla
tabla<-cbind(mce,mlp,mcp) %>% 
  dplyr::rename("MCE"=1,"Modelo LP"=2,"Modelo CP"=3) 
rownames(tabla) = c("Intercepto","PIB","TCRM","TCE")

kable(tabla, align=rep('c', 4), format.args = list(big.mark = ".")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 14)


# Paso 10. Proyecci?n din?mica con el MCE

# Recuperamos los par?metros de las regresiones
b0<-as.numeric(mlp[1,1])
b1<-as.numeric(mlp[2,1])
b2<-as.numeric(mlp[3,1])

s0<-as.numeric(mce[1,1])
s1<-as.numeric(mce[2,1])
s2<-as.numeric(mce[3,1])
s3<-as.numeric(mce[4,1])

# Definimos las variables de inter?s
PIB_s <- ts(c(PIB,rep(NA,14)), start=c(2004,1),end=c(2024,4), frequency = 4)
TCRM_s <- ts(c(TCRM,rep(NA,14)), start=c(2004,1),end=c(2024,4), frequency = 4)
M_f <- ts(c(M,rep(NA,14)), start=c(2004,1),end=c(2024,4), frequency = 4)

logM <- log(M_f)
e <- log(M_f)-logM

# Realizamos una proyecci?n din?mica fuera de la muestra
for(j in 71:84){
  
  # Escenarios ex?genas
  PIB_s[j] <- (1+0.005)*PIB_s[j - 1]
  TCRM_s[j] <- TCRM_s[j - 1]
  
  # Proyecci?n del modelo de correcci?n de errores
  M_f[j] <- (1+s0+s1*(log(PIB_s[j])-log(PIB_s[j-1]))+s2*(log(TCRM_s[j])-log(TCRM_s[j-1]))+
               s3*e[j-1])*M_f[j-1]
  
  # Relaci?n de largo plazo y desalineamiento respecto al equilibrio de LP
  logM[j] <- b0+b1*log(PIB_s[j])+b2*log(TCRM_s[j])
  e[j] <- log(M_f[j])-logM[j]
}

# C?digo a veces necesario para evitar errores en autoplot()
#dev.off()

autoplot(cbind(M_f,M), facets = FALSE)+
  ggtitle("Importaciones: Proyecci?n din?mica fuera de la muestra utilizando un MCE") +
  labs(x = "", y = "", subtitle="Millones de pesos de 2004") +
  guides(colour=guide_legend(title="Variables"))+
  scale_color_manual(labels = c("Proyectado","Observado"), values = c(2, 1))
