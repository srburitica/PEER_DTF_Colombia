# Autor: YKE
# Objetivo: Generar bases separadas para cada ventana para usar en excel
# Fecha:13/12/2021

library(tidyverse)
library(sf)
library(ggmap)
library(corrr)
library(rstatix)
library(ggcorrplot)
library(dplyr) 
getwd()

# leer datos censo
setwd("G:/.shortcut-targets-by-id/1xILRoUFJk4N2a1JhsjSeNNQHrYsEpcyY/PEER_LastMile/DatosPropiosDelProyecto/Objetivo_2_HistoriaUso/CNA")
data1 <- read.csv("d_CNA2014_S6Areas_BST_cultiv.csv")
dim(data1)
data2 <- read.csv("d_CNA2014_S6Areas_BST_cultiv_adic.csv")
dim(data2)

# leer veredas cuencas
data.cuencas <- read.csv("Veredas6cuencas.csv")
names(data.cuencas)
data.cuencas<-data.cuencas[ c(1:6,15:16) ]
names(data.cuencas)

# quitar columna repetioda
data2 <- data2[ -c(33) ]

names(data1)
names(data2)
names(data)

names(data1)<-names(data2)

# pegar datos censo
data <- rbind(data=data1,data=data2)
data <- rename(data, CODIGO_VER=COD_VEREDA)

names(data)
names(data.cuencas)

data.total <- merge(data,data.cuencas,by="CODIGO_VER")
names(data.total)

# dividir por ventana

data.tolima <- data.total[data.total$Ventana == "Tolima",]
data.huila <- data.total[data.total$Ventana == "Huila",]
data.narino <- data.total[data.total$Ventana == "Narino",]
data.bolivar <- data.total[data.total$Ventana == "Bolivar",]
data.cesar <- data.total[data.total$Ventana == "Cesar",]

# escribir csv

write.csv(data.total,"CensoyCuencas.csv")




