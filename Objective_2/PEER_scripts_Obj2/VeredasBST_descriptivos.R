#Data.created.May.25.2021
#Data.modified.May.25.2021

#Script to synthetize descriptive data from Vereda BST groups

#1) Define directories
InD<-file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','GIS_Base')
OutD<-file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','Objetivo_2_HistoriaUso','TablasPorVereda')
library(rgdal)

#2) read the data
verd<-readOGR(file.path(InD,'VeredasBST.shp'),encoding="UTF-8", use_iconv = T)
verdd<-verd@data
#solve encoding problems
for (col in colnames(verdd)){
  if(class(verdd[[col]])=="character"){
    Encoding(verdd[[col]]) <- "UTF-8"
  }}
verdd2<-verdd[!is.na(verdd$Grupo)&verdd$Grupo>0,]
AOI_ver<-data.frame('Grupo'=seq(1,9),'AOI_ver'=c("Bolivar","Antioquia","Cesar","","Patia","Huila","Tolima","Valle","Mariquita"))
verdd2$AOI_ver<-AOI_ver[match(verdd2$Grupo,AOI_ver$Grupo),'AOI_ver']
verdd2<-verdd2[verdd2$Grupo!=4,]
#3)Veredas por departamento
table(verdd2$NOM_DEP)
table(verdd2$Grupo)
table(verdd2$AOI_ver)
table(verdd2[,c("Grupo","NOM_DEP")])
verMean<-aggregate(AREA_HA~AOI_ver,FUN='mean',data=verdd2)
verSD<-aggregate(AREA_HA~AOI_ver,FUN='sd',data=verdd2)
verRang<-aggregate(AREA_HA~AOI_ver,FUN='range',data=verdd2)
verSum<-aggregate(AREA_HA~AOI_ver,FUN='sum',data=verdd2)
verT<-do.call(cbind,list(verMean,verSD,verSum))
verT<-verT[,-c(3,5)]
names(verT)[c(2,3,4)]<-c('Area_ha_mean','Area_ha_sd','Area_ha_sum')
verT<-cbind(verT,verRang$AREA_HA)
names(verT)[5:6]<-c('Area_ha_min','Area_ha_max')
verT$Area_ha_cv<-verT$Area_ha_sd/verT$Area_ha_mean
write.csv(verT,file.path(OutD,'AreasVeredas.csv'))
