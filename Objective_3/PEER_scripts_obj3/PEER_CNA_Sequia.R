#date.created.may.13.2022
#Date.modified.May.13.2022

#Script to extract CNA data from data processed before

library(sf)
library(tidyverse)
library(readxl)
library(purrr)
library(rgdal)
library(openxlsx)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(chemCal)

ruta_datos<-"Y:/Informacion_Externa/"

dCNA_Categories<-read.csv(file.path('F:','Mi Unidad','PEER_LastMile','DatosPropiosDelProyecto', 'Objetivo_2_HistoriaUso','Salidas2021','Insumos','dCNA_Categories_0521.csv'),header=T)
dCNA_Sequia<-dCNA_Categories%>%filter(.,Pregunta=='P_S11P126'&P_DEPTO%in%c(73,52,47))%>%
  left_join(dCNA2017_Coordenadas%>%dplyr::select(ENCUESTA,X_GEO,Y_GEO),by="ENCUESTA")
dCNA_sqCo<-cbind(dCNA_Sequia$X_GEO,dCNA_Sequia$Y_GEO)
dCNA_sq<-SpatialPointsDataFrame(coords=dCNA_sqCo,data=dCNA_Sequia,proj4string = CRS("+init=epsg:4326"))

cuencas_selT<-readOGR(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto', 
                                                         'Objetivo_3_Valoraciones','Res_SEModellingInvest','invest_waterYield','2013','tolima11','CUENCAS_tolima11.shp'),encoding = 'UTF-8',use_iconv = T)
cuencas_selT<-cuencas_selT%>%spTransform(.,CRS=CRS("+init=epsg:4326"))
selpnt<-!is.na(over(dCNA_sq,cuencas_selT))[,1]
pts_T<-dCNA_sq[selpnt,]
pts_T.o<-pts_T@data
WS_T<-over(pts_T,cuencas_selT)%>%dplyr::select('codigo_ws')
pts_T.o$WS_ID<-WS_T$codigo_ws
cuencas_selM<-readOGR(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto', 
                                'Objetivo_3_Valoraciones','Res_SEModellingInvest','invest_waterYield','2013','magdalena4','CUENCAS_magdalena4.shp'),encoding = 'UTF-8',use_iconv = T)%>%spTransform(.,CRS=CRS("+init=epsg:4326"))
cuencas_selM<-cuencas_selM%>%spTransform(.,CRS=CRS("+init=epsg:4326"))

selpnt<-!is.na(over(dCNA_sq,cuencas_selM))[,1]
pts_M<-dCNA_sq[selpnt,]
pts_M.o<-pts_M@data
WS_M<-over(pts_M,cuencas_selM)%>%dplyr::select('codigo_ws')
pts_M.o$WS_ID<-WS_M$codigo_ws
cuencas_selN<-readOGR(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto', 
                                'Objetivo_3_Valoraciones','Res_SEModellingInvest','invest_waterYield','2013','narino5','CUENCAS_narino5.shp'),encoding = 'UTF-8',use_iconv = T)
cuencas_selN<-cuencas_selN%>%spTransform(.,CRS=CRS("+init=epsg:4326"))
selpnt<-!is.na(over(dCNA_sq,cuencas_selN))[,1]
pts_N<-dCNA_sq[selpnt,]
pts_N.o<-pts_N@data
WS_N<-over(pts_N,cuencas_selN)%>%dplyr::select('codigo_WS')
pts_N.o$WS_ID<-WS_N$codigo_WS

pts<-rbind(pts_T.o,pts_M.o)
pts<-rbind(pts,pts_N.o)
pts$Dp_WS<-trimws(paste(pts$P_DEPTO,pts$WS_ID,sep='_'))
#Datos invest
dataI<-read.csv(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto', 
                           'Objetivo_3_Valoraciones','Res_SEModellingInvest','invest_waterYield','datosRegresion.csv'),
                header=T,sep=";")
dataI$P_DEPTO<-0
dataI$P_DEPTO[dataI$dep=='tol']<-73
dataI$P_DEPTO[dataI$dep=='mag']<-47
dataI$P_DEPTO[dataI$dep=='nar']<-52
dataI$Dp_WS<-trimws(paste(dataI$P_DEPTO,dataI$ws,sep='_'))
unique(dataI$Dp_WS)

dataI13<-dataI[dataI$serie=='2013',]
names(dataI13)<-c("PorcBosq","AreaHa","pendiente",
                  "TempPrAn","PrecPrAn","indice",
                  "altitud","wyield","ws","dep","serie","P_DEPTO","Dp_WS")

pts$seq<-'nosequia'
pts$seq[pts$value%in%c("SuspensiÃ³n por sequÃ­a","SuspensiÃ³n de provisiÃ³n")]<-'sequia'
pts.ag<-as.data.frame(with(pts,table(Dp_WS,seq)))
pts.ag<-reshape(data=pts.ag,idvar='Dp_WS',v.names='Freq',timevar='seq',
                                                            direction='wide')
pts.ag$n<-pts.ag$Freq.nosequia+pts.ag$Freq.sequia

mtch<-match(pts.ag$Dp_WS,dataI13$Dp_WS)
sum(is.na(mtch))
pts.ag[,c('PrecPrAn','wyield','PorcBosq','AreaHa','P_DEPTO','altitud')]<-dataI13[mtch,c('PrecPrAn','wyield','PorcBosq','AreaHa','P_DEPTO','altitud')]
pts.ag<-pts.ag[!is.na(pts.ag$PrecPrAn),]
names(pts.ag)[c(2,3)]<-c('nosequia','sequia')
pts.ag$p<-pts.ag$sequia/(pts.ag$sequia+pts.ag$nosequia)
pts.ag$Prec_ha<-pts.ag$PrecPrAn/pts.ag$AreaHa
pts.ag$lgArea<-log(pts.ag$AreaHa)
pts.ag$lgAlt<-log10(pts.ag$altitud)
pts.ag$lgPrc_ha<-log10(pts.ag$Prec_ha)
pts.ag$dep<-'Tol'
pts.ag$dep[pts.ag$P_DEPTO==47]<-'Mag'
pts.ag$dep[pts.ag$P_DEPTO==52]<-'Nar'
pts.ag$dep<-as.factor(pts.ag$dep)
pts.ag$wgts<-sqrt(pts.ag$n)
pts.ag$sqrtWY<-sqrt(pts.ag$wyield)
rownames(pts.ag)<-NULL

#exploratory
for(x in c('PorcBosq','sqrtWY','PrecPrAn','lgArea','lgPrc_ha','lgAlt')){
  hist(pts.ag[,x],main=x)
  readline(prompt='ENTER')
  g<-ggplot(aes(x=get(x),y=p),data=pts.ag)+geom_point(alpha=0.5)+
    stat_smooth(method="lm", se=TRUE) + 
    ylab("proportion")+xlab(x)+facet_wrap(~dep)
  print(g)
  readline(prompt='ENTER')
  g<-ggplot(aes(x=dep,y=get(x)),data=pts.ag)+geom_boxplot()+
  ylab(x)
  print(g)
  readline(prompt='ENTER')
}
ggplot(data=pts.ag,aes(x=dep,y=p))+geom_boxplot()
M1<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgArea*lgAlt*dep,data=pts.ag,family=binomial,weights=wgts)
summary(M1)
plot(M1)
#explore outliers
pts.ag[4,]
pts.agg<-pts.ag[-4,]

M1<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgArea*lgAlt*dep,data=pts.agg,family=binomial,weights=wgts)
summary(M1)
plot(M1)
pts.agg[15,]

Mo<-step(M1)
summary(Mo)
plot(Mo)

M2<-glm(cbind(sequia,nosequia)~PorcBosq*lgPrc_ha*lgAlt*dep,data=pts.agg,family=binomial,weights=wgts)
summary(M2)
plot(M2)
Mo2<-step(M2)
summary(Mo2)
anova(Mo2,Mo,test="Chisq")

M3<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgArea*dep,data=pts.agg,family=binomial,weights =wgts)
summary(M3)
plot(M3)
Mo3<-step(M3)
summary(Mo3)
anova(Mo,Mo3,test='Chisq')
anova(Mo3,Mo2,test='Chisq')

M7<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgAlt*dep,data=pts.agg,family=binomial,weights =wgts)
anova(M1,M7,test="Chisq")
M8<-glm(cbind(sequia,nosequia)~PorcBosq*lgArea*lgAlt*dep,data=pts.agg,family=binomial,weights =wgts)
summary(M8)
anova(M1,M8,test="Chisq")
M9<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgArea,data=pts.agg,family=binomial,weights =wgts)
anova(M1,M9,test="Chisq")
summary(M9)

Moo<-update(Mo,family=quasibinomial)
summary(Moo)

#Explore merging two departments
pts.agg$dep2<-as.character(pts.agg$dep)
pts.agg$dep2[pts.agg$dep%in%c('Nar','Mag')]<-'NarMag'
pts.agg$dep2<-as.factor(pts.agg$dep2)

M4<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgArea*lgAlt*dep2,data=pts.agg,family=binomial,weights=wgts)
summary(M4)
plot(M4)
Mo4<-step(M4)
anova(Mo,Mo4,test="Chisq")
M5<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgArea*dep2,data=pts.agg,family=binomial,weights=wgts)
summary(M5)
plot(M5)
anova(M4,M5,test="Chisq")
M6<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgAlt*dep2,data=pts.agg,family=binomial,weights=wgts)
summary(M6)
anova(M4,M6,test="Chisq")

#Model only tolima
datat<-pts.agg[pts.agg$dep=='Tol'&pts.agg$p<1,]
M1T<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgArea*lgAlt,
         data=datat,family=binomial)
summary(M1T)
MoT<-step(M1T)
summary(MoT)

MooT<-update(M1T,family=quasibinomial)
summary(MooT)

MooT2<-update(MooT,~.-PorcBosq:PrecPrAn:lgArea:lgAlt)
summary(MooT2)
anova(MooT,MooT2,test="F")

MooT3<-update(MooT2,~.-PorcBosq:PrecPrAn:lgAlt)
summary(MooT3)
anova(MooT2,MooT3,test="F")

MooT4<-update(MooT3,~.-PrecPrAn:lgArea:lgAlt )
summary(MooT4)
anova(MooT3,MooT4,test="F")



plot(x=datat$p,y=predict(MoT,type="response"))

#Final plot
summary(MoT)
plot(MoT)
datat$fitted<-predict(MoT,type="response")
newdataT<-data.frame(PorcBosq=rep(median(datat$PorcBosq),100),
                     PrecPrAn=seq(min(datat$PrecPrAn),max(datat$PrecPrAn),length.out=100),
                     lgArea=rep(median(datat$lgArea),100),
                     lgAlt=rep(median(datat$lgAlt),100))
predictT<-predict(MoT,newdata=newdataT,type="response",se.fit=TRUE)
newdataT$predM<-predictT$fit
newdataT$predM.se<-predictT$se.fit

gt<-ggplot(aes(x=PrecPrAn,y=predM),data=newdataT)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=PrecPrAn,y=fitted),data=datat)+
  xlab("Precipitación Anual")+theme_classic()+theme(legend.position="none")+labs(title='Tolima')
print(gt)
summary(newdataT$predM)

MFPrecT<-lm(predM~PrecPrAn,data=newdataT)
summary(MFPrecT)

 #precipitation below which odd is higher than 0.4
newdataT$MinPred<-newdataT$predM-newdataT$predM.se
newdataT$MaxPred<-newdataT$predM+newdataT$predM.se

#[1174.509,1406.555] with odds [0.212-0.44]
#min precipitation of maximum stress
#1174.509 with odds 0.19303879 0.4448693
#2218.714 with odds of 0.04612731 0.4370997

#model with forest 
newdataT2<-data.frame(PorcBosq=seq(min(datat$PorcBosq),max(datat$PorcBosq),length.out=100),
                      PrecPrAn=rep(median(datat$PrecPrAn),100),
                     lgArea=rep(median(datat$lgArea),100),
                     lgAlt=rep(median(datat$lgAlt),100))
predictTT<-predict(MoT,newdata=newdataT2,type="response",se.fit=TRUE)
newdataT2$predM<-predictT$fit
newdataT2$predM.se<-predictT$se.fit

gtt<-ggplot(aes(x=PorcBosq,y=predM),data=newdataT2)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=PorcBosq,y=fitted),data=datat)+
  xlab("Porcentaje Bosque")+theme_classic()+theme(legend.position="none")
print(gtt)
summary(newdataT$predM)


#Model only Magdalena
datam<-pts.agg[pts.agg$dep=='Mag'&pts.agg$p<1,]
M1M<-glm(cbind(sequia,nosequia)~PorcBosq*PrecPrAn*lgAlt,
         data=datam,family=binomial,subset=dep=='Mag')
summary(M1M)
MoM<-step(M1M)
summary(MoM)
plot(MoM)
plot(x=datam$p,y=MoM$fitted.values)

MooM<-update(M1M,family=quasibinomial)
summary(MooM)

MooM2<-update(MooM,~.-PorcBosq:PrecPrAn:lgAlt)
summary(MooM2)
anova(MooM,MooM2,test="F")

MooM3<-update(MooM2,~.-PrecPrAn:lgAlt)
summary(MooM3)
anova(MooM2,MooM3,test="F")

MooM4<-update(MooM3,~.-PorcBosq:lgAlt )
summary(MooM4)
anova(MooM3,MooM4,test="F")

MooM5<-update(MooM4,~.-PorcBosq:PrecPrAn )
summary(MooM5)
anova(MooM4,MooM5,test="F")

MooM6<-update(MooM4,~.-lgAlt )
summary(MooM6)
anova(MooM4,MooM6,test="F")

MooM7<-update(MooM5,~.-PrecPrAn )
summary(MooM7)
anova(MooM5,MooM7,test="F")


datam$fitted<-MooM4$fitted.values
for(x in c('PorcBosq','lgArea','lgAlt','PrecPrAn')){
  g<-ggplot(aes(x=get(x),y=fitted),data=datam)+geom_point()+
    geom_smooth(method='glm',se=TRUE, method.args = list(family=binomial))+xlab(x)
  print(g)
  readline(prompt='ENTER')
}

#Get final plot
summary(MooM4)

newdataM<-data.frame(PorcBosq=rep(median(datam$PorcBosq),100),
                     PrecPrAn=seq(min(datam$PrecPrAn),max(datam$PrecPrAn),length.out=100),
                     lgArea=rep(median(datam$lgArea),100),
                     lgAlt=rep(median(datam$lgAlt),100))
predictM<-predict(MooM4,newdata=newdataM,type="response",se.fit=TRUE)
newdataM$predM<-predictM$fit
newdataM$predM.se<-predictM$se.fit

gm<-ggplot(aes(x=PrecPrAn,y=predM),data=newdataM)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=PrecPrAn,y=fitted),data=datam)+
  xlab("Precipitación Anual")+theme_classic()+theme(legend.position="none")+
  labs(title='Magdalena')
print(gm)

MFPrecM<-lm(predM~PrecPrAn,data=newdataM)
summary(MFPrecM)


#precipitation below which odd is higher than 0.4
newdataM$MinPred<-newdataM$predM-newdataM$predM.se
newdataM$MaxPred<-newdataM$predM+newdataM$predM.se
#[1825.04045-3117.64454] with odds [0.5054406-0.5408719]
#70.79204 with odd of  0.7937401 0.8601237
#3117.64454 with odds of 0.2025419 0.5408719


#model with forest 
newdataM2<-data.frame(PorcBosq=seq(min(datam$PorcBosq),max(datam$PorcBosq),length.out=100),
                      PrecPrAn=rep(median(datam$PrecPrAn),100),
                      lgArea=rep(median(datam$lgArea),100),
                      lgAlt=rep(median(datam$lgAlt),100))
predictMM<-predict(MooM4,newdata=newdataM2,type="response",se.fit=TRUE)
newdataM2$predM<-predictMM$fit
newdataM2$predM.se<-predictMM$se.fit

gmm<-ggplot(aes(x=PorcBosq,y=predM),data=newdataM2)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=PorcBosq,y=fitted),data=datam)+
  xlab("Porcentaje Bosque")+theme_classic()+theme(legend.position="none")
print(gmm)


#Model only nariño
datan<-pts.agg[pts.agg$dep=='Nar',]
M1N<-glm(cbind(sequia,nosequia)~PrecPrAn*lgArea*lgAlt,
         data=datan,family=binomial)
summary(M1N)
MoN<-step(M1N)
summary(MoN)
plot(MoN)
MooN<-update(M1N,family=quasibinomial)
summary(MooN)

MooN2<-update(MooN,~.-PrecPrAn:lgArea:lgAlt)
summary(MooN2)
anova(MooN,MooN2,test="F")

MooN3<-update(MooN2,~.-PrecPrAn:lgArea)
summary(MooN3)
anova(MooN2,MooN3,test="F")

MooN4<-update(MooN3,~.-lgArea:lgAlt )
summary(MooN4)
anova(MooN3,MooN4,test="F")

MooN5<-update(MooN4,~.-lgArea)
summary(MooN5)
anova(MooN4,MooN5,test="F")

MooN6<-update(MooN5,~.+PorcBosq)
summary(MooN6)
anova(MooN5,MooN6,test="F")

MooN7<-update(MooN6,~.+PorcBosq:PrecPrAn)
summary(MooN7)
anova(MooN6,MooN7,test="F")

MooN8<-update(MooN6,~.+PorcBosq:lgAlt)
summary(MooN8)
anova(MooN6,MooN7,test="F")

plot(x=datan$p,y=MooN6$fitted.values)
datan$fitted<-MooN6$fitted.values


for(x in c('PorcBosq','lgArea','lgAlt','PrecPrAn')){
  g<-ggplot(aes(x=get(x),y=fitted),data=datan)+geom_point()+
    geom_smooth(method='lm')+xlab(x)
  print(g)
  readline(prompt='ENTER')
}

#Get final plot
summary(MooN6)

newdataN<-data.frame(PorcBosq=rep(median(datan$PorcBosq),100),
                     PrecPrAn=seq(min(datan$PrecPrAn),max(datan$PrecPrAn),length.out=100),
                     lgArea=rep(median(datan$lgArea),100),
                     lgAlt=rep(median(datan$lgAlt),100))

predictN<-predict(MooN6,newdata=newdataN,type="response",se.fit=TRUE)
newdataN$predM<-predictN$fit
newdataN$predM.se<-predictN$se.fit

gn<-ggplot(aes(x=PrecPrAn,y=predM),data=newdataN)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=PrecPrAn,y=fitted),data=datan)+
  xlab("Precipitación Anual")+theme_classic()+theme(legend.position="none")+
  labs(title='Nariño')
print(gn)

MFPrecN<-lm(predM~PrecPrAn,data=newdataN)
summary(MFPrecN)


#precipitation below which odd is higher than 0.4
newdataN$MinPred<-newdataN$predM-newdataN$predM.se
newdataN$MaxPred<-newdataN$predM+newdataN$predM.se
#[1787.859-1906.388] with odds [0.5050647 -0.5783455]
#1254.479 mm with 0.7840522 0.9313204
#1906.388 with odds of 0.3898940 0.5783455


#model with forest 
newdataN2<-data.frame(PorcBosq=seq(min(datan$PorcBosq),max(datan$PorcBosq),length.out=100),
                      PrecPrAn=rep(median(datan$PrecPrAn),100),
                      lgArea=rep(median(datan$lgArea),100),
                      lgAlt=rep(median(datan$lgAlt),100))
predictNN<-predict(MooN6,newdata=newdataN2,type="response",se.fit=TRUE)
newdataN2$predM<-predictNN$fit
newdataN2$predM.se<-predictNN$se.fit

gnn<-ggplot(aes(x=PorcBosq,y=predM),data=newdataN2)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=PorcBosq,y=fitted),data=datan)+
  xlab("Porcentaje Bosque")+theme_classic()+theme(legend.position="none")
print(gnn)



#2) Model with wateryield
M1y<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*lgAlt*dep,data=pts.ag,family=binomial,weights=wgts)
summary(M1y)
plot(M1y)
#explore outliers
pts.ag[4,]
pts.wy<-pts.ag[pts.ag$p<1&pts.ag$p>0,]
table(pts.wy$dep)

M1y<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*lgAlt*dep,data=pts.wy,family=binomial)
summary(M1y)
plot(M1y)
pts.wy[11,]

Moy<-step(M1y)
summary(Moy)
plot(Moy)

M2y<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*dep,data=pts.wy,family=binomial)
summary(M2y)
plot(M2y)
Mo2y<-step(M2y)
summary(Mo2y)
anova(Mo2y,Moy,test="Chisq")

M3y<-glm(cbind(sequia,nosequia)~sqrtWY*lgAlt*dep,data=pts.wy,family=binomial)
summary(M3y)
plot(M3y)
Mo3y<-step(M3y)
summary(Mo3y)
anova(Moy,Mo3y,test='Chisq')
anova(Mo3y,Mo2y,test='Chisq')

Mooy<-update(Moy,family=quasibinomial)
summary(Mooy)
Mooy1<-update(Mooy,~.-PorcBosq:sqrtWY:lgAlt:dep)
summary(Mooy1)
anova(Mooy,Mooy1, test="F")

Mooy2<-update(Mooy1,~.-sqrtWY:lgAlt:dep)
summary(Mooy2)
anova(Mooy1,Mooy2, test="F")

Mooy3<-update(Mooy1,~.-PorcBosq:lgAlt:dep)
summary(Mooy3)
anova(Mooy1,Mooy3, test="F")

Mooy4<-update(Mooy3,~.-PorcBosq:sqrtWY:dep)
summary(Mooy4)
anova(Mooy3,Mooy4, test="F")
plot(Mooy4)

dataWY<-pts.wy[,c('PorcBosq','sqrtWY','lgAlt','dep','p')]
dataWY$fitted<-Mooy4$fitted.values
for(x in c('PorcBosq','sqrtWY','lgAlt')){
  g<-ggplot(aes(x=get(x),y=fitted),data=dataWY)+geom_point()+
    geom_smooth(method='lm',se=TRUE)+xlab(x)+facet_grid(~dep)
  print(g)
  readline(prompt='ENTER')
}


newdataWY<-data.frame(PorcBosq=rep(median(dataWY$PorcBosq),300),
                     sqrtWY=rep(seq(min(dataWY$sqrtWY),max(dataWY$sqrtWY),length.out=100),3),
                     lgAlt=rep(median(dataWY$lgAlt),300),
                     dep=rep(c('Tol','Nar','Mag'),each=100))
predictWY<-predict(Mooy4,newdata=newdataWY,type="response",se.fit=TRUE)
newdataWY$predM<-predictWY$fit
newdataWY$predM.se<-predictWY$se.fit

gwy<-ggplot(aes(x=sqrtWY,y=predM),data=newdataWY)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=sqrtWY,y=fitted),data=dataWY,col='red')+
  geom_point(aes(x=sqrtWY,y=p),data=dataWY,col='blue')+
  xlab("SqrtWater yield")+theme_classic()+theme(legend.position="none")+facet_grid(~dep)
print(gwy)
summary(newdataT$predM)

### Model for magdalena and for the probability of sequia
1/(1+(1/exp(2.0509-(0.0246*dataWY$sqrtWY))))
simsqrWY<-seq(0,200,1)
yy<-1/(1+(1/exp(2.0509-(0.0246*simsqrWY))))
plot(simsqrWY,yy)
1/(1+(1/exp(2.0509-(0.0246*sqrt(6950.543)))))
newdataWY_2<-data.frame(PorcBosq=rep(median(dataWY$PorcBosq),300),
                      sqrtWY=rep(seq(0,200,length.out=100),3),
                      lgAlt=rep(median(dataWY$lgAlt),300),
                      dep=rep(c('Tol','Nar','Mag'),each=100))
predictWY_2<-predict(Mooy4,newdata=newdataWY_2,type="response",se.fit=TRUE)
newdataWY_2$predM<-predictWY_2$fit
newdataWY_2$predM.se<-predictWY_2$se.fit

gwy2<-ggplot(aes(x=sqrtWY,y=predM),data=newdataWY_2)+geom_line()+ylim(0,1)+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  xlab("SqrtWater yield")+theme_classic()+theme(legend.position="none")+facet_grid(~dep)
print(gwy2)

#Model only tolima
dataty<-pts.wy[pts.wy$dep=='Tol',]
M1Ty<-glm(cbind(sequia,nosequia)~PorcBosq*lgAlt*sqrtWY,
         data=dataty,family=binomial)
summary(M1Ty)
MoTy<-step(M1Ty)
summary(MoTy)

#best model with sqrtWY
MoTy_f<-glm(cbind(sequia,nosequia)~sqrtWY,
          data=dataty,family=binomial)

summary(MoTy_f)
plot(MoTy_f)
plot(x=dataty$p,y=predict(MoTy_f,type="response"))

dataty$fitted<-predict(MoTy_f,type="response")
newdataTy<-data.frame(lgAlt=seq(min(dataty$lgAlt),max(dataty$lgAlt),length.out=100),
                      sqrtWY=seq(min(dataty$sqrtWY),max(dataty$sqrtWY),length.out=100))
predictT<-predict(MoTy_f,newdata=newdataTy,type="response",se.fit=TRUE)
newdataTy$predM<-predictT$fit
newdataTy$predM.se<-predictT$se.fit

gty<-ggplot(aes(x=sqrtWY,y=predM),data=newdataTy)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=sqrtWY,y=p),data=dataty)+
  xlab("water yield")+theme_classic()+theme(legend.position="none")
print(gty)
summary(newdataTy$predM)


MFPrecTy<-lm(predM~sqrtWY,data=newdataTy)
summary(MFPrecTy)

#0.5 lim water yield

(MoTy_f$coefficients[1])^2/(MoTy_f$coefficients[2])^2
estT<-dose.p(MoTy_f,p=0.5)
estT[1]^2
(estT[1]+(2*attributes(estT)$SE))^2
(estT[1]-(2*attributes(estT)$SE))^2
#Model only Magdalena
datamy<-pts.wy[pts.wy$dep=='Mag',]
M1My<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*lgAlt,
         data=datamy,family=binomial)
summary(M1My)
MoMy<-step(M1My)
summary(MoMy)
plot(MoMy)

MooMy<-update(M1My,family=quasibinomial)
summary(MooMy)

MooMy2<-update(MooMy,~.-PorcBosq:sqrtWY:lgAlt)
summary(MooMy2)
anova(MooMy,MooMy2,test="F") 

MooMy3<-update(MooMy2,~.-sqrtWY:lgAlt)
summary(MooMy3)
anova(MooMy2,MooMy3,test="F")

MooMy4<-update(MooMy3,~.-PorcBosq:lgAlt)
summary(MooMy4)
anova(MooMy3,MooMy4,test="F")

MooMy5<-update(MooMy4,~.-PorcBosq:sqrtWY)
summary(MooMy5)
anova(MooMy4,MooMy5,test="F")

MooMy6<-update(MooMy5,~.-lgAlt)
summary(MooMy6)
anova(MooMy5,MooMy6,test="F")

MooMy7<-update(MooMy6,~.+PorcBosq:sqrtWY)
summary(MooMy7)
anova(MooMy6,MooMy7,test="F")

MooMy8<-update(MooMy6,~.-PorcBosq)
summary(MooMy8)
anova(MooMy6,MooMy8,test="F")

MooMy9<-update(MooMy6,~.-sqrtWY)
summary(MooMy9)
anova(MooMy6,MooMy9,test="F")

plot(x=datamy$p,y=MooMy4$fitted.values)
datamy$fitted<-MooMy4$fitted.values
for(x in c('PorcBosq','sqrtWY','lgAlt')){
  g<-ggplot(aes(x=get(x),y=fitted),data=datamy)+geom_point()+
    geom_smooth(method='glm',se=TRUE, method.args = list(family=binomial))+xlab(x)
  print(g)
  readline(prompt='ENTER')
}

#Get  plot final
newdataMy<-data.frame(PorcBosq=rep(median(datamy$PorcBosq),100),
                     sqrtWY=seq(min(datamy$sqrtWY),max(datamy$sqrtWY),length.out=100),
                     lgAlt=rep(median(datamy$lgAlt),100))
predictM<-predict(MooMy4,newdata=newdataMy,type="response",se.fit=TRUE)
newdataMy$predM<-predictM$fit
newdataMy$predM.se<-predictM$se.fit

gmy<-ggplot(aes(x=sqrtWY,y=predM),data=newdataMy)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=sqrtWY,y=fitted),data=datamy)+
  geom_point(aes(x=sqrtWY,y=p),data=datamy, color='blue')+
  xlab("water Yield")+theme_classic()+theme(legend.position="none")
print(gmy)

MFPrecMy<-lm(predM~sqrtWY,data=newdataMy)
summary(MFPrecMy)
##timate limits for 0.5 probability

estM<-dose.p(MooMy4,p=0.5)
estM[1]^2
(estM[1]-(2*attributes(estM)$SE))^2
(estM[1]+(2*attributes(estM)$SE))^2




#Model only nariño
datany<-pts.wy[pts.wy$dep=='Nar',]
M1Ny<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*lgAlt,
         data=datany,family=binomial)
summary(M1Ny)
MoNy<-step(M1Ny)
summary(MoNy)
plot(MoNy)
MooNy<-update(M1Ny,family=quasibinomial)
summary(MooNy)


MooNy2<-update(MooNy,~.-PorcBosq:sqrtWY:lgAlt)
summary(MooNy2)
anova(MooNy,MooNy2,test="F") 

MooNy3<-update(MooNy2,~.-PorcBosq:lgAlt)
summary(MooNy3)
anova(MooNy2,MooNy3,test="F") 

MooNy4<-update(MooNy3,~.-PorcBosq:sqrtWY)
summary(MooNy4)
anova(MooNy3,MooNy4,test="F")

MooNy5<-update(MooNy4,~.-PorcBosq)
summary(MooNy5)
anova(MooNy4,MooNy5,test="F")
plot(MooNy5)


plot(x=datany$p,y=MooNy5$fitted.values)
datany$fitted<-MooNy5$fitted.values

#fix outliers
datany[rownames(datany)=="32",]
datanys<-datany[(rownames(datany)!="32"),]

M1Nys<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*lgAlt,
          data=datanys,family=binomial)
summary(M1Nys)
MoNys<-step(M1Nys)
summary(MoNys)
MooNys<-update(M1Nys,family=quasibinomial)
summary(MooNys)

MooNys2<-update(MooNys,~.-PorcBosq:sqrtWY:lgAlt)
summary(MooNys2)
anova(MooNys,MooNys2,test="F") 

MooNys3<-update(MooNys2,~.-PorcBosq:lgAlt)
summary(MooNys3)
anova(MooNys2,MooNys3,test="F") 

MooNys4<-update(MooNys3,~.-PorcBosq:sqrtWY)
summary(MooNys4)
anova(MooNys3,MooNys4,test="F")

MooNys5<-update(MooNys4,~.-PorcBosq)
summary(MooNys5)
anova(MooNys4,MooNys5,test="F")
plot(MooNys5)

datanys$fitted<-MooNys5$fitted.values

for(x in c('PorcBosq','sqrtWY','lgAlt')){
  g<-ggplot(aes(x=get(x),y=fitted),data=datanys)+geom_point()+
    geom_smooth(method='lm')+xlab(x)
  print(g)
  readline(prompt='ENTER')
}

#Get final plot
newdataNy<-data.frame(sqrtWY=seq(min(datanys$sqrtWY),max(datanys$sqrtWY),length.out=100),
                     lgAlt=rep(median(datanys$lgAlt),100))

predictN<-predict(MooNys5,newdata=newdataNy,type="response",se.fit=TRUE)
newdataNy$predM<-predictN$fit
newdataNy$predM.se<-predictN$se.fit

gny<-ggplot(aes(x=sqrtWY,y=predM),data=newdataNy)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=sqrtWY,y=fitted),data=datanys)+
  xlab("water yield")+theme_classic()+theme(legend.position="none")
print(gny)

MFPrecNy<-lm(predM~sqrtWY,data=newdataNy)
summary(MFPrecNy)

##timate limits for 0.5 probability

estN<-dose.p(MooNys5,p=0.5)
estN[1]^2
(estN[1]-(2*attributes(estN)$SE))^2
(estN[1]+(2*attributes(estN)$SE))^2


#10) Regresiones con desviación de sequia
dataD<-dataI%>%filter(.,serie%in%c("2013","medio")&P_DEPTO>0)%>%
  dplyr::select(c("Dp_WS","wyield","serie"))%>%
  reshape(.,timevar='serie',idvar='Dp_WS',direction='wide')%>%
  mutate(dvWY=(wyield.medio-wyield.2013)/100)
hist(dataD$dvWY)
mtch<-match(as.character(pts.ag$Dp_WS),dataD$Dp_WS)
pts.ag$dvWY<-dataD$dvWY[mtch]
pts.wyy<-pts.ag[pts.ag$p<1&pts.ag$p>0,]

#Model only tolima
dataTD<-pts.wyy[pts.wyy$dep=='Tol',]
rownames(dataTD)<-NULL
M1Td<-glm(cbind(sequia,nosequia)~PorcBosq*lgAlt*dvWY-PorcBosq:lgAlt:dvWY,
          data=dataTD,family=binomial)
summary(M1Td)
MoTd<-step(M1Td)
summary(MoTd)
plot(MoTd)
M2Td<-update(M1Td,data=dataTD[-14,])
plot(M2Td)
MoTd<-step(M2Td)
summary(MoTd)
plot(MoTd)

plot(x=dataTD$p[-14],y=predict(MoTd,type="response"))
dataTDD<-dataTD[-14,]
dataTDD$fitted<-predict(MoTd,type="response")
newdataTD<-data.frame(PorcBosq=rep(median(dataTDD$PorcBosq),100),
  lgAlt=rep(median(dataTDD$lgAlt),100),
  dvWY=seq(min(dataTDD$dvWY),max(dataTDD$dvWY),length.out=100))
predictT<-predict(MoTd,newdata=newdataTD,type="response",se.fit=TRUE)
newdataTD$predM<-predictT$fit
newdataTD$predM.se<-predictT$se.fit

gtD<-ggplot(aes(x=dvWY,y=predM),data=newdataTD)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=dvWY,y=p),data=dataTDD)+
  xlab("Deviation from avg. water yield")+theme_classic()+theme(legend.position="none")
print(gtD)
summary(newdataTy$predM)

#given that in tolima during 2013 was superior to the average, 
#the estimation was conducted directly 
M3Td<-glm(cbind(sequia,nosequia)~dvWY,
          data=dataTD,family=binomial)

summary(M3Td)
M4Td<-update(M3Td,family='quasibinomial')
summary(M4Td)


#0.5 lim water yield
estTd<-dose.p(M4Td,p=0.5)
estTd[1]^2
(estTd[1]+(2*attributes(estTd)$SE))^2
(estTd[1]-(2*attributes(estTd)$SE))^2

#the limit for the wateryield is unrealistic
M5Td<-lm(p~dvWY,data=dataTD)
summary(M5Td)

ivP<-inverse.predict(M5Td,0.5)
[1] -14.959639   4.551755
# limits on orginal units
ivP$`Confidence Limits`*100
-1495.9639   455.1755
ivP$Prediction*100
-520.3942
plot(x=dataTD$dvWY,y=M5Td$fitted.values)
### continue here ####

#Model only Magdalena
datamy<-pts.wy[pts.wy$dep=='Mag',]
M1My<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*lgAlt,
          data=datamy,family=binomial)
summary(M1My)
MoMy<-step(M1My)
summary(MoMy)
plot(MoMy)

MooMy<-update(M1My,family=quasibinomial)
summary(MooMy)

MooMy2<-update(MooMy,~.-PorcBosq:sqrtWY:lgAlt)
summary(MooMy2)
anova(MooMy,MooMy2,test="F") 

MooMy3<-update(MooMy2,~.-sqrtWY:lgAlt)
summary(MooMy3)
anova(MooMy2,MooMy3,test="F")

MooMy4<-update(MooMy3,~.-PorcBosq:lgAlt)
summary(MooMy4)
anova(MooMy3,MooMy4,test="F")

MooMy5<-update(MooMy4,~.-PorcBosq:sqrtWY)
summary(MooMy5)
anova(MooMy4,MooMy5,test="F")

MooMy6<-update(MooMy5,~.-lgAlt)
summary(MooMy6)
anova(MooMy5,MooMy6,test="F")

MooMy7<-update(MooMy6,~.+PorcBosq:sqrtWY)
summary(MooMy7)
anova(MooMy6,MooMy7,test="F")

MooMy8<-update(MooMy6,~.-PorcBosq)
summary(MooMy8)
anova(MooMy6,MooMy8,test="F")

MooMy9<-update(MooMy6,~.-sqrtWY)
summary(MooMy9)
anova(MooMy6,MooMy9,test="F")

plot(x=datamy$p,y=MooMy4$fitted.values)
datamy$fitted<-MooMy4$fitted.values
for(x in c('PorcBosq','sqrtWY','lgAlt')){
  g<-ggplot(aes(x=get(x),y=fitted),data=datamy)+geom_point()+
    geom_smooth(method='glm',se=TRUE, method.args = list(family=binomial))+xlab(x)
  print(g)
  readline(prompt='ENTER')
}

#Get  plot final
newdataMy<-data.frame(PorcBosq=rep(median(datamy$PorcBosq),100),
                      sqrtWY=seq(min(datamy$sqrtWY),max(datamy$sqrtWY),length.out=100),
                      lgAlt=rep(median(datamy$lgAlt),100))
predictM<-predict(MooMy4,newdata=newdataMy,type="response",se.fit=TRUE)
newdataMy$predM<-predictM$fit
newdataMy$predM.se<-predictM$se.fit

gmy<-ggplot(aes(x=sqrtWY,y=predM),data=newdataMy)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=sqrtWY,y=fitted),data=datamy)+
  geom_point(aes(x=sqrtWY,y=p),data=datamy, color='blue')+
  xlab("water Yield")+theme_classic()+theme(legend.position="none")
print(gmy)

MFPrecMy<-lm(predM~sqrtWY,data=newdataMy)
summary(MFPrecMy)
##timate limits for 0.5 probability

estM<-dose.p(MooMy4,p=0.5)
estM[1]^2
(estM[1]-(2*attributes(estM)$SE))^2
(estM[1]+(2*attributes(estM)$SE))^2




#Model only nariño
datany<-pts.wy[pts.wy$dep=='Nar',]
M1Ny<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*lgAlt,
          data=datany,family=binomial)
summary(M1Ny)
MoNy<-step(M1Ny)
summary(MoNy)
plot(MoNy)
MooNy<-update(M1Ny,family=quasibinomial)
summary(MooNy)


MooNy2<-update(MooNy,~.-PorcBosq:sqrtWY:lgAlt)
summary(MooNy2)
anova(MooNy,MooNy2,test="F") 

MooNy3<-update(MooNy2,~.-PorcBosq:lgAlt)
summary(MooNy3)
anova(MooNy2,MooNy3,test="F") 

MooNy4<-update(MooNy3,~.-PorcBosq:sqrtWY)
summary(MooNy4)
anova(MooNy3,MooNy4,test="F")

MooNy5<-update(MooNy4,~.-PorcBosq)
summary(MooNy5)
anova(MooNy4,MooNy5,test="F")
plot(MooNy5)


plot(x=datany$p,y=MooNy5$fitted.values)
datany$fitted<-MooNy5$fitted.values

#fix outliers
datany[rownames(datany)=="32",]
datanys<-datany[(rownames(datany)!="32"),]

M1Nys<-glm(cbind(sequia,nosequia)~PorcBosq*sqrtWY*lgAlt,
           data=datanys,family=binomial)
summary(M1Nys)
MoNys<-step(M1Nys)
summary(MoNys)
MooNys<-update(M1Nys,family=quasibinomial)
summary(MooNys)

MooNys2<-update(MooNys,~.-PorcBosq:sqrtWY:lgAlt)
summary(MooNys2)
anova(MooNys,MooNys2,test="F") 

MooNys3<-update(MooNys2,~.-PorcBosq:lgAlt)
summary(MooNys3)
anova(MooNys2,MooNys3,test="F") 

MooNys4<-update(MooNys3,~.-PorcBosq:sqrtWY)
summary(MooNys4)
anova(MooNys3,MooNys4,test="F")

MooNys5<-update(MooNys4,~.-PorcBosq)
summary(MooNys5)
anova(MooNys4,MooNys5,test="F")
plot(MooNys5)

datanys$fitted<-MooNys5$fitted.values

for(x in c('PorcBosq','sqrtWY','lgAlt')){
  g<-ggplot(aes(x=get(x),y=fitted),data=datanys)+geom_point()+
    geom_smooth(method='lm')+xlab(x)
  print(g)
  readline(prompt='ENTER')
}

#Get final plot
newdataNy<-data.frame(sqrtWY=seq(min(datanys$sqrtWY),max(datanys$sqrtWY),length.out=100),
                      lgAlt=rep(median(datanys$lgAlt),100))

predictN<-predict(MooNys5,newdata=newdataNy,type="response",se.fit=TRUE)
newdataNy$predM<-predictN$fit
newdataNy$predM.se<-predictN$se.fit

gny<-ggplot(aes(x=sqrtWY,y=predM),data=newdataNy)+geom_line()+
  geom_ribbon(aes(ymin = predM- predM.se, ymax = predM + predM.se,alpha=0.5), fill = "lightgray")+
  geom_point(aes(x=sqrtWY,y=fitted),data=datanys)+
  xlab("water yield")+theme_classic()+theme(legend.position="none")
print(gny)

MFPrecNy<-lm(predM~sqrtWY,data=newdataNy)
summary(MFPrecNy)

##timate limits for 0.5 probability

estN<-dose.p(MooNys5,p=0.5)
estN[1]^2
(estN[1]-(2*attributes(estN)$SE))^2
(estN[1]+(2*attributes(estN)$SE))^2


save(list=ls(),file=file.path(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto', 
                              'Objetivo_3_Valoraciones',paste('WrkSp_EE_',Sys.Date(),'.RData',sep=''))))
rm(list=ls())
