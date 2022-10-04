# source("~/Desktop/IAvH/PNUD/R/0-ImportAllData.R")
# source("~/Desktop/IAvH/PNUD/R/1-calc.divAll.R")
source(file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Scripts_R","0-ImportAllData.R"))
source(file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Scripts_R","1-calc.divAll.R"))

library(vegan)
library(ggplot2)
#data= DataV_PT
#wcl="watershed"; tcl="transfC";pcl="permC";abcl="IndNumber";spcl="Species"; pltc="NewPlotCode"
#j=1


calc.compo=function(data,wcl,tcl,pcl,abcl,spcl,pltc)
{
  # data<-LDataAT[[1]]
  # wcl<-"Departamento" Column name for watershed ID
  # tcl<-"Trans_c" Column name for transformation
  # pcl<-"permC" Column name for permanence (succession)
  # abcl<-"individuos" column name for number of individuals
  # spcl<-"Species" column name of species (or other taxon) ID
  #pltc<-"plot code columns"
  
  wcln<-which(names(data)==wcl)
  tcln<-which(names(data)==tcl)
  #if(!is.null(pcl)) pcln<-which(names(data)==pcl) #only if successional stage is taken into account
  pcln<-which(names(data)==pcl) #only if successional stage is taken into account
  abcln<-which(names(data)==abcl)
  spcln<-which(names(data)==spcl)
  pltn<-which(names(data)==pltc)

  Lws<-unique(data[,wcln])
  nws<-length(Lws)  #number of wathershed
  tab.trat <-list()
  tratam<-list()

  for (j in 1:nws){
    wsnm<-Lws[j]
    print(wsnm)
    data2<-subset(data,data[,wcln]==wsnm&!is.null(data[,spcl]))
    data2$trat<-paste(data2[,tcl],data2[,pcl],sep="-")
    tab.trat[[wsnm]]<-as.data.frame.matrix(xtabs(data2[,abcln]~data2[,pltn]+data2[,spcln],data=data2))
    tratDF=unique(data.frame(data2[,"trat"],data2[,tcl],data2[,pcl],data2[,pltc]))
    names(tratDF)<-c("CombTrat","transfC","permC","plot")
    tratDF$watershed=wsnm
    tratam[[wsnm]]=tratDF
  }
  return(list(tab.trat,tratam))
}
comp.lst<-function(l1,l2){
  l3<-list()
  nl<-length(l1)
  
  for (i in 1:nl){
    ll1<-as.data.frame(l1[[i]])
    ll1$plot<-rownames(ll1)
    ll2<-as.data.frame(l2[[i]])
    ll2[order(ll2$plot),]
    l3[[i]]<-merge(ll1,ll2,all.x=TRUE)
  }
  return(l3)
} #merges information from the two lists 
#l1=list of nmds scores, l2=list with attributes by plot



##FIGURAS
##################


#path="~/Desktop/IAvH/PNUD/R/figuras/"
path<-file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Resultados Natalia_Indicadores")

# a) vegetación
DataV_PT$watershed[DataV_PT$watershed=="Valle del Cauca"]<-"Valle"
CompV<-lapply(X=list(DataV_PT), FUN= calc.compo, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species",pltc="NewPlotCode")
ab.matsV=CompV[[1]][[1]]
nm.matsV=CompV[[1]][[2]]
#check
# bol=ab.mats[[2]]
# nm.bol=metaMDS(bol,distance="horn",binary=F)
# ordiplot(nm.bol,type="n",xlab="",ylab="")
# orditorp(nm.bol,display="sites",labels=rownames(bol))

nmdss=lapply(ab.matsV,FUN=metaMDS,distance="horn", binary=F)
#scores(nmdss[[1]])
scores.temp=lapply(nmdss,FUN=scores)
scores.comp<-comp.lst(scores.temp,nm.matsV)
scores.nmds=as.data.frame(do.call(rbind, scores.comp))
names(scores.nmds)[7]<-"site"
scores.nmds$site[scores.nmds$site=="TolimaS"]="Tolima"
scores.nmds$site[scores.nmds$site=="Valle del Cauca"]="Valle"
scores.nmds$trans2[scores.nmds$transfC=="High"]="Alta"
scores.nmds$trans2[scores.nmds$transfC=="Medium"]="Media"
scores.nmds$trans2[scores.nmds$transfC=="Low"]="Baja"

scores.nmds$Transformación<-factor(scores.nmds$trans2,levels=c("Baja","Media","Alta"))

ggnmds=ggplot()+geom_point(data=scores.nmds,aes(x=NMDS1,y=NMDS2,shape=Transformación,colour=Transformación),size=4)+
  theme_bw()+facet_wrap(~site) + theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
                                       strip.text.x=element_text(size=14,face="bold"),
                                       strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),
                                       axis.title=element_text(size=14),panel.spacing.x=unit(1.2,"lines"),
                                       plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),plot.title=element_text(hjust=0.5))+
  ggtitle("VEGETACIÓN")
print(ggnmds)
ggsave(paste(path,"nmdsV_SR.pdf",sep="/"),ggnmds,scale=2)




# b) Aves
DataA_PT$watershed[DataA_PT$watershed=="Valle del Cauca"]<-"Valle"
#Todas
CompA<-lapply(X=list(DataA_PT), FUN= calc.compo, wcl="watershed", tcl="transfC",pcl="permC",
              abcl="IndNumber",spcl="Species",pltc="PlotAsoc")
CompAt<-CompA
ab.matsA=CompA[[1]][[1]]
nm.matsA=CompA[[1]][[2]]
nmdss=lapply(ab.matsA,FUN=metaMDS,distance="horn", binary=F)
scores.temp=lapply(nmdss,FUN=scores)
scores.comp<-comp.lst(scores.temp,nm.matsA)
scores.nmds=as.data.frame(do.call(rbind, scores.comp))
names(scores.nmds)[7]<-"site"
scores.nmds$site[scores.nmds$site=="Valle del Cauca"]="Valle"
scores.nmds$trans2[scores.nmds$transfC=="High"]="Alta"
scores.nmds$trans2[scores.nmds$transfC=="Medium"]="Media"
scores.nmds$trans2[scores.nmds$transfC=="Low"]="Baja"
scores.nmds$Transformación<-factor(scores.nmds$trans2,levels=c("Baja","Media","Alta"))

ggnmds=ggplot()+geom_point(data=scores.nmds,aes(x=NMDS1,y=NMDS2,shape=Transformación,colour=Transformación),
                           size=4)+theme_bw()+facet_wrap(~site) + theme(axis.text.x = element_text(size=12),
                                                                        axis.text.y = element_text(size=12),
                                                                        strip.text.x=element_text(size=14,
                                                                                                  face="bold"),
                                                                        strip.background=element_rect(colour="lightsalmon",
                                                                                                      fill="lightsalmon"),
                                                                        axis.title=element_text(size=14),
                                                                        panel.spacing.x=unit(1.2,"lines"),
                                                                        plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),
                                                                        plot.title=element_text(hjust=0.5))+
  ggtitle("AVES_todas")

print(ggnmds)
ggsave(paste(path,"nmdsA_SR.pdf",sep="/"),ggnmds,scale=2)

#Seca
DataAS_PT<-DataA_PT[DataA_PT$Estacion=="Seca",]
CompA<-lapply(X=list(DataAS_PT), FUN= calc.compo, wcl="watershed", tcl="transfC",pcl="permC",
              abcl="IndNumber",spcl="Species",pltc="PlotAsoc")
CompAs<-CompA
ab.matsA=CompA[[1]][[1]]
nm.matsA=CompA[[1]][[2]]
nmdss=lapply(ab.matsA,FUN=metaMDS,distance="horn", binary=F)
scores.temp=lapply(nmdss,FUN=scores)
scores.comp<-comp.lst(scores.temp,nm.matsA)
scores.nmds=as.data.frame(do.call(rbind, scores.comp))
names(scores.nmds)[7]<-"site"
scores.nmds$site[scores.nmds$site=="Valle del Cauca"]="Valle"
scores.nmds$trans2[scores.nmds$transfC=="High"]="Alta"
scores.nmds$trans2[scores.nmds$transfC=="Medium"]="Media"
scores.nmds$trans2[scores.nmds$transfC=="Low"]="Baja"
scores.nmds$Transformación<-factor(scores.nmds$trans2,levels=c("Baja","Media","Alta"))

ggnmds=ggplot()+geom_point(data=scores.nmds,aes(x=NMDS1,y=NMDS2,shape=Transformación,colour=Transformación),
                           size=4)+theme_bw()+facet_wrap(~site) + theme(axis.text.x = element_text(size=12),
                                                                        axis.text.y = element_text(size=12),
                                                                        strip.text.x=element_text(size=14,
                                                                                                  face="bold"),
                                                                        strip.background=element_rect(colour="lightsalmon",
                                                                                                      fill="lightsalmon"),
                                                                        axis.title=element_text(size=14),
                                                                        panel.spacing.x=unit(1.2,"lines"),
                                                                        plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),
                                                                        plot.title=element_text(hjust=0.5))+
  ggtitle("AVES")
  
print(ggnmds)
ggsave(paste(path,"nmdsAs_SR.pdf",sep="/"),ggnmds,scale=2)

#Lluvias
DataAL_PT<-DataA_PT[DataA_PT$Estacion=="Lluviosa",]
CompA<-lapply(X=list(DataAL_PT), FUN= calc.compo, wcl="watershed", tcl="transfC",pcl="permC",
              abcl="IndNumber",spcl="Species",pltc="PlotAsoc")
CompAl<-CompA
ab.matsA=CompA[[1]][[1]]
nm.matsA=CompA[[1]][[2]]
nmdss=lapply(ab.matsA,FUN=metaMDS,distance="horn", binary=F)
scores.temp=lapply(nmdss,FUN=scores)
scores.comp<-comp.lst(scores.temp,nm.matsA)
scores.nmds=as.data.frame(do.call(rbind, scores.comp))
names(scores.nmds)[7]<-"site"
scores.nmds$site[scores.nmds$site=="Valle del Cauca"]="Valle"
scores.nmds$trans2[scores.nmds$transfC=="High"]="Alta"
scores.nmds$trans2[scores.nmds$transfC=="Medium"]="Media"
scores.nmds$trans2[scores.nmds$transfC=="Low"]="Baja"
scores.nmds$Transformación<-factor(scores.nmds$trans2,levels=c("Baja","Media","Alta"))

ggnmds=ggplot()+geom_point(data=scores.nmds,aes(x=NMDS1,y=NMDS2,shape=Transformación,colour=Transformación),
                           size=4)+theme_bw()+facet_wrap(~site) + theme(axis.text.x = element_text(size=12),
                                                                        axis.text.y = element_text(size=12),
                                                                        strip.text.x=element_text(size=14,
                                                                                                  face="bold"),
                                                                        strip.background=element_rect(colour="lightsalmon",
                                                                                                      fill="lightsalmon"),
                                                                        axis.title=element_text(size=14),
                                                                        panel.spacing.x=unit(1.2,"lines"),
                                                                        plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),
                                                                        plot.title=element_text(hjust=0.5))+
  ggtitle("AVES (lluvias)")

print(ggnmds)
ggsave(paste(path,"nmdsAl_SR.pdf",sep="/"),ggnmds,scale=2)

# c) Mamíferos

DataM1_T$transfC[DataM1_T$transfC=="Alta"]="High"
DataM1_T$transfC[DataM1_T$transfC=="Baja"]="Low"
DataM1_T$transfC[DataM1_T$transfC=="Med"]="Medium"
DataM1_T$transfC[DataM1_T$transfC=="Media"]="Medium"
DataM1_T$plot<-paste(DataM1_T$transfC,DataM1_T$Estacionf2,sep="-")
CompM<-lapply(X=list(DataM1_T), FUN= calc.compo, wcl="watershed", tcl="transfC",pcl="Estacionf2",abcl="IndNumber",spcl="Species",pltc="plot")

ab.matsM=CompM[[1]][[1]]
nm.matsM<-CompM[[1]][[2]]
nmdss=lapply(ab.matsM,FUN=metaMDS,distance="horn", binary=F)
###Error in convergence, it couldn't be run
scores.temp=lapply(nmdss,FUN=scores)
scores.comp<-comp.lst(scores.temp,nm.matsM)

scores.nmds=as.data.frame(do.call(rbind, scores.comp))
names(scores.nmds)[7]<-"site"
scores.nmds$trans2[scores.nmds$transfC=="High"]="Alta"
scores.nmds$trans2[scores.nmds$transfC=="Medium"]="Media"
scores.nmds$trans2[scores.nmds$transfC=="Low"]="Baja"
scores.nmds$Transformación<-factor(scores.nmds$trans2,levels=c("Baja","Media","Alta"))

ggnmds=ggplot()+geom_point(data=scores.nmds,aes(x=NMDS1,y=NMDS2,shape=Transformación,colour=Transformación),size=4)+theme_bw()+facet_wrap(~site) + theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),panel.spacing.x=unit(1.2,"lines"),plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),plot.title=element_text(hjust=0.5))+ggtitle("VERTEBRADOS TERRESTRES")

  
print(ggnmds)
ggsave(paste(path,"nmdsM_SR.pdf",sep="/"),ggnmds)


# d) Hormigas

DataH_PT$Estacion[DataH_PT$Estacion=="recenso"]<-"Recenso"
DataH_PT$transfC[DataH_PT$transfC=="Med"]<-"Medium"

DataHc<-list(subset(DataH_PT,Estacion=="Censo"))
DataHh<-list(subset(DataH_PT,Estacion=="Recenso"))
#Todas

CompH<-lapply(X=list(DataH_PT), FUN= calc.compo, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species",pltc="AsocPlot")
CompHt<-CompH
ab.matsH=CompH[[1]][[1]]
nm.matsH<-CompH[[1]][[2]]
nmdss=lapply(ab.matsH,FUN=metaMDS,distance="horn", binary=F)
scores.temp=lapply(nmdss,FUN=scores)
scores.comp<-comp.lst(scores.temp,nm.matsH)
scores.nmds=as.data.frame(do.call(rbind, scores.comp))
names(scores.nmds)[7]<-"site"
scores.nmds$trans2[scores.nmds$transfC=="High"]="Alta"
scores.nmds$trans2[scores.nmds$transfC=="Medium"]="Media"
scores.nmds$trans2[scores.nmds$transfC=="Low"]="Baja"
scores.nmds$Transformación<-factor(scores.nmds$trans2,levels=c("Baja","Media","Alta"))
ggnmds=ggplot()+geom_point(data=scores.nmds,aes(x=NMDS1,y=NMDS2,shape=Transformación,colour=Transformación),size=4)+
  theme_bw()+facet_wrap(~site) + theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),panel.spacing.x=unit(1.2,"lines"),plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),plot.title=element_text(hjust=0.5))+
  ggtitle("HORMIGAS (todas)")
print(ggnmds)
ggsave(paste(path,"nmdsH_SR.pdf",sep="/"),ggnmds,scale=2)
#Censo

CompH<-lapply(X=DataHc, FUN= calc.compo, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species",pltc="AsocPlot")
CompHc<-CompH
ab.matsH=CompH[[1]][[1]]
nm.matsH<-CompH[[1]][[2]]
nmdss=lapply(ab.matsH,FUN=metaMDS,distance="horn", binary=F)
scores.temp=lapply(nmdss,FUN=scores)
scores.comp<-comp.lst(scores.temp,nm.matsH)
scores.nmds=as.data.frame(do.call(rbind, scores.comp))
names(scores.nmds)[7]<-"site"
scores.nmds$trans2[scores.nmds$transfC=="High"]="Alta"
scores.nmds$trans2[scores.nmds$transfC=="Medium"]="Media"
scores.nmds$trans2[scores.nmds$transfC=="Low"]="Baja"
scores.nmds$Transformación<-factor(scores.nmds$trans2,levels=c("Baja","Media","Alta"))
ggnmds=ggplot()+geom_point(data=scores.nmds,aes(x=NMDS1,y=NMDS2,shape=Transformación,colour=Transformación),size=4)+theme_bw()+facet_wrap(~site) + theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),panel.spacing.x=unit(1.2,"lines"),plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),plot.title=element_text(hjust=0.5))+ggtitle("HORMIGAS (censo)")
print(ggnmds)
ggsave(paste(path,"nmdsHc_SR.pdf",sep="/"),ggnmds,scale=2)

#Recenso

CompH<-lapply(X=DataHh, FUN= calc.compo, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species",pltc="AsocPlot")
CompHr<-CompH
ab.matsH=CompH[[1]][[1]]
nm.matsH<-CompH[[1]][[2]]
nmdss=lapply(ab.matsH,FUN=metaMDS,distance="mahalanobis", binary=F)
scores.temp=lapply(nmdss,FUN=scores)
scores.comp<-comp.lst(scores.temp,nm.matsH)
scores.nmds=as.data.frame(do.call(rbind, scores.comp))
names(scores.nmds)[7]<-"site"
scores.nmds$trans2[scores.nmds$transfC=="High"]="Alta"
scores.nmds$trans2[scores.nmds$transfC=="Medium"]="Media"
scores.nmds$trans2[scores.nmds$transfC=="Low"]="Baja"
scores.nmds$Transformación<-factor(scores.nmds$trans2,levels=c("Baja","Media","Alta"))
ggnmds=ggplot()+geom_point(data=scores.nmds,aes(x=NMDS1,y=NMDS2,shape=Transformación,colour=Transformación),size=4)+theme_bw()+facet_wrap(~site) + theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),panel.spacing.x=unit(1.2,"lines"),plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),plot.title=element_text(hjust=0.5))+ggtitle("HORMIGAS (Recenso)")
print(ggnmds)
ggsave(paste(path,"nmdsHr_SR.pdf",sep="/"),ggnmds,scale=2)

save(CompV,CompAt,CompAs,CompAl,CompHt, CompHc,CompHr,CompM, file="G:/My Drive/GEF_BosqueSeco/Analisis_Integrados/Scripts_R/commMatr.RData")

