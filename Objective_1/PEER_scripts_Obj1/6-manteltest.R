library(vegan)
library(ggplot2)
library(reshape2)
#load community data
load(file="G:/My Drive/GEF_BosqueSeco/Analisis_Integrados/Scripts_R/commMatr.RData")
#load distance matrix
DD<-"G:/My Drive/GEF_BosqueSeco/Analisis_Integrados/Matrix_Distancia"
lfl<-list.files(DD,full.names = T,pattern="*.RData")
path<-file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Resultados Natalia_Indicadores")

for (i in lfl){
  ii<-gsub("(^.*)([A-Z]{2}[a-z]{2})(\\.RData)","\\2",i)
  load(file=i)
  assign(ii,dmati)
}

getMantel<-function(l1){
  wtnm<-names(l1[[1]][[1]])
  mantl<-list()
  distl<-list()
  for (i in wtnm){
    if(i=="Valle del Cauca") i="Valle"
    if(i=="TolimaS") i="Tolima"
    print(i)
    wtnm2<-substr(i,1,3)
    Dgeo<-paste("D",wtnm2,sep="")
    test1<-l1[[1]][[1]][[i]]
    commdist<-vegdist(test1,method="bray")
    commdist2<-melt(as.matrix(commdist),varnames=c("PlotII","PlotI"),value.name="commDist")
    geodist<-as.dist(get(Dgeo),upper=F)
    geodist2<-melt(as.matrix(geodist),varnames=c("PlotII","PlotI"),value.name="geoDist")
    geodist2<-geodist2[geodist2$geoDist!=0,]
    distF<-merge(commdist2,geodist2)
    distF$site<-i
    distl[[i]]<-distF
    mr<-match(attributes(commdist)$Labels,attributes(geodist)$Labels)
    geodist3<-as.matrix(geodist)
    geodist<-as.dist(geodist3[mr,mr])
    mtest<-mantel(commdist, geodist, method="pearson", permutations=999)
    mantl[[i]]<-data.frame("corr"=mtest$statistic,"sign"=mtest$signif,"site"=i)
  }
  return(list(mantl,distl))
}

plotMantel<-function(DF1,DF2,nmwtsh){
  DF2$geoDist<-aggregate(geoDist~site,data=DF1,FUN=function(x){max(x)-(max(x)*0.1)})[,"geoDist"]
  DF2$commDist<-aggregate(commDist~site,data=DF1,FUN=function(x){min(x)+(min(x)*0.1)})[,"commDist"]
  plotm<-ggplot(data=DF1,aes(x=geoDist,y=commDist))+geom_point()+
    geom_smooth(method="lm")+theme_bw()+facet_wrap(~site,scales="free")+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text.x=element_text(size=14,face="bold"),
          strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),
          axis.title=element_text(size=14),panel.spacing.x=unit(1.2,"lines"),
          plot.margin=unit(c(0.5,0.3,0.3,0.3),"cm"),plot.title=element_text(hjust=0.5))+
    xlab("Distancia Geográfica (m)")+ylab("Distancia Ecológica")+ggtitle(nmwtsh)+
    geom_text(data=DF2,aes(x=geoDist,y=commDist,label=paste("corr=",round(corr,2),"(",round(sign,3),")",sep="")))
  return(plotm)
}

#Vegetacion
MantelV<-getMantel(CompV)
Mant.testV<-do.call(rbind,MantelV[[1]])
Mant.distV<-do.call(rbind,MantelV[[2]])
rownames(Mant.distV)<-NULL
Mant.testV<-Mant.testV[order(as.character(Mant.testV$site)),]
plotV<-plotMantel(Mant.distV,Mant.testV,nmwtsh="VEGETACIÓN")
print(plotV)
ggsave(paste(path,"MantelV.pdf",sep="/"),plotV,scale=2)

#Aves
#Seco

MantelA<-getMantel(CompAs)
Mant.testA<-do.call(rbind,MantelA[[1]])
Mant.distA<-do.call(rbind,MantelA[[2]])
rownames(Mant.distA)<-NULL
Mant.testA<-Mant.testA[order(as.character(Mant.testA$site)),]
Mant.testA$geoDist<-12000
Mant.testA$commDist<-0.8
plotA<-plotMantel(Mant.distA,Mant.testA,nmwtsh="AVES_seca")
print(plotA)
ggsave(paste(path,"MantelAs.pdf",sep="/"),plotA,scale=2)

#lluvias
MantelA<-getMantel(CompAl)
Mant.testA<-do.call(rbind,MantelA[[1]])
Mant.distA<-do.call(rbind,MantelA[[2]])
rownames(Mant.distA)<-NULL
Mant.testA<-Mant.testA[order(as.character(Mant.testA$site)),]
Mant.testA$geoDist<-12000
Mant.testA$commDist<-0.8
plotA<-plotMantel(Mant.distA,Mant.testA,nmwtsh="AVES_lluvias")
print(plotA)
ggsave(paste(path,"MantelAl.pdf",sep="/"),plotA,scale=2)

#Hormigas
#Censo
MantelH<-getMantel(CompHc)
Mant.testH<-do.call(rbind,MantelH[[1]])
Mant.distH<-do.call(rbind,MantelH[[2]])
rownames(Mant.distA)<-NULL
Mant.testH<-Mant.testH[order(as.character(Mant.testH$site)),]
plotH<-plotMantel(Mant.distH,Mant.testH,"HORMIGAS_Censo")
print(plotH)
ggsave(paste(path,"MantelHc.pdf",sep="/"),plotH,scale=2)

#recenso
MantelH<-getMantel(CompHr)
Mant.testH<-do.call(rbind,MantelH[[1]])
Mant.distH<-do.call(rbind,MantelH[[2]])
rownames(Mant.distA)<-NULL
Mant.testH<-Mant.testH[order(as.character(Mant.testH$site)),]
plotH<-plotMantel(Mant.distH,Mant.testH,"HORMIGAS_Recenso")
print(plotH)
ggsave(paste(path,"MantelHr.pdf",sep="/"),plotH,scale=2)

#Mamiferos ###Revisar esto porque la matriz de distacía no corresponde
MantelM<-getMantel(CompM)
Mant.testM<-do.call(rbind,MantelM[[1]])
Mant.distM<-do.call(rbind,MantelM[[2]])
rownames(Mant.distA)<-NULL
Mant.testM<-Mant.testM[order(as.character(Mant.testM$site)),]
Mant.testM$geoDist<-1100
Mant.testM$commDist<-1.0
plotM<-plotMantel(Mant.distM,Mant.testM,"MAMÍFEROS")
ggsave(paste(path,"MantelM.pdf",sep="/"),plotM)

