library(vegan)
library(ggplot2)
library(gridExtra)
library(raster)
library(BiodiversityR)
library(adiv)
library(randomForest)
library(dplyr)
source ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/cleanplot.pca.R')
source ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR1/evplot.R')

#1) Define working spaces 
WD<-("C:/Users/dsrbu/Dropbox/Humboldt/Documentos de Trabajo/BosqueSeco/GEF/Analysis/MultiTRes")
setwd(WD)
path<-file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Resultados Natalia_Indicadores")

#2) Check COmmunity data
load(file="G:/My Drive/GEF_BosqueSeco/Analisis_Integrados/Scripts_R/commMatr.RData")
colty<-data.frame("site"=c("Cesar","Valle del Cauca","Guajira",
                           "Bolivar","Tolima","Huila"),"col"=c("red","blue","green","black","magenta","gray"),stringsAsFactors =F )
coltyt<-data.frame("Trans"=c("High","Med","Low"),"col"=c("red","blue","green"), stringsAsFactors = F)
coltyp<-data.frame("Perm"=c("Med","High","Fire","Low"),"col"=c("blue","green","orange","red"),stringsAsFactors = F)

#3) get covariates
#Covariates include Average Forest integrity index (AvFIntr201), distance to roads and settlements 
#Mean canopy height and its standard deviation, minimun and maximun year of disturbance (from 2000=1 to 2016=16),
#Maximum and minimum forest condition index (90-2016), and landscape sum of forest condition index (sums polygons with the
#same transformation level).
CovD<-read.table(file.path("C:","Users","dsrbu","Dropbox","Humboldt","Documentos de trabajo",
                          "BosqueSeco","Analisis","CovariatesBIDGEF","PlotsCov.csv"), stringsAsFactors=F,header=T,sep=",")

#Get plot info
Shp<-shapefile(file.path("C:","Users","dsrbu","Dropbox","Humboldt","Documentos de trabajo","BosqueSeco",
                         "GEF","Analysis","Exported","PlotsAll.shp"))
selc<-Shp@data$OBJECTID%in%CovD$Site
ShpD<-Shp@data[selc,]

#join covariate and plot info
matchC<-match(CovD$Site,ShpD$OBJECTID)
CovD[,c("Code","Elev","Transf","Perm","Depart")]<-ShpD[matchC,c("NAME","ELEVATION","Transforma","Permanenci","Departamen")]

CovD$Code<-gsub("B0","",CovD$Code)
CovD$Transf[CovD$Transf=="Medium"]<-"Med"
CovD$Perm[CovD$Perm=="Medium"]<-"Med"
write.csv(CovD,file=file.path("C:","Users","dsrbu","Dropbox","Humboldt","Documentos de trabajo",
                         "BosqueSeco","Analisis","CovariatesBIDGEF","PlotsCovCodes.csv"))
infc<-c("Site","Code","Transf","Perm","Depart")
varc<-!names(CovD)%in%infc
#Select variables
selv<-list()
loopv<-names(CovD)[varc]
loopv<-loopv[c(36:length(loopv))]
for (i in loopv ){
  CovDD<-CovD
  names(CovDD)[which(names(CovDD)==i)]<-"value"
  print(paste("start with",i))
  try(print(ggplot(data=CovDD,aes(y=value,x=Transf,colour=Perm))+geom_boxplot()+
        facet_wrap(~Depart)+ggtitle(i)))
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
  #try(hist(CovDD$value,main=i))
 # n1<-readline(prompt="Enter [1] to include, [0] to exclude variable: " )
  #n2<-readline(prompt="which type of transformation: [0]=non,log,sqrt,1/x,^: " )
  #selv[[i]]<-data.frame("include"=as.integer(n1), "transf"=as.character(n2),stringsAsFactors = F)
}
selvv<-do.call(rbind,selv)
selvv$variable<-rownames(selvv)
save(selvv,file=file.path(WD,"selvDF.RData"))
# Explore correlation among variables
inclv<-selvv$variable[selvv$include==1]
ESACv<-inclv[grep("^ESA",inclv)]
Distv<-inclv[grep("^DisA|dvia",inclv)]
Fintv<-inclv[grep("^MSPA|FInt",inclv)]
Climv<-inclv[grep("^Tmax|Precp|Hume",inclv)]
Verdv<-inclv[grep("^evi|ndvi",inclv)]
#list(ESACv,Distv,Fintv,Climv,Verdv)
selv2<-list()
for (i in list(ESACv,Distv,Fintv,Climv,Verdv)){
  pairs(CovD[,i])
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
  for(j in i){
    if(j%in%test){
    CovDD<-CovD[,c(j,"Transf","Perm","Depart")]
    names(CovDD)[which(names(CovDD)==j)]<-"value"
    print(j)
    try(print(ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
                facet_wrap(~Depart)+ggtitle(j)))
    readline(prompt="Press [enter] to continue or [ESC] to exit: ")
    try(print(ggplot(data=CovDD,aes(y=value,x=Perm))+geom_boxplot()+
                            facet_wrap(~Depart)+ggtitle(j)))
    readline(prompt="Press [enter] to continue or [ESC] to exit: ")
    try(print(ggplot(data=CovDD,aes(y=value,x=Depart))+geom_boxplot()+
                ggtitle(j)))
    n1<-readline(prompt="Enter [1] to include, [0] to exclude variable: " )
    n2<-readline(prompt="which scale plot,transf,dep?: " )
    selv2[[j]]<-data.frame("include"=as.integer(n1), "transf"=as.character(n2),stringsAsFactors = F)
  }}
}
selvv2<-do.call(rbind,selv2)
save(WD,file="selvv2.RData")
finv<-selvv2[selvv2$include==1,]
finv<-finv[!is.na(finv$include),]



#final plots covariates selected using correlation plots and boxplots
nmfinv<-rownames(finv)
pairs(CovD[,nmfinv[1:15]])
#correct variables excluding those correlated
finv<-finv[!nmfinv%in%c("dviameanest.500","dviameanest.5000",
                       "evi90trend.500","ndvi90trend.500",
                       "evi10season.5000","ndvi10season.500","evi90peak.30",
                       "ndvi10season.30","ndvi90trend.30"),]
finv<-finv[!nmfinv%in%c("TmaxcofvarApr.500","FIntcofv.500",
                        "FIntcofv.500","FIntcofv.5000",
                        "MSPAMeanx00.5000","MSPAcofv13.500","Precp10Jul.500",
                        "MSPAdynM.5000","TmaxcofvarOct.500","FInt90.5000",
                        "HumecofvarAug.500"),]
nmfinv<-rownames(finv)

selvv2<-selvv2[rownames(selvv2)%in%nmfinv,]
save(WD,file="selvv2.RData")
#Transform variables
for (i in nmfinv){
  print(i)
  hist(CovD[,i],main=i)
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
}
CovDF<-CovD[,nmfinv]
CovDF$lgDisAcoefvest.30<-log(CovDF$DisAcoefvest.30+0.0001)
CovDF$lgdviacoefvest.5000<-log(CovDF$dviacoefvest.5000+0.01)
CovDF$lgndvi10season.5000<-log(CovDF$ndvi10season.5000)
CovDF$lgndvi90peak.5000<-log(CovDF$ndvi90peak.5000)
CovDF$sqrtTmaxp90Feb.30<-sqrt(CovDF$Tmaxp90Feb.30)
CovDF$lgHumesigma.30<-log(CovDF$Humesigma.30)
CovDF$lgTmaxsigma.500<-log(CovDF$Tmaxsigma.500)
CovDF$sqrtPrecp10May.500<-sqrt(CovDF$Precp10May.500)
CovDF$lgHumecofvarJan.5000<-log(CovDF$HumecofvarJan.5000)
CovDF$lgFIntMeanx.30<-log(CovDF$FIntMeanx.30)
CovDF$lgMSPAMeanx13.500<-log(CovDF$MSPAMeanx13.500)
CovDF$lgMSPAcofv00.500<-log(CovDF$MSPAcofv00.500)
CovDF$sqrtFIntMeanx.5000<-sqrt(CovDF$FIntMeanx.5000)
CovDF$lgMSPAMeanx13.5000<-log(CovDF$MSPAMeanx13.5000)
CovDF$IvMSPAcofv00.5000<-1/(CovDF$MSPAcofv00.5000)
CovDF$lgMSPAdynCf.5000<-log(CovDF$MSPAdynCf.5000+50)

for (i in colnames(CovDF)){
  print(i)
  hist(CovDF[,i],main=i)
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
}

nmfinv<-names(CovDF)[!names(CovDF)%in%c("DisAcoefvest.30","dviacoefvest.5000","Tmaxp90Feb.30",
                                        "Humesigma.30","Tmaxsigma.500","Precp10May.500",
                                        "HumecofvarJan.5000","FIntMeanx.30","MSPAMeanx13.500",
                                        "MSPAcofv00.500","FIntMeanx.5000","MSPAMeanx13.5000",
                                        "MSPAcofv00.5000","MSPAdynCf.5000","dviacoefvest.5000",
                                        "ndvi10season.5000","ndvi90peak.5000")]

CovDF<-CovDF[,nmfinv]
CovDF<-cbind(CovD[,infc],CovDF)
#check scale
selv3<-list()
for (i in nmfinv){
  #readline(prompt="Press [enter] to continue or [ESC] to exit: ")
  CovDD<-CovDF[,c(i,"Transf","Perm","Depart")]
  names(CovDD)[which(names(CovDD)==i)]<-"value"
  print(i)
  try(print(ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
              facet_wrap(~Depart)+ggtitle(i)))
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
  try(print(ggplot(data=CovDD,aes(y=value,x=Perm))+geom_boxplot()+
              facet_wrap(~Depart)+ggtitle(i)))
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
  try(print(ggplot(data=CovDD,aes(y=value,x=Depart))+geom_boxplot()+
              ggtitle(i)))
  n2<-readline(prompt="which scale plot,transf,dep?: " )
  selv3[[i]]<-data.frame("scale"=as.character(n2),stringsAsFactors = F)
  
}
selvv3<-do.call(rbind,selv3)
selvv3$varbl<-rownames(selvv3)
rownames(selvv3)<-NULL
save(selvv3,file=file.path(WD,"selvv3.RData"))
write.csv(CovDF,file=file.path("C:","Users","dsrbu","Dropbox","Humboldt","Documentos de trabajo",
                              "BosqueSeco","Analisis","CovariatesBIDGEF","GEFCovFinal.csv"))
# Database by scale
pl_var<-selvv3[selvv3$scale=="plot","varbl"]
CovPl<-cbind(CovDF[,infc],CovDF[,pl_var])
trns_var<-selvv3[selvv3$scale=="trans","varbl"]
CovTrns<-cbind(CovDF[,infc],CovDF[,trns_var])
dep_var<-selvv3[selvv3$scale=="dep","varbl"]
CovDep<-cbind(CovDF[,infc],CovDF[,dep_var])

#4) ordination analyses by scale

PlotPCA<-function(PCA,des="NN",selr=NULL){
  par(mfrow=c(1,1))
  if(is.null(selr)) selr=1:dim(CovDF)[1]
  nax<-length(attributes(PCA$Ybar)$dimnames[[2]])
  sig <- PCAsignificance (PCA, axes = nax)
  print(sig)
  barplot (sig[c('percentage of variance', 'broken-stick percentage'), ], beside = T, 
           xlab = 'PCA axis', ylab = 'explained variation [%]', col = c('grey', 'black'), 
           legend = TRUE)
  q1<-as.numeric(readline(prompt="Numer of significan axis: " ))
  if(q1==1|is.null(q1)) q1<-2
  loadings<-scores(PCA,display='species', scaling=0,choices=c(1:q1))
  for (a in 1:q1){
    print("loadings= ",a)
    print(sort (abs (loadings[,a]), decreasing = TRUE))
  }
  minPCA<-apply(scores(PCA)$sites,2,max)
  par (mfrow = c(1,2))
  cleanplot.pca (PCA, scaling = 1)
  cleanplot.pca (PCA, scaling = 2)
  readline(prompt="press [ENTER] " )
  if(q1>=2){
    cleanplot.pca (PCA, scaling = 1,ax1=2,ax2=3)
    cleanplot.pca (PCA, scaling = 1,ax1=1,ax2=3)
    readline(prompt="press [ENTER] " )
  }
  
  for(ii in 1:(q1-1)){
    for(jj in (ii+1):q1){
      print(paste("plots of axis",ii,"and",jj))
      par (mfrow = c(2,2))
      cleanplot.pca (PCA, scaling = 1,cex=0.8,ax1=ii,ax2=jj)
      ordiplot(PCA,display='sites',type='n',main="Cuenca",cex=0.8,choices=c(ii,jj))
      colty2<-match(CovDF$Depart[selr],colty$site)
      colty22<-unique(colty2)
      points(PCA,col=colty$col[colty2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=substr(colty$site[colty22],1,4),col=colty$col[colty22],cex=0.5,pch=18,bty="n")
      coltyt2<-match(CovDF$Trans[selr],coltyt$Trans)
      coltyt22<-unique(coltyt2)
      ordiplot(PCA,display='sites',type='n',main="Transformación",cex=0.8,choices=c(ii,jj))
      points(PCA,col=coltyt$col[coltyt2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyt$Trans[coltyt22],col=coltyt$col[coltyt22],cex=0.5,pch=18,bty="n")
      coltyp2<-match(CovDF$Perm[selr],coltyp$Perm)
      coltyp22<-unique(coltyp2)
      ordiplot(PCA,display='sites',type='n',main="Plot",cex=0.8,choices=c(ii,jj))
      points(PCA,col=coltyp$col[coltyp2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyp$Perm[coltyp22],col=coltyp$col[coltyp22],cex=0.5,pch=18,bty="n")
      readline(prompt="press [ENTER] " )
    }
  }
  
  #save plots
  for(ii in 1:(q1-1)){
    for(jj in (ii+1):q1){
      des1<-paste(des,ii,jj,sep="")
      nmp<-paste(WD,"Plots",paste(des1,"jpeg",sep="."),sep="/")
      jpeg(filename=nmp,quality=100,res=300,width=2800,height=2000)
      par (mfrow = c(2,2))
      cleanplot.pca (PCA, scaling = 1,cex=0.8,ax1=ii,ax2=jj)
      ordiplot(PCA,display='sites',type='n',main="Cuenca",cex=0.8,choices=c(ii,jj))
      colty2<-match(CovDF$Depart[selr],colty$site)
      colty22<-unique(colty2)
      points(PCA,col=colty$col[colty2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=substr(colty$site[colty22],1,4),col=colty$col[colty22],cex=0.5,pch=18,bty="n")
      coltyt2<-match(CovDF$Trans[selr],coltyt$Trans)
      coltyt22<-unique(coltyt2)
      ordiplot(PCA,display='sites',type='n',main="Transformación",cex=0.8,choices=c(ii,jj))
      points(PCA,col=coltyt$col[coltyt2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyt$Trans[coltyt22],col=coltyt$col[coltyt22],cex=0.5,pch=18,bty="n")
      coltyp2<-match(CovDF$Perm[selr],coltyp$Perm)
      coltyp22<-unique(coltyp2)
      ordiplot(PCA,display='sites',type='n',main="Plot",cex=0.8,choices=c(ii,jj))
      points(PCA,col=coltyp$col[coltyp2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyp$Perm[coltyp22],col=coltyp$col[coltyp22],cex=0.5,pch=18,bty="n")
      dev.off()
    }
  }
}


##Plot Scale
plotBox<-function(varlist,des="NN",selr=NULL){
  if(is.null(selr)) selr=1:dim(CovDF)[1]
  for(j in varlist){
    CovDD<-CovDF[selr,c(j,"Transf","Perm","Depart")]
    names(CovDD)[which(names(CovDD)==j)]<-"value"
    print(j)
    G1<-ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
                ggtitle(j)
    G2<-ggplot(data=CovDD,aes(y=value,x=Perm))+geom_boxplot()
    print(grid.arrange(G1, G2, ncol=2))
    n2<-readline(prompt="save plot?: " )
    if(n2==1){
      nmp<-paste(gsub("\\.","",paste(j,des,sep="")),"jpeg",sep=".")
      nmp<-paste(WD,"Plots",nmp,sep="/")
      jpeg(filename=nmp,quality=100,res=300,width=2800,height=2000)
      print(grid.arrange(G1, G2, ncol=2))
      dev.off()
    }
  }
}

PCA<-rda(CovPl[,pl_var],scale=T)
summary(PCA)

PlotPCA(PCA,des="plotv")

for (j in pl_var){
  
  CovDD<-CovDF[,c(j,"Transf","Perm","Depart")]
  names(CovDD)[which(names(CovDD)==j)]<-"value"
  print(j)
  try(print(ggplot(data=CovDD,aes(y=value,x=Perm))+geom_boxplot()+
              facet_wrap(~Depart)+ggtitle(j)))
  n2<-readline(prompt="save plot?: " )
  if(n2==1){
    plotV<-ggplot(data=CovDD,aes(y=value,x=Perm))+geom_boxplot()+
      facet_wrap(~Depart)+ggtitle(j)
    nmp<-paste(gsub("\\.","",j),"jpeg",sep=".")
    ggsave(paste(WD,"Plots",nmp,sep="/"),plotV,device="jpeg")
    
  }
}

##Transformation scale
PCA<-rda(CovTrns[,trns_var],scale=T)
summary(PCA)
Plo
PlotPCA(PCA,des="transv")

for (j in trns_var){
  
  CovDD<-CovDF[,c(j,"Transf","Perm","Depart")]
  names(CovDD)[which(names(CovDD)==j)]<-"value"
  print(j)
  try(print(ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
              facet_wrap(~Depart)+ggtitle(j)))
  n2<-readline(prompt="save plot?: " )
  if(n2==1){
    plotV<-ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
      facet_wrap(~Depart)+ggtitle(j)
    nmp<-paste(gsub("\\.","",j),"jpeg",sep=".")
    ggsave(paste(WD,"Plots",nmp,sep="/"),plotV,device="jpeg")
    
  }
}

##Dep scale
PCA<-rda(CovDep[,dep_var],scale=T)
summary(PCA)
PlotPCA(PCA,des="Depv")

##Trans Perm scale
PCA<-rda(CovDF[,c(pl_var,trns_var)],scale=T)
summary(PCA)
PlotPCA(PCA,des="TrDpv")

##trans dep
PCA<-rda(CovDF[,c(dep_var,trns_var)],scale=T)
summary(PCA)
PlotPCA(PCA,des="trplv")

##All variables
PCA<-rda(CovDF[,c(dep_var,trns_var,pl_var)],scale=T)
head(summary(PCA))
par(mfrow=c(1,1))
PlotPCA(PCA,des="Allv")

###Random Forest analysis
RForest<-function(X,y,des="NN",maint=""){
  tabobs<-table(y)  # compensar el peso de T.alt con más observaciones
  X = randomForest(X, y, importance = T, sampsize=rep(min(tabobs),length(tabobs)), ntree = 1000, mtry = 5,na.action=na.omit)
  print(X)
  par(mfrow=c(1,1))
  varImpPlot(X,type=1, main = 'Random forest: Importancia de las variables')
  des1<-paste("rf",des,sep="")
  nmp<-paste(WD,"Plots",paste(des1,"jpeg",sep="."),sep="/")
  jpeg(filename=nmp,quality=100,res=300,width=2800,height=2000)
  varImpPlot(X,type=1, main = paste('Random forest_Importancia en',maint,sep=": "))
  selv<-importance(X)[,"MeanDecreaseAccuracy"]
  selv2<-selv[order(selv,decreasing=T)]
  names(selv2)
  dev.off()
}
Plotvar<-function(selv){
  for (j in selv){
    CovDD<-CovDF[,c(j,"Transf","Perm","Depart")]
    names(CovDD)[which(names(CovDD)==j)]<-"value"
    print(j)
    try(print(ggplot(data=CovDD,aes(y=value,x=Perm))+geom_boxplot()+
                facet_wrap(~Depart)+ggtitle(j)))
    n2<-readline(prompt="save plot?: " )
    if(n2==1){
      plotV<-ggplot(data=CovDD,aes(y=value,x=Perm))+geom_boxplot()+
        facet_wrap(~Depart)+ggtitle(j)
      nmp<-paste(gsub("\\.","",j),"jpeg",sep=".")
      ggsave(paste(WD,"Plots",nmp,sep="/"),plotV,device="jpeg")
      
    }
    try(print(ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
                facet_wrap(~Depart)+ggtitle(j)))
    n2<-readline(prompt="save plot?: " )
    if(n2==1){
      plotV<-ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
        facet_wrap(~Depart)+ggtitle(j)
      nmp<-paste(gsub("\\.","",j),"jpeg",sep=".")
      ggsave(paste(WD,"Plots",nmp,sep="/"),plotV,device="jpeg")
      
    }
    try(print(ggplot(data=CovDD,aes(y=value,x=Depart))+geom_boxplot()+
                ggtitle(j)))
    n2<-readline(prompt="save plot?: " )
    if(n2==1){
      plotV<-ggplot(data=CovDD,aes(y=value,x=Depart))+geom_boxplot()+
        facet_wrap(~Depart)+ggtitle(j)
      nmp<-paste(gsub("\\.","",j),"jpeg",sep=".")
      ggsave(paste(WD,"Plots",nmp,sep="/"),plotV,device="jpeg")
      
    }
  }
}
#Cuenca
X = CovDF[,nmfinv]  # variables explicativas 
range(X$lgMSPAcofv00.500[X$lgMSPAcofv00.500!=Inf])
nreep<-sum(X$lgMSPAcofv00.500==Inf)
X$lgMSPAcofv00.500[X$lgMSPAcofv00.500==Inf]<-runif(nreep,min=5,max=10)
range(X$lgndvi10season.5000[X$lgndvi10season.5000!=-Inf])
nreep<-sum(X$lgndvi10season.5000==-Inf)
X$lgndvi10season.5000[X$lgndvi10season.5000==-Inf]<-runif(nreep,min=-10,max=-4)

#Cuenca
y = as.factor(substr(CovDF$Depart,1,4)) # variable respuesta
vcu<-RForest(X,y,des="dep")
vcuu<-vcu[1:9]
Plotvar(vcuu)

# Transformación
y = as.factor(CovDF$Transf)# variable respuesta
vtr<-RForest(X,y,des="tran")
vtrr<-vtr[1:7]
Plotvar(vtrr)

# Permanence
elr<-which(CovDF$Perm=="Fire")
X<-X[-3,]
y = as.factor(CovDF$Perm[-3])# variable respuesta
y<-droplevels(y)
vpl<-RForest(X,y,de="plot")
X<-X[,vpl]
vpl2<-RForest(X,y,de="plot2")

####################
load(file=file.path(WD,"selvv3.RData"))
CovDF<-read.csv(file.path("C:","Users","dsrbu","Dropbox","Humboldt","Documentos de trabajo",
                              "BosqueSeco","Analisis","CovariatesBIDGEF","GEFCovFinal.csv"),stringsAsFactors = F,header=T)
nmfinv<-names(CovDF)[!names(CovDF)%in%c("DisAcoefvest.30","dviacoefvest.5000","Tmaxp90Feb.30",
                                        "Humesigma.30","Tmaxsigma.500","Precp10May.500",
                                        "HumecofvarJan.5000","FIntMeanx.30","MSPAMeanx13.500",
                                        "MSPAcofv00.500","FIntMeanx.5000","MSPAMeanx13.5000",
                                        "MSPAcofv00.5000","MSPAdynCf.5000","dviacoefvest.5000",
                                        "ndvi10season.5000","ndvi90peak.5000")]
X = CovDF[,nmfinv]  # variables explicativas 
range(X$lgMSPAcofv00.500[X$lgMSPAcofv00.500!=Inf])
nreep<-sum(X$lgMSPAcofv00.500==Inf)
CovDF$lgMSPAcofv00.500[CovDF$lgMSPAcofv00.500==Inf]<-runif(nreep,min=5,max=10)
range(X$lgndvi10season.5000[X$lgndvi10season.5000!=-Inf])
nreep<-sum(X$lgndvi10season.5000==-Inf)
CovDF$lgndvi10season.5000[CovDF$lgndvi10season.5000==-Inf]<-runif(nreep,min=-10,max=-4)
pl_var<-selvv3[selvv3$scale=="plot","varbl"]
trns_var<-selvv3[selvv3$scale=="trans","varbl"]

#Grupo#Vegetacion
#Redundancy analysis
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
ab.matsV<-CompV[[1]][[1]]
comdV.hel<-lapply(ab.matsV,FUN=decostand,method="hellinger") #transform community data
nm.matsV<-CompV[[1]][[2]]
nm.matsV<-lapply(nm.matsV,FUN=function(x){
  x$splot<-paste(substr(x$watershed,1,1),x$plot,sep="-")
  return(x)})
compLV<-comp.lst(comdV.hel,nm.matsV)
compLV2<-lapply(compLV,FUN=function(x){
  a<-length(names(x))
  y<-x[,c(a,2:(a-5))]
  return(y)
})


#Get the covariate data
CovDF$SiteP<-paste(substr(CovDF$Depart,1,1),substr(CovDF$Code,1,4),sep="-")

#Grupo#Vegetacion#Guajira

test1<-compLV2[[1]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,de="Guaj") #variables important for RF classification
vGG<-vG[1:6]
vGp<-RForest(test2,facB,de="Guajp") #variables important for RF classification
vGGp<-vG[1:3]
varG<-intersect(vGG,vGp)
print(varG)
plotBox(varG,des="G",selr=selr)
PCA<-rda(CovDF[selr,varG],scale=T)
PlotPCA(PCA,des="Guaj",selr=selr)
test2<-CovDF[selr,varG]

# redundancy with selected variables
PlotRda<-function(tbRDA,des="NN",selr=NULL){
  par(mfrow=c(1,1))
  if(is.null(selr)) selr=1:dim(CovDF)[1]
  constrained_eig <- tbRDA$CCA$eig/tbRDA$CA$tot.chi*100
  unconstrained_eig <- tbRDA$CA$eig/tbRDA$CA$tot.chi*100
  try(barplot (c(constrained_eig, unconstrained_eig), col = c(rep ('red', length (constrained_eig)), 
                                                          rep ('black', length (unconstrained_eig))), 
           las = 2, ylab = '% variation'))
  n1<-as.numeric(readline(prompt="initial PCA axis: "))
  n2<-as.numeric(readline(prompt="Final PCA axis: "))
  q1<-as.numeric(readline(prompt="number of RDA axis: "))
  minPCA<-apply(scores(tbRDA)$sites,2,max)
  print(vif.cca(tbRDA))
  readline(prompt="press[ENTER]")
  print(anova(tbRDA,by="axis"))
  readline(prompt="press[ENTER]")
  print(anova(tbRDA,by="terms"))
  readline(prompt="press[ENTER]")
  print(anova(tbRDA,by="margin"))
  readline(prompt="press[ENTER]")
  
  for(ii in 1:(q1-1)){
    for(jj in (ii+1):q1){
      print(paste("plots of RDA axis",ii,"and",jj))
      par (mfrow = c(1,2))
      coltyt2<-match(CovDF$Trans[selr],coltyt$Trans)
      coltyt22<-unique(coltyt2)
      ordiplot(tbRDA,main="Transformación",cex=0.4,choices=c(ii,jj))
      points(tbRDA,col=coltyt$col[coltyt2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyt$Trans[coltyt22],col=coltyt$col[coltyt22],cex=0.5,pch=18,bty="n")
      coltyp2<-match(CovDF$Perm[selr],coltyp$Perm)
      coltyp22<-unique(coltyp2)
      ordiplot(tbRDA,main="Plot",cex=0.4,choices=c(ii,jj))
      points(tbRDA,col=coltyp$col[coltyp2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyp$Perm[coltyp22],col=coltyp$col[coltyp22],cex=0.5,pch=18,bty="n")
      readline(prompt="save plot and then press [ENTER] " )
    }
  }
  if(n1<n2){for(ii in n1:(n2-1)){
    for(jj in (ii+1):n2){
      print(paste("plots of PCA axis",ii,"and",jj))
      par (mfrow = c(1,2))
      coltyt2<-match(CovDF$Trans[selr],coltyt$Trans)
      coltyt22<-unique(coltyt2)
      ordiplot(tbRDA,main="Transformación",cex=0.6,choices=c(ii,jj))
      points(tbRDA,col=coltyt$col[coltyt2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyt$Trans[coltyt22],col=coltyt$col[coltyt22],cex=0.5,pch=18,bty="n")
      coltyp2<-match(CovDF$Perm[selr],coltyp$Perm)
      coltyp22<-unique(coltyp2)
      ordiplot(tbRDA,main="Plot",cex=0.6,choices=c(ii,jj))
      points(tbRDA,col=coltyp$col[coltyp2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyp$Perm[coltyp22],col=coltyp$col[coltyp22],cex=0.5,pch=18,bty="n")
      readline(prompt="save plot and then press [ENTER] " )
    }}
  }
}
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) #plots ana prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)[!c(pl_var,trns_var)%in%varG]
attv_c<-altV[c(2,5,6,7,14)]
test2_c<-CovDF[selr,attv_c]
pairs(test2_c)
attv_c<-altV[c(2,6,7,13)]
print(attv_c)
test2_c<-CovDF[selr,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(4,12,13)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)
#Greeness and preassure
attv_g<-altV[c(1,3,8,9,10,11)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
runDPCoA<-function(spe_df,des="NN",maint=""){
  ## Community analysis - DPCoA V1 - Factor Trasformacion
  cd_v1 <- crossdpcoa_version1(spe_df, facA, facB, dis = NULL,
                               scannf = FALSE,
                               nf=3)
  coord_sites=cd_v1$l3[,c(1,2)]
  minPCA<-apply(coord_sites,2,max)
  minPCA<-minPCA-(minPCA*0.3)
  eig=round(cd_v1$eig[1:2]/sum(cd_v1$eig)*100,1)
  coltyt2<-match(CovDF$Trans[selr],coltyt$Trans)
  coltyt22<-unique(coltyt2)
  par(mfrow=c(1,2))
  plot(coord_sites,col='gray',pch=20,bty='n',cex.lab=0.8,
       xlab=paste('PC1',eig[1],'%'),ylab=paste('PC2',eig[2],'%'),main=paste("Transformation",maint))
  abline(v=0,col='gray',lty=2);abline(h=0,col='gray',lty=2)
  points(coord_sites,col=coltyt$col[coltyt2],pch=20,bty='n')
  legend(minPCA[1],minPCA[2],legend=coltyt$Trans[coltyt22],
         col=coltyt$col[coltyt22],cex=0.5,pch=20,bty="n",y.intersp = 0.3)
  coltyp2<-match(CovDF$Perm[selr],coltyp$Perm)
  coltyp22<-unique(coltyp2)
  plot(coord_sites,col='gray',pch=20,bty='n', cex.lab=0.8,
       xlab=paste('PC1',eig[1],'%'),ylab=paste('PC2',eig[2],'%'),
       main=paste("Permanence",maint))
  abline(v=0,col='gray',lty=2);abline(h=0,col='gray',lty=2)
  points(coord_sites,col=coltyp$col[coltyp2],pch=20,bty='n')
  legend(minPCA[1],minPCA[2],legend=coltyp$Perm[coltyp22],
         col=coltyp$col[coltyp22],cex=0.5,pch=20,bty="n",
        y.intersp=0.3)
  readline(prompt="press [ENTER] " )
  #save plots
  des1<-paste("dcPcA",des,sep="")
  nmp<-paste(WD,"Plots",paste(des1,"jpeg",sep="."),sep="/")
  jpeg(filename=nmp,quality=100,res=300,width=2800,height=2000)
  par(mfrow=c(1,2))
  plot(coord_sites,col='gray',pch=20,bty='n',cex.lab=0.8,
       xlab=paste('PC1',eig[1],'%'),ylab=paste('PC2',eig[2],'%'),main=paste("Transformation",maint))
  abline(v=0,col='gray',lty=2);abline(h=0,col='gray',lty=2)
  points(coord_sites,col=coltyt$col[coltyt2],pch=20,bty='n')
  legend(minPCA[1],minPCA[2],legend=coltyt$Trans[coltyt22],
         col=coltyt$col[coltyt22],cex=0.5,pch=20,bty="n",
         y.intersp=0.3)
  plot(coord_sites,col='gray',pch=20,bty='n', cex.lab=0.8,
       xlab=paste('PC1',eig[1],'%'),ylab=paste('PC2',eig[2],'%'),
       main=paste("Permanence",maint))
  abline(v=0,col='gray',lty=2);abline(h=0,col='gray',lty=2)
  points(coord_sites,col=coltyp$col[coltyp2],pch=20,bty='n')
  legend(minPCA[1],minPCA[2],legend=coltyp$Perm[coltyp22],
         col=coltyp$col[coltyp22],cex=0.5,pch=20,bty="n",
         y.intersp=0.3)
  dev.off()
  
  # effecto de las especies
  cd_mainB <- crossdpcoa_maineffect(spe_df, facA, facB, dis = NULL, 
                                    nf=3,
                                    scannf = FALSE)
  
  fac_t=cd_mainB$l2[,c(1,2)]
  sp=cd_mainB$l1[,c(1,2)]
  minx=min(sp$Axis1)*1.1
  miny=min(sp$Axis2)*1.1
  maxx=max(sp$Axis1)*1.1
  maxy=max(sp$Axis2)*1.1
  psp1<-rownames(sp)[order (abs (sp[,1]), decreasing = TRUE)][1:4]
  psp2<-rownames(sp)[order (abs (sp[,2]), decreasing = TRUE)][1:4]
  sel_sp=is.element(rownames(sp),c(psp1,psp2))
  eig=round(cd_mainB$eig[1:2]/sum(cd_mainB$eig)*100,1)
  # plot species analysis - Main effect
  par(mfrow=c(1,1))
  plot_color="#5cb254"
  plot(sp,col=plot_color,pch=20, 
       bty='n',xlab=paste('PC1',eig[1],'%'),ylab=paste('PC2',eig[2],'%'),cex.lab=0.8,
       ylim=c(miny,maxy),xlim=c(minx,maxx),main=maint)
  abline(v=0,col='gray',lty=2);abline(h=0,col='gray',lty=2)
  text(sp[sel_sp,],labels = rownames(sp)[sel_sp],pos=1,offset=0.5,col=plot_color,cex = 0.8)
  points(fac_t,pch=20,cex=2,col='#a5694f')
  text(fac_t,rownames(fac_t),pos = 3, offset = 0.5,col='#a5694f')
  readline(prompt="press[ENTER]")
  # save plot 
  des1<-paste("dcPcASp",des,sep="")
  nmp<-paste(WD,"Plots",paste(des1,"jpeg",sep="."),sep="/")
  jpeg(filename=nmp,quality=100,res=300,width=2800,height=2000)
  par(mfrow=c(1,1))
  plot(sp,col=plot_color,pch=20, 
       bty='n',xlab=paste('PC1',eig[1],'%'),ylab=paste('PC2',eig[2],'%'),cex.lab=0.8,
       ylim=c(miny,maxy),xlim=c(minx,maxx),main=maint)
  abline(v=0,col='gray',lty=2);abline(h=0,col='gray',lty=2)
  text(sp[sel_sp,],labels = rownames(sp)[sel_sp],pos=1,offset=0.5,col=plot_color,cex = 0.8)
  points(fac_t,pch=20,cex=2,col='#a5694f')
  text(fac_t,rownames(fac_t),pos = 3, offset = 0.5,col='#a5694f')
  dev.off()
  #plot important species
  spnm = rownames(sp)
  idx_pc1 = which((sp$Axis1<  -0.15) | (sp$Axis1>0.15) )
  idx_pc2 = which((sp$Axis2<  -0.15) | (sp$Axis2>0.15) )
  par(mfrow=c(1,2))
  dotchart(sp$Axis1[idx_pc1],spnm[idx_pc1], main = 'Importancia sp PC1', xlab='PC1')
  dotchart(sp$Axis2[idx_pc2],spnm[idx_pc2], main = 'Importancia sp PC2', xlab='PC2')
  readline(prompt="press[ENTER]")
  #save Plot
  des1<-paste("dcPcADC",des,sep="")
  nmp<-paste(WD,"Plots",paste(des1,"jpeg",sep="."),sep="/")
  jpeg(filename=nmp,quality=100,res=300,width=2800,height=2000)
  par(mfrow=c(1,2))
  dotchart(sp$Axis1[idx_pc1],spnm[idx_pc1], main = 'Importancia sp PC1', xlab='PC1',cex=0.8)
  dotchart(sp$Axis2[idx_pc2],spnm[idx_pc2], main = 'Importancia sp PC2', xlab='PC2',cex=0.8)
  dev.off()
}

spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="G",maint="Veg.Guajira")

#Random Forest on species
SpG<-RForest(test1,facA,de="Gsp",maint="Guajira")
SpG2<-RForest(test1,facB,de="GspP",maint="Guajira")

###Check variables by type
Pressv<-nmfinv[grep("ESAC|DisA|dvia",nmfinv)]
Conditv<-nmfinv[grep("MSPA|FInt|evi|ndvi",nmfinv)]
Climv<-nmfinv[grep("Tmax|Precp|Hume",nmfinv)]

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="Guaj_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="G_p",selr=selr)
PCA<-rda(CovDF[selr,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="Guaj_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

##Condition variables

test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="Guaj_c") #variables important for RF classification
vGG<-vG[1:7]
plotBox(vGG,des="G_c",selr=selr)
PCA<-rda(CovDF[selr,Conditv],scale=T)
summary(PCA)
PlotPCA(PCA,des="Guaj_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="Guaj_pc") #variables important for RF classification
vGG<-vG[1:6]
PCA<-rda(test2,scale=T)
summary(PCA)
PlotPCA(PCA,des="Guaj_pc",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Vegetacion#Bolivar
test1<-compLV2[[2]]
CovDF$SiteP[CovDF$SiteP=="B-T2B2"]<-"B-T2P2"
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,des="Bol") #variables important for RF classification
vGG<-vG[1:6]
vGp<-RForest(test2,facB,des="Bolp") #variables important for RF classification
vGGp<-vG[1:4]
varG<-intersect(vGG,vGp)
print(varG)
plotBox(varG,des="B",selr=selr)
PCA<-rda(CovDF[selr,varG],scale=T)
summary(PCA)
PlotPCA(PCA,des="Bol",selr=selr)
test2<-CovDF[selr,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
par(mfrow=c(1,1))
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-CovDF[selr,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,19,20)]
print(attv_c)
test2_c<-CovDF[selr,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)
#Greeness and preassure
attv_g<-altV[c(2,4,10,11,12,14,15)]
test2_g<-CovDF[selr,attv_g]
attv_g<-altV[c(2,4,10,12,15)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="B",maint='Veg.Bolivar')

#Random Forest on species
SpG<-RForest(test1,facA,de="Bsp",maint='Bolivar')
SpG2<-RForest(test1,facB,de="BspP",maint='Bolivar')

###Check variables by type

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="Bol_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="B_p",selr=selr)
PCA<-rda(CovDF[selr,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="Bol_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="Bol_c") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="B_c",selr=selr)
PCA<-rda(CovDF[selr,Conditv],scale=T)
summary(PCA)
PlotPCA(PCA,des="Bol_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="Bol_pc") #variables important for RF classification
vGG<-vG[1:6]
PCA<-rda(test2,scale=T)
summary(PCA)
PlotPCA(PCA,des="Bol_pc",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Vegetacion#Huila
test1<-compLV2[[4]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
fixr<-which(test2==Inf,arr.ind=T)
range(test2[test2[,7]!=Inf,7])
test2[fixr[1],7]<-runif(1,3.2,6)
fixr<-which(test2==-Inf,arr.ind=T)
range(test2[test2[,14]!=-Inf,14])
test2[fixr[1],14]<--runif(1,3.4,6)
vG<-RForest(test2,facA,des="Hui") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2,facB,des="Huip") #variables important for RF classification
vGGp<-vG[1:2]
varG<-intersect(vGG,vGp)
print(varG)
plotBox(varG,des="H",selr=selr)
PCA<-rda(CovDF[selr,varG],scale=T)
summary(PCA)
PlotPCA(PCA,des="Hui",selr=selr)
test2_2<-test2[,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_2)
print(tbRDA)
par(mfrow=c(1,1))
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(3,6,7,19,20)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) 


#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,10,13,15)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="H",maint='Veg.Huila')

#Random Forest on species
SpG<-RForest(test1,facA,de="Hsp",maint='Huila')
SpG2<-RForest(test1,facB,de="HspP",maint='Huila')

###Check variables by type

test2<-CovDF[selr,c(Pressv,Conditv)]
fixr<-which(test2==Inf,arr.ind=T)
range(test2[test2[,16]!=Inf,16])
test2[fixr[1],16]<-runif(1,3.2,5)
fixr<-which(test2==-Inf,arr.ind=T)
range(test2[test2[,12]!=-Inf,12])
test2[fixr[1],12]<--runif(1,3.3,5)

test2_p<-test2[,Pressv]
#Radom forest to select variables
vG<-RForest(test2_p,facA,de="Hui_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="H_p",selr=selr)
PCA<-rda(test2[,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="Hui_p",selr=selr)
test2_p<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_p)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2_c<-test2[,Conditv]

#Radom forest to select variables
vG<-RForest(test2_c,facA,de="Hui_c") #variables important for RF classification
vGG<-vG[1:7]
plotBox(vGG,des="H_c",selr=selr)
PCA<-rda(test2[,Conditv],scale=T)
summary(PCA)
PlotPCA(PCA,des="Hui_c",selr=selr)
test2_c<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2_pc<-test2[,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2_pc,facA,de="Hui_pc") #variables important for RF classification
vGG<-vG[1:5]
PCA<-rda(test2_pc,scale=T)
summary(PCA)
PlotPCA(PCA,des="Hui_pc",selr=selr)
test2_pc<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_pc)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))


#Grupo#Vegetacion#Tolima
test1<-compLV2[[5]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
fixr<-which(test2==Inf,arr.ind=T)
range(test2[test2[,7]!=Inf,7])
test2[fixr[,1],7]<-runif(3,2.7,5)
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,des="Tol") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2,facB,des="Tolp") #variables important for RF classification
vGGp<-vG[1:2]
varG<-intersect(vGG,vGp)
print(varG)
plotBox(varG,des="T",selr=selr)
PCA<-rda(CovDF[selr,varG],scale=T)
summary(PCA)
PlotPCA(PCA,des="Tol",selr=selr)
test2_rf<-test2[,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
par(mfrow=c(1,1))
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6:8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,6,7,20)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
test2_l<-test2[,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,10,12,15)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="T",maint='Veg.Tolima')

#Random Forest on species
SpG<-RForest(test1,facA,de="Tsp",maint='Tolima')
SpG2<-RForest(test1,facB,de="TspP",maint='Tolima')

###Check variables by type

test2<-CovDF[selr,c(Pressv,Conditv)]
fixr<-which(test2==Inf,arr.ind=T)
range(test2[test2[,16]!=Inf,16])
test2[fixr[,1],16]<-runif(3,2.7,5)

test2_p<-test2[,Pressv]
#Radom forest to select variables
vG<-RForest(test2_p,facA,de="Tol_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="T_p",selr=selr)
PCA<-rda(test2[,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="Tol_p",selr=selr)
test2_p<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_p)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2_c<-test2[,Conditv]

#Radom forest to select variables
vG<-RForest(test2_c,facA,de="Tol_c") #variables important for RF classification
vGG<-vG[1:7]
plotBox(vGG,des="T_c",selr=selr)
PCA<-rda(test2_c,scale=T)
summary(PCA)
PlotPCA(PCA,des="Tol_c",selr=selr)
test2_c<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2_pc<-test2[,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2_pc,facA,de="Tol_pc") #variables important for RF classification
vGG<-vG[1:7]
PCA<-rda(test2_pc,scale=T)
summary(PCA)
PlotPCA(PCA,des="Tol_pc",selr=selr)
test2_pc<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_pc)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Vegetacion#Cesar
test1<-compLV2[[3]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facB=factor(CovDF$Perm[selr])
rmr<-which(facB=='Fire')
test1<-test1[-rmr,]
test2<-test2[-rmr,]
facB<-droplevels(facB[-rmr])
#Radom forest to select variables

vGp<-RForest(test2,facB,des="Cesp") #variables important for RF classification
vGGp<-vG[1:4]
print(vGGp)
plotBox(vGGp,des="C",selr=selr)
PCA<-rda(CovDF[selr,c(pl_var,trns_var)],scale=T)
summary(PCA)
PlotPCA(PCA,des="Ces",selr=selr)
test2_rf<-test2[,vGGp]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
par(mfrow=c(1,1))
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,19)]
print(attv_c)
test2_c<-test2[,attv_c]

#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
test2_l<-test2[,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,14)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#Random Forest on species
SpG2<-RForest(test1,facB,de="CspP")

###Check variables by type

test2<-CovDF[selr,c(Pressv,Conditv)]
test2<-test2[-rmr,]

test2_p<-test2[,Pressv]
#Radom forest to select variables
vG<-RForest(test2_p,facB,de="Ces_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="C_p",selr=selr)
PCA<-rda(test2[,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="Ces_p",selr=selr)
test2_p<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_p)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2_c<-test2[,Conditv]

#Radom forest to select variables
vG<-RForest(test2_c,facB,de="Ces_c") #variables important for RF classification
vGG<-vG[1:3]
plotBox(vGG,des="C_c",selr=selr)
PCA<-rda(test2_c,scale=T)
summary(PCA)
PlotPCA(PCA,des="Ces_c",selr=selr)
test2_c<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2_pc<-test2[,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2_pc,facB,de="Ces_pc") #variables important for RF classification
vGG<-vG[1:4]
PCA<-rda(test2_pc,scale=T)
summary(PCA)
PlotPCA(PCA,des="Ces_pc",selr=selr)
test2_pc<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_pc)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Vegetacion#Valle
test1<-compLV2[[6]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
fixr<-which(test2==Inf,arr.ind=T)
range(test2[test2[,7]!=Inf,7])
test2[fixr[,1],7]<-runif(2,3.4,5)
test1<-test1[,c(2:dim(test1)[2])]
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vGp<-RForest(test2,facB,des="Valp") #variables important for RF classification
vGGp<-vGp[1:7]
print(vGGp)
plotBox(vGGp,des="V",selr=selr)
PCA<-rda(CovDF[selr,c(pl_var,trns_var)],scale=T)
summary(PCA)
PlotPCA(PCA,des="Val",selr=selr)


# redundancy with selected variables

test2_rf<-test2[,vGGp]
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
par(mfrow=c(1,1))
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,19)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
test2_l<-test2[,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,12,15)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

#Random Forest on species
SpG2<-RForest(test1,facB,de="VspP")
rm(list=c("test1","test2","selr","facA","facB"))
##########################################################
#Grupo#Aves
CompA<-CompAt
ab.matsA<-CompA[[1]][[1]]
comdA.hel<-lapply(ab.matsA,FUN=decostand,method="hellinger")
nm.matsA<-CompA[[1]][[2]]
nm.matsA<-lapply(nm.matsA,FUN=function(x){
  x$splot<-paste(substr(x$watershed,1,1),x$plot,sep="-")
  return(x)})
compLA<-comp.lst(comdA.hel,nm.matsA)
compLA2<-lapply(compLA,FUN=function(x){
  a<-length(names(x))
  y<-x[,c(a,2:(a-5))]
  return(y)
})


#Grupo#Aves#Guajira
test1<-compLA2[[5]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,de="Guaj") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2,facB,de="Guajp") #variables important for RF classification
vGGp<-vG[1:4]
varG<-intersect(vGG,vGp)
print(varG)

# redundancy with selected variables
test2<-test2[,varG]
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr) #plots ana prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-CovDF[selr,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,8,19,20)]
print(attv_c)
test2_c<-CovDF[selr,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16:18)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9:11,13)]
test2_g<-CovDF[selr,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence

spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="AG",maint='Av.Guajira')

#Random Forest on species
SpG<-RForest(test1,facA,de="AGsp",maint='Guajira')
SpG2<-RForest(test1,facB,de="AGspP",maint='Guajira')

###Check variables by type
Pressv<-nmfinv[grep("ESAC|DisA|dvia",nmfinv)]
Conditv<-nmfinv[grep("MSPA|FInt|evi|ndvi",nmfinv)]
Climv<-nmfinv[grep("Tmax|Precp|Hume",nmfinv)]

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="AGuaj_p") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="AG_p",selr=selr)
PCA<-rda(CovDF[selr,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="AGuaj_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

##Condition variables

test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="AGuaj_c") #variables important for RF classification
vGG<-vG[1:7]
plotBox(vGG,des="AG_c",selr=selr)
PCA<-rda(CovDF[selr,Conditv],scale=T)
summary(PCA)
PlotPCA(PCA,des="AGuaj_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="AGuaj_pc") #variables important for RF classification
vGG<-vG[1:7]
PCA<-rda(test2,scale=T)
summary(PCA)
PlotPCA(PCA,des="AGuaj_pc",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Aves#Bolivar
test1<-compLA2[[4]]
CovDF$SiteP[CovDF$SiteP=="B-T2B2"]<-"B-T2P2"
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,des="ABol") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2,facB,des="ABolp") #variables important for RF classification
vGGp<-vG[1:5]
varG<-intersect(vGG,vGp)
print(varG)
test2<-CovDF[selr,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
par(mfrow=c(1,1))
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,7,8,19,20)]
test2_c<-CovDF[selr,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,8,19,20)]
print(attv_c)
test2_c<-CovDF[selr,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 


#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,11:13)]
test2_g<-CovDF[selr,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="AB",maint='Av.Bolivar')
AvesB <- crossdpcoa_maineffect(spe_df, facA, facB, dis = NULL, 
                                  nf=3,
                                  scannf = FALSE)
write.csv(AvesB$l1,file=file.path(WD,"BAvesDPCoA.csv"))
#Random Forest on species
SpG<-RForest(test1,facA,de="ABsp",maint='Bolivar')
SpG2<-RForest(test1,facB,de="ABspP",maint='Bolivar')


###Check variables by type

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="ABol_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="AB_p",selr=selr)
PCA<-rda(CovDF[selr,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="ABol_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="ABol_c") #variables important for RF classification
vGG<-vG[1:7]
plotBox(vGG,des="AB_c",selr=selr)
PCA<-rda(CovDF[selr,Conditv],scale=T)
summary(PCA)
PlotPCA(PCA,des="ABol_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="ABol_pc") #variables important for RF classification
vGG<-vG[1:6]
PCA<-rda(test2,scale=T)
summary(PCA)
PlotPCA(PCA,des="ABol_pc",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Aves#Huila
test1<-compLA2[[6]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
fixr<-which(test2==Inf,arr.ind=T)
range(test2[test2[,27]!=Inf,27])
test2[fixr[1],27]<-runif(1,3.2,6)
fixr<-which(test2==-Inf,arr.ind=T)
range(test2[test2[,18]!=-Inf,18])
test2[fixr[1],18]<--runif(1,3.4,6)

test2_rf<-test2[selr,c(pl_var,trns_var)]
vG<-RForest(test2_rf,facA,des="AHui") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2_rf,facB,des="AHuip") #variables important for RF classification
vGGp<-vG[1:7]
varG<-intersect(vGG,vGp)
print(varG)
test2<-CovDF[selr,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(3,6,7,19,20)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,11,13:14)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)


#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="AH",maint='Av.Huila')
AvesH <- crossdpcoa_maineffect(spe_df, facA, facB, dis = NULL, 
                               nf=3,
                               scannf = FALSE)
write.csv(AvesH$l1,file=file.path(WD,"HAvesDPCoA.csv"))
#Random Forest on species
SpG<-RForest(test1,facA,de="AHsp",maint='Huila')
SpG2<-RForest(test1,facB,de="AHspP",maint='Huila')

###Check variables by type

test2_p<-test2[,Pressv]
#Radom forest to select variables
vG<-RForest(test2_p,facA,de="AHui_p") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="AH_p",selr=selr)
PCA<-rda(test2[,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="AHui_p",selr=selr)
test2_p<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_p)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2_c<-test2[,Conditv]

#Radom forest to select variables
vG<-RForest(test2_c,facA,de="AHui_c") #variables important for RF classification
vGG<-vG[1:7]
plotBox(vGG,des="AH_c",selr=selr)
PCA<-rda(test2[,Conditv],scale=T)
summary(PCA)
PlotPCA(PCA,des="AHui_c",selr=selr)
test2_c<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2_pc<-test2[,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2_pc,facA,de="AHui_pc") #variables important for RF classification
vGG<-vG[1:7]
PCA<-rda(test2_pc,scale=T)
summary(PCA)
PlotPCA(PCA,des="AHui_pc",selr=selr)
test2_pc<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_pc)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Aves#Tolima
test1<-compLA2[[3]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,]
fixr<-which(test2==Inf,arr.ind=T)
range(test2[test2[,27]!=Inf,27])
test2[fixr[,1],27]<-runif(3,2.7,5)
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

test2_rf<-test2[,c(pl_var,trns_var)]
#Radom forest to select variables
vG<-RForest(test2_rf,facA,des="ATol") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2_rf,facB,des="ATolp") #variables important for RF classification
vGGp<-vG[1:7]
varG<-intersect(vGG,vGp)
print(varG)
test2_rf<-test2[,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,6,7,19,20)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
test2_l<-test2[,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,10,12,13,15)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="AT",maint='Av.Tolima')
AvesT <- crossdpcoa_maineffect(spe_df, facA, facB, dis = NULL, 
                               nf=3,
                               scannf = FALSE)
write.csv(AvesT$l1,file=file.path(WD,"TAvesDPCoA.csv"))

#Random Forest on species
SpG<-RForest(test1,facA,de="ATsp",maint='Tolima')
SpG2<-RForest(test1,facB,de="ATspP",maint='Tolima')


###Check variables by type

test2_p<-test2[,Pressv]
#Radom forest to select variables
vG<-RForest(test2_p,facA,de="ATol_p") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="AT_p",selr=selr)
PCA<-rda(test2[,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="ATol_p",selr=selr)
test2_p<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_p)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2_c<-test2[,Conditv]

#Radom forest to select variables
vG<-RForest(test2_c,facA,de="ATol_c") #variables important for RF classification
vGG<-vG[1:7]
plotBox(vGG,des="AT_c",selr=selr)
PCA<-rda(test2_c,scale=T)
summary(PCA)
PlotPCA(PCA,des="ATol_c",selr=selr)
test2_c<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2_pc<-test2[,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2_pc,facA,de="ATol_pc") #variables important for RF classification
vGG<-vG[1:7]
PCA<-rda(test2_pc,scale=T)
summary(PCA)
PlotPCA(PCA,des="ATol_pc",selr=selr)
test2_pc<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_pc)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Aves#Cesar
test1<-compLA2[[1]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,]
test1<-test1[,c(2:dim(test1)[2])]
facB=factor(CovDF$Perm[selr])
rmr<-which(facB=='Fire')
test1<-test1[-rmr,]
test2<-test2[-rmr,]
facB<-droplevels(facB[-rmr])
#Radom forest to select variables
test2_rf<-test2[,c(pl_var,trns_var)]
vGp<-RForest(test2_rf,facB,des="ACesp") #variables important for RF classification
vGGp<-vG[1:4]
print(vGGp)
test2_rf<-test2[,c(vGGp)]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,20)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
test2_l<-test2[,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,12)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Random Forest on species
SpG2<-RForest(test1,facB,des="ACspP")

###Check variables by type

test2_p<-test2[,Pressv]
#Radom forest to select variables
vG<-RForest(test2_p,facB,de="ACes_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="AC_p",selr=selr)
PCA<-rda(test2[,Pressv],scale=T)
summary(PCA)
PlotPCA(PCA,des="ACes_p",selr=selr)
test2_p<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_p)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2_c<-test2[,Conditv]

#Radom forest to select variables
vG<-RForest(test2_c,facB,de="ACes_c") #variables important for RF classification
vGG<-vG[1:3]
plotBox(vGG,des="AC_c",selr=selr)
PCA<-rda(test2_c,scale=T)
summary(PCA)
PlotPCA(PCA,des="ACes_c",selr=selr)
test2_c<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2_pc<-test2[,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2_pc,facB,de="ACes_pc") #variables important for RF classification
vGG<-vG[1:4]
PCA<-rda(test2_pc,scale=T)
summary(PCA)
PlotPCA(PCA,des="ACes_pc",selr=selr)
test2_pc<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_pc)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Aves#Valle
test1<-compLA2[[2]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,]
fixr<-which(test2==Inf,arr.ind=T)
range(test2[test2[,27]!=Inf,27])
test2[fixr[,1],27]<-runif(2,3.4,5)
test1<-test1[,c(2:dim(test1)[2])]
facB=factor(CovDF$Perm[selr])

test2_rf<-test2[,c(pl_var,trns_var)]
#Radom forest to select variables
vGp<-RForest(test2_rf,facB,des="AValp") #variables important for RF classification

# redundancy with selected variables
vGGp<-vG[1:7]
test2_rf<-test2[,vGGp]
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,6,7,8,19,20)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
test2_l<-test2[,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,12,13,14,15)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Random Forest on species
SpG2<-RForest(test1,facB,de="AVspP")
rm(list=c("test1","test2","selr","facA","facB"))

#################################### 
#Grupo#Hormigas
CompH<-CompHt
ab.matsH<-CompH[[1]][[1]]
comdH.hel<-lapply(ab.matsH,FUN=decostand,method="hellinger")
nm.matsH<-CompH[[1]][[2]]
nm.matsH<-lapply(nm.matsH,FUN=function(x){
  x$splot<-paste(substr(x$watershed,1,1),x$plot,sep="-")
  return(x)})
compLH<-comp.lst(comdH.hel,nm.matsH)
compLH2<-lapply(compLH,FUN=function(x){
  a<-length(names(x))
  y<-x[,c(a,2:(a-5))]
  return(y)
})


#Get the covariate data

#Grupo#Hormigas#Guajira
test1<-compLH2[[3]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,de="HGuaj") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2,facB,de="HGuajp") #variables important for RF classification
vGGp<-vG[1:4]
varG<-intersect(vGG,vGp)
print(varG)
PCA<-rda(CovDF[selr,varG],scale=T)
summary(PCA)
PlotPCA(PCA,des="HGuaj",selr=selr)
test2_rf<-test2[,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots ana prints statisticas, VIF and anovas


#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-CovDF[selr,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,6,19,20)]
print(attv_c)
test2_c<-CovDF[selr,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16:18)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,10,13)]
test2_g<-CovDF[selr,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence

spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="HG",maint='Horm.Guajira')

#Random Forest on species
SpG<-RForest(test1,facA,de="HGsp",maint='Guajira')
SpG2<-RForest(test1,facB,de="HGspP",maint='Guajira')

###Check variables by type
Pressv<-nmfinv[grep("ESAC|DisA|dvia",nmfinv)]
Conditv<-nmfinv[grep("MSPA|FInt|evi|ndvi",nmfinv)]
Climv<-nmfinv[grep("Tmax|Precp|Hume",nmfinv)]

test2_p<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2_p,facA,de="HGuaj_p") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="HG_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

##Condition variables

test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HGuaj_c") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="HG_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HGuaj_pc") #variables important for RF classification
vGG<-vG[1:6]
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Hormigas#Bolivar
test1<-compLH2[[5]]
CovDF$SiteP[CovDF$SiteP=="B-T2B2"]<-"B-T2P2"
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,des="HBol") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2,facB,des="HBolp") #variables important for RF classification
vGGp<-vG[1:4]
varG<-intersect(vGG,vGp)
print(varG)
test2_rf<-CovDF[selr,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,7,8,19,20)]
test2_c<-CovDF[selr,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,19,20)]
print(attv_c)
test2_c<-CovDF[selr,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 


#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,10,14)]
test2_g<-CovDF[selr,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="HB",maint='Horm.Bolivar')

#Random Forest on species
SpG<-RForest(test1,facA,de="HBsp",maint='Bolivar')
SpG2<-RForest(test1,facB,de="HBspP",maint='Bolivar')

###Check variables by type

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HBol_p") #variables important for RF classification
vGG<-vG[1:5]
plotBox(vGG,des="HB_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HBol_c") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="HB_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HBol_pc") #variables important for RF classification
vGG<-vG[1:7]

test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Hormigas#Cesar
test1<-compLH2[[1]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facB=factor(CovDF$Perm[selr])
rmr<-which(facB=='Fire')
test1<-test1[-rmr,]
test2<-test2[-rmr,]
facB<-droplevels(facB[-rmr])
#Radom forest to select variables
vGp<-RForest(test2,facB,des="HCesp") #variables important for RF classification
vGGp<-vGp[1:4]
print(vGGp)
test2_rf<-test2[,c(vGGp)]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,19)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
test2_l<-test2[,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,12)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Random Forest on species
SpG2<-RForest(test1,facB,des="HCspP")


###Check variables by type

test2<-CovDF[selr,c(Pressv,Conditv)]
test2<-test2[-rmr,]

test2_p<-test2[,Pressv]
#Radom forest to select variables
vG<-RForest(test2_p,facB,de="HCes_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="HC_p",selr=selr)
test2_p<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_p)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2_c<-test2[,Conditv]

#Radom forest to select variables
vG<-RForest(test2_c,facB,de="HCes_c") #variables important for RF classification
vGG<-vG[1:3]
plotBox(vGG,des="HC_c",selr=selr)
test2_c<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2_pc<-test2[,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2_pc,facB,de="HCes_pc") #variables important for RF classification
vGG<-vG[1:4]
PCA<-rda(test2_pc,scale=T)
summary(PCA)
PlotPCA(PCA,des="HCes_pc",selr=selr)
test2_pc<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_pc)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Hormigas#Tolima
test1<-compLH2[[6]]
#CovDF$SiteP[CovDF$SiteP=="B-T2B2"]<-"B-T2P2"
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,des="HTol") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2,facB,des="HTolp") #variables important for RF classification
vGGp<-vG[1:4]
varG<-intersect(vGG,vGp)
print(varG)
test2_rf<-CovDF[selr,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,7,8,19,20)]
test2_c<-CovDF[selr,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,19,20)]
print(attv_c)
test2_c<-CovDF[selr,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 


#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,10,14)]
test2_g<-CovDF[selr,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="HT",maint='Horm.Tolima')

#Random Forest on species
SpG<-RForest(test1,facA,de="HTsp",maint='Tolima')
SpG2<-RForest(test1,facB,de="HTspP",maint='Tolima')


###Check variables by type

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HTol_p") #variables important for RF classification
vGG<-vG[1:5]
plotBox(vGG,des="HT_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HTol_c") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="HT_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HTol_pc") #variables important for RF classification
vGG<-vG[1:7]

test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Hormigas#Huila
test1<-compLH2[[4]]
#CovDF$SiteP[CovDF$SiteP=="B-T2B2"]<-"B-T2P2"
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vG<-RForest(test2,facA,des="HHui") #variables important for RF classification
vGG<-vG[1:7]
vGp<-RForest(test2,facB,des="HHuip") #variables important for RF classification
vGGp<-vG[1:4]
varG<-intersect(vGG,vGp)
print(varG)
test2_rf<-CovDF[selr,varG]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,7,8,19,20)]
test2_c<-CovDF[selr,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,19,20)]
print(attv_c)
test2_c<-CovDF[selr,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 


#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,10,13,15)]
test2_g<-CovDF[selr,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence
spe_df = test1  # matriz de especies x punto de conteo
facA=recode(facA,High="Alt",Low="Baj")
# Factor A es transformación y factor B es permanencia, previamente definidos
runDPCoA(spe_df,des="HH",maint='Horm.Huila')

#Random Forest on species
SpG<-RForest(test1,facA,de="HHsp",maint='Huila')
SpG2<-RForest(test1,facB,de="HHspP",maint='Huila')


###Check variables by type

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HHui_p") #variables important for RF classification
vGG<-vG[1:5]
plotBox(vGG,des="HH_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HHui_c") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="HH_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="HHui_pc") #variables important for RF classification
vGG<-vG[1:7]

test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Hormigas#Valle
test1<-compLH2[[2]]
selr<-match(test1$splot,CovDF$SiteP)
test2<-CovDF[selr,c(pl_var,trns_var)]
test1<-test1[,c(2:dim(test1)[2])]
facB=factor(CovDF$Perm[selr])

#Radom forest to select variables
vGp<-RForest(test2,facB,des="HValp") #variables important for RF classification
vGGp<-vGp[1:4]
print(vGGp)
test2_rf<-test2[,c(vGGp)]

# redundancy with selected variables
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr) #plots and prints statisticas, VIF and anovas

#test with other variables
altV<-c(pl_var,trns_var)
attv_c<-altV[c(1,3,6,7,8,19,20)]
test2_c<-test2[,attv_c]
pairs(test2_c)
attv_c<-altV[c(1,3,7,19)]
print(attv_c)
test2_c<-test2[,attv_c]
#Forest condition variables
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-altV[c(5,16,17,18)]
test2_l<-test2[,attv_l]
pairs(test2_l)
test2_l<-test2[,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-altV[c(2,4,9:15)]
test2_g<-test2[,attv_g]
pairs(test2_g)
attv_g<-altV[c(2,4,9,12,13,14)]
test2_g<-test2[,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Random Forest on species
SpG2<-RForest(test1,facB,des="HVspP")

###Check variables by type

test2<-CovDF[selr,c(Pressv,Conditv)]


test2_p<-test2[,Pressv]
#Radom forest to select variables
vG<-RForest(test2_p,facB,de="HVal_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="HV_p",selr=selr)
test2_p<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_p)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2_c<-test2[,Conditv]

#Radom forest to select variables
vG<-RForest(test2_c,facB,de="HVal_c") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="HV_c",selr=selr)
test2_c<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2_pc<-test2[,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2_pc,facB,de="HVal_pc") #variables important for RF classification
vGG<-vG[1:5]
PCA<-rda(test2_pc,scale=T)
summary(PCA)
PlotPCA(PCA,des="HVal_pc",selr=selr)
test2_pc<-test2[,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2_pc)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

##################################################
#Grupo#Mamiferos

ab.matsM<-CompM[[1]][[1]]
comdM.hel<-lapply(ab.matsM,FUN=decostand,method="hellinger")
compLM<-lapply(comdM.hel,FUN=function(X){
  Y<-colnames(X)
  X$Trans<-gsub("-Low$|-High$|-Medium$","",rownames(X))
  X$Trans<-substr(X$Trans,1,3)
  X$Trans[X$Trans=="Hig"]<-"High"
  rownames(X)<-gsub("^.*-","",rownames(X))
  X<-X[c('Trans',Y)]
})


#Get the covariate data
#agregated data by transformation
#CovDFm<-aggregate(.~Depart*Transf,data=CovDF[,c(3,5,6:31)],FUN=mean)
CovDF$SiteT<-paste(substr(CovDF$Depart,1,1),CovDF$Transf,CovDF$Perm,sep="-")

#Grupo#Mamiferos#Guajira
test1<-compLM[["Guajira"]]
test1$splot<-paste("G",rownames(test1),sep="-")
selr<-match(test1$splot,CovDF$SiteT)
test2<-CovDF[selr,6:(ncol(CovDF)-2)]
test1<-test1[,!colnames(test1)%in%c("Trans","splot")]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])
# Random Forest

vG<-RForest(test2,facA,des="MGua") #variables important for RF classification
vGG<-vG[1:6]
print(vGG)
test2_rf<-CovDF[selr,vGG]
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Forest condition variables
altc<-colnames(test2)[c(1:3,10,20:26)]
test2_c<-test2[,altc]
pairs(test2_c)
altc<-colnames(test2)[c(1,3,21,22,26)]
test2_c<-test2[,altc]
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-colnames(test2)[c(15:19)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
attv_l<-colnames(test2)[c(15:18)]
test2_l<-CovDF[selr,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Greeness and preassure
attv_g<-colnames(test2)[c(4:9,11:14)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
attv_g<-altV[c(4,6,8,11,13)]
test2_g<-CovDF[selr,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence

spe_df = test1 
spe_df = test1[-8,]
facA<-facA[-8]
facB<-facB[-8]
# matriz de especies x punto de conteo
# Factor A es transformación y factor B es permanencia, previamente definidos
facA=recode(facA,High="Alt",Low="Baj")
runDPCoA(spe_df,des="MG",maint='VerT.Guajira')

facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])
#Random Forest on species
SpG<-RForest(test1,facA,de="MGsp",maint='Guajira')

###Check variables by type
Pressv<-nmfinv[grep("ESAC|DisA|dvia",nmfinv)]
Conditv<-nmfinv[grep("MSPA|FInt|evi|ndvi",nmfinv)]
Climv<-nmfinv[grep("Tmax|Precp|Hume",nmfinv)]

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="MGuaj_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="MG_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
plot(tbRDA, type='t')
PlotRda(tbRDA,selr=selr)

##Condition variables

test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="MGuaj_c") #variables important for RF classification
vGG<-vG[1:6]
plotBox(vGG,des="MG_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="MGuaj_pc") #variables important for RF classification
vGG<-vG[1:6]
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)
rm(list=c("test1","test2","selr","facA","facB"))

#Grupo#Mamiferos#Bolivar
test1<-compLM[[1]]
test1$splot<-paste("B",rownames(test1),sep="-")
selr<-match(test1$splot,CovDF$SiteT)
test2<-CovDF[selr,6:(ncol(CovDF)-2)]
test1<-test1[,!colnames(test1)%in%c("Trans","splot")]
facA=factor(CovDF$Transf[selr]) # transformacion
facB=factor(CovDF$Perm[selr])
# Random Forest

vG<-RForest(test2,facA,des="MBol") #variables important for RF classification
vGG<-vG[1:4]
print(vGG)
test2_rf<-CovDF[selr,vGG]
tbRDA<-rda(formula=test1~.,data=test2_rf)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#Forest condition variables
altc<-colnames(test2)[c(1:3,10,20:26)]
test2_c<-test2[,altc]
pairs(test2_c)
altc<-colnames(test2)[c(3,20,21)]
test2_c<-test2[,altc]
tbRDA<-rda(formula=test1~.,data=test2_c)
print(tbRDA)
PlotRda(tbRDA,selr=selr) 

#Climatic variables
attv_l<-colnames(test2)[c(15:19)]
test2_l<-CovDF[selr,attv_l]
pairs(test2_l)
attv_l<-colnames(test2)[c(15,16,17)]
test2_l<-CovDF[selr,attv_l]
tbRDA<-rda(formula=test1~.,data=test2_l)
print(tbRDA)
PlotRda(tbRDA,selr=selr)


#Greeness and preassure
attv_g<-colnames(test2)[c(4:9,11:14)]
test2_g<-CovDF[selr,attv_g]
pairs(test2_g)
attv_g<-colnames(test2)[c(4:7)]
test2_g<-CovDF[selr,attv_g]
tbRDA<-rda(formula=test1~.,data=test2_g)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

#crossed DpCoA with transformation and permanence

# matriz de especies x punto de conteo
# Factor A es transformación y factor B es permanencia, previamente definidos
facA=recode(facA,High="Alt",Low="Baj")
runDPCoA(test1,des="MB",maint='VerT.Bolivar')


#Random Forest on species
SpG<-RForest(test1,facA,de="MBsp",maint='Bolivar')

###Check variables by type

test2<-CovDF[selr,Pressv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="MBol_p") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="MB_p",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

##Condition variables
test2<-CovDF[selr,Conditv]

#Radom forest to select variables
vG<-RForest(test2,facA,de="MBol_c") #variables important for RF classification
vGG<-vG[1:4]
plotBox(vGG,des="MB_c",selr=selr)
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)

###Pressure and condition
test2<-CovDF[selr,c(Pressv,Conditv)]

#Radom forest to select variables
vG<-RForest(test2,facA,de="MBol_pc") #variables important for RF classification
vGG<-vG[1:4]
test2<-CovDF[selr,vGG]

#RDA
tbRDA<-rda(formula=test1~.,data=test2)
print(tbRDA)
PlotRda(tbRDA,selr=selr)


###############Check covariate selection with random forests
# test variables by category
#Condition
ESACvv<-names(CovD)[grep("ESAC",names(CovD))]
pairs(CovD[,ESACvv])
ESACvv<-ESACvv[-c(1,2,4,6)]
facB=factor(CovD$Transf) # transformacion
facA=factor(CovD$Depart)

test1<-as.matrix(CovD[,ESACvv])
which(is.na(test1),arr.ind=T)
#Radom forest to select variables
vG<-RForest(test1,facA,de="CvESA") #variables important for RF classification
vGG<-vG[1:4]
vGp<-RForest(test1,facB,de="CvESAt") #variables important for RF classification
vGGp<-vG[1:4]
selv2b<-list("Dep"=vGG,"Trans"=vGGp)

#Distance
Distvv<-names(CovD)[grep("DisA|dvia",names(CovD))]
pairs(CovD[,Distvv])
Distvv<-Distvv[-c(5,9,7,11)]
test1<-as.matrix(CovD[,Distvv])
which(is.na(test1),arr.ind=T)
#Radom forest to select variables
vG<-RForest(test1,facA,de="CvDis") #variables important for RF classification
vGG<-vG[1:4]
vGp<-RForest(test1,facB,de="CvDist") #variables important for RF classification
vGGp<-vG[1:4]
selv2b[["Dep"]]=c(selv2b[["Dep"]],vGG)
selv2b[["Trans"]]=c(selv2b[["Trans"]],vGGp)

#Forest Condition
Fintv<-names(CovD)[grep("MSPA|FInt",names(CovD))]
test1<-as.matrix(CovD[,Fintv])
selcrm<-which(is.na(test1)|test1==Inf,arr.ind=T)[,2]
rmcol<-colnames(test1)[unique(selcrm)]
test11<-test1[,!colnames(test1)%in%rmcol]
pairs(test11)
rmcol<-c(rmcol,colnames(test11)[c(2,4,11,13,15,18)])
test11<-test1[,!colnames(test1)%in%rmcol]
#Radom forest to select variables without col with NA
vG<-RForest(test11,facA,de="CvFint1") #variables important for RF classification
vGG<-vG[1:6]
vGp<-RForest(test11,facB,de="CvFint1t") #variables important for RF classification
vGGp<-vG[1:5]
selv2b[["Dep"]]=c(selv2b[["Dep"]],vGG)
selv2b[["Trans"]]=c(selv2b[["Trans"]],vGGp)

#analyses without rows with NA or Inf
selcrm<-which(is.na(test1)|test1==Inf,arr.ind=T)[,2]
rmcol<-colnames(test1)[c(6,7,10)]
test11<-test1[,!colnames(test1)%in%rmcol]
selcrm<-which(is.na(test11)|test11==Inf,arr.ind=T)[,1]
rmcol<-unique(selcrm)
test11<-test11[-rmcol,]
#Radom forest to select variables without col with NA
vG<-RForest(test11,facA[-rmcol],de="CvFint2") #variables important for RF classification
vGG<-vG[1:8]
vGp<-RForest(test11,facB[-rmcol],de="CvFint2t") #variables important for RF classification
vGGp<-vG[1:5]
selv2b[["Dep"]]=c(selv2b[["Dep"]],vGG)
selv2b[["Trans"]]=c(selv2b[["Trans"]],vGGp)

#Climate
Climv<-names(CovD)[grep("Tmax|Precp|Hume",names(CovD))]
test1<-as.matrix(CovD[,Climv])
selcrm<-which(is.na(test1)|test1==Inf,arr.ind=T)[,2]
rmcol<-colnames(test1)[unique(selcrm)]
test11<-test1[,!colnames(test1)%in%rmcol]
pairs(test11[,(colnames(test11)[c(1:14)])])
rmcol<-c(rmcol,colnames(test11)[c(2,8,9,11,13,14)])
test11<-test1[,!colnames(test1)%in%rmcol]
#Radom forest to select variables without col with NA
vG<-RForest(test11,facA,de="CvCli1") #variables important for RF classification
vGG<-vG[1:8]
vGp<-RForest(test11,facB,de="CvCli1t") #variables important for RF classification
vGGp<-vG[1:7]
selv2b[["Dep"]]=c(selv2b[["Dep"]],vGG)
selv2b[["Trans"]]=c(selv2b[["Trans"]],vGGp)

#analyses without rows with NA or Inf
selcrm<-which(is.na(test1)|test1==Inf,arr.ind=T)[,2]
rmcol<-colnames(test1)[c(9,10)]
test11<-test1[,!colnames(test1)%in%rmcol]
selcrm<-which(is.na(test11)|test11==Inf,arr.ind=T)[,1]
rmcol<-unique(selcrm)
test11<-test11[-rmcol,]
#Radom forest to select variables without col with NA
vG<-RForest(test11,facA[-rmcol],de="CvCli2") #variables important for RF classification
vGG<-vG[1:8]
vGp<-RForest(test11,facB[-rmcol],de="CvCli2t") #variables important for RF classification
vGGp<-vG[1:8]
selv2b[["Dep"]]=c(selv2b[["Dep"]],vGG)
selv2b[["Trans"]]=c(selv2b[["Trans"]],vGGp)

#Greeness
Verdv<-names(CovD)[grep("evi|ndvi",names(CovD))]
test1<-as.matrix(CovD[,Verdv])
vG<-RForest(test1,facA,de="CvEvi") #variables important for RF classification
vGG<-vG[1:5]
vGp<-RForest(test1,facB,de="CvEvit") #variables important for RF classification
vGGp<-vG[1:4]
selv2b[["Dep"]]=c(selv2b[["Dep"]],vGG)
selv2b[["Trans"]]=c(selv2b[["Trans"]],vGGp)

selv2b[["Dep"]]=unique(selv2b[["Dep"]])
selv2b[["Trans"]]=unique(selv2b[["Trans"]])
selv4nm<-unique(c(selv2b[["Dep"]],selv2b[["Trans"]]))

$Dep
[1] "ESACmeanpx.5000"    "ESACmeant.5000"     "ESACsdt.5000"      
[4] "ESACsdpx.5000"      "DisAmeanest.30"     "DisAcoefvest.500"  
[7] "DisAcoefvest.5000"  "dviacoefvest.5000"  "MSPAdynM.5000"     
[10] "FIntMeanx.5000"     "MSPAMeanx13.5000"   "MSPAdynCf.5000"    
[13] "MSPAMeanx13.500"    "MSPAcofv13.5000"    "MSPAMeanx00.5000"  
[16] "FIntcofv.5000"      "MSPAdynSt.5000"     "Precp90Jan.5000"   
[19] "Precp90Jan.30"      "Humesigma.5000"     "HumecofvarAug.5000"
[22] "Precp10Jul.5000"    "Precp10Jul.500"     "Humesigma.500"     
[25] "Humesigma.30"       "Precp90Jan.500"     "Precp10Jul.30"     
[28] "ndvi90peak.5000"    "evi90trend.5000"    "ndvi10season.5000" 
[31] "evi10season.5000"   "evi90peak.5000"  

#Transform variables
ESACvv<-selv4nm[grep("ESAC",selv4nm)]
Distvv<-selv4nm[grep("DisA|dvia",selv4nm)]
Fintv<-selv4nm[grep("MSPA|FInt",selv4nm)]
Climv<-selv4nm[grep("Tmax|Precp|Hume",selv4nm)]
Verdv<-selv4nm[grep("evi|ndvi",selv4nm)]

for (i in selv4nm){
  print(i)
  hist(CovD[,i],main=i)
  print(summary(CovD[,i]))
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
}
CovDF3<-CovD
hist(log(CovDF3$MSPAcofv13.5000))
CovDF3$lgESACmeant.5000<-log(CovDF3$ESACmeant.5000)#
CovDF3$lgESACsdt.5000<-log(CovDF3$ESACsdt.5000+0.01)
CovDF3$lgESACsdpx.5000<-log(CovDF3$ESACsdpx.5000+1)#
CovDF3$sqrtDisAmeanest.30<-sqrt(CovDF3$DisAmeanest.30+657)
CovDF3$sqrtDisAmeanest.500<-sqrt(CovDF3$DisAmeanest.500+657)
CovDF3$sqrtDisAmeanest.5000<-sqrt(CovDF3$DisAmeanest.5000+645)
CovDF3$lgDisAcoefvest.500<-log(CovDF3$DisAcoefvest.500+0.0001)
CovDF3$IvDisAcoefvest.5000<-1/(CovDF3$DisAcoefvest.5000+0.01)
CovDF3$lgdviacoefvest.5000<-log(CovDF3$dviacoefvest.5000+0.01)
CovDF3$p2MSPAdynM.5000<-(CovDF3$MSPAdynM.5000+31)^2
CovDF3$sqrtFIntMeanx.5000<-sqrt(CovDF3$FIntMeanx.5000)
CovDF3$lgMSPAMeanx13.5000<-log(CovDF3$MSPAMeanx13.5000)
CovDF3$lgMSPAMeanx00.5000<-log(CovDF3$MSPAMeanx00.5000)
CovDF3$lgMSPAdynCf.5000<-log(CovDF3$MSPAdynCf.5000+0.5)
CovDF3$lgMSPAMeanx13.500<-log(CovDF3$MSPAMeanx13.500)
CovDF3$lgMSPAcofv13.5000<-log(CovDF3$MSPAcofv13.5000)
CovDF3$lgFIntcofv.5000<-log(CovDF3$FIntcofv.5000)
CovDF3$lgMSPAdynSt.5000<-log(CovDF3$MSPAdynSt.5000+18)
CovDF3$sqrtPrecp90Jan.5000<-sqrt(CovDF3$Precp90Jan.5000)
CovDF3$sqrtPrecp90Jan.30<-sqrt(CovDF3$Precp90Jan.30)
CovDF3$lgHumesigma.5000<-log(CovDF3$Humesigma.5000)
CovDF3$lgHumecofvarAug.5000<-log(CovDF3$HumecofvarAug.5000)
CovDF3$sqrtPrecp10Jul.5000<-sqrt(CovDF3$Precp10Jul.5000)
CovDF3$sqrtPrecp10Jul.500<-sqrt(CovDF3$Precp10Jul.500)
CovDF3$lgHumesigma.500<-log(CovDF3$Humesigma.500)
CovDF3$lgHumesigma.30<-log(CovDF3$Humesigma.30)
CovDF3$sqrtPrecp90Jan.500<-sqrt(CovDF3$Precp90Jan.500)
CovDF3$sqrtPrecp10Jul.30<-sqrt(CovDF3$Precp10Jul.30)
CovDF3$lgndvi90peak.5000<-log(CovDF3$ndvi90peak.5000)
CovDF3$sqrtevi90trend.5000<-sqrt(CovDF3$evi90trend.5000)
CovDF3$lgndvi10season.5000<-log(CovDF3$ndvi10season.5000+0.01)
CovDF3$lgevi10season.5000<-log(CovDF3$evi10season.5000)


nmvar<-colnames(CovDF3)[grep("^lg|sqrt|Iv|p2",colnames(CovDF3))]
for (i in nmvar){
  print(i)
  hist(CovDF3[,i],main=i)
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
}
nmvar2<-gsub("^lg|sqrt|Iv|p2","",nmvar)
nmvar3<-names(CovDF3)[!names(CovDF3)%in%nmvar2]

CovDF3<-CovDF3[,nmvar3]
CovDF3<-cbind(CovD[,infc],CovDF3)

#check scale
selv5<-list()
for (i in adnmv){
  #readline(prompt="Press [enter] to continue or [ESC] to exit: ")
  CovDD<-CovDF3[,c(i,"Transf","Perm","Depart")]
  names(CovDD)[which(names(CovDD)==i)]<-"value"
  print(i)
  try(print(ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
              facet_wrap(~Depart)+ggtitle(i)))
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
  try(print(ggplot(data=CovDD,aes(y=value,x=Perm))+geom_boxplot()+
              facet_wrap(~Depart)+ggtitle(i)))
  readline(prompt="Press [enter] to continue or [ESC] to exit: ")
  try(print(ggplot(data=CovDD,aes(y=value,x=Depart))+geom_boxplot()+
              ggtitle(i)))
  n2<-readline(prompt="which scale plot,transf,dep?: " )
  selv5[[i]]<-data.frame("scale"=as.character(n2),stringsAsFactors = F)
  
}
selvv5<-do.call(rbind,selv5)
selvv5$scale[3]<-"t"
selvv5$scale[12]<-"t"
selvv5$varbl<-rownames(selvv5)
rownames(selvv5)<-NULL
save(selvv5,file=file.path(WD,"selvv5.RData"))
write.csv(CovDF3,file=file.path("C:","Users","dsrbu","Dropbox","Humboldt","Documentos de trabajo",
                               "BosqueSeco","Analisis","CovariatesBIDGEF","GEFCovFinal_RF.csv"))
# Database by scale
trns_var<-selvv5[selvv5$scale=="t","varbl"]
CovTrns<-cbind(CovDF3[,infc],CovDF3[,trns_var])
dep_var<-selvv5[selvv5$scale=="d","varbl"]
CovDep<-cbind(CovDF3[,infc],CovDF3[,dep_var])

#4) ordination analyses by scale
colty<-data.frame("site"=c("Cesar","Valle del Cauca","Guajira",
                           "Bolivar","Tolima","Huila"),"col"=c("red","blue","green","black","magenta","gray"),stringsAsFactors =F )
coltyt<-data.frame("Trans"=c("High","Med","Low"),"col"=c("red","blue","green"), stringsAsFactors = F)
coltyp<-data.frame("Perm"=c("Med","High","Fire","Low"),"col"=c("blue","green","orange","red"),stringsAsFactors = F)

PlotPCArf<-function(PCA,des="NN",selr=NULL){
  par(mfrow=c(1,1))
  if(is.null(selr)) selr=1:dim(CovDF3)[1]
  nax<-length(attributes(PCA$Ybar)$dimnames[[2]])
  sig <- PCAsignificance (PCA, axes = nax)
  print(sig)
  barplot (sig[c('percentage of variance', 'broken-stick percentage'), ], beside = T, 
           xlab = 'PCA axis', ylab = 'explained variation [%]', col = c('grey', 'black'), 
           legend = TRUE)
  q1<-as.numeric(readline(prompt="Numer of significan axis: " ))
  if(q1==1|is.null(q1)) q1<-2
  loadings<-scores(PCA,display='species', scaling=0,choices=c(1:q1))
  for (a in 1:q1){
    print("loadings= ",a)
    print(sort (abs (loadings[,a]), decreasing = TRUE))
  }
  minPCA<-apply(scores(PCA)$sites,2,max)
  par (mfrow = c(1,2))
  cleanplot.pca (PCA, scaling = 1)
  cleanplot.pca (PCA, scaling = 2)
  readline(prompt="press [ENTER] " )
  if(q1>=2){
    cleanplot.pca (PCA, scaling = 1,ax1=2,ax2=3)
    cleanplot.pca (PCA, scaling = 1,ax1=1,ax2=3)
    readline(prompt="press [ENTER] " )
  }
  
  for(ii in 1:(q1-1)){
    for(jj in (ii+1):q1){
      print(paste("plots of axis",ii,"and",jj))
      par (mfrow = c(2,2))
      cleanplot.pca (PCA, scaling = 1,cex=0.8,ax1=ii,ax2=jj)
      ordiplot(PCA,display='sites',type='n',main="Cuenca",cex=0.8,choices=c(ii,jj))
      colty2<-match(CovDF3$Depart[selr],colty$site)
      colty22<-unique(colty2)
      points(PCA,col=colty$col[colty2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=substr(colty$site[colty22],1,4),col=colty$col[colty22],cex=0.5,pch=18,bty="n")
      coltyt2<-match(CovDF3$Trans[selr],coltyt$Trans)
      coltyt22<-unique(coltyt2)
      ordiplot(PCA,display='sites',type='n',main="Transformación",cex=0.8,choices=c(ii,jj))
      points(PCA,col=coltyt$col[coltyt2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyt$Trans[coltyt22],col=coltyt$col[coltyt22],cex=0.5,pch=18,bty="n")
      coltyp2<-match(CovDF3$Perm[selr],coltyp$Perm)
      coltyp22<-unique(coltyp2)
      ordiplot(PCA,display='sites',type='n',main="Plot",cex=0.8,choices=c(ii,jj))
      points(PCA,col=coltyp$col[coltyp2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyp$Perm[coltyp22],col=coltyp$col[coltyp22],cex=0.5,pch=18,bty="n")
      readline(prompt="press [ENTER] " )
    }
  }
  
  #save plots
  for(ii in 1:(q1-1)){
    for(jj in (ii+1):q1){
      des1<-paste(des,ii,jj,sep="")
      nmp<-paste(WD,"Plots",paste(des1,"jpeg",sep="."),sep="/")
      jpeg(filename=nmp,quality=100,res=300,width=2800,height=2000)
      par (mfrow = c(2,2))
      cleanplot.pca (PCA, scaling = 1,cex=0.8,ax1=ii,ax2=jj)
      ordiplot(PCA,display='sites',type='n',main="Cuenca",cex=0.8,choices=c(ii,jj))
      colty2<-match(CovDF3$Depart[selr],colty$site)
      colty22<-unique(colty2)
      points(PCA,col=colty$col[colty2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=substr(colty$site[colty22],1,4),col=colty$col[colty22],cex=0.5,pch=18,bty="n")
      coltyt2<-match(CovDF3$Trans[selr],coltyt$Trans)
      coltyt22<-unique(coltyt2)
      ordiplot(PCA,display='sites',type='n',main="Transformación",cex=0.8,choices=c(ii,jj))
      points(PCA,col=coltyt$col[coltyt2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyt$Trans[coltyt22],col=coltyt$col[coltyt22],cex=0.5,pch=18,bty="n")
      coltyp2<-match(CovDF3$Perm[selr],coltyp$Perm)
      coltyp22<-unique(coltyp2)
      ordiplot(PCA,display='sites',type='n',main="Plot",cex=0.8,choices=c(ii,jj))
      points(PCA,col=coltyp$col[coltyp2],pch=18,choices=c(ii,jj))
      legend(minPCA[1],minPCA[2],legend=coltyp$Perm[coltyp22],col=coltyp$col[coltyp22],cex=0.5,pch=18,bty="n")
      dev.off()
    }
  }
}
##Transformation scale
PCA<-rda(CovTrns[,trns_var],scale=T)
summary(PCA)
PlotPCArf(PCA,des="transvRF")

for (j in trns_var){
  
  CovDD<-CovDF3[,c(j,"Transf","Perm","Depart")]
  names(CovDD)[which(names(CovDD)==j)]<-"value"
  print(j)
  try(print(ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
              facet_wrap(~Depart)+ggtitle(j)))
  n2<-readline(prompt="save plot?: " )
  if(n2==1){
    plotV<-ggplot(data=CovDD,aes(y=value,x=Transf))+geom_boxplot()+
      facet_wrap(~Depart)+ggtitle(j)
    nmp<-paste(gsub("\\.","",j),"jpeg",sep=".")
    ggsave(paste(WD,"Plots",nmp,sep="/"),plotV,device="jpeg")
    
  }
}

##Dep scale
PCA<-rda(CovDep[,dep_var],scale=T)
summary(PCA)
PlotPCArf(PCA,des="DepvRF")
for (j in dep_var){
  
  CovDD<-CovDF3[,c(j,"Transf","Perm","Depart")]
  names(CovDD)[which(names(CovDD)==j)]<-"value"
  print(j)
  try(print(ggplot(data=CovDD,aes(y=value,x=Depart))+geom_boxplot()+
              ggtitle(j)))
  n2<-readline(prompt="save plot?: " )
  if(n2==1){
    plotV<-ggplot(data=CovDD,aes(y=value,x=Depart))+geom_boxplot()+
     ggtitle(j)
    nmp<-paste(gsub("\\.","",j),"jpeg",sep=".")
    ggsave(paste(WD,"Plots",nmp,sep="/"),plotV,device="jpeg")
    
  }
}

##All variables
PCA<-rda(CovDF3[,c(dep_var,trns_var)],scale=T)
summary(PCA)
par(mfrow=c(1,1))
PlotPCArf(PCA,des="Allv_RF")


##Ordination by type of variables
Pressv<-nmvar3[grep("ESAC|DisA|dvia",nmvar3)]
Conditv<-nmvar3[grep("MSPA|FInt|evi|ndvi",nmvar3)]
Climv<-nmvar3[grep("Tmax|Precp|Hume",nmvar3)]

#Preassure

PCA<-rda(CovDF3[,Pressv],scale=T)
summary(PCA)
PlotPCArf(PCA,des="Press_RF")

#Condition

PCA<-rda(CovDF3[,Conditv],scale=T)
summary(PCA)
PlotPCArf(PCA,des="Condit_RF")

#Condition

PCA<-rda(CovDF3[,Climv],scale=T)
summary(PCA)
PlotPCArf(PCA,des="Clim_RF")
