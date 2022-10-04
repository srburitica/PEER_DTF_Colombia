# source("~/Desktop/IAvH/PNUD/R/0-ImportAllData.R")
# source("~/Desktop/IAvH/PNUD/R/1-calc.divAll.R")
WD<-("C:/Users/dsrbu/Dropbox/Humboldt/Documentos de Trabajo/BosqueSeco/GEF/Analysis")
setwd(WD)
RD<-file.path(WD,"MultiTRes") #directory to store results
source(file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Scripts_R","0-ImportAllData.R"))
source(file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Scripts_R","1-calc.divAll.R"))

library(ggplot2)
library(GGally)

# a) vegetación
DataV_PT$watershed[DataV_PT$watershed=="Valle del Cauca"]<-"Valle"
DivV<-lapply(X=list(DataV_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")

DivsV=as.data.frame(DivV[[1]]$StrD)
rownames(DivsV)[rownames(DivsV)=="TolimaS"]="Tolima"
DivsV2=reshape(DivsV,idvar="Sitio",ids=rownames(DivsV),times=colnames(DivsV),varying=list(colnames(DivsV)),timevar="Characteristic",direction="long")
DivsV2$Organismo="Plantas"
DivsV2$Transf=c(rep(NA,6),rep("Baja",6),rep("Media",6),rep("Alta",6))


# b) aves
DataA_PT$watershed[DataA_PT$watershed=="Valle del Cauca"]<-"Valle"
DataAS_PT<-DataA_PT[DataA_PT$Estacion=="Seca",]
DivA<-lapply(X=list(DataAS_PT), FUN= calc.div, wcl="watershed",tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
DivsA=as.data.frame(DivA[[1]]$StrD)
DivsA2=reshape(DivsA,idvar="Sitio",ids=rownames(DivsA),times=colnames(DivsA),varying=list(colnames(DivsA)),timevar="Characteristic",direction="long")
DivsA2$Organismo="Aves"
DivsA2$Transf=c(rep(NA,6),rep("Baja",6),rep("Media",6),rep("Alta",6))
DivsA2$Estacion<-"Seca"

#

DataAL_PT<-DataA_PT[DataA_PT$Estacion=="Lluviosa",]
DivA<-lapply(X=list(DataAL_PT), FUN= calc.div, wcl="watershed",tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
DivlA=as.data.frame(DivA[[1]]$StrD)
DivlA2=reshape(DivsA,idvar="Sitio",ids=rownames(DivlA),times=colnames(DivlA),varying=list(colnames(DivlA)),timevar="Characteristic",direction="long")
DivlA2$Organismo="Aves"
DivlA2$Transf=c(rep(NA,6),rep("Baja",6),rep("Media",6),rep("Alta",6))
DivlA2$Estacion<-"Lluvias"


# c) vertebrados

unique(DataM1_T$transfC)
DataM1_T$transfC[DataM1_T$transfC=="Alta"]="High"
DataM1_T$transfC[DataM1_T$transfC=="Baja"]="Low"
DataM1_T$transfC[DataM1_T$transfC=="Med"]="Medium"
DataM1_T$transfC[DataM1_T$transfC=="Media"]="Medium"
DivM<-lapply(X=list(DataM1_T), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="transfC",abcl="IndNumber",spcl="Species")
DivsM=as.data.frame(DivM[[1]]$StrD)
DivsM2=reshape(DivsM,idvar="Sitio",ids=rownames(DivsM),times=colnames(DivsM),varying=list(colnames(DivsM)),timevar="Characteristic",direction="long")
DivsM2$Organismo="Vertebrados"
DivsM2$Transf=c(rep(NA,6),rep("Baja",6),rep("Media",6),rep("Alta",6))


# d) hormigas
DataH_PT$Estacion[DataH_PT$Estacion=="recenso"]<-"Recenso"
DataH_PT$transfC[DataH_PT$transfC=="Med"]<-"Medium"

DataHc<-list(subset(DataH_PT,Estacion=="Censo"))
DataHh<-list(subset(DataH_PT,Estacion=="Recenso"))

DivH<-lapply(X=DataHc, FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
DivsH=as.data.frame(DivH[[1]]$StrD)
DivsH2=reshape(DivsH,idvar="Sitio",ids=rownames(DivsH),times=colnames(DivsH),varying=list(colnames(DivsH)),timevar="Characteristic",direction="long")
DivsH2$Organismo="Hormigas"
DivsH2$Transf=c(rep(NA,6),rep("Baja",6),rep("Media",6),rep("Alta",6))
DivsH2$Estacion="Censo"

#recenso
DivH<-lapply(X=DataHh, FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
DivrH=as.data.frame(DivH[[1]]$StrD)
DivrH2=reshape(DivrH,idvar="Sitio",ids=rownames(DivrH),times=colnames(DivrH),varying=list(colnames(DivrH)),timevar="Characteristic",direction="long")
DivrH2$Organismo="Hormigas"
DivrH2$Transf=c(rep(NA,6),rep("Baja",6),rep("Media",6),rep("Alta",6))
DivrH2$Estacion="Recenso"

savel<-c("DivsV2","DivsA2","DivlA2","DivsM2","DivsH2","DivrH2","RD","WD")

rm(list=ls()[!ls()%in%savel])



####Correlaciones

# 1- GLOBALES 
#solo entre aves y plantas que tienen todos los datos
names(DivsV2)[2]<-"q0V"
m=match(rownames(DivsV2),rownames(DivsA2))
DivsA2=DivsA2[m,]
names(DivsA2)[2]<-"q0As"
m=match(rownames(DivsV2),rownames(DivlA2))
DivlA2=DivlA2[m,]
names(DivlA2)[2]<-"q0Al"
m=match(rownames(DivsV2),rownames(DivsM2))
DivsM2=DivsM2[m,]
names(DivsM2)[2]<-"q0M"
m=match(rownames(DivsV2),rownames(DivsH2))
DivsH2=DivsH2[m,]
names(DivsH2)[2]<-"q0Hs"
m=match(rownames(DivsV2),rownames(DivrH2))
DivrH2=DivrH2[m,]
names(DivrH2)[2]<-"q0Hr"

for (i in savel){
  fname<-paste("StrD",paste(i,"csv",sep="."),sep="")
  write.csv(get(i),file.path(RD,fname))
}


CorAll<-data.frame("Veg"=DivsV2$q0V[-c(1:6)],"Avs"=DivsA2$q0As[-c(1:6)],"Avl"=DivlA2$q0Al[-c(1:6)],
                   "Mam"=DivsM2$q0M[-c(1:6)],"Horc"=DivsH2$q0Hs[-c(1:6)],"Horr"=DivrH2$q0Hr[-c(1:6)])

CorDep<-data.frame("Veg"=DivsV2$q0V[c(1:6)],"Avs"=DivsA2$q0As[c(1:6)],"Avl"=DivlA2$q0Al[c(1:6)],
                   "Mam"=DivsM2$q0M[c(1:6)],"Horc"=DivsH2$q0Hs[c(1:6)],"Horr"=DivrH2$q0Hr[c(1:6)])
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(CorAll, lower = list(continuous = my_fn))
g

g = ggpairs(CorDep, lower = list(continuous = my_fn))
g

#sp tot
plot(DivsV2[-(1:6),2],DivsA2[-(1:6),2],pch=19,xlab="N�mero de especies de plantas",ylab="N�mero de especies de aves")
cor(DivsV2[-(1:6),2],DivsA2[-(1:6),2],method="pearson",use="complete.obs") #0.39
summary(lm(DivsA2[-(1:6),2]~DivsV2[-(1:6),2]))







mycol <- rgb(80, 80, 80, max = 255, alpha = 255)
mycol1 <- rgb(80, 80, 80, max = 255, alpha = 150)
mycol2 <- rgb(80, 80, 80, max = 255, alpha = 100)
col2rgb("blue")


plot(DivsV2[-(1:6),2],DivsA2[-(1:6),2],pch=19,xlab="Número de especies de plantas",ylab="Número de especies de aves",col=mycol)

col2rgb("yellow4")
rgb2col("yellow4")

gc1= rgb(0,0,255,max=255,alpha=255)
gc2= rgb(0,0,255,max=255,alpha=180)
gc3= rgb(0,0,255,max=255,alpha=120)

bc1= rgb(255,0,0,max=255,alpha=255)
bc2= rgb(255,0,0,max=255,alpha=180)
bc3= rgb(255,0,0,max=255,alpha=120)

hc1= rgb(139,0,139,max=255,alpha=255)
hc2= rgb(139,0,139,max=255,alpha=180)
hc3= rgb(139,0,139,max=255,alpha=120)

tc1= rgb(80,80,80,max=255,alpha=255)
tc2= rgb(80,80,80,max=255,alpha=180)
tc3= rgb(80,80,80,max=255,alpha=120)

cc2= rgb(139,139,0,max=255,alpha=180)

vc1= rgb(139,76,57,max=255,alpha=120)

cc4=c(gc1,bc1,hc1,tc1,gc2,bc2,cc2,hc2,tc2,gc3,bc3,hc3,tc3,vc1)
pie(rep(1,15),cc4)

plot(DivsV2[-(1:6),2],DivsA2[-(1:6),2],pch=19,xlab="Número de especies de plantas",ylab="Número de especies de aves",col=cc4,cex=1.5)




# # 
# fg <- colorRampPalette(c("blue", "darkgreen")); g = fg(3)
# fr <- colorRampPalette(c("red", "darkred")); r = fr(3)
# fb <- colorRampPalette(c("lightblue", "darkblue")); b = fb(3)
# fp <- colorRampPalette(c("pink", "deeppink4")); p = fp(3)
# fc <- colorRampPalette(c("magenta", "magenta4")); c = fc(3)
# fk <- colorRampPalette(c("khaki", "khaki4")); k = fk(3)



# cc=list(g,r,b,p,c,k)
# cc2=do.call("rbind",cc)
# cc3=rep(c("green","red","blue","pink","magenta","khaki"),each=6)
# plot(DivsV2[-(1:6),2],DivsA2[-(1:6),2],pch=19,xlab="Número de especies de plantas",ylab="Número de especies de aves",col=cc3)

# plot(DivsV2[-(1:6),2],DivsA2[-(1:6),2],pch=19,xlab="Número de especies de plantas",ylab="Número de especies de aves",cc3)

### por cuenca
#dcor=data.frame(DV=c(DivsV[,1],DivsV[,2],DivsV[,3]),DA=c(DivsA[,1],DivsA[,2],DivsA[,3]),cat=rep(c("B","M","A"),each=6))

#bolivar
bv=subset(DivsV2,Sitio=="Bolivar")
bh=subset(DivsH2,Sitio=="Bolivar")
bm=subset(DivsM2,Sitio=="Bolivar")
ba=subset(DivsA2,Sitio=="Bolivar")


plot(bv$NoSpT,bh$NoSpT)


dcor=rbind(DivsV2,DivsA2,DivsM2,DivsH2)


plot(DivsV[,1],DivsA[,1])

plot(c(DivsV[,1],DivsV[,2],DivsV[,3]),c(DivsA[,1],DivsA[,2],DivsA[,3]))


cor(DivsV[,1],DivsA[,1],method="spearman")
cor(c(DivsV[,2],DivsV[,3],DivsV[,4]),c(DivsA[,2],DivsA[,3],DivsA[,4]),use="complete.obs")

dcor=data.frame(DV=c(DivsV[,2],DivsV[,3],DivsV[,4]),DA=c(DivsA[,2],DivsA[,3],DivsA[,4]),cat=rep(c("B","M","A"),each=6))

plot(dcor$DV,dcor$DA,col=rep(c("red","blue","green"),each=6),pch=16)


ggcor=ggplot(dcor,aes(DV,DA,color=cat))+geom_line()
print(ggcor)
