#Run diversity function. different datasets are mixed in a list the "T" at the end indicates that
#analyses merging data by transformation
library(ggplot2)
#source("~/Desktop/IAvH/PNUD/R/0-ImportAllData.R")
#source("~/Desktop/IAvH/PNUD/R/1-calc.divAll.R")
source(file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Scripts_R","0-ImportAllData.R"))
source(file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Scripts_R","1-calc.divAll.R"))
#path="~/Desktop/IAvH/PNUD/R/figuras/"
path="G:/My Drive/GEF_BosqueSeco/Analisis_Integrados/Resultados Natalia_Indicadores"
#1) VEGETACION


#RUN
DivV<-lapply(X=list(DataV_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")


#PLOTS

DivV_div<-unlist(lapply(DivV,"[[",1), recursive=F)
DivV_div2<-do.call(rbind,DivV_div)
DivV_div2$succe[DivV_div2$succe=="Medium"]<-"Med"
DivV_div2$trans[DivV_div2$trans=="Medium"]<-"Media"
DivV_div2$trans[DivV_div2$trans=="High"]<-"Alta"
DivV_div2$trans[DivV_div2$trans=="Low"]<-"Baja"
DivV_div2$transF<-factor(DivV_div2$trans,levels=c("Baja","Media","Alta"))

DivV_div2$trat<-paste(DivV_div2$trans,DivV_div2$succe,sep="-")
DivV_div2$watershed<-gsub("^(.*)\\.(.*)$","\\1",rownames(DivV_div2))
DivV_div2$watershed[DivV_div2$watershed=="TolimaS"]="Tolima"
DivV_div2$watershed[DivV_div2$watershed=="Valle del Cauca"]="Valle"


#levelsTrat<-c("High-Low","High-Med","High-High","Med-Low","Med-Med","Med-Fire","Med-High","Low-Low","Low-Med","Low-High")

  
ggpV1T<-ggplot(aes(x=transF,y=q0),data=DivV_div2)+
  geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, 
               notch=FALSE)+
  facet_wrap(~watershed)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,
                                                                      face="bold"),
        strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),
        axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+
  xlab("Transformación") + ylab("Número de especies (q0)")+ggtitle("VEGETACIÓN")
print(ggpV1T)

ggsave(paste(path,"BoxplotVq0.pdf",sep="/"), ggpV1T)


ggpV2T<-ggplot(aes(x=transF,y=q1),data=DivV_div2)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Diversidad de especies (q1)")+ggtitle("VEGETACIÓN")
print(ggpV2T)

ggsave(paste(path,"BoxplotVq1.pdf",sep="/"), ggpV2T)


ggpV3T<-ggplot(aes(x=transF,y=q2),data=DivV_div2)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies dominantes (q2)")+ggtitle("VEGETACIÓN")
print(ggpV3T)

ggsave(paste(path,"BoxplotVq2.pdf",sep="/"), ggpV3T)


#AVES
#run

DataAL_PT<-DataA_PT[DataA_PT$Estacion=="Lluviosa",]
DataAS_PT<-DataA_PT[DataA_PT$Estacion=="Seca",] #tomar solo la seca para cohenrencia con vertebrados

DivAs<-lapply(X=list(DataAS_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
DivAs_div<-unlist(lapply(DivAs,"[[",1), recursive=F)
DivAs_div2<-do.call(rbind,DivAs_div)
DivAs_div2$Estacion<-"Seca"

DivAh<-lapply(X=list(DataAL_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
DivAh_div<-unlist(lapply(DivAh,"[[",1), recursive=F)
DivAh_div2<-do.call(rbind,DivAh_div)
DivAh_div2$Estacion<-"Lluvias"
DivA_div2<-rbind(DivAh_div2,DivAs_div2)

DivA_div2$succe[DivA_div2$succe=="Medium"]<-"Med"
DivA_div2$trans[DivA_div2$trans=="Medium"]<-"Media"
DivA_div2$trans[DivA_div2$trans=="Low"]<-"Baja"
DivA_div2$trans[DivA_div2$trans=="High"]<-"Alta"

DivA_div2$transF<-factor(DivA_div2$trans,levels=c("Baja","Media","Alta"))
DivA_div2$trat<-paste(DivA_div2$trans,DivA_div2$succe,sep="-")
DivA_div2$watershed<-gsub("^(.*)\\.(.*)$","\\1",rownames(DivA_div2))
DivA_div2$watershed[DivA_div2$watershed=="Valle del Cauca"]="Valle"

#Seca
DataAves<-DivA_div2[DivA_div2$Estacion=="Seca",]
ggpA1T<-ggplot(aes(x=transF,y=q0),data=DataAves)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies (q0)")+ggtitle("AVES")
print(ggpA1T)
ggsave(paste(path,"BoxplotAsq0.pdf",sep="/"), ggpA1T)
ggpA2T<-ggplot(aes(x=transF,y=q1),data=DataAves)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies (q1)")+ggtitle("AVES")
print(ggpA2T)
ggsave(paste(path,"BoxplotAsq1.pdf",sep="/"), ggpA2T)
ggpA3T<-ggplot(aes(x=transF,y=q2),data=DataAves)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies (q2)")+ggtitle("AVES")
print(ggpA3T)
ggsave(paste(path,"BoxplotAsq2.pdf",sep="/"), ggpA3T)

#lluvias
DataAves<-DivA_div2[DivA_div2$Estacion=="Lluvias",]
ggpA1T<-ggplot(aes(x=transF,y=q0),data=DataAves)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies (q0)")+ggtitle("AVES")
print(ggpA1T)
ggsave(paste(path,"BoxplotAlq0.pdf",sep="/"), ggpA1T)
ggpA2T<-ggplot(aes(x=transF,y=q1),data=DataAves)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Diversidad de especies (q1)")+ggtitle("AVES")
print(ggpA2T)
ggsave(paste(path,"BoxplotAlq1.pdf",sep="/"), ggpA2T)
ggpA3T<-ggplot(aes(x=transF,y=q2),data=DataAves)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies dominantes (q2)")+ggtitle("AVES")
print(ggpA3T)
ggsave(paste(path,"BoxplotAlq2.pdf",sep="/"), ggpA3T)

##VERTEBRADOS
#run
DataM<-list(DataM1_T)
DivM<-lapply(X=DataM, FUN= calc.div, wcl="watershed", tcl="transfC",pcl="Estacionf2",abcl="IndNumber",spcl="Species")


#plot
DivM_div<-unlist(lapply(DivM,"[[",1), recursive=F)
DivM_div2<-do.call(rbind,DivM_div)
DivM_div2$trans[DivM_div2$trans=="Med"]<-"Media"
DivM_div2$trans[DivM_div2$trans=="Low"]<-"Baja"
DivM_div2$trans[DivM_div2$trans=="High"]<-"Alta"
DivM_div2$transF<-factor(DivM_div2$trans,levels=c("Baja","Media","Alta"))
DivM_div2$trat<-DivM_div2$trans
DivM_div2$watershed<-gsub("^(.*)\\.(.*)$","\\1",rownames(DivM_div2))


ggpM1T<-ggplot(aes(x=transF,y=q0),data=DivM_div2)+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                              size=13),axis.text.y = element_text(size=12),
                   strip.text.x=element_text(size=14,face="bold"),
                   strip.background=element_rect(colour="lightsalmon",
                                                 fill="lightsalmon"),
                   axis.title=element_text(size=14),
                   plot.title=element_text(hjust=0.5))+
  xlab("Transformación") + ylab("Número de especies (q0)")+
  ggtitle("VERTEBRADOS TERRESTRES")
print(ggpM1T)

ggsave(paste(path,"BoxplotM1q0.pdf",sep="/"), ggpM1T)


ggpM2T<-ggplot(aes(x=transF,y=q1),data=DivM_div2)+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, 
                                              hjust = 1,size=13),
                   axis.text.y = element_text(size=12),
                   strip.text.x=element_text(size=14,face="bold"),
                   strip.background=element_rect(colour="lightsalmon",
                                                 fill="lightsalmon"),
                   axis.title=element_text(size=14),
                   plot.title=element_text(hjust=0.5))+
  xlab("Transformación") + ylab("Diversidad de especies (q1)")+
  ggtitle("VERTEBRADOS TERRESTRES")
print(ggpM2T)

ggsave(paste(path,"BoxplotM1q1.pdf",sep="/"), ggpM2T)


ggpM3T<-ggplot(aes(x=transF,y=q2),data=DivM_div2)+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                              size=13),
                   axis.text.y = element_text(size=12),
                   strip.text.x=element_text(size=14,face="bold"),
                   strip.background=element_rect(colour="lightsalmon",
                                                 fill="lightsalmon"),
                   axis.title=element_text(size=14),
                   plot.title=element_text(hjust=0.5))+
  xlab("Transformación") + ylab("Número de especies dominantes (q2)")+
  ggtitle("VERTEBRADOS TERRESTRES")
print(ggpM3T)

ggsave(paste(path,"BoxplotM1q2.pdf",sep="/"), ggpM3T)



#HORMIGAS
#DataH<-list(DataH_PT)
DataHc<-list(subset(DataH_PT,Estacion=="Censo"))
DivHc<-lapply(X=DataHc, FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")

DataHr<-list(subset(DataH_PT,Estacion=="Recenso"))
DivHr<-lapply(X=DataHr, FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")

#plot
DivHc_div<-unlist(lapply(DivHc,"[[",1), recursive=F)
DivHc_div2<-do.call(rbind,DivHc_div)
DivHc_div2$Estacion="Censo"

DivHr_div<-unlist(lapply(DivHr,"[[",1), recursive=F)
DivHr_div2<-do.call(rbind,DivHr_div)
DivHr_div2$Estacion="Recenso"

DivH_div2<-rbind(DivHc_div2,DivHr_div2)

DivH_div2$trans[DivH_div2$trans=="Med"]<-"Media"
DivH_div2$trans[DivH_div2$trans=="Low"]<-"Baja"
DivH_div2$trans[DivH_div2$trans=="High"]<-"Alta"
DivH_div2$succe[DivH_div2$succe=="Med"]<-"Media"
DivH_div2$succe[DivH_div2$succe=="Low"]<-"Baja"
DivH_div2$succe[DivH_div2$succe=="High"]<-"Alta"
DivH_div2$succe[DivH_div2$succe=="Fire"]<-"Fuego"
DivH_div2$transF<-factor(DivH_div2$trans,levels=c("Baja","Media","Alta"))
DivH_div2$trat<-paste(DivH_div2$trans,DivH_div2$succe,sep="-")
DivH_div2$watershed<-gsub("^(.*)\\.(.*)$","\\1",rownames(DivH_div2))

#plot
DataHorm<-DivH_div2[DivH_div2$Estacion=="Censo",]
ggpH1T<-ggplot(aes(x=transF,y=q0),data=DataHorm)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies (q0)")+ggtitle("HORMIGAS")
print(ggpH1T)
ggsave(paste(path,"BoxplotHcq0.pdf",sep="/"), ggpH1T)
ggpH2T<-ggplot(aes(x=transF,y=q1),data=DataHorm)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Diversidad de especies (q1)")+ggtitle("HORMIGAS")
print(ggpH2T)
ggsave(paste(path,"BoxplotHcq1.pdf",sep="/"), ggpH2T)
ggpH3T<-ggplot(aes(x=transF,y=q2),data=DataHorm)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies dominantes (q2)")+ggtitle("HORMIGAS")
print(ggpH3T)
ggsave(paste(path,"BoxplotHcq2.pdf",sep="/"), ggpH3T)

DataHorm<-DivH_div2[DivH_div2$Estacion=="Recenso",]
ggpH1T<-ggplot(aes(x=transF,y=q0),data=DataHorm)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies (q0)")+ggtitle("HORMIGAS")
print(ggpH1T)
ggsave(paste(path,"BoxplotHrq0.pdf",sep="/"), ggpH1T)
ggpH2T<-ggplot(aes(x=transF,y=q1),data=DataHorm)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Diversidad de especies (q1)")+ggtitle("HORMIGAS")
print(ggpH2T)
ggsave(paste(path,"BoxplotHrq1.pdf",sep="/"), ggpH2T)
ggpH3T<-ggplot(aes(x=transF,y=q2),data=DataHorm)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)+facet_wrap(~watershed)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+xlab("Transformación") + ylab("Número de especies dominantes (q2)")+ggtitle("HORMIGAS")
print(ggpH3T)
ggsave(paste(path,"BoxplotHrq2.pdf",sep="/"), ggpH3T)


