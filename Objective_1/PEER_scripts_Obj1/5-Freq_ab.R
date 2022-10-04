source(file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Scripts_R","0-ImportAllData.R"))
source(file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Scripts_R","1-calc.divAll.R"))


path="G:/My Drive/GEF_BosqueSeco/Analisis_Integrados/Plots"

fill.colmatrix=function(dataarray,class1,fill=0)
{
	l=dim(dataarray)[2]
	l2=length(class1)
	if(l<l2)
	{
		m=match(class1,dimnames(dataarray)[[2]])
		dataarray=dataarray[,m]
		dimnames(dataarray)[[2]]=class1
	}
	
	for (i in 1:l2) dataarray[,i][is.na(dataarray[,i])]=fill
		
		return(dataarray)
}

fill.1dimension=function(dataarray,class2,fill=0)
{
  if(dim(dataarray)[2]<length(class2))
  {
   m=match(class2,dimnames(dataarray)[[2]])
   dataarray=array(dataarray[,m],dim=c(1,length(class2)))   
   colnames(dataarray)=class2
  }
  
  dataarray[is.na(dataarray)]=fill
  return(dataarray)
 }



#### 1a) vegetaciÃ³n

sppV=sort(unique(DataV_PT$Species)) #468

DivV<-lapply(X=list(DataV_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
FreqV=DivV[[1]]$Freq
AbuV=DivV[[1]]$Abu
FreqV2=AbuV2=list()
#names(FreqV2)=names(AbuV2)=names(FreqV)

for (i in 1:length(FreqV))
{
	if (dim(FreqV[[i]])[1]>1) FreqV2[[i]]=fill.colmatrix(FreqV[[i]],class1=sppV,fill=0)
	else FreqV2[[i]]=fill.1dimension(FreqV[[i]],class2=sppV,fill=0)	
}
nn=rep(names(FreqV2),sapply(lapply(FreqV,dim),"[[",1))
FreqV3=do.call("rbind",FreqV2)
rownames(FreqV3)=paste(nn,rownames(FreqV3),sep="-")
SpFreqV=colSums(FreqV3)
for (i in 1:length(AbuV))
{
	if (dim(AbuV[[i]])[1]>1) AbuV2[[i]]=fill.colmatrix(AbuV[[i]],class1=sppV,fill=0)
	else AbuV2[[i]]=fill.1dimension(AbuV[[i]],class2=sppV,fill=0)	
}
AbuV3=do.call("rbind",AbuV2)
SpTotV=colSums(AbuV3)

#match(names(SpTot),names(SpFreq))
plot(SpFreqV ,SpTotV)

## 2) aves
DataA_PT$watershed[DataA_PT$watershed=="Valle del Cauca"]<-"Valle"
#todas
sppA=sort(unique(DataA_PT$Species)) #279
DivA<-lapply(X=list(DataA_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
FreqA=DivA[[1]]$Freq
AbuA=DivA[[1]]$Abu
FreqA2=AbuA2=list()
#names(FreqV2)=names(AbuV2)=names(FreqV)

for (i in 1:length(FreqA))
{
  if (dim(FreqA[[i]])[1]>1) FreqA2[[i]]=fill.colmatrix(FreqA[[i]],class1=sppA,fill=0)
  else FreqA2[[i]]=fill.1dimension(FreqA[[i]],class2=sppA,fill=0)	
}
#nn=rep(names(FreqV2),sapply(lapply(FreqV,dim),"[[",1))
FreqA3=do.call("rbind",FreqA2)
#rownames(FreqA3)=paste(nn,rownames(FreqA3),sep="-")
SpFreqA=colSums(FreqA3)
for (i in 1:length(AbuA))
{
  if (dim(AbuA[[i]])[1]>1) AbuA2[[i]]=fill.colmatrix(AbuA[[i]],class1=sppA,fill=0)
  else AbuA2[[i]]=fill.1dimension(AbuA[[i]],class2=sppA,fill=0)	
}
AbuA3=do.call("rbind",AbuA2)
SpTotA=colSums(AbuA3)

plot(SpFreqA ,SpTotA)

#Seca
DataAS_PT<-DataA_PT[DataA_PT$Estacion=="Seca",]
sppAs=sort(unique(DataAS_PT$Species)) #279
DivA<-lapply(X=list(DataAS_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
FreqA=DivA[[1]]$Freq
AbuA=DivA[[1]]$Abu
FreqA2=AbuA2=list()
#names(FreqV2)=names(AbuV2)=names(FreqV)

for (i in 1:length(FreqA))
{
	if (dim(FreqA[[i]])[1]>1) FreqA2[[i]]=fill.colmatrix(FreqA[[i]],class1=sppA,fill=0)
	else FreqA2[[i]]=fill.1dimension(FreqA[[i]],class2=sppA,fill=0)	
}
#nn=rep(names(FreqV2),sapply(lapply(FreqV,dim),"[[",1))
FreqA3=do.call("rbind",FreqA2)
#rownames(FreqA3)=paste(nn,rownames(FreqA3),sep="-")
SpFreqAs=colSums(FreqA3)
for (i in 1:length(AbuA))
{
	if (dim(AbuA[[i]])[1]>1) AbuA2[[i]]=fill.colmatrix(AbuA[[i]],class1=sppA,fill=0)
	else AbuA2[[i]]=fill.1dimension(AbuA[[i]],class2=sppA,fill=0)	
}
AbuA3=do.call("rbind",AbuA2)
SpTotAs=colSums(AbuA3)

plot(SpFreqAs ,SpTotAs)
#match(names(SpTotA),names(SpFreqA))
#Lluvias
DataAL_PT<-DataA_PT[DataA_PT$Estacion=="Lluviosa",]
sppAl=sort(unique(DataAL_PT$Species)) #279
DivA<-lapply(X=list(DataAL_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
FreqA=DivA[[1]]$Freq
AbuA=DivA[[1]]$Abu
FreqA2=AbuA2=list()
#names(FreqV2)=names(AbuV2)=names(FreqV)

for (i in 1:length(FreqA))
{
  if (dim(FreqA[[i]])[1]>1) FreqA2[[i]]=fill.colmatrix(FreqA[[i]],class1=sppA,fill=0)
  else FreqA2[[i]]=fill.1dimension(FreqA[[i]],class2=sppA,fill=0)	
}
#nn=rep(names(FreqV2),sapply(lapply(FreqV,dim),"[[",1))
FreqA3=do.call("rbind",FreqA2)
#rownames(FreqA3)=paste(nn,rownames(FreqA3),sep="-")
SpFreqAl=colSums(FreqA3)

for (i in 1:length(AbuA))
{
  if (dim(AbuA[[i]])[1]>1) AbuA2[[i]]=fill.colmatrix(AbuA[[i]],class1=sppA,fill=0)
  else AbuA2[[i]]=fill.1dimension(AbuA[[i]],class2=sppA,fill=0)	
}
AbuA3=do.call("rbind",AbuA2)
SpTotAl=colSums(AbuA3)

plot(SpFreqAl ,SpTotAl)


## 3) hormigas

DataH_PT$Estacion[DataH_PT$Estacion=="recenso"]<-"Recenso"
DataH_PT$transfC[DataH_PT$transfC=="Med"]<-"Medium"

DataHc<-list(subset(DataH_PT,Estacion=="Censo"))
DataHh<-list(subset(DataH_PT,Estacion=="Recenso"))
#Todas
sppH=sort(DataH_PT$Species) #175
DivH<-lapply(X=list(DataH_PT), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
FreqH=DivH[[1]]$Freq
AbuH=DivH[[1]]$Abu
FreqH2=AbuH2=list()
#names(FreqV2)=names(AbuV2)=names(FreqV)

for (i in 1:length(FreqH))
{
  if (dim(FreqH[[i]])[1]>1) FreqH2[[i]]=fill.colmatrix(FreqH[[i]],class1=sppH,fill=0)
  else FreqH2[[i]]=fill.1dimension(FreqH[[i]],class2=sppH,fill=0)	
}
#nn=rep(names(FreqV2),sapply(lapply(FreqV,dim),"[[",1))
FreqH3=do.call("rbind",FreqH2)
#rownames(FreqH3)=paste(nn,rownames(FreqH3),sep="-")
SpFreqH=colSums(FreqH3)

for (i in 1:length(AbuH))
{
  if (dim(AbuH[[i]])[1]>1) AbuH2[[i]]=fill.colmatrix(AbuH[[i]],class1=sppH,fill=0)
  else AbuH2[[i]]=fill.1dimension(AbuH[[i]],class2=sppH,fill=0)	
}
AbuH3=do.call("rbind",AbuH2)
SpTotH=colSums(AbuH3)
plot(SpFreqH ,SpTotH)
#Censo

sppHc=sort(unique(DataHc[[1]]$Species)) #175
DivH<-lapply(X=DataHc, FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
FreqH=DivH[[1]]$Freq
AbuH=DivH[[1]]$Abu
FreqH2=AbuH2=list()
#names(FreqV2)=names(AbuV2)=names(FreqV)

for (i in 1:length(FreqH))
{
	if (dim(FreqH[[i]])[1]>1) FreqH2[[i]]=fill.colmatrix(FreqH[[i]],class1=sppH,fill=0)
	else FreqH2[[i]]=fill.1dimension(FreqH[[i]],class2=sppH,fill=0)	
}
#nn=rep(names(FreqV2),sapply(lapply(FreqV,dim),"[[",1))
FreqH3=do.call("rbind",FreqH2)
#rownames(FreqH3)=paste(nn,rownames(FreqH3),sep="-")
SpFreqHc=colSums(FreqH3)

for (i in 1:length(AbuH))
{
	if (dim(AbuH[[i]])[1]>1) AbuH2[[i]]=fill.colmatrix(AbuH[[i]],class1=sppH,fill=0)
	else AbuH2[[i]]=fill.1dimension(AbuH[[i]],class2=sppH,fill=0)	
}
AbuH3=do.call("rbind",AbuH2)
SpTotHc=colSums(AbuH3)

plot(SpFreqHc ,SpTotHc)
#match(names(SpTotA),names(SpFreqA))

#Recensos
sppHr=sort(unique(DataHh[[1]]$Species)) #154
DivH<-lapply(X=DataHh, FUN= calc.div, wcl="watershed", tcl="transfC",pcl="permC",abcl="IndNumber",spcl="Species")
FreqH=DivH[[1]]$Freq
AbuH=DivH[[1]]$Abu
FreqH2=AbuH2=list()
#names(FreqV2)=names(AbuV2)=names(FreqV)

for (i in 1:length(FreqH))
{
  if (dim(FreqH[[i]])[1]>1) FreqH2[[i]]=fill.colmatrix(FreqH[[i]],class1=sppH,fill=0)
  else FreqH2[[i]]=fill.1dimension(FreqH[[i]],class2=sppH,fill=0)	
}
#nn=rep(names(FreqV2),sapply(lapply(FreqV,dim),"[[",1))
FreqH3=do.call("rbind",FreqH2)
#rownames(FreqH3)=paste(nn,rownames(FreqH3),sep="-")
SpFreqHr=colSums(FreqH3)

for (i in 1:length(AbuH))
{
  if (dim(AbuH[[i]])[1]>1) AbuH2[[i]]=fill.colmatrix(AbuH[[i]],class1=sppH,fill=0)
  else AbuH2[[i]]=fill.1dimension(AbuH[[i]],class2=sppH,fill=0)	
}
AbuH3=do.call("rbind",AbuH2)
SpTotHr=colSums(AbuH3)


plot(SpFreqHr ,SpTotHr)
## 4) vertebrados
DataM1_T$transfC[DataM1_T$transfC=="Alta"]="High"
DataM1_T$transfC[DataM1_T$transfC=="Baja"]="Low"
DataM1_T$transfC[DataM1_T$transfC=="Med"]="Medium"
DataM1_T$transfC[DataM1_T$transfC=="Media"]="Medium"
sppM=sort(unique(DataM1_T$Species)) #35

DivM<-lapply(X=list(DataM1_T), FUN= calc.div, wcl="watershed", tcl="transfC",pcl="transfC",abcl="IndNumber",spcl="Species")
FreqM=DivM[[1]]$Freq
AbuM=DivM[[1]]$Abu
FreqM2=AbuM2=list()
#names(FreqV2)=names(AbuV2)=names(FreqV)

for (i in 1:length(FreqM))
{
	if (dim(FreqM[[i]])[1]>1) FreqM2[[i]]=fill.colmatrix(FreqM[[i]],class1=sppM,fill=0)
	else FreqM2[[i]]=fill.1dimension(FreqM[[i]],class2=sppM,fill=0)	
}
#nn=rep(names(FreqV2),sapply(lapply(FreqV,dim),"[[",1))
FreqM3=do.call("rbind",FreqM2)
#rownames(FreqH3)=paste(nn,rownames(FreqH3),sep="-")
SpFreqM=colSums(FreqM3)

for (i in 1:length(AbuM))
{
	if (dim(AbuM[[i]])[1]>1) AbuM2[[i]]=fill.colmatrix(AbuM[[i]],class1=sppM,fill=0)
	else AbuM2[[i]]=fill.1dimension(AbuM[[i]],class2=sppM,fill=0)	
}
AbuM3=do.call("rbind",AbuM2)
SpTotM=colSums(AbuM3)


plot(SpFreqM ,SpTotM)
#match(names(SpTotM),names(SpFreqM))

dd=data.frame(freq=c(SpFreqV, SpFreqA,SpFreqAs ,SpFreqAl,SpFreqH, SpFreqHc,SpFreqHr, SpFreqM),abu=c(SpTotV, SpTotA,SpTotAs,SpTotAl,SpTotH,SpTotHc,SpTotHr, SpTotM),organism=c(rep("Plantas",length(SpTotV)),rep("Aves",length(SpTotA)),rep("Aves_s",length(SpTotAs)),rep("Aves_l",length(SpTotAl)),rep("Hormigas",length(SpTotH)),rep("Hormigas_c",length(SpTotHc)),rep("Hormigas_r",length(SpTotHr)),rep("Vertebrados",length(SpTotM))))

yy<-ggplot()+
  geom_point(mapping=aes(x=log(freq+1),y=log(abu+1)),data=dd,size=1,col=rgb(80, 80, 80, max = 255, alpha = 140))+
  geom_smooth(
    mapping = aes(x=log(freq+1),y=log(abu+1)),
    data = dd,
    stat = "smooth",
    method = "lm"
  )+
  facet_wrap(~organism)+ theme_bw()+
  theme(axis.text.x = element_text(size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+
  labs(x="log(Frecuencias)",y="log(Abundancia)")
print(yy)
ggsave(paste(path,"LogFeqAbu.pdf",sep="/"),yy,scale=2)

xx=ggplot(aes(x=freq,y=abu),data=dd)+geom_point(col=rgb(80, 80, 80, max = 255, alpha = 140))+
  geom_smooth(method="loess")+
  facet_wrap(~organism,scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(size=13),axis.text.y = element_text(size=12),strip.text.x=element_text(size=14,face="bold"),strip.background=element_rect(colour="lightsalmon",fill="lightsalmon"),axis.title=element_text(size=14),plot.title=element_text(hjust=0.5))+
  xlab("Frecuencia") + ylab("Número total de individuos")
print(xx)
ggsave(paste(path,"FeqAbu.pdf",sep="/"),xx,scale=2)
############	FIGURA

