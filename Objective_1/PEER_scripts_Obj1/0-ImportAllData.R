#Import data

#WD<-("~/Desktop/IAvH/PNUD/datos/compiladoSusana")

#path="~/Desktop/IAvH/PNUD/datos/compiladoSusana"
path<-file.path("G:","My Drive","GEF_BosqueSeco","Analisis_Integrados","Matriz_Abundancia")
#setwd(WD)
#RD<-file.path(WD,"MultiTRes") #directory to store results
#dir.create(RD)
#DD<-file.path(WD,"Bases de datos_FINALES PNUD")


DataMG_PT<-read.csv(file.path(path,"AbMamG_PT.csv"),stringsAsFactors = F) #permanencia & transformación
DataMG_PT$watershed<-"Guajira"
DataMG_PT$Year<-"2016"


#DataMG_T<-read.csv(file.path(path,"AbMamG_T.csv"),stringsAsFactors = F) #compilado por transformación
#DataMG_T$watershed<-"Guajira"
#DataMG_T$Year<-"2016"

# ens1=subset(DataMG_PT,transfC=="High")
# ens2=subset(DataMG_T,transfC=="High")
# ens1=subset(DataMG_PT,transfC=="Med")
# ens2=subset(DataMG_T,transfC=="Med")
# length(unique(ens1$Species));length(unique(ens2$Species))
# sort(unique(ens1$Species));sort(unique(ens2$Species))

#hay que quitar homo sapiens
DataMG_PT=subset(DataMG_PT,!Species%in%c("Homo  sapiens","Homo  sapiens","Bos",
                                         "Rodentia sp.","Sciurus","Canis  lupus familiaris"))
DataMG_PT=DataMG_PT[,-4]

#DataMG1_T<-subset(DataMG_T,!Species%in%c("Homo  sapiens","Homo  sapiens","Bos",
#                                         "Rodentia sp.","Sciurus","Canis  lupus familiaris"))

#Bolivar
DataMB1_PT<-read.csv(file.path(path,"AbMamB1_PT.csv"),stringsAsFactors = F)
DataMB1_PT$watershed<-"Bolivar"
DataMB1_PT$Year<-"2016"
DataMB1_PT=subset(DataMB1_PT,!Species%in%c("Homo  sapiens","Homo  sapiens","Bos",
                                           "Rodentia sp.","Sciurus","Canis  lupus familiaris"))
# 
# DataMB1_T<-read.csv(file.path(path,"AbMamB1_T.csv"),stringsAsFactors = F)
# DataMB1_T$watershed<-"Bolivar"
# DataMB1_T$Year<-"2016"
# DataMB1_T=subset(DataMB1_T,!Species%in%c("Homo  sapiens","Homo  sapiens","Bos",
#                                            "Rodentia sp.","Sciurus","Canis  lupus familiaris"))


DataMB2_PT<-read.csv(file.path(path,"AbMamB2_PT.csv"),stringsAsFactors = F)
DataMB2_PT$watershed<-"Bolivar"
DataMB2_PT$Year<-"2017"

# DataM_PT=rbind(DataMB1_PT,DataMG_PT)
# DataM_PT$transfC[DataM_PT$transfC=="Med"]="Medium"

# DataMB2_T<-read.csv(file.path(path,"AbMamB2_T.csv"),stringsAsFactors = F)
# DataMB2_T$watershed<-"Bolivar"
# DataMB2_T$Year<-"2017"
# DataMB2_T=subset(DataMB2_T,!Species%in%c("Homo  sapiens","Homo  sapiens","Bos",
#                                          "Rodentia sp.","Sciurus","Canis  lupus familiaris"))

#Cesar
DataMC1_PT<-read.csv(file.path(path,"AbMamC1_PT.csv"),stringsAsFactors = F)
DataMC1_PT$Estacionf2<-gsub("^Station: ","",DataMC1_PT$Estacionf2)
DataMC1_PT$watershed<-"Cesar"
DataMC1_PT$Year<-"2016"

#Tolima
DataMT1_PT<-read.csv(file.path(path,"AbMamT1_PT.csv"),stringsAsFactors = F)
DataMT1_PT$Estacionf2<-gsub("^Station: ","",DataMT1_PT$Estacionf2)
DataMT1_PT$watershed<-"Tolima"
DataMT1_PT$Year<-"2016"

#Huila
DataMH1_PT<-read.csv(file.path(path,"AbMamH1_PT.csv"),stringsAsFactors = F)
DataMH1_PT$Estacionf2<-gsub("^Estaci?n:","",DataMH1_PT$Estacionf2)
DataMH1_PT$watershed<-"Huila"
DataMH1_PT$Year<-"2016"
names(DataMH1_PT)[4:5]<-c("Order","Family")

DataMH2_PT<-read.csv(file.path(path,"AbMamH2_PT.csv"),stringsAsFactors = F)
DataMH2_PT$Estacionf2<-gsub("^Station: ","",DataMH2_PT$Estacionf2)
DataMH2_PT$watershed<-"Huila"
DataMH2_PT$Year<-"2017"

#Valle
DataMV1_PT<-read.csv(file.path(path,"AbMamV1_PT.csv"),stringsAsFactors = F)
DataMV1_PT$Estacionf2<-gsub("^Station: ","",DataMV1_PT$Estacionf2)
DataMV1_PT$watershed<-"Valle"
DataMV1_PT$Year<-"2017"

DataMV2_PT<-read.csv(file.path(path,"AbMamV2_PT.csv"),stringsAsFactors = F)
DataMV2_PT$Estacionf2<-gsub("^Station: ","",DataMV2_PT$Estacionf2)
DataMV2_PT$watershed<-"Valle"
DataMV2_PT$Year<-"2018"


#Compile all data
dataM<-ls()[grep("^DataM.*_PT$",ls())]
for (i in dataM){
  print(i)
  print(names(get(i)))
}
selc<-c("Estacionf2","Species","Order","Family","transfC","IndNumber","watershed","Year" )

DataM1_T<-rbind(DataMG_PT[,selc],DataMB1_PT[,selc],DataMC1_PT[,selc],DataMT1_PT[,selc],
                DataMH1_PT[,selc],DataMV1_PT[,selc])
rm(list=c("DataMG_PT","DataMB1_PT","DataMB2_PT","DataMC1_PT","DataMH1_PT","DataMH2_PT",
          "DataMT1_PT","DataMT2_PT","DataMV1_PT","DataMV2_PT"))

#3c) Get data for Birds
DataA_PT<-read.csv(file.path(path,"AbAv_PT.csv"),stringsAsFactors = F)
#DataA_T<-read.csv(file.path(path,"AbAv_T.csv"),stringsAsFactors = F)
names(DataA_PT)[c(4,6,7,9,10,11)]=c("watershed","Family","Species","transfC","permC","IndNumber")
DataA_PT$watershed[which(DataA_PT$watershed=="Bol\xedvar")]<-"Bolivar"

# ens1=subset(DataA_PT,Trans_c=="High")
# ens2=subset(DataA_T,Trans_c=="High")
# #length(unique(ens1$Especie));length(unique(ens2$Especie)) #OK
# match(sort(unique(ens1$Especie)),sort(unique(ens2$Especie))) #OK

#3d) Hormigas
DataH_PT<-read.csv(file.path(path,"AbHorm_PT.csv"),stringsAsFactors = F)
DataH_PT<-DataH_PT[,c("State","Estacion","Species","AsocPlot","Number","Trans","Perm")]
names(DataH_PT)[c(1,5:7)]<-c("watershed","IndNumber","transfC","permC")
head(DataH_PT)
#names(DataH_PT)[c(2,3,8,9,10)]=c("watershed","Species","transfC","permC","IndNumber")
#DataH_PT$watershed[grep("Bo*",DataH_PT$watershed)]<-"Bolivar"
#DataH_T<-read.csv(file.path(path,"AbHorm_T.csv"),stringsAsFactors = F)

#3e) 
DataV_PT<-read.csv(file.path(path,"AbVeg_PT.csv"),stringsAsFactors = F)
#DataV_T<-read.csv(file.path(path,"AbVeg_T.csv"),stringsAsFactors = F)
DataV_PT$Species=paste(DataV_PT$genus,DataV_PT$epithet)
names(DataV_PT)[c(16:18)]=c("IndNumber","transfC","permC")
DataV_PT<-subset(DataV_PT,ramet==1 & Species!=" ")
##selV<-c(ls(),"selV") #variables not to be deleted when switching groups





