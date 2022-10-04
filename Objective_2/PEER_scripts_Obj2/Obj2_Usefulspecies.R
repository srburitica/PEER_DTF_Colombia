#Date.created. April.13.2021
#Date.Modified. April.13.2021

#1) Specify working directories
WD<-file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','Objetivo_2_HistoriaUso')
WDIn<-file.path(WD,'Info_usos')
setwd(WDIn)

#2) Packages, libraries and functions
library(readxl)
library(ggplot2)

#3) read info of species uses

uses<-read_xlsx('modeledSpFromR. Usos.xlsx',sheet=2)
Modsp<-read_xlsx('modeledSpFromR. Usos.xlsx',sheet=1)

#4) graph uses
smUses<-colSums(uses[,-1],na.rm=T)
names(smUses)<-colnames(uses[-1])
noUses<-sum(!Modsp$Var1%in%uses$`Etiquetas de fila`)
smUses[12]<-noUses
names(smUses)[12]<-'Without reg.Uses'
smUses<-data.frame('Uses'=smUses,'type'=names(smUses))
rownames(smUses)<-NULL
smUses$perc<-round(c(smUses$Uses[-12]/sum(smUses$Uses[-12]),smUses$Uses[12]/nrow(uses)),3)

cxc <- ggplot(smUses, aes(x=0, y=Uses,fill=type))+geom_bar(stat="identity")+
                geom_text(aes(label=perc), position=position_stack(vjust=0.5),size=3)+
                scale_x_continuous(expand = c(0,0)) +
                labs(fill = 'Type', x = NULL, y = NULL, title = 'Uses', subtitle = 'in proportion') +
                coord_polar(theta = "y") +
                theme_minimal()
ggsave(file=file.path(WDIn,"PieChartUses.jpeg"), cxc, device="jpeg",width=10, height=8)
geom_text(aes(y = pos, label = paste(dis,"Km",sep="")), color= "black",size=2)+