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
library(gridExtra)
library(cowplot)
library(reshape2)
library(grid)
library(lme4)
library(buildmer)
library(ggeffects)
library(broom)
library(broom.mixed)
library(margins)
library(nlme)
devtools::install_dev("parsnip")

#function
get.call<-function(ddata,yy,dd,hh=NULL){
  vn<-ddata%>%select(!all_of(c(yy,dd)))%>%names(.)
  vnn<-ddata%>%select(!all_of(c(yy,dd,hh)))%>%names(.)
  as.formula(paste(yy,'~',paste(vn,collapse='*'),
  '+(',paste(vnn,collapse='*'),'|dep)'))  
}
#Datos invest
WD<-file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto', 
              'Objetivo_3_Valoraciones','Res_SEModellingInvest','invest_waterYield')
dataI<-read.csv(file.path(WD,'regresiones.csv'),
                header=T,sep=";")
WD<-file.path(WD,'Analyses_SE')
dir.create(file.path(WD,'MixedModels'))
WD<-file.path(WD,'MixedModels')
dataI$Dp_WS<-trimws(paste(dataI$dep,dataI$ws,sep='_'))
names(dataI)<-c("PorcBosq","AreaHa","pendiente",
                  "TempPrAn","PrecPrAn","indice","PendRwy",
                  "altitud","wyield","ws","dep","serie","region","Dp_WS")
dataI<-dataI[dataI$serie!="2013",]

dataI$PendRwy<-as.numeric(dataI$PendRwy)
#check series
rowSums(with(dataI,table(Dp_WS,serie)))

dataI$indice<-dataI$PrecPrAn/dataI$TempPrAn
dataI$Pr_Ha<-dataI$PrecPrAn/dataI$AreaHa
dataI$ScPrec<-gsub('(.*)(medio|humedo|seco)','\\2',dataI$serie)
dataI$ScCul<-gsub('(reforestar|pasto|cereal|mosaico)(.*)','\\1',dataI$serie)
dataI$ScCul<-gsub('medio|humedo|seco','actual',dataI$ScCul)
unique(dataI$ScCul)

#exploratory
selc<-c('wyield','PorcBosq','PrecPrAn','AreaHa','Pr_Ha','altitud','pendiente','indice','TempPrAn')
M<-cor(dataI[,selc])
corrplot(M,order = 'hclust')

for(x in selc ){
  g<-ggplot(aes(x=get(x)),data=dataI[dataI$ScCul=='actual'&dataI$ScPrec=='medio',])+geom_histogram()+facet_wrap(~dep)+
           ggtitle(x)
  print(g)  
  readline(prompt='ENTER')
  g<-ggplot(aes(x=dep,y=get(x)),data=dataI)+geom_boxplot()+
    ylab(x)+facet_wrap(~ScCul+ScPrec,ncol=3)
  print(g)
  readline(prompt='ENTER')
  for(y in unique(dataI$dep)){
  g<-ggplot(aes(x=get(x),y=wyield),data=dataI[dataI$dep==y,])+geom_point(alpha=0.5)+
    stat_smooth(method="lm", se=TRUE) +
    ylab("Wyield")+xlab(x)+ggtitle(y)+facet_wrap(~ScCul+ScPrec,ncol=3)
  print(g)
  readline(prompt='ENTER')
  }
}

#Transformations
for(i in selc){
  print(i)
  print(summary(dataI[,i]))
  readline(prompt='ENTER')
  }
dataI$lgWY<-log10(dataI$wyield)
dataI$aSBosq<-asin(round(dataI$PorcBosq,0)/100)
dataI$lgHa<-log10(dataI$AreaHa)
dataI$lgAlt<-log10(dataI$altitud)
dataI$lgPend<-log10(dataI$pendiente)
dataI$dep<-as.factor(dataI$dep)
dataI$lgInd<-log(dataI$indice)

#round 1: 
selc<-c('lgWY','aSBosq','lgHa','lgAlt','lgPend')
#round 2: 
selc<-c('lgWY','aSBosq','lgHa','lgAlt','lgInd')
#round 3:
selc<-c('lgWY','aSBosq','lgHa','lgPend','lgInd')
for(x in selc){
  # g<-ggplot(aes(x=get(x)),data=dataI[dataI$ScCul=='actual'&dataI$ScPrec=='medio',])+
  #   geom_histogram()+facet_wrap(~dep,scale="free")+
  #   ggtitle(x)
  # print(g)  
  # readline(prompt='ENTER')
  # g<-ggplot(aes(x=dep,y=get(x)),data=dataI)+geom_boxplot()+
  #   ylab(x)+facet_wrap(ScCul~ScPrec,ncol=3,scale="free")
  # print(g)
  # readline(prompt='ENTER')
  # for(y in unique(dataI$dep)){
  #   g<-ggplot(aes(x=get(x),y=lgWY),data=dataI[dataI$dep==y,])+geom_point(alpha=0.5)+
  #     stat_smooth(method="lm", se=TRUE) +
  #     ylab("LgWyield")+xlab(x)+ggtitle(y)+facet_wrap(ScCul~ScPrec,ncol=3,scale="free")
  #   print(g)
  #   readline(prompt='ENTER')
  # }
  for(t in unique(dataI$ScCul)){
  g<-ggplot(aes(x=get(x),y=lgWY,col=dep),data=dataI[dataI$ScCul==t,])+geom_point(alpha=0.5)+
    geom_smooth(method='lm')+xlab(x)+
    ylab('lgWY')+ggtitle(t)+facet_wrap(~ScPrec,ncol=3)
  print(g)
  readline(prompt='ENTER')
  }
}

#analisis de provisión
dir.create(file.path(WD,'FittedGraphs'))
dir.create(file.path(WD,'FitCoeffs'))

with(dataI,table(dep,serie))

dataIs<-dataI[dataI$dep!='ces',]
dataIs$dep<-as.factor(as.character(dataIs$dep))
#check series
rowSums(with(dataI,table(Dp_WS,ScCul)))

# Genearal models with random factors
#models by serie, 

mod<-list()
outf<-file.path(WD,'ModelsRound_MLEM_3.txt') #round 2 with index but without slope
sink(outf,split=TRUE)
for(i in unique(dataIs$ScCul)){
  mcul<-list()
  for(j in unique(dataIs$ScPrec)){
    print(paste('starting with',i,'y',j,sep=" "))
    datam<-dataIs[dataIs$ScCul==i&dataIs$ScPrec==j,selc]
    selcc<-c(selc[colSums(datam)>0],'dep')
    datam<-dataIs[dataIs$ScCul==i&dataIs$ScPrec==j,selcc]
    f<-get.call(datam,'lgWY','dep','lgHa')
    M<-lmer(f,data=datam)
    # print(summary(M))
    # readline(prompt='ENTER')
    Mo<-buildlme(f,data=datam,
                 buildmerControl = buildmerControl(direction="order"))
    print(summary(Mo))
    print(plot(Mo))
    # readline(prompt='ENTER')
    print(plot(Mo,lgWY~fitted(.)))
    # readline(prompt='ENTER')
    print(qqnorm(Mo,~resid(.)|dep))
    mcul[[j]]<-Mo
  }
  mod[[i]]<-mcul
}
sink()
#mod1<-mod #mod1 is from first round, without indice but with slope
#mod2<-mod
#mod3<-mod



#Generate estimated plots 
grnm<-'Fit_All_3_rndm'
plotsM<-map(names(mod), function(xx){
  x<-names(mod[[xx]])
  map(x, function(yy){
    y<-mod[[xx]][[yy]]
    datam<-y@model$data
    vars<-datam%>%dplyr::select(where(is.numeric))%>%dplyr::select(-lgWY)%>%names(.)
    plotsl<-map(vars,function(z){
      flnm<-paste(xx,yy,z,sep='_')
      predm<-ggpredict(y@model,terms=c(z,'dep'),ci.lvl=0.95, type='random')
      g<-ggplot(aes(x=x,y=predicted),data=predm)+
        geom_line() +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
        # geom_point(aes(x=get(z),y=lgWY),col="black",alpha=0.5,data=datam)+
        xlab(z)+ facet_wrap(~group,nrow=2, scales="free")+theme_classic()+
        theme(legend.position="none")+labs(title = paste("Fitted lines for",flnm,sep=" "))
      # print(g)
      # readline(prompt='ENTER')
      ggsave(file.path(WD,'FittedGraphs',paste(grnm,' for_',flnm,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)
      return(g)
    })
  })
})
#plotsM_G_1<-plotsM
#plotsM_G_2<-plotsM
#plotsM_G_3<-plotsM


#plot coefficients
CoeffD<-map(names(mod), function(xx){
  print(xx)
  x<-names(mod[[xx]])
  plotsl3<-map(x, function(yy){
    print(yy)
    y<-mod[[xx]][[yy]]
    datam<-y@model$data
    callm<-y@model$coefficients$random$dep%>%colnames(.)
    vars<-datam%>%dplyr::select(where(is.numeric))%>%dplyr::select(-lgWY)%>%names(.)
    plotsl<-map(vars,function(z){
      print(z)
      if(z%in%callm){
        predm<-ggpredict(y@model,terms=c(z,'dep'),ci.lvl=0.95, type='random')
        fitLM<-predm%>% nest_by(group) %>%
          mutate(fitWY = list(lm(predicted ~ x, data = data)))%>%
          summarise(tidy(fitWY))%>%mutate(var=z,Cul=xx,Prec=yy)
      }else{
        predm<-ggpredict(y@model,terms=z,ci.lvl=0.95)
        fitLM<-predm%>%lm(predicted ~ x, data = .)%>%
         tidy(.)%>%mutate(var=z,Cul=xx,Prec=yy)%>%mutate(group='All')
      }
      return(fitLM)
    })
    plotsl2<-do.call(rbind,plotsl)
  })
  plotsl4<-do.call(rbind,plotsl3)
})
Coeffs<-do.call(rbind,CoeffD)
rownames(Coeffs)<-NULL
Coeffs$Prec<-factor(Coeffs$Prec,levels=c('seco','medio','humedo'))
Coeffs$Cul<-factor(Coeffs$Cul,levels=c('pasto','cereal','mosaico','actual','reforestar'))
Coeffs$col<-Coeffs$estimate<0
Coeffs<-Coeffs%>%filter(term=='x')
#Coeffs1<-Coeffs
#Coeffs2<-Coeffs
#Coeffs3<-Coeffs

#Plot by Cultivar
grnm<-'Coef3_rnm'
g<-ggplot(aes(x=Prec,y=estimate,col=group),data=Coeffs)+geom_point(size=3,alpha=0.5)+
  #geom_linerange(aes(ymin=coefm,ymax=coefx),size=1,inherit.aes = TRUE)+
  geom_hline(yintercept=0,linetype='dashed',col='blue')+
  #scale_color_manual(values = c("darkblue", "darkred"),guide="none")+
  facet_grid(Cul~var,scales="free")+theme_bw()
print(g)
ggsave(file.path(WD,'FitCoeffs',paste(grnm,j,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)

#plot by variable

grnm<-'Coef3_rnm2'
g<-ggplot(aes(x=Cul,y=estimate,col=group),data=Coeffs)+geom_point(size=3,alpha=0.5)+
  #geom_linerange(aes(ymin=coefm,ymax=coefx),size=1,inherit.aes = TRUE)+
  geom_hline(yintercept=0,linetype='dashed',col='blue')+
  #scale_color_manual(values = c("darkblue", "darkred"),guide="none")+
  facet_grid(var~Prec,scales="free")+theme_bw()
print(g)
ggsave(file.path(WD,'FitCoeffs',paste(grnm,j,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)



rm(list=c( "CoeffD","Coeffs","Coeffss","datam","g",
           "M","mcul","Mo","mod","plotsM","f","grnm","i","j",
           "outf","selc","selcc","t","x",'y'))



#3) análisis with regulación
dir.create(WD,'RegWY')
WD<-file.path(WD,'RegWY')
dir.create(file.path(WD,'FittedGraphs'))
dir.create(file.path(WD,'FitCoeffs'))

dataR<-aggregate(wyield~Dp_WS+ScCul, data=dataIs,FUN=function(x){max(x)-min(x)})
rownames(dataR)<-NULL
dataR$ID<-paste(dataR$Dp_WS,dataR$ScCul,sep='')
dataRR<-unique(dataIs[dataIs$ScPrec=='medio',c('dep','ScCul',"Dp_WS","Pr_Ha","aSBosq","lgHa","lgAlt","lgPend","lgInd")])
rownames(dataRR)<-NULL
dataRR$ID<-paste(dataRR$Dp_WS,dataRR$ScCul,sep='')
mtch<-match(dataR$ID,dataRR$ID)
dataR<-cbind(dataR,dataRR[mtch,])
dataR<-dataR[,c("Dp_WS","ScCul","wyield","dep","Pr_Ha","aSBosq",
                "lgHa","lgAlt","lgPend","lgInd")]
hist(log(dataR$wyield))
names(dataR)[3]<-'RngWR'
dataR$lgRwy<-log(dataR$RngWR)


selc1<-c('lgRwy','aSBosq','lgHa','lgPend','lgInd','lgAlt')

for(x in selc1){
  # g<-ggplot(aes(x=get(x)),data=dataR[dataR$ScCul=='actual',])+geom_histogram()+facet_wrap(~dep)+
  #   ggtitle(x)
  # print(g)  
  # #readline(prompt='ENTER')
  # g<-ggplot(aes(x=dep,y=get(x)),data=dataR)+geom_boxplot()+
  #   ylab(x)+facet_wrap(~ScCul,ncol=3)
  # print(g)
  #readline(prompt='ENTER')
  
    g<-ggplot(aes(x=get(x),y=lgRwy,col=dep),data=dataR)+geom_point(alpha=0.5)+
      stat_smooth(method="lm", se=TRUE) +
      ylab("LgRngWyield")+xlab(x)+ggtitle(y)+facet_wrap(~ScCul,ncol=3)
    print(g)
    readline(prompt='ENTER')
  
}


#round1
selc<-c('lgRwy','aSBosq','lgHa','lgPend','lgAlt')
#round2
selc<-c('lgRwy','aSBosq','lgHa','lgAlt','lgInd')
#round3
selc<-c('lgRwy','aSBosq','lgHa','lgPend','lgInd')

mod<-list()
outf<-file.path(WD,'ModelsRound2__MLEM_R.txt') 
sink(outf,split=TRUE)
for(i in unique(dataR$ScCul)){
  print(paste('starting with',i,sep=" "))
  datam<-dataR[dataR$ScCul==i,selc]
  selcc<-c(selc[colSums(datam)>0],'dep')
  datam<-dataR[dataR$ScCul==i,selcc]
  f<-get.call(datam,'lgRwy','dep')
  M<-lmer(f,data=datam)
  #print(summary(M))
  #readline(prompt='ENTER')
  Mo<-buildlme(f,data=datam,
               buildmerControl = buildmerControl(direction="order"))
  print(summary(Mo))
  print(plot(Mo))
  readline(prompt='ENTER')
  print(plot(Mo,lgRwy~fitted(.)))
  readline(prompt='ENTER')
  print(qqnorm(Mo,~resid(.)|dep))
  mod[[i]]<-Mo
}
sink()
#modR1<-mod #mod1 is from first round, without indice but with slope
#modR2<-mod
#modR3<-mod

#Generate estimated plots
grnm<-'Fit_G_R3_rndm'
plotsM<-map(names(mod), function(xx){
  y<-mod[[xx]]
  datam<-y@model$data
  vars<-datam%>%dplyr::select(where(is.numeric))%>%dplyr::select(-lgRwy)%>%names(.)
  plotsl<-map(vars,function(z){
    flnm<-paste(xx,z,sep='_')
    predm<-ggpredict(y@model,terms=c(z,'dep'),ci.lvl=0.95, type='random')
    g<-ggplot(aes(x=x,y=predicted),data=predm)+
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high,alpha=0.1), fill = "lightgray")+
      geom_line()+
      # geom_point(aes(x=get(z),y=lgRwy),col="black",alpha=0.5,data=datam)+
      xlab(z)+ facet_wrap(~group,nrow=2, scales="free")+theme_classic()+
      theme(legend.position="none")+labs(title = paste("Fitted lines for",flnm,sep=" "))
    print(g)
    #readline(prompt='ENTER')
    ggsave(file.path(WD,'FittedGraphs',paste(grnm,' for_',flnm,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)
    return(g)
  })
})
#plotsMR1<-plotsM
#plotsMR2<-plotsM
#plotsMR3<-plotsM


#plot coefficients
CoeffD<-map(names(mod), function(xx){
  print(xx)
    y<-mod[[xx]]
    datam<-y@model$data
    callm<-y@model$coefficients$random$dep%>%colnames(.)
    vars<-datam%>%dplyr::select(where(is.numeric))%>%dplyr::select(-lgRwy)%>%names(.)
    plotsl<-map(vars,function(z){
      print(z)
      if(z%in%callm){
        predm<-ggpredict(y@model,terms=c(z,'dep'),ci.lvl=0.95, type='random')
        fitLM<-predm%>% nest_by(group) %>%
          mutate(fitWY = list(lm(predicted ~ x, data = data)))%>%
          summarise(tidy(fitWY))%>%mutate(var=z,Cul=xx)
      }else{
        predm<-ggpredict(y@model,terms=z,ci.lvl=0.95)
        fitLM<-predm%>%lm(predicted ~ x, data = .)%>%
          tidy(.)%>%mutate(var=z,Cul=xx)%>%mutate(group='All')
      }
      return(fitLM)
    })
    plotsl2<-do.call(rbind,plotsl)
})
Coeffs<-do.call(rbind,CoeffD)
rownames(Coeffs)<-NULL
Coeffs$Cul<-factor(Coeffs$Cul,levels=c('pasto','cereal','mosaico','actual','reforestar'))
Coeffs$col<-Coeffs$estimate<0
Coeffs<-Coeffs%>%filter(term=='x')
#CoeffR1<-Coeffs
#CoeffR2<-Coeffs
#CoeffR3<-Coeffs

#Plot by Cultivar
grnm<-'Coef3_R_rnm1'
g<-ggplot(aes(x=Cul,y=estimate,col=group),data=Coeffs)+
  geom_point(size=3,alpha=0.5)+
  geom_hline(yintercept=0,linetype='dashed',col='blue')+
  facet_grid(~var,scales="free")+theme_bw()
print(g)
ggsave(file.path(WD,'FitCoeffs',paste(grnm,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)

grnm<-'Coef3_R_rnm2'
g<-ggplot(aes(x=Cul,y=estimate,col=var),data=Coeffs)+
  geom_point(size=3,alpha=0.5)+
  geom_hline(yintercept=0,linetype='dashed',col='blue')+
  facet_grid(~group,scales="free")+theme_bw()
print(g)
ggsave(file.path(WD,'FitCoeffs',paste(grnm,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)


rm(list=c( "CoeffD","Coeffs","Coeffss","datam","g",
           "M","mcul","Mo","mod","plotsM","f","grnm","i","j",
           "outf","selc","selcc","t","x",'y','predm','fitLM'))


#4) analysis de regulación con pendiente
WD<-file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto', 
              'Objetivo_3_Valoraciones','Res_SEModellingInvest',
              'invest_waterYield','Analyses_SE','MixedModels')

dir.create(file.path(WD,'RegWY_P'))
WD<-file.path(WD,'RegWY_P')
dir.create(file.path(WD,'FittedGraphs'))
dir.create(file.path(WD,'FitCoeffs'))

#get data
dataRP<-aggregate(PendRwy~Dp_WS, data=dataIs,FUN=function(x){unique(x)})
rownames(dataRP)<-NULL
dataRR<-unique(dataIs[dataIs$ScPrec=='medio'&dataIs$ScCul=='actual',c('dep','ScCul',"Dp_WS","Pr_Ha","aSBosq","lgHa","lgAlt","lgPend","lgInd")])
rownames(dataRR)<-NULL

mtch<-match(dataRP$Dp_WS,dataRR$Dp_WS)
dataRP<-cbind(dataRP,dataRR[mtch,])
dataRP<-dataRP[,c("Dp_WS","PendRwy","dep","Pr_Ha","aSBosq",
                 "lgHa" ,"lgAlt","lgPend","lgInd")]
rownames(dataRP)<-NULL
hist(dataRP$PendRwy)


selc1<-c('PendRwy','aSBosq','lgHa','lgPend','lgInd','lgAlt')
for(x in selc1){
  # g<-ggplot(aes(x=get(x)),data=dataRP)+geom_histogram()+facet_wrap(~dep)+
  #   ggtitle(x)
  # print(g)  
  # readline(prompt='ENTER')
  # g<-ggplot(aes(x=dep,y=get(x)),data=dataRP)+geom_boxplot()+
  #   ylab(x)
  # print(g)
  # readline(prompt='ENTER')
  g<-ggplot(aes(x=get(x),y=PendRwy,col=dep),data=dataRP)+geom_point(alpha=0.5)+
    stat_smooth(method="lm", se=TRUE) +
    ylab("Slope Regulation")+xlab(x)
  print(g)
  readline(prompt='ENTER')
}
g<-ggplot(aes(x=dep,y=PendRwy),data=dataRP)+geom_boxplot()+
  ylab('Slope of range water yield per scenario')+xlab('')+theme_classic()
print(g)
ggsave(file.path(WD,paste('boxplot_pendWY','.jpeg',sep='')),g,device='jpeg',width=12,height=8)
m<-lm(PendRwy~dep,data=dataRP)
summary(m)
plot(m)


for(y in unique(dataRP$dep)){
  print(y)
  data.s<-dataRP[dataRP$dep==y,selc1]
  data.s<-data.s[,colSums(data.s)!=0]
  M<-melt(cor(data.s))
  gg1<-ggplot(data = M, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = 0.5, mid ="grey70", 
                         limits = c(-1, +1)) +
    labs(title = y, 
         x = "", y = "", fill = "Correlation \n Measure") +
    theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
          axis.title.x = element_text(face="bold", colour="darkgreen", size = 6),
          axis.title.y = element_text(face="bold", colour="darkgreen", size = 6),
          legend.title = element_text(face="bold", colour="brown", size = 5)) +
    geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
              fontface = "bold", size = 3)
  print(gg1)
  ggsave(file.path(WD,'CorrMatrix',paste('corr1',y,'.jpeg',sep='')),gg1,device='jpeg',width=12,height=8)
  readline(prompt='ENTER')
}

## Continue here###
#round1_r
selc<-c('PendRwy','aSBosq','lgHa','lgInd')
#round2_r
selc<-c('PendRwy','aSBosq','lgAlt','lgInd')
#round3_r
selc<-c('PendRwy','aSBosq','lgPend','lgInd')

outf<-file.path(WD,'ModelsRound3_RS.txt') #round 2 with indice but without slope
sink(outf,split=TRUE)
print('##### runnin models with the following variables #####')
print(selc)
datam<-dataRP[,selc]
selcc<-c(selc[colSums(datam)!=0],'dep')
datam<-dataRP[,selcc]
f<-get.call(datam,'PendRwy','dep')
M<-lmer(f,data=datam)
print(summary(M))
Mo<-buildlme(f,data=datam,
             buildmerControl = buildmerControl(direction="order"))
print(summary(Mo))
print(plot(Mo))
print(plot(Mo,PendRwy~fitted(.)))
print(qqnorm(Mo,~resid(.)|dep))
sink()
#modRS1<-Mo 
#modRS2<-Mo
#modRS3<-Mo

#Generate estimated plots
grnm<-'fitted_RS3_rndm'
datam<-Mo@model$data
vars<-datam%>%dplyr::select(where(is.numeric))%>%dplyr::select(-PendRwy)%>%names(.)
plotsl<-map(vars,function(z){
  flnm<-z
  predm<-ggpredict(Mo@model,terms=c(z,'dep'),ci.lvl=0.95, type='random')
  g<-ggplot(aes(x=x,y=predicted),data=predm)+
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high,alpha=0.5), fill = "lightgray")+
    geom_line(aes(x=x,y=predicted),col="black")+
    #geom_point(aes(x=get(z),y=PendRwy),col="black",alpha=0.5,data=datamm)+
    xlab(z)+ facet_wrap(~group,nrow=2, scales="free")+theme_classic()+
    theme(legend.position="none")+labs(title = paste("Fitted lines for",flnm,sep=" "))
  print(g)
  readline(prompt='ENTER')
  ggsave(file.path(WD,'FittedGraphs',paste(grnm,' for_',flnm,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)
  return(g)
})
  
#plotsMRS1<-plotsl
#plotsMRS2<-plotsl
#plotsMRS3<-plotsl


datam<-Mo@model$data
callm<-Mo@model$coefficients$random$dep%>%colnames(.)
vars<-datam%>%dplyr::select(where(is.numeric))%>%dplyr::select(-PendRwy)%>%names(.)
plotsl<-map(vars,function(z){
  print(z)
  if(z%in%callm){
    predm<-ggpredict(Mo@model,terms=c(z,'dep'),ci.lvl=0.95, type='random')
    fitLM<-predm%>% nest_by(group) %>%
      mutate(fitWY = list(lm(predicted ~ x, data = data)))%>%
      summarise(tidy(fitWY))%>%mutate(var=z)
  }else{
    predm<-ggpredict(Mo@model,terms=z,ci.lvl=0.95)
    fitLM<-predm%>%lm(predicted ~ x, data = .)%>%
      tidy(.)%>%mutate(var=z)%>%mutate(group='All')
  }
  return(fitLM)
})

Coeffs<-do.call(rbind,plotsl)
rownames(Coeffs)<-NULL
Coeffs$col<-Coeffs$estimate<0
Coeffs<-Coeffs%>%filter(term=='x')
#CoeffRS1<-Coeffs
#CoeffRS2<-Coeffs
#CoeffRS3<-Coeffs

#Plot by Cultivar
grnm<-'Coef3_RS_rnm'
g<-ggplot(aes(x=var,y=estimate,col=group),data=Coeffs)+
  geom_point(size=3,alpha=0.5)+
  geom_hline(yintercept=0,linetype='dashed',col='blue')+
  theme_bw()
print(g)
ggsave(file.path(WD,'FitCoeffs',paste(grnm,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)


save(list=ls(),file=file.path(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto', 
                              'Objetivo_3_Valoraciones',paste('WrkSp_SE_Rndm',Sys.Date(),'.RData',sep=''))))
rm(list=ls())

