#Date.created.Feb.23.2022
#Date.Modified.Feb.23.2022


#Script to run congruence analyses on PEER data

#0)Required packages
rqurd<-c("lme4","buildmer","ggeffects","broom","broom.mixed","margins","nlme",
         "corrplot","rgeos","sp","rgdal","tidyverse","raster","sf","reshape",
         "iNEXT","BiodiversityR","betapart","zetadiv","fastDummies","vegan",
         "openxlsx","rgdal","sp","devtools","plot.matrix",
         "gridExtra","Hmisc","ggplot2","ggfortify")#"openxlsx","BiodiversityR","MASS",","data.table",'reshape2','ggpubr',
         # 'ggpmisc','evaluate',"fastDummies"," corrplot","Hmisc")#,"maptools","rgdal",
  #","lattice","ggplot2","ade4","Rtsne","reshape2")
for(p in rqurd ){
  if (!is.element(p, installed.packages()[,1])){			#if package is not installed
    # r <- getOption("repos")								#assign R mirror for download
    # r["CRAN"] <- "http://cran.us.r-project.org"
    # options(repos = r)
    # rm(r)
    install.packages(p, dep = TRUE)			#install tuneR
    require(p, character.only = TRUE)
  }
}
for (p in rqurd){
  print(c("installing",p))
  try(library(p,character.only=T))
}


get.call<-function(ddata,yy,dd='Watershed',hh=NULL,expF='*',fixed=T){
  vn<-ddata%>%dplyr::select(!all_of(c(yy,dd)))%>%names(.)
  if(fixed==F){
    if(is.null(hh)){
      as.formula(paste(yy,'~',paste(vn,collapse=expF),
                       '+(1|',dd,')')) 
    }else{
      as.formula(paste(yy,'~',paste(vn,collapse=expF),
                       '+(',hh,'|',dd,')'))  
    }
  }
  else{
    as.formula(paste(yy,'~',paste(vn,collapse=expF)))
  }
}
devtools::install_github("slowkow/ggrepel")
library(ggrepel)
# 
# 
source(file.path("C:","Users","dsrbu","Dropbox","Humboldt","6_RcodeRepository",
                 "14_Script_others","NEwR-2ed_code_data","NEwR2-Functions","cleanplot.pca.R"))

#1) Directories
WD<-file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto')
dir.create(file.path(WD,'Objetivo_1_Congruencia','Analysis_congr'))
WDOut<-file.path(WD,'Analysis_congr')
setwd(WD)
WDIn<-file.path('G:','My Drive','PEER_LastMile','Datos_Proyectos')

#1B Archivos
GEFBIDp<-readOGR(file.path(WD,'Objetivo_3_Valoraciones','EcosystemServiceData','AllPlotInfo2.shp'))
GEFBIDp<-spTransform(GEFBIDp, CRS("+init=epsg:4326"))
plot(GEFBIDp[GEFBIDp$Depart=='Tolima',])
GEFBIDp@data<-GEFBIDp@data%>%mutate(plotCorr22=ifelse(is.na(PlotCorr),plotName,PlotCorr))%>%
  mutate(WS_plnm22=paste(Watershed,plotCorr22,sep='_'))

BSTp_newCov<-readOGR(file.path(WD,'Objetivo_3_Valoraciones','EcosystemServiceData','BSTPlot_SciBiomAge2.shp'))
GEFBIDpl.d<-readRDS(file.path(WD,'Objetivo_1_Congruencia','DatosConsolidados','Plantas','dataForests_PNUD-BID.rds'))

#2) Extracting abundance matrices GEFBID plants
Data.GB.pl<-map(names(GEFBIDpl.d[[3]][[1]]),function(x){
  xx<-GEFBIDpl.d[[3]][[1]][[x]]
  y<-xx%>%add_column(organismQuantity=1)%>%
    select(code,organismQuantity)%>%
    pivot_wider(.,names_from=code,values_from=organismQuantity, 
                     values_fn=sum,values_fill=0)
})
names(Data.GB.pl)<-names(GEFBIDpl.d[[3]][[1]])
Data.GB.pl2<-Data.GB.pl%>%bind_rows(.,.id='column_label')%>%mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))
#Extracting coordinates
Coor.GB.pl<-GEFBIDpl.d[[1]]%>%bind_rows(.,.id='column_label')%>%
  select('column_label','registerDate','latitude_dec','longitude_dec','plotType','state')
coord<-Coor.GB.pl[,c('longitude_dec','latitude_dec')]
shp.Coor.GB.pl<-SpatialPointsDataFrame(coords=coord,data=Coor.GB.pl,proj4string = CRS("+init=epsg:4326"))
plot(shp.Coor.GB.pl,add=T,col='red',pch=19)

#join spatial point info
GEFBIDp2<-st_is_within_distance(st_as_sf(GEFBIDp,coords=c("latDec","longDec")),
                                st_as_sf(shp.Coor.GB.pl,coords=c("latitude_dec","longitude_dec")),
                                dist=100,sparse=FALSE)
WS_plnm22<-GEFBIDp@data%>%pull(WS_plnm22)
lnm_p<-apply(GEFBIDp2,2,function(x){WS_plnm22[which(x==1)]})
##manually modified equivalences with other data
selpnts<-data.frame(shp.Coor.GB.pl@coords)%>%
  rownames_to_column('plot')
rownames(selpnts)<-NULL
grdb<-0.05
for(i in 1:nrow(selpnts)){
  print(selpnts[i,'plot'])
  print(lnm_p[[i]])
g<-ggplot()+
  geom_point(data=selpnts[-i,],aes(x=longitude_dec,y=latitude_dec,col='blue'))+
  geom_text(aes(x=longitude_dec,y=latitude_dec,label=plot,col='blue'),data=selpnts[-i,])+
  geom_point(data=selpnts[i,],aes(x=longitude_dec,y=latitude_dec))+
  geom_text(aes(x=longitude_dec,y=latitude_dec,label=plot),data=selpnts[i,])+
  xlim(min=selpnts$longitude_dec[i]-grdb,max=selpnts$longitude_dec[i]+grdb)+
  ylim(min=selpnts$latitude_dec[i]-grdb,max=selpnts$latitude_dec[i]+grdb)+
  geom_point(data=GEFBIDp@data,aes(x=longDec,y=latDec,col='red'),inherit.aes = F)+
  geom_text(aes(x=longDec,y=latDec,label=plotCorr22,col='red'),data=GEFBIDp@data)
  
plot(g)
Answer<-readline('Enter equivalent: ')
if(Answer==""){lnm_p[[i]]<-lnm_p[[i]]}
else{lnm_p[[i]]<-Answer}
}
#Get final name equivalence
lnm_pp<-data.frame('WS_plnm22'=do.call(rbind,lnm_p))%>%mutate(watershed=gsub('_.*','',WS_plnm22))%>%
  mutate(corrPlot22=gsub('.*_','',WS_plnm22))

#Get data ready for diversity estimations
Data.r<-melt(as.data.frame(Data.GB.pl2),id.vars='column_label',variable_name='scientificName_2')%>%
  filter(value>0)
names(Data.r)[3]<-'organismQuantity'
mtch<-match(Data.r$column_label,Data.GB.pl2$column_label)
Data.r[,c('parentEventID','eventID','Watershed','WS_plnm22')]<-lnm_pp[mtch,c('corrPlot22','WS_plnm22','watershed','WS_plnm22')]
Data.r<-Data.r%>%mutate(parentEventID=paste(Watershed,gsub('P[0-9]|[T|D|Te|I]','',parentEventID),sep='_'))%>%
  mutate(samplingProtocol='RAP')
Data.r$samplingProtolcol<-'RAP'

with(Data.r,table(eventID))

#get_coveriates
cov_vias_pp<-read.csv(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','Objetivo_1_Congruencia','Analysis_congr','FinalCovariatesProjects','BSTPlot_dviasTren.csv'))
cov_vias_pp<-cov_vias_pp%>%dplyr::select(.,c('Std114','mean114','Std118','mean118','meanEstimate','Site'))
names(cov_vias_pp)<-c('Std114_v','mean114_v','Std118_v','mean118_v','meanEstimate_v','Site')
cov_ase_pp<-read.csv(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','Objetivo_1_Congruencia','Analysis_congr','FinalCovariatesProjects','BSTPlot_DisAsen.csv'))
cov_ase_pp<-cov_ase_pp%>%dplyr::select(.,c('Std114','mean114','Std118','mean118','meanEstimate','Site'))
names(cov_ase_pp)<-c('Std114_a','mean114_a','Std118_a','mean118_a','meanEstimate_a','Site')
mtch<-match(cov_vias_pp$Site,cov_ase_pp$Site)
cov1_pp<-cbind(cov_vias_pp,cov_ase_pp[mtch,-6])
cov1_pp$buff<-gsub('[0-9]*_','',cov1_pp$Site)
cov1_pp$FID<-gsub('_[0-9]*','',cov1_pp$Site)
cov1_pp<-cov1_pp%>%reshape(.,v.names=c('Std114_v','mean114_v','Std118_v','mean118_v','meanEstimate_v',
                                       'Std114_a','mean114_a','Std118_a','mean118_a','meanEstimate_a'),
                           timevar='buff',idvar='FID',drop='Site',direction='wide')%>%
  dplyr::select(.,all_of('FID'),all_of(ends_with('.500')))
cov_oth_pp<-read.csv(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','Objetivo_1_Congruencia','Analysis_congr','FinalCovariatesProjects','BSTPlot_covars.csv'))
cov_oth_pp$buff<-gsub('[0-9]*_','',cov_oth_pp$polid)
cov_oth_pp$FID<-gsub('_[0-9]*','',cov_oth_pp$polid)
cov_oth_pp<-cov_oth_pp%>%reshape(.,v.names=c('Altitud30m','CHeight','DensidadPobiavh18','DensidadPobUN20','FInt19','HEH18','HtreeCover20'),
                                 timevar='buff',idvar='FID',direction='wide',drop='polid')%>%dplyr::select(.,all_of('FID'),all_of(ends_with('.500')))
mtch<-match(cov_oth_pp$FID,cov1_pp$FID)
cov_pp<-cbind(cov_oth_pp,cov1_pp[mtch,])
cov1_pp<-cov_pp[,-9]
kpvr<-c('kpvr','WD','WDOut','WDIn','BSTp_newCov','GEFBIDpl.d','GEFBIDp','shp.Coor.GB.pl',
       'Data.r','cov1_pp','PrintggiNext','PrintRefiNext','PrintggiNextInc','PrintPredD_Cov',
       'Data.a.f','Data.i.f','get.call')
rm(list=ls()[!ls()%in%kpvr])
#join other covariates
BSTp_newCov<-spTransform(BSTp_newCov, CRS("+init=epsg:4326"))
plot(BSTp_newCov[BSTp_newCov$Depart=='Tolima',])
BSTp_newCov@data<-BSTp_newCov@data%>%mutate(plotCorr22=ifelse(is.na(PlotCrr),plotNam,PlotCrr))%>%
  mutate(WS_plnm22=paste(Watersheed,plotCorr22,sep='_'))


mtch<-match(BSTp_newCov@data$FID_t,cov1_pp$FID)
cov<-cbind(BSTp_newCov@data[,c('FID_t','SCI_m','Bms_t_1','Bm__0_1',
                               'pr_frst','age_min','age_max','AllPID','WS_plnm22','Watersheed','latDec','longDec')],cov1_pp[mtch,])

cov<-cov[,-13]
names(cov)[1]<-'FID'
kpvr<-c(kpvr,'cov')
cov[,c('parentEventID','eventID','Watershed','WS_plnm22')]<-cov[,c('WS_plnm22','WS_plnm22','Watersheed','WS_plnm22')]
cov<-cov%>%mutate(parentEventID=paste(Watershed,gsub('(^.*_)|P[0-9]|[T|D|Te|I]','',parentEventID),sep='_'))
cov$age_rng<-cov$age_max-cov$age_min
spa.c<-c("longDec","latDec")
cat.c<-c("Watershed")#
v.pres<-c("DensidadPobiavh18.500","DensidadPobUN20.500", "HEH18.500","mean114_v.500",
          "mean114_a.500","meanEstimate_v.500","meanEstimate_a.500")
v.rec<-c("Bms_t_1","Bm__0_1","age_max", "age_min","Altitud30m.500","CHeight.500","FInt19.500","HtreeCover20.500")#,"FInt19meanx")
v.msite<-NULL

kpvr<-c(kpvr,'cov','spa.c','cat.c','v.pres','v.rec','v.msite')
rm(list=ls()[!ls()%in%kpvr])

#analysis diversidad

dir.create(file.path(WDOut,'GEFBID_Veg'))
WDOut<-file.path(WDOut,'GEFBID_Veg')
dir.create(file.path(WDOut,'CurvasDiversidad'))

##incidence ParentID Overall
gnm<-'GEFBID'
Data.ii.r<-Data.r%>%
  dplyr::select(parentEventID,organismQuantity,scientificName_2)%>%
  pivot_wider(names_from=parentEventID,values_from=organismQuantity, values_fn=sum,values_fill=0)%>%
  mutate_if(is.numeric,~1*(.>0))%>%column_to_rownames(.,var="scientificName_2")%>%list(.)
Hill.rr<-iNEXT(Data.ii.r,q=c(0,1,2),datatype = "incidence_raw")
names(Hill.rr$iNextEst)<-"Regional"
PrintggiNext(paste(gnm,'_incO',sep=''),Hill.rr)
kpvr<-c(kpvr,'Hill.r','Data.ee.r','gnm')

##Abundance ParentID Overall
ommt<-c("") 
ompv<-c("")
Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
nsp<-unique(Data.r2$parentEventID)
nsp<-length(!nsp%in%ompv)
Data.ee.r<-Data.r2%>%
  dplyr::select(parentEventID,organismQuantity,samplingProtocol,scientificName_2)%>%
  pivot_wider(names_from=parentEventID,values_from=organismQuantity, 
              values_fn=sum,values_fill=0)%>%
  mutate(TotAbu=rowSums(.[,3:(nsp+2)]),.keep="unused")%>%
  dplyr::select(scientificName_2,samplingProtocol,TotAbu)
Data.ee.r<-Data.ee.r%>%pivot_wider(.,names_from=samplingProtocol,values_from=TotAbu, values_fn=sum,values_fill=0)%>%
  column_to_rownames(.,var="scientificName_2")%>%as.list(.)

Hill.r<-iNEXT(Data.ee.r,q=c(0,1,2),datatype = "abundance")
PrintggiNext(paste(gnm,'_abM',sep=''),Hill.r)
kpvr<-c(kpvr,'Hill.r','Data.ee.r')
rm(list=ls()[!ls()%in%kpvr])

#by watershed Incidence
ompv<-c("")
ommt<-c("")
ctnm<-'Watershed'
Data.ei.r<-Data.i.f(ctnm)
Data.ei.r2<-iNEXT(Data.ei.r,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm,ctnm,sep='_')
PrintggiNextInc(fnm2,Data.ei.r2)
fnm2<-paste(gnm,ctnm,'Inc',sep='_')
PrintRefiNext(fnm2,ctnm,Data.ei.r2)
PrintPredD_Cov(fnm,Data.ei.r,0.6,ctnm)
kpvr<-c(kpvr,'Data.ei.oo')

#by Watershed abundance
ommt<-c("")
ompv<-c("")

# ctnm
Data.ee.oo<-Data.a.f(ctnm,fn="sum",scale=TRUE) #scale is used for groups non-integer abundance
map(names(Data.ee.oo), function (x){
  write.csv(Data.ee.oo[[x]],file.path(WDOut,
                                      paste("species_watershed", x,".csv", sep="")))
})

Data.ee.oo2<-map(names(Data.ee.oo),function(xx){
  x<-Data.ee.oo[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.oo2)<-names(Data.ee.oo)
map(names(Data.ee.oo),function(xx){
  fnm2<-paste(gnm,ctnm,xx,sep='_')
  v<-Data.ee.oo2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,ctnm,v)
  PrintPredD_Cov(fnm2,Data.ee.oo[[xx]],0.9,ctnm,type="abundance")
  return()
})
kpvr<-c(kpvr,'Data.ee.oo','Data.ee.oo2')
rm(list=ls()[!ls()%in%kpvr])

#4) Hills by MU
rowSums(table(Data.r$parentEventID,Data.r$organismQuantity))
ommt<-c("")
ompv<-c("")
Data.ee.mm0<-Data.a.MU(Data.r,'parentEventID',
                       "^(.*_.*)$",scale=FALSE)
Data.ee.mm<-map(names(Data.ee.mm0),function(x){
  xx<-Data.ee.mm0[[x]]
  y<-CompletEmpty(xx,'parentEventID',NULL,x)
})
names(Data.ee.mm)<-names(Data.ee.mm0)

#plot with cover color
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,'Watershed','parentEventID','MU_Est')
})

map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,'Watershed','WShMU_est')
})

Data.ee.e<-map(names(Data.ee.mm),function(x){
  point.df<-Data.ee.mm[[x]]%>%mutate(samplEff.1=1)
  write.csv(point.df,file.path(WDOut,'CurvasDiversidad',paste(gnm,'_',x,'MU_Estim.csv',sep='')))
  return(point.df)
})
names(Data.ee.e)<-names(Data.ee.mm)
map(names(Data.ee.e), function(x){
  map(c(v.pres,v.rec), function (vv){
    print(vv)
    plnm<-paste('DivP_',gnm,'_',x,'_',vv,sep='')
    point.df<-Data.ee.e[[x]]
    plotCvar(point.df,vv,plnm,x)
  })
})

kpvr<-c(kpvr,'Data.ee.mm','Data.ee.e')
rm(list=ls()[!ls()%in%kpvr])

#4b) Hills by sub-MU with abundance
rowSums(table(Data.r$eventID,Data.r$organismQuantity))


ommt<-c("")
ompv<-c("")
Data.ee.nn0<-Data.a.MU(Data.r,'eventID',expPEID="(^.*_)((T[0-9]{1}P)|(T|D|Te|I))",scale=FALSE,replc="\\1",summ=F)
Data.ee.nn<-map(names(Data.ee.nn0),function(x){
  xx<-Data.ee.nn0[[x]]
  y<-CompletEmpty(xx,'eventID',NULL,x)
  #shorter names for coprofagos
  #y$eventID<-gsub("(ANH_[0-9]+)(_T. Exc. Human)([0-9]+)(_2021-)([0-9]+-[0-9]+)(/.*)","\\1_\\3/\\5",y$eventID)
  return(y)
})
names(Data.ee.nn)<-names(Data.ee.nn0)

map(names(Data.ee.nn),function(x){
  point.dff<-Data.ee.nn[[x]]
  plotMU_cat(x,point.dff,'Watershed','eventID','subMU_Est')
})
kpvr<-c(kpvr,'Data.ee.nn')
rm(list=ls()[!ls()%in%kpvr])

Data.ee.n<-map(names(Data.ee.nn),function(x){
  point.df<-Data.ee.nn[[x]]%>%mutate(samplEff.1=1)
  write.csv(point.df,file.path(WDOut,'CurvasDiversidad',paste(gnm,'_',x,'SMU_Estim.csv',sep='')))
  return(point.df)
})
names(Data.ee.n)<-names(Data.ee.nn)
map(names(Data.ee.n), function(x){
  map(c(v.pres,v.rec), function (vv){
    print(vv)
    plnm<-paste('DivP_',gnm,'_',x,'_',vv,sep='')
    point.df<-Data.ee.n[[x]]
    plotCvar(point.df,vv,plnm,x)
  })
})

  ##Diversity by eventID incidence
grp<-list('RAP'=c('RAP'))
ommt<-c("")
ompv<-c("")
Data.pt<-Data.r%>%
  filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
Data.ee.pr<-Data.a.pt(grp,Data.pt,'eventID','sum',scale=TRUE)
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='(^.*_)((T[0-9]{1}P)|(T|D|Te|I))',
                       c('RAP'),replc='\\1')
vars<-c('Watershed','parentEventID')
cov.1<-cov%>%filter(parentEventID%in%Data.ee.prr$parentEventID)%>%
  dplyr::select(.,c(parentEventID,all_of(v.rec),all_of(v.pres),all_of(cat.c)))%>%
  group_by_at(vars)%>%
  dplyr::summarise(across(everything(), list(mean)))%>%
  rename_at(.vars = vars(ends_with("_1")),list(~sub("_1", "", .)))
Data.ee.prr<-Data.ee.prr%>%
  inner_join(.,cov.1, by="parentEventID")

write.csv(Data.ee.prr,file.path(WDOut,'CurvasDiversidad',
                                paste(gnm,'_','MU_Inc_Estim_Mean.csv',sep='')))
kpvr<-c(kpvr,'Data.ee.pr','Data.ee.prr','grp')


##random models on diversity

get_rndm_model<-function(selc,varsL,txtnm='ModelsRandom.txt',DataL=Data.ee.e){
  outf<-file.path(WDOut,txtnm) #round 2 with index but without slope
  sink(outf,split=TRUE)
  mod<-map(selc,function(i){
    mcul<-map(varsL,function(j){
      print(paste('starting with',i,'y',j,sep=" "))
      datam<-DataL[[1]]%>%filter(order==j)%>%dplyr::select(estimado,Watershed,all_of(i))%>%
        mutate(.,across(all_of(i),list(scale)),.keep="unused")%>%
        rename_at(.vars = vars(ends_with("_1")),list(~sub("_1", "", .)))
      print(corrplot(cor(datam[,i])))
      readline(prompt = 'ENTER')
      for(h in i){
        g<-ggplot(aes(y=estimado,x=get(h),col=Watershed),data=datam)+
          geom_point()+geom_smooth(method='lm',se=T)+labs(x=h)
        theme_classic()
        print(g)
        readline(prompt="ENTER")
      }
      print(i)
      answ<-readline("which variable to use: ")
      answ2<-readline("expansion factor : ")
      f<-get.call(datam,'estimado','Watershed',answ,expF=answ2)
      Mo<-buildlme(f,data=datam,
                   buildmerControl = buildmerControl(direction="order"))
      print(summary(Mo))
      print(plot(Mo))
      readline(prompt='ENTER')
      print(plot(Mo,estimado~fitted(.)))
      readline(prompt='ENTER')
      print(qqnorm(Mo,~resid(.)|Watershed))
      readline(prompt='ENTER')
      return(Mo)
    })
    names(mcul)<-varsL
    return(mcul)
  })
  names(mod)<-names(selc)
  sink()
  return(mod)
}
#Generate estimated plots
Get_est_fig<-function(grnm='Est_fig',mod=mod){
  grnm<-'Fit_rndm'
  plotsM<-map(names(mod), function(xx){
    x<-names(mod[[xx]])
    map(x, function(yy){
      y<-mod[[xx]][[yy]]
      print(y@model)
      datam<-y@model$data
      vars<-datam%>%dplyr::select(where(is.numeric))%>%dplyr::select(-estimado)%>%names(.)
      plotsl<-map(vars,function(z){
        flnm<-paste(xx,yy,z,sep='_')
        predm<-ggpredict(y@model,terms=c(z,'Watershed'),ci.lvl=0.95, type='random')
        g<-ggplot(aes(x=x,y=predicted,group=group),data=predm)+
          geom_line() +
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, fill="gray")+
          expand_limits(y=c(0, 80))+
          # geom_point(aes(x=get(z),y=lgWY),col="black",alpha=0.5,data=datam)+
          xlab(z)+ theme_classic()+
          # theme(legend.position="none")+
          labs(title = paste("Fitted lines for",flnm,sep=" "))
        print(g)
        # readline(prompt='ENTER')
        ggsave(file.path(WDOut,paste(grnm,' for_',flnm,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)
        return(g)
      })
    })
  })
}

selcc<-list('sel1'=c('CHeight.500','DensidadPobUN20.500','age_min','mean114_a.500'),
          'sel2'=c('FInt19.500','HEH18.500','age_min'))
varsLL<-c('Species richness',  'Shannon diversity', 'Simpson diversity')
kpvr<-c(kpvr,'Get_est_fig','get_rndm_model','selcc','varsLL','get.call')
#models bu UM/watershed
modUM<-get_rndm_model(selc=selcc,varsL=varsLL,txtnm='M_UM_Rand.txt')
Get_est_fig(grnm='MU_estFig',mod=modUM)

#modesl for eventID
modEV<-get_rndm_model(selc=selcc[2],varsL=varsLL,txtnm='M_EV_Rand.txt',DataL=Data.ee.nn)
Get_est_fig(grnm='EV_estFig',mod=modEV)

#model fix effects
get_fixed_model<-function(selc,varsL,txtnm='ModelsFixed.txt',DataL=Data.ee.e){
  outf<-file.path(WDOut,txtnm) #round 2 with index but without slope
  sink(outf,split=TRUE)
  mod<-map(selc,function(i){
    mcul<-map(varsL,function(j){
      print(paste('starting with',i,'y',j,sep=" "))
      datam<-DataL[[1]]%>%filter(order==j)%>%dplyr::select(estimado,Watershed,all_of(i))%>%
        mutate(.,across(all_of(i),list(scale)),.keep="unused")%>%
        rename_at(.vars = vars(ends_with("_1")),list(~sub("_1", "", .)))
      # print(corrplot(cor(datam[,i])))
      # readline(prompt = 'ENTER')
      f<-get.call(datam,yy='estimado',fixed=T)
      print(f)
      M<-lm(f,data=datam)
      Mo<-step(M)
      print(summary(Mo))
      # print(plot(Mo))
      return(Mo)
    })
    names(mcul)<-varsL
    return(mcul)
  })
  names(mod)<-names(selc)
  sink()
  return(mod)
}
get_fixed_figure<-function(grnm='Fit_All',mod=modEVF){
  plotsM<-map(names(mod), function(xx){
    x<-names(mod[[xx]])
    map(x, function(yy){
      y<-mod[[xx]][[yy]]
      datam<-y$model
      vars<-datam%>%dplyr::select(where(is.numeric))%>%dplyr::select(-estimado)%>%names(.)
      newdat<-lapply(vars,function(v){
        rep(median(datam[,v]),100)
      })
      newdat<-data.frame(do.call(cbind,newdat))
      names(newdat)<-vars
      plotsl<-map(vars,function(z){
        print(z)
        flnm<-paste(xx,yy,z,sep='_')
        predm<-predict.lm(y,newdata=newdat,interval="confidence",type="response")
        newdat[,c('fitted','minfit','maxfit')]<-predm
        g<-ggplot(aes(x=get(z),y=fitted),data=newdat)+
          geom_ribbon(aes(ymin = minfit, ymax = maxfit,alpha=0.5), fill = "lightgray")+
          geom_line(aes(x=get(z),y=fitted),col="blue")+
          xlab(z)+ theme_classic()+
          theme(legend.position="none")+labs(title = paste("Fitted lines for",flnm,sep=" "))
        print(g)
        readline(prompt='ENTER')
        ggsave(file.path(WD,'FittedGraphs',paste(grnm,' for_',z,'.jpeg',sep='')),g,device='jpeg',width=12,height=8)
        return(g)
      })
    })
  })
}
modEVF<-get_fixed_model(selc=selcc[2],varsL=varsLL,txtnm='M_EV_Fixed.txt',DataL=Data.ee.nn)
get_fixed_figure(grnm='EVFx_estFig',mod=modEVF)

kpvr<-c(kpvr,'modEVF','modUM','Get_est_fig','modEV','Get_est_fig','get_fixed_figure','get_fixed_model')
rm(list=ls()[!ls()%in%kpvr])

ommt<-c("")
ompv<-c("")

Data.rk.a<-Data.a.r('Watershed',gnm,'sum',scale=TRUE)
map(names(Data.rk.a), function(x){
  RA.data<-Data.rk.a[[x]]
  plotRnkAb(RA.data,gnm,ctnm,x)
})

#NMDS
ommt<-c("")
ompv<-c("")

#in funcion=sum

Data.r.ab<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))%>%
  dplyr::select(parentEventID,samplingProtocol,
                organismQuantity,scientificName_2)%>%
  pivot_wider(id_cols=c(parentEventID,samplingProtocol),
              names_from=scientificName_2,values_from=organismQuantity, 
              values_fn=sum,values_fill=0)
Data.r.ab<-Data.r.ab%>%group_split(samplingProtocol)
names(Data.r.ab)<-names(Data.ee.oo)

Data.r.ord<-getMNDS_abu(Data.r.ab,ctnm,'parentEventID')
names(Data.r.ord)<-names(Data.r.ab)
Data.r.df<-getNMDS_DF(Data.r.ord,'parentEventID')
names(Data.r.df)<-names(Data.r.ab)
PlotNMDS(Data.r.df,ctnm,'abu','parentEventID',lab=TRUE)
kpvr<-c(kpvr,'Data.r.ord','Data.r.df')

##Redundancy
#8) how environmental variables structure the MU
v.pres<-c("DensidadPobUN20.500", "HEH18.500","mean114_v.500",
          "mean114_a.500","meanEstimate_v.500","meanEstimate_a.500")
v.rec<-c("age_min","Altitud30m.500","CHeight.500","FInt19.500","HtreeCover20.500")#,"FInt19meanx")

cov.e<-cov%>%dplyr::select(all_of("parentEventID"),all_of(v.pres), all_of(v.rec),all_of(v.msite), all_of(spa.c))%>%
 filter(parentEventID%in%Data.r$parentEventID)%>%filter(!duplicated(parentEventID))%>%
  melt(.,id.vars=c('parentEventID','latDec','longDec'))%>%
  group_by(variable)%>%
  mutate(z=scale(value))
kpvr<-c(kpvr,'cov.e')
f.nm<-file.path(WDOut,paste('Covar_',gnm,'.jpeg',sep=''))
g<-ggplot(aes(x=longDec,y=latDec,group=variable),data=cov.e)+
  geom_point(aes(size=abs(z),col=(z>0)))+
  facet_wrap(vars(variable),scale="free")+theme_test()
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
print(g)
f.nm<-file.path(WDOut,paste('Covar_hist',gnm,'.jpeg',sep=''))
g<-ggplot(aes(x=z,group=variable),data=cov.e)+
  geom_histogram()+
  facet_wrap(vars(variable))+theme_test()
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
print(g)
rm(list=ls()[!ls()%in%kpv])

#8) how environment explain community structure...exploration of contraits
#PCA macro-variables 
cov.ee.r<-cov%>%dplyr::select(all_of("parentEventID"),all_of(v.pres),all_of(v.rec))%>%
  filter(!duplicated(parentEventID))%>%
  filter(parentEventID%in%Data.r$parentEventID)%>%remove_rownames(.)%>%column_to_rownames(.,var="parentEventID")

env.pca<-rda(cov.ee.r,scale=TRUE)
sm<-summary(env.pca)
print(sm)
ev<-env.pca$CA$eig
ev.f<-ev[ev>mean(ev)]

PCA_v<-sm$sites[,c('PC1','PC2','PC3')]


outf<-file.path(WDOut,paste("PCA_macro",gnm,".txt",sep=""))
sink(outf)
print('### PCA con variables macro###')
print(sm)
print(ev.f)
sink()

par(mfrow=c(1,2))
f.nm<-file.path(WDOut,paste('Env_PCA',gnm,'%1d.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
cleanplot.pca(env.pca,scaling=1)
cleanplot.pca(env.pca,scaling=2)
dev.off()
cleanplot.pca(env.pca,scaling=1)
cleanplot.pca(env.pca,scaling=2)

par(mfrow=c(1,1))
env.w<-hclust(dist(scale(cov.ee.r)))
f.nm<-file.path(WDOut,paste('Env_ClustP',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
plot(env.w)
dev.off()
plot(env.w)
f.nm<-file.path(WDOut,paste('Env_Clust',gnm,'.jpeg',sep=''))

gr<-cutree(env.w,k=5)
grl<-levels(factor(gr))
sit.scl<-scores(env.pca,display='wa',scaling=1 )
sit.gr<-data.frame(sit.scl)
sit.gr$gr<-gr[match(names(gr),rownames(sit.scl))]
sit.gr$pch<-14+sit.gr$gr
sit.gr$col<-1+sit.gr$gr
jpeg(f.nm, width = 800, height = 480, quality=300)
p<-plot(env.pca, display='wa',scaling=1,type='n',main="PCA correlation + clusters",
        xlim=c(min(sit.scl[,1]),1.5*max(sit.scl[,1])),
        ylim=c(min(sit.scl[,2]),1.5*max(sit.scl[,2])))
points(sit.gr,pch=sit.gr$pch,col=sit.gr$col,cex=2)
text(sit.scl,gsub('ANH_','',rownames(sit.scl)),cex=.7,pos=3)
legend(x=1.5*max(p$default[,1]),y=1.5*max(p$default[,2]),paste("grupo",c(1:length(grl))),pch=14+c(1:length(grl)),col=1+c(1:length(grl)),
       cex=0.7, y.intersp = 1,x.intersp=1, bty="n")
dev.off()
#map
cov.ee.r2<-cov.ee.r%>%rownames_to_column(.,'parentEventID')%>%
  mutate(pcaGr=gr)%>%
  inner_join(.,cov[,c('parentEventID','latDec','longDec')])%>%
  distinct(parentEventID,.keep_all=T)
g<-ggplot(cov.ee.r2,aes(x=longDec,y=latDec,color=as.factor(pcaGr)))+
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(data = cov.ee.r2, aes(x=longDec,y=latDec,label=parentEventID), 
                 color = "olivedrab4")+
  scale_color_manual(values = pal[1:length(unique(cov.ee.r2$pcaGr))]) +
  guides(colour=guide_legend(title='PCA Group'))+
  clean_background +
  labs(title = paste(gnm,': Map of groups'))
print(g)
f.nm<-file.path(WDOut,paste('MapaGruposPCA_',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
kpvr<-c(kpvr,"gr","env.pca")


#8b) post-hoc plotting Macro
Data.r.dfE<-getNMDS_gr(Data.r.ord,gr)
names(Data.r.dfE)<-names(Data.r.ord)
v.final<-c(v.pres,v.rec)
Data.r.spE<-NMDS_env(Data.r.ord,Data.r.ab,v.final,'abu')
names(Data.r.spE)<-names(Data.r.ord)
plotNMDS_sp(Data.r.spE,Data.r.dfE,'PCA grupo','abu','parentEventID',lab=FALSE)
kpvr<-c(kpvr,'Data.r.dfE','Data.r.spE')
rm(list=ls()[!ls()%in%kpv])


#9) Ordenamiento NMDS usando incidencia 
#coprofagos/zooplancton/fitoplancton
ommt<-c("")
ompv<-c("")

v.final<-c(v.pres,v.rec)

Data.ei.ab<-map(names(grp), function(x){
  selpID<-Data.ee.prr%>%filter(grp==x)%>%
    distinct(parentEventID)%>%pull(parentEventID)
  Y<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))%>%
    dplyr::select(parentEventID,organismQuantity,scientificName_2)%>%
    filter(parentEventID%in%selpID)%>%
    pivot_wider(id_cols=parentEventID,names_from=scientificName_2,
                values_from=organismQuantity, values_fn=sum,values_fill=0)%>%
    mutate_if(is.numeric,~1*(.>0))
})
names(Data.ei.ab)<-names(grp)
Data.ei.ord<-getNMDS_i(Data.ei.ab,ctnm,'parentEventID')
names(Data.ei.ord)<-names(Data.ei.ab)
Data.ei.df<-getNMDS_i_DF(Data.ei.ord,'parentEventID')
names(Data.ei.df)<-names(Data.ei.ab)
PlotNMDS(Data.ei.df,ctnm,'Inc','parentEventID',lab=TRUE)
kpvr<-c(kpvr,'Data.ei.ab','Data.ei.ord')


#10) post-hoc plotting
Data.ei.dfE<-getNMDS_gr(Data.ei.ord,gr)
names(Data.ei.dfE)<-names(Data.ei.ord)
Data.ei.dfEm<-getNMDS_gr(Data.ei.ord,grm)
names(Data.ei.dfEm)<-names(Data.ei.ord)
Data.ei.spE<-NMDS_Sp(Data.ei.ord,Data.ei.ab,'inc','parentEventID')
names(Data.ei.spE)<-names(Data.ei.ord)
plotNMDS_sp(Data.ei.spE,Data.ei.dfE,'PCA grupo','inc','parentEventID',lab=FALSE)
plotNMDS_sp(Data.ei.spE,Data.ei.dfEm,'PCA grupoM','inc','parentEventID')
plotNMDS_sp(Data.ei.spE,Data.ei.df,ctnm,'inc','parentEventID',lab=FALSE)
plotNMDS_sp(Data.ei.spE,Data.ei.df_rh,'Red.Hidrica','inc','parentEventID')
plotNMDS_sp(Data.ei.spE,Data.ei.df_hb,'Cobertura','inc','parentEventID')
plotNMDS_sp(Data.ei.spE,Data.ei.df_ht,'habitat','inc','parentEventID')
plotNMDS_sp(Data.ei.spE,Data.ei.df_sl,'UCSuelo','inc','parentEventID',lab=FALSE)

#plot environment correlation
Data.ei.En<-NMDS_env(Data.ei.ord,Data.ei.ab,v.final,'inc')
names(Data.ei.En)<-names(Data.ei.ord)
plotNMDS_sp(Data.ei.En,Data.ei.dfE,'PCA grupo','inc_E','parentEventID',lab=FALSE)
plotNMDS_sp(Data.ei.En,Data.ei.df,ctnm,'inc_E','parentEventID',lab=FALSE)
plotNMDS_sp(Data.ei.En,Data.ei.df_sl,'UCSuelo','inc_E','parentEventID',lab=FALSE)


#SubMU period
Data.si.spE<-NMDS_Sp(Data.si.ord,Data.si.ab,'inc','eventPer')
names(Data.si.spE)<-names(Data.si.ord)
plotNMDS_sp(Data.si.spE,Data.si.df,ctnm,'incPer','Per')
kpv<-c(kpv,'Data.ei.dfE','Data.si.spE')
rm(list=ls()[!ls()%in%kpv])


#9)how environmental variables relate to community structure once contraits are known
v.pres2<-c("DensidadPobUN20.500","HEH18.500","mean114_a.500","meanEstimate_v.500")
v.rec2<-c("age_min","Altitud30m.500","FInt19.500")
v.msite2<-NULL

kpv<-c(kpv,c('v.rec2','v.pres2','v.msite2'))


#dummy variables
# covc<-dummy_cols(cov,select_columns=c(ctnm),remove_first_dummy = TRUE)
# chngnm<-names(covc)[grep(paste('(',ctnm,'_)(.*)',sep=''),names(covc))]
# names(covc)[which(names(covc)%in%chngnm)]<-gsub(paste(ctnm,'_',sep=''),'',chngnm)
# chngnm<-gsub(paste(ctnm,'_',sep=''),'',chngnm)
# cov.ee<-covc%>%dplyr::select(all_of("parentEventID"),all_of(v.pres2),all_of(v.rec2),all_of(v.msite2),all_of(chngnm))%>%
#   filter(parentEventID%in%UM)%>%
#   filter(!duplicated(parentEventID))

cov.ee<-cov%>%dplyr::select(all_of("parentEventID"),
                            all_of(v.pres2),all_of(v.rec2),all_of(v.msite2),
                            all_of(ctnm))%>%
  filter(parentEventID%in%Data.r$parentEventID)%>%
  filter(!duplicated(parentEventID))



#9a) abundance data
outf<-file.path(WDOut,paste("RDA_res_abu",gnm,".txt",sep=""))
sink(outf)
Data.r.rda<-map(names(Data.r.ab),function(xx){
  x<-Data.r.ab[[xx]]
  if(nrow(x)>1){
    y<-x[,-2]%>%column_to_rownames(.,var="parentEventID")
    ints<-intersect(rownames(y),cov.ee$parentEventID)
    y<-y%>%dplyr::select(-which(colSums(y)==0))%>%filter(.,rownames(y)%in%ints)
    cov.ee2<-cov.ee%>%filter(.,parentEventID%in%ints)%>%filter(!duplicated(parentEventID))%>%
      arrange(match(parentEventID,rownames(y)))%>%dplyr::select(-parentEventID)
    y.hel<-decostand(y,method='hellinger')
    RDA.t<-rda(y.hel~.,data=cov.ee2)
    Ganova.rda<-anova.cca(RDA.t,step=1000)
    Sanova.rda<-anova.cca(RDA.t,by="axis", step=1000)
    pRDA.t1<-plot(RDA.t,scaling=1, main="scaling 1")
    arrow.mul<-attributes(pRDA.t1$biplot)$arrow.mul
    pRDA.t2<-plot(RDA.t,scaling=2,main="scaling 2")
    arrow.mul<-attributes(pRDA.t2$biplot)$arrow.mul
    print(paste("#### RDA for: ",xx,"####"))
    print(summary(RDA.t))
    print("#Canonical Coeficient#")
    print(coef(RDA.t))
    print("#Global significance#")
    print(Ganova.rda)
    print("#Axis significance#")
    print(Sanova.rda)
    print("-------------END-----------")
    vexp<-round(RsquareAdj(RDA.t)$adj.r.squared,3)
    vunexp<-round(RDA.t$CA$tot.chi/RDA.t$tot.chi,3)
    list("RDA.t"=RDA.t,"arrow.mul"=arrow.mul,"var.Exp"=vexp,"var.unexp"=vunexp)
  }else{
    list("RDA.t"=NULL,"arrow.mul"=NULL,"var.Exp"=NULL,"var.unexp"=NULL)
  }
})
sink()
names(Data.r.rda)<-names(Data.r.ab)
#ctnm
Data.r.rda.p(Data.r.rda,Data.r.ord,paste('abu_', ctnm,sep='')) #change names

#9b) Incidence data
# outf<-file.path(WDOut,'RDA',paste("RDA_res_Inc",gnm,".txt",sep=""))
# sink(outf)
# Data.r.rdaI<-map(names(Data.ei.ab),function(xx){
#   x<-Data.ei.ab[[xx]]
#   if(nrow(x)>1){
#     y<-x%>%column_to_rownames(.,var="parentEventID")
#     colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",
#                       colnames(y))
#     ints<-intersect(rownames(y),cov.ee$parentEventID)
#     y<-y%>%dplyr::select(-which(colSums(y)==0))%>%filter(.,rownames(y)%in%ints)
#     cov.ee2<-cov.ee%>%filter(.,parentEventID%in%ints)%>%filter(!duplicated(parentEventID))%>%
#       arrange(match(parentEventID,rownames(y)))%>%dplyr::select(-1)
#     y.hel<-decostand(y,"hellinger")
#     RDA.t<-rda(y.hel~.,data=cov.ee2)
#     Ganova.rda<-anova.cca(RDA.t,step=1000)
#     Sanova.rda<-anova.cca(RDA.t,by="axis", step=1000)
#     pRDA.t1<-plot(RDA.t,scaling=1, main="scaling 1")
#     arrow.mul<-attributes(pRDA.t1$biplot)$arrow.mul
#     pRDA.t2<-plot(RDA.t,scaling=2,main="scaling 2")
#     arrow.mul<-attributes(pRDA.t2$biplot)$arrow.mul
#     print(paste("#### RDA for: ",xx,"####"))
#     print(summary(RDA.t))
#     print("#Canonical Coeficient#")
#     print(coef(RDA.t))
#     print("#Global significance#")
#     print(Ganova.rda)
#     print("#Axis significance#")
#     print(Sanova.rda)
#     print("-------------END-----------")
#     vexp<-round(RsquareAdj(RDA.t)$adj.r.squared,3)
#     vunexp<-round(RDA.t$CA$tot.chi/RDA.t$tot.chi,3)
#     list("RDA.t"=RDA.t,"arrow.mul"=arrow.mul,"var.Exp"=vexp,"var.unexp"=vunexp)
#   }else{
#     list("RDA.t"=NULL,"arrow.mul"=NULL,"var.Exp"=NULL,"var.unexp"=NULL)
#   }
# })
# sink()
# names(Data.r.rdaI)<-names(Data.ei.ab)
# 
# Data.r.rda.p(Data.r.rdaI,Data.ei.ord,paste('Inc_', ctnm,sep='')) #Changenames
# Data.r.rda.p(Data.r.rdaI,Data.ei.hb,'Inc_Cobertura')
# Data.r.rda.p(Data.r.rdaI,Data.ei.ht,'Inc_habitat')
# Data.r.rda.p(Data.r.rdaI,Data.ei.rh,'Inc_redhidrica')
# Data.r.rda.p(Data.r.rdaI,Data.ei.sl,'Inc_suelo')
# kpv<-c(kpv,'Data.r.rda','Data.r.rdaI')




#Beta Diversity analyses
Data.ee.nn0<-Data.a.MU(Data.r,'eventID',expPEID="(^.*_)((T[0-9]{1}P)|(T|D|Te|I))",scale=FALSE,replc="\\1",summ=F)
#-get sampled points
coord<-cov[,spa.c]
crs    <- CRS("+init=epsg:4326") 
pntd<-SpatialPointsDataFrame(coords=coord,data=cov,proj4string = crs)
pnt<-spTransform(pntd,CRS("+init=epsg:3116"))
kpvr<-c(kpvr,'pnt')

#5) Get and plot beta Diversity
vcat='Watershed'
Ev.reclass<-map(names(Data.r.ab),function(xx){
  print(xx)
  x<-Data.r.ab[[xx]]
  c.df<-getCat(vcat,x$parentEventID)
  if(iii=="Anf"){
    c.df<-c.df%>%mutate(is.exit = !grepl('Bosque.*',as.character(c.df[,vcat])),
                        categ = ifelse(is.exit,'NoBosque', 'Bosque'))%>%
      select(.,-c(is.exit,all_of(vcat)))%>%group_split(categ)}
  if(iii=="Aves"){
    names(c.df)[names(c.df)==vcat]<-'categ2'
    c.df<-c.df%>%mutate(categ2=as.character(categ2))%>%
      mutate(is.exit = !grepl('Bosque.*',categ2),
             categ = ifelse(is.exit,categ2, 'Bosque'))%>%
      mutate(categ=recode(categ,`Herbazal`="VegBaja",`Pastos`="VegBaja"))%>%
      select(.,-c(is.exit,categ2))%>%group_split(categ)
  }
  if(iii=="Cprf_ad"){
    names(c.df)[names(c.df)==vcat]<-'categ2'
    c.df<-c.df%>%mutate(categ2=as.character(categ2))%>%
      mutate(is.exit = !grepl('Bosque.*',categ2),
             categ = ifelse(is.exit,categ2, 'Bosque'))%>%
      mutate(categ=recode(categ,`Herbazal`="VegBaja",`Pastos`="VegBaja"))%>%
      select(.,-c(is.exit,categ2))%>%group_split(categ)
  }
  if(iii=="Murc_S"){
    c.df<-c.df%>%mutate(is.exit = !grepl('Bosque.*',as.character(c.df[,vcat])),
                        categ = ifelse(is.exit,'Palma', 'Bosque'))%>%
      select(.,-c(is.exit,all_of(vcat)))%>%group_split(categ)}
  names(c.df)<-sapply(c.df,"[[",1,2)
  return(c.df)
})
names(Ev.reclass)<-names(Data.r.ab)

Beta.ab<-map(names(Data.r.ab),function(v){
  x<-Data.r.ab[[v]]
  xx<-x%>%mutate(Watershed=gsub("_.*","",parentEventID))%>%group_split(Watershed,.keep=F)
  names(xx)<-gsub("_.*","",lapply(xx,function(x){x$parentEventID[1]}))
  return(xx)
  map(xx,get)
  })

kpvr<-c(kpvr,'Beta.ab')
#plot Beta
sff<-paste('Ab',vcat,sep='_')
plotBeta(Beta.ab,flnm=sff)

save.image(file.path(WDOut,paste("wrkspc",gnm,Sys.Date(),".RData",sep="")))

#9b) save data for indicators
svLst<-c('Data.e','Data.r','SbUME','SbUMT','UM','UME','UMT','ctnm','fnn','cnm.smp','gnm','cov',
         'samEff.ttt','Data.ee.mm','Data.ee.nn','Data.ee.pr','Data.ee.prr','grp','Data.ei.t','Data.ei.ttt',
         'Data.ee.e','Data.rk.a','Data.r.ab','Data.s.abb','Data.r.pl','Data.r.pl_df','Data.s.ord','Data.s.df')
svLst<-intersect(svLst,ls())
save(list=svLst,
     file=file.path(WDOut,paste('WrkSp_Ind_',gnm,Sys.Date(),'.RData',sep='')))
rm(list=ls())


