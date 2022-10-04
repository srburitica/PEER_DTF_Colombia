library(vegan)

#data= DataV_PT
#wcl="watershed"; tcl="transfC";pcl="permC";abcl="IndNumber";spcl="Species"
#j=1

calc.div<-function(data,wcl,tcl,pcl,abcl,spcl)
{
  	# data<-LDataAT[[1]]
  	# wcl<-"Departamento" Column name for watershed ID
  	# tcl<-"Trans_c" Column name for transformation
  	# pcl<-"permC" Column name for permanence (succession)
  	# abcl<-"individuos" column name for number of individuals
  	# spcl<-"Especie" column name of species (or other taxon) ID
  	wcln<-which(names(data)==wcl)
  	tcln<-which(names(data)==tcl)
  	#if(!is.null(pcl)) pcln<-which(names(data)==pcl) #only if successional stage is taken into account
  	pcln<-which(names(data)==pcl) #only if successional stage is taken into account
  	abcln<-which(names(data)==abcl)
  	spcln<-which(names(data)==spcl)
  
  	Lws<-unique(data[,wcln])
  	nws<-length(Lws)  #number of wathershed
 

  	divD<-list()
  	anovaD<-list()
  	strD<-matrix(ncol=4,nrow=nws)
  	freq.table=list()
  	tab.trans=list()
  	colnames(strD)=c("NoSpT","NoSpBaja","NoSpMedia","NoSpAlta")
  	rownames(strD)= Lws
  
  
  
  	#for each watershed
  	for (j in 1:nws)
  	{
  		wsnm<-Lws[j]
    	print(wsnm)
    	#data2<-data[data[,wcln]==wsnm,]
    	data2<-subset(data,watershed==wsnm)

   		#Diversity
    
    	#no. total de especies en cada cuenca
    	NoSp=length(unique(data2$Species))
    
    	#if(!is.null(pcl)) data2$trat<-paste(data2[,tcl],data2[,pcl],sep="-") else data2$trat<-data2[,tcl]
    	data2$trat<-paste(data2[,tcl],data2[,pcl],sep="-") 
		tab.trat<-xtabs(data2[,abcln]~data2$trat+data2[,spcln],data=data2)
    	tab.trans[[j]]<-xtabs(data2[,abcln]~data2[,tcln]+data2[,spcln],data=data2)
    	if(!is.null(pcl)) tab.succe=xtabs(data2[,abcln]~data2[,pcln]+data2[,spcln],data=data2) 
    
    	freq.table[[j]]=decostand(tab.trans[[j]],method="pa")
    	NoSpT=apply(freq.table[[j]],1,sum)
    	NoSpAlta=NoSpT["High"]
    	NoSpBaja=NoSpT["Low"]
    	NoSpMedia=NoSpT["Medium"]

		strD[j,1]=NoSp
		strD[j,2]=NoSpBaja
		strD[j,3]=NoSpMedia
		strD[j,4]=NoSpAlta

    	#trans.prop=prop.table(tab.trans,1)
    
    
 	   # sing=doub=dom=vector(length=3)
 	   # names(sing)=names(doub)=names(dom)=rownames(tab.trans)
 	   # for(i in 1:dim(tab.trans)[1])
 	   # {
 	     # sing[i]=length(which(tab.trans[i,]==1))
  	    # doub[i]=length(which(tab.trans[i,]==2))
   	   # dom[i]=length(which(trans.prop[i,]>0.1))
   		 # }
    
  	  #stem.dens=tapply(data[,abcl],data$trat,length)
 	   divs=renyi(tab.trat, scales=c(0,1,2),hill=TRUE)
  	  temp=rownames(divs)
    
 	   if(class(divs)[2]=="numeric") 
 	   {
 	   		divs<-data.frame(matrix(divs,ncol=3,nrow=1))
 	     	temp<-rownames(tab.trat)
  	 	}
    colnames(divs)=c("q0","q1","q2")
    
    suc=tran=vector()
    for (i in 1:length(temp)) 
    {
      suc[i]=strsplit(temp[i],"-")[[1]][2]
      tran[i]=strsplit(temp[i],"-")[[1]][1]
    }
    if(!is.null(pcl)) divs$succe=suc
    divs$trans=tran
    
    if(length(unique(tran))>1)
    {
    aov.q0=summary(aov(q0~trans,divs))
    aov.q1=summary(aov(q1~trans,divs))
    aov.q2=summary(aov(q2~trans,divs))
    }
    else 
    {
      aov.q0=NA
      aov.q1=NA
      aov.q2=NA
    }

    divD[[wsnm]]<-divs
    anovaD[[wsnm]]<-list(ANOVAq0=aov.q0,ANOVAq1=aov.q1,ANOVAq2=aov.q2)
    #strD[[wsnm]]<-list(NoSp=NoSp,NoSpAlta,NoSpBaja,NoSpMedia,singletons=sing,doubletons=doub,dominants=dom)
    

  }
  names(divD)=names(anovaD)=names(strD)=names(tab.trans)=names(freq.table)=Lws

  return(list(DivD=divD,AnovaD=anovaD,StrD=strD,Abu=tab.trans,Freq=freq.table))
}



