source(file.path('C:','Users','dsrbu','Dropbox','Humboldt','6_RcodeRepository','herramienta-para-verificar-calidad-de-datos-master','taxon_valid.R'))
dataList<-read.csv(file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','Objetivo_1_Congruencia',
                         'Analysis_congr','Sp_Nm1_SR.csv'),header=T)
validList <- tax_res(dataList$Adjusted)
write.csv(validList,file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','Objetivo_1_Congruencia',
                    'Analysis_congr','Sp_Nm1_VL.csv'))
#BST names from pplots
dataList<-read.xlsx(file.path('C:','Users','dsrbu','Dropbox','Humboldt','2_Documentos de trabajo',
                              '2_BosqueSeco','4_OriginalData','ParcelasPermanentes','speciesCodePPlotsTDF.xlsx'),sheet=1)
dataList$scientificName2<-paste(dataList$genus,dataList$specificEpithet)
dataList$scientificName2<-paste(dataList$genus,gsub('[0-9]','',dataList$specificEpithet))
validList <- tax_res(dataList$scientificName2)
mtchScNm2<-match(dataList$scientificName2,validList$user_supplied_name)
dataList$scientificNameV<-validList$matched_name2[mtchScNm2]
write.csv(dataList,file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto','Objetivo_1_Congruencia',
                              'Analysis_congr','speciesCodePPlotsTDF_validated.csv'))
names