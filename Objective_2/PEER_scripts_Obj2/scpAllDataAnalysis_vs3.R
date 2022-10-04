library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(exactextractr)
library(spatstat)
library(foreign)
# library(stars)

#--------------------------------------------------------------------
#cargo info de veredas con BST
#--------------------------------------------------------------------
ruta_datos <- "/home/marce/Documentos/MArce/AnalisisR/"

#--------------------------------------------------------------------
#Info por municipios
#--------------------------------------------------------------------
#CNP_bovinos
l_files_CNP <- list.files("./Datos/Comp2/Censo Pecuario/",pattern = "_2.csv$")

l_CNP <- map(l_files_CNP,function(x) read.csv(paste0("./Datos/Comp2/Censo Pecuario/",x)))
l_CNP <- map(l_CNP, function(x) x %>% mutate(CODIGO.MUNICIPIO=formatC(CODIGO.MUNICIPIO, width = 5, format = "d", flag = "0")))

l_CNP_2 <- c(l_CNP[[1]] %>% select(AÑO, DPTOMPIO=CODIGO.MUNICIPIO, TOTAL.BUFALOS, PREDIOS.CON.BUFALOS) %>% pivot_wider())

dCNP_CabezasGanado <- read_csv("./Datos/Comp2/Censo Pecuario/CabezasGanado.csv",local = locale(encoding = "latin1"))[,-1]
dCNP_CabezasGanado2 <- dCNP_CabezasGanado %>% group_by(DPTOMPIO=codigo_dpto_mpio) %>% mutate(Bovinos_Finca=TOTAL_BOVINOS/TOTAL_FINCAS_CON_BOVINOS) %>%
  summarise(MeanBovinos_FincaHa=mean(Bovinos_Finca),.groups="drop")

#--------------------------------------------------------------------
#Cultivo
dEVA_Cultivos <- read_csv("./Datos/Comp2/HectareasCultivos.csv",local = locale(encoding = "latin1"))[,-1]
dEVA_Cultivos <- left_join(dEVA_Cultivos,dAreasMuni %>% st_drop_geometry(), by=c("codigo_dpto_mpio"="DPTOMPIO"))
dEVA_Cultivos2 <- dEVA_Cultivos %>% group_by(DPTOMPIO=codigo_dpto_mpio) %>% 
  mutate(Ha_Sembrada_HaMuni=Ha_Sembrada/Area_Ha) %>% 
  summarise(MeanHa_Sembrada_HaMuni=mean(Ha_Sembrada_HaMuni))

dEVA_Cultivos_yr <- dEVA_Cultivos %>% group_by(Anios) %>% summarise(Mean=mean(Quemas_Ha),SD=sd(Quemas_Ha))

#--------------------------------------------------------------------
#Gini
sf_gini <- st_read(paste0(ruta_datos,"maps/DANE/nbs/nbs.shp")) 
d_gini <- sf_gini %>% st_drop_geometry() %>% mutate(DPTOMPIO=formatC(as.numeric(Codigo_Mun), width = 5, format = "d", flag = "0")) #problemas con algunos códigos de municipios que están mal y no se unen

#--------------------------------------------------------------------
#Quemas -> ¿usar estos o hacer suma de puntos?
d_no_quemas <- read.csv("./Datos/Proyecto_Fuego/EventTrends2017v2_.csv") 
d_pr_areas <- read.csv("./Datos/Proyecto_Fuego/PAreaTrends2017v2.csv",encoding = "latin1") 

d_no_quemas <- d_no_quemas %>% mutate(DPTOMPIO=formatC(DaneMun, width = 5, format = "d", flag = "0"))
d_pr_areas <- d_pr_areas %>% mutate(DPTOMPIO=formatC(DaneMun, width = 5, format = "d", flag = "0")) 

#--------------------------------------------------------------------
#Biofin
d_biofin <- read.csv("./Datos/Comp2/Biofin_ajustados.csv",dec = ",") # a los datos originales les faltaba codigo de cartagena y santa marta
d_biofin <- d_biofin %>% mutate(DPTOMPIO=formatC(as.numeric(Cod_Dane), width = 5, format = "d", flag = "0"), PARTICIPACIÓN_Por=as.numeric(as.character(gsub(",",".",gsub("%","",PARTICIPACIÓN)))))
names(d_biofin) <- gsub("X","Biofin_",names(d_biofin))

#--------------------------------------------------------------------
#Unir datos Muni a Cod Veredas
d_data_muni <- sf_areas2 %>% st_drop_geometry() %>% dplyr::select(DPTOMPIO:NOMBRE_VER,AREA_HA,COD_DPTO) %>% 
  left_join(d_gini %>% dplyr::select(DPTOMPIO,Nombre,GINI_2011,NBI_2018),by="DPTOMPIO") %>%
  left_join(d_no_quemas %>% dplyr::select(No_Eventos_Quemas=trendNum1000,DPTOMPIO),by="DPTOMPIO") %>%
  left_join(d_pr_areas %>% dplyr::select(Area_Quemada_Muni=trendNum,DPTOMPIO),by="DPTOMPIO") %>%
  left_join(d_biofin %>% dplyr::select(DPTOMPIO, starts_with("Biofin"), Total_Inv=TOTAL,PARTICIPACIÓN_Por),by="DPTOMPIO")

write.csv("./Datos/Comp2/d_data_muni.csv",row.names = F)

#--------------------------------------------------------------------
#Info espacialmente explícita
#--------------------------------------------------------------------
#Quemas MODIS
d_MODIS_1218_verBST_sum <- read.csv("./Datos/Comp2/d_MODIS_1218_verBST_sum.csv")
d_MODIS_1218_verBST_sum <- d_MODIS_1218_verBST_sum %>% mutate(No_pixel=AREA_HA/25)
d_MODIS_1218_verBST_sum_prop <- d_MODIS_1218_verBST_sum %>% mutate(COD_VEREDA=formatC(CODIGO_VER, width = 8, format = "d", flag = "0")) %>% mutate_at(vars(matches("MODIS")),function(x) x/d_MODIS_1218_verBST_sum$No_pixel)

#--------------------------------------------------------------------
#Deforestación
dDef_Prop_Def12_17BST <- read_csv("./Datos/Comp2/dDef_Prop_Def12_17BST.csv")

#--------------------------------------------------------------------
#CNA
dCNA_Categoriestab_wide <- read_csv("./Datos/Comp2/dCNA_Categoriestab_wide_Manejo_0521.csv")

dCNA2014_areas_activ_VerBST_categ <- read_csv("./Datos/Comp2/dCNA2014_areas_activ_VerBST_categ.csv")

dCNA2014_areas_activ_VerBST_tot <- read_csv("./Datos/Comp2/dCNA2014_areas_activ_VerBST_tot.csv")

d_CNA2014_S6Areas_BST_indic_div_cultiv <- read_csv("./Datos/Comp2/d_CNA2014_S6Areas_BST_indic_div_cultiv.csv")

#---------------------------------------------------------------------
#Erosión suelos
# eros_suel <- st_read(paste0(ruta_datos,"maps/IDEAM/E_DS_Erosion_100K_2010_2011/E_DS_Erosion_100K_2010_2011.shp")) %>% st_transform(sf_crs)
# 
# #Cruzo con mapa de veredas para extraer sólo estas áreas
# sf_areas_eros <- st_intersection(sf_areas2,eros_suel %>% st_make_valid() %>% st_buffer(dist=0))
# sf_areas_eros <- sf_areas_eros %>% mutate(ZONIFICACI2= recode (ZONIFICACI,
#                                                         `Erosión Moderada` = "Erosión Moderada",
#                                                         `Erosión Ligera` = "Erosión Ligera",
#                                                         `Sin Evidencia de Erosión` = "Sin Evidencia de Erosión",
#                                                         `Erosión Severa` = "Erosión Severa a Muy Severa",
#                                                         `Erosión Muy Severa` = "Erosión Severa a Muy Severa",
#                                                         `Sin Suelo con Cuerpos de Agua` = "Sin Suelo",
#                                                         `Sin Suelo con Afloramiento Rocoso` = "Sin Suelo",
#                                                         `Sin Suelo en Zonas Urbanas` = "Sin Suelo"))
# sf_areas_eros$ZONIFICACI2 <- factor(sf_areas_eros$ZONIFICACI2)
# 
# #Para extraer todas las categorias
# sf_areas_eros1 <- sf_areas_eros %>% mutate(Area=st_area(.)) %>% group_by(CODIGO_VER,ZONIFICACI2) %>% summarise(Area_Ha=sum(Area)/10000) %>% 
#   mutate(Tot=sum(Area_Ha), Prop=round(Area_Ha/Tot,4), COD_VEREDA=formatC(as.numeric(CODIGO_VER), width = 8, format = "d", flag = "0")) %>% st_drop_geometry() %>% 
#   dplyr::select(-Area_Ha) %>% pivot_wider(names_from = "ZONIFICACI2",values_from="Prop")
# d_data_eros <- sf_areas_eros1 %>% filter(COD_VEREDA %in% sf_areas2$CODIGO_VER) %>% dplyr::select(-Tot) %>% replace_na(list(`Erosión Ligera`=0,`Erosión Moderada`=0,`Sin Evidencia de Erosión`=0, 
#                                                                                                                            `Sin Suelo`=0,`Erosión Severa a Muy Severa`=0)) 
# write.csv(d_data_eros,"./salidas/d_data_eros.csv",row.names = F)

d_data_eros <- read_csv("./salidas/d_data_eros.csv")
d_data_eros <- d_data_eros %>% mutate(COD_VEREDA=formatC(as.numeric(CODIGO_VER), width = 8, format = "d", flag = "0")) %>% dplyr::select(-CODIGO_VER)

#Para sacar maximo
sf_areas_eros2 <- sf_areas_eros %>% mutate(Area=st_area(.)) %>% group_by(CODIGO_VER,ZONIFICACI2) %>% slice(which.max(Area))
sf_areas_eros_max <- sf_areas_eros2 %>% mutate(COD_VEREDA=formatC(as.numeric(CODIGO_VER), width = 8, format = "d", flag = "0")) %>% group_by(COD_VEREDA) %>% slice(which.max(Area))

d_data_eros_max <- sf_areas_eros_max %>% st_drop_geometry() %>% filter(COD_VEREDA %in% sf_areas2$CODIGO_VER) %>% dplyr::select(COD_VEREDA,Eros_Max=ZONIFICACI2)
write.csv(d_data_eros_max,"./salidas/d_data_eros_max.csv")
d_data_eros_max <- read_csv("./salidas/d_data_eros_max.csv")
# data_eros_max <- data_eros_max[,-1]

d_data_eros_prop_max <- left_join(d_data_eros %>% ungroup() %>% dplyr::select(COD_VEREDA:`Erosión Severa a Muy Severa`), d_data_eros_max, by="COD_VEREDA")
write.csv(d_data_eros_prop_max,"./salidas/d_data_eros_prop_max.csv")
d_data_eros_prop_max <- read_csv("./salidas/d_data_eros_prop_max.csv")

#---------------------------------------------------------------------
#Conflicto uso suelos
# library(units)
# sf_usos_suel <- st_read("./Datos/Capas/Conflictos_Uso_100k_Veredas.shp")
# st_crs(sf_usos_suel) <- st_crs(4686)
# sf_usos_suel <- sf_usos_suel %>% st_transform(crs=sf_crs) %>% st_make_valid()
# sf_usos_suel <- sf_usos_suel %>% st_make_valid()
# sf_usos_suel$Area_Uso_Vereda <- st_area(sf_usos_suel)
# 
# sf_usos_suel_ver <- sf_usos_suel %>% group_by(COD_VEREDA=CODIGO_VER,Conflicto_) %>% summarise(Area_Conflicto=sum(Area_Uso_Vereda)) %>% 
#   mutate(Area_tot=sum(Area_Conflicto),Prop_Conf_Uso=Area_Conflicto/Area_tot)
# 
# d_usos_suel_ver <- sf_usos_suel %>% group_by(COD_VEREDA=CODIGO_VER,Conflicto_)  %>% st_drop_geometry() %>% summarise(Area_Conflicto=sum(Area_Uso_Vereda)) %>% 
#   mutate(Area_tot=sum(Area_Conflicto),Prop_Conf_Uso=Area_Conflicto/Area_tot)
# d_usos_suel_ver_wide <- d_usos_suel_ver %>% as.data.frame() %>% dplyr::select(-Area_Conflicto) %>% pivot_wider(names_from=Conflicto_,values_from=Prop_Conf_Uso) %>% 
#   dplyr::select(COD_VEREDA,Sobreutilización,`Usos adecuados o sin conflicto`,Subutilización) 
# 
# write.csv(d_usos_suel_ver_wide,"./salidas/d_usos_suel_ver_wide.csv")
d_usos_suel_ver_wide <- read.csv("./salidas/d_usos_suel_ver_wide.csv")[,-1]
d_usos_suel_ver_wide <- d_usos_suel_ver_wide %>% mutate(COD_VEREDA=as.character(formatC(COD_VEREDA, width = 8, format = "d", flag = "0")))

#---------------------------------------------------------------------
#Pendiente
# sf_area <- st_read("/home/marce/Documentos/MArce/PEER_BST/Datos/VeredasBST_fixed.shp")
# st_crs(sf_area) <- st_crs(4326)
# sf_area_3116 <- sf_area %>% st_transform(st_crs(3116))

# pend <- raster(paste0(ruta_datos,"maps/IGAC/DEM30m/pendienteEpsg3116.tif"))
# 
# pend_verBST_stats <- exact_extract(pend,sf_area_3116,c('mean','median','stdev','majority','min','max'))
# names_prev <- names(pend_verBST_stats)
# names(pend_verBST_stats) <- paste0("pend_",names_prev)
# pend_verBST_stats2 <- cbind(CODIGO_VER=sf_area_3116$CODIGO_VER,pend_verBST_stats)
# 
# sf_pend_verBST_stats <- cbind(sf_area_3116,pend_verBST_stats)
# 
# st_write(sf_pend_verBST_stats,"/home/marce/Documentos/MArce/PEER_BST/Datos/Capas/Salidas/Pendiente/sf_pend_verBST_stats.shp")
# write.csv(pend_verBST_stats2,"/home/marce/Documentos/MArce/PEER_BST/Datos/Capas/Salidas/Pendiente/pend_verBST_stats.csv",row.names = F)

pend_verBST_stats2 <- read.csv("/home/marce/Documentos/MArce/PEER_BST/Datos/Capas/Salidas/Pendiente/pend_verBST_stats.csv")
pend_verBST_stats2 <- pend_verBST_stats2 %>% mutate(COD_VEREDA=as.character(formatC(CODIGO_VER, width = 8, format = "d", flag = "0")))

#---------------------------------------------------------------------
#Runap
# sf_runap <- st_read(paste0(ruta_datos,"maps/RUNAP/runap2/runap2Polygon.shp"))
# sf_runap_3116 <- sf_runap %>% st_transform(st_crs(3116))
# sf_runap_3116_to2014 <- sf_runap_3116 %>% filter(fecha_act<as.Date("2015-01-01"))
# 
# res <- 100
# #Creo raster
# sf_ex<-extent(sf_areas2)
# r_frame <- raster(sf_ex,resolution=res,crs=epsg_crs)
# values(r_frame) <- rep(1,ncell(r_frame))
# # r_base <- mask(r_frame,sf_runap_3116)
# 
# #Rasterizo
# sf_runap_3116_to2014$id_ras<-1
# r_runap_3116_to2014 <- fasterize(sf_runap_3116_bf2014,r_frame,field="id_ras")
# 
# #Creo mapa de distancias
# r_dist_runap_3116_to2014 <- gridDistance(r_runap_3116,origin=1)
# # r_dist_runap_3116_veredasBST <- mask(r_dist_runap_3116,sf_areas2)
# 
# writeRaster(r_dist_runap_3116,paste0(ruta_datos,"maps/RUNAP/runap2/runap2_distances_to2014.tiff"),overwrite=T)
# r_dist_runap_3116_to2014 <- raster(paste0(ruta_datos,"maps/RUNAP/runap2/runap2_distances_to2014.tif"))
#   
# sf_VerBST_runap_to2014 <- sf_areas2
# sf_VerBST_runap_to2014$APDs_min <- exact_extract(r_dist_runap_3116_to2014,sf_areas2['CODIGO_VER'],fun="min")
# sf_VerBST_runap_to2014$APDs_max <- exact_extract(r_dist_runap_3116_to2014,sf_areas2['CODIGO_VER'],fun="max")
# sf_VerBST_runap_to2014$APDs_mean <- exact_extract(r_dist_runap_3116_to2014,sf_areas2['CODIGO_VER'],fun="mean")
# sf_VerBST_runap_to2014$APDs_sd <- exact_extract(r_dist_runap_3116_to2014,sf_areas2['CODIGO_VER'],fun="stdev")
# sf_VerBST_runap_to2014$APDs_mode <- exact_extract(r_dist_runap_3116_to2014,sf_areas2['CODIGO_VER'],fun="majority")
# 
# st_write(sf_VerBST_runap_to2014,dsn="./Datos/Capas/DistRunap_to2014_VerBST.shp")
# # sf_VerBST_runap_to2014 <- st_read(dsn="./Datos/Capas/DistRunap_to2014_VerBST.shp")
# d_VerBST_runap_to2014 <- sf_VerBST_runap_to2014  %>% dplyr::select(COD_VEREDA=CODIGO_VER,starts_with("APD"))%>% st_drop_geometry()
# write.csv(d_VerBST_runap_to2014,file ="./Datos/Comp2/d_DistRunap_to2014_VerBST.csv")
# 
# plot(sf_VerBST_runap_to2014['RunapDist_mean'])

d_VerBST_runap_to2014 <- read.csv("./Datos/Comp2/d_DistRunap_to2014_VerBST.csv")[,-1]
d_VerBST_runap_to2014 <- d_VerBST_runap_to2014 %>% mutate(COD_VEREDA=as.character(formatC(COD_VEREDA, width = 8, format = "d", flag = "0")))


#------------------------------
#Distritos riego
# sf_dstr_riego <- st_read(paste0(ruta_datos,"/maps/UPRA/Distritos Riego/Distritos_ADT_Punto_Dic/Distritos_ADT_Punto_Dic.shp"))
# sf_dstr_riego <- sf_dstr_riego %>% st_transform(st_crs(3116)) 
# 
# sf_dstr_riego_buf <- sf_dstr_riego %>% st_buffer(dist=sqrt(sf_dstr_riego$area_neta*10000/pi)) #área promedio de distritos en cundinamarca de 50ha
# 
# #Rasterizo
# sf_dstr_riego_buf$id_ras<-1
# r_dstr_riego_buf <- fasterize(sf_dstr_riego_buf,r_frame,field="id_ras")
# 
# #Creo mapa de distancias
# r_dist_dstr_riego_buf <- gridDistance(r_dstr_riego_buf,origin=1)
# writeRaster(r_dist_dstr_riego_buf,paste0(ruta_datos,"/maps/UPRA/Distritos Riego/Distritos_ADT_Punto_Dic/r_dist_dstr_riego_buf.tiff"),overwrite=T)
# # r_dist_dstr_riego_buf <- raster(paste0(ruta_datos,"./Datos/Capas/capasdistritorydistanciasa/r_dist_dstr_riego_buf.tiff"))
# 
# # r_dist_runap_3116_veredasBST <- mask(r_dist_runap_3116,sf_areas2)
# # writeRaster(r_dist_dstr_riego_buf,paste0(ruta_datos,"/maps/UPRA/Distritos Riego/Distritos_ADT_Punto_Dic/r_dist_dstr_riego_buf.tiff"),overwrite=T)
# 
# sf_VerBST_dstr_riego <- sf_areas2
# sf_VerBST_dstr_riego$DRDs_min <- exact_extract(r_dist_dstr_riego_buf,sf_areas2['CODIGO_VER'],fun="min")
# sf_VerBST_dstr_riego$DRDs_max <- exact_extract(r_dist_dstr_riego_buf,sf_areas2['CODIGO_VER'],fun="max")
# sf_VerBST_dstr_riego$DRDs_mean <- exact_extract(r_dist_dstr_riego_buf,sf_areas2['CODIGO_VER'],fun="mean")
# sf_VerBST_dstr_riego$DRDs_sd <- exact_extract(r_dist_dstr_riego_buf,sf_areas2['CODIGO_VER'],fun="stdev")
# sf_VerBST_dstr_riego$DRDs_mode <- exact_extract(r_dist_dstr_riego_buf,sf_areas2['CODIGO_VER'],fun="majority")
# 
# sf_VerBST_dstr_riego_dist <- sf_VerBST_dstr_riego
# 
# st_write(sf_VerBST_dstr_riego_dist,dsn="./Datos/Capas/sf_VerBST_dstr_riego.shp")
# # sf_VerBST_runap_to2014 <- st_read(dsn="./Datos/Capas/DistRunap_to2014_VerBST.shp")
# d_VerBST_dstr_riego <- sf_VerBST_dstr_riego  %>% dplyr::select(COD_VEREDA=CODIGO_VER,starts_with("DRD"))%>% st_drop_geometry()
# write.csv(d_VerBST_dstr_riego,file ="./Datos/Comp2/d_VerBST_dstr_riego.csv",row.names = F)

d_VerBST_dstr_riego <- read.csv("./Datos/Comp2/d_VerBST_dstr_riego.csv")
d_VerBST_dstr_riego <- d_VerBST_dstr_riego %>% mutate(COD_VEREDA=as.character(formatC(COD_VEREDA, width = 8, format = "d", flag = "0")))

#---------------------------------------------------------------
#Análisis degradación
#---------------------------------------------------------------
#-------------------------------------------------------------------------------
#Leer tablas Susana
#-------------------------------------------------------------------------------
id_veredas <- read.table(paste0("./Datos/Comp2/TablasPorVereda/","VeredasBST.txt"),sep=",",header = T)
id_veredas$COD_VEREDA <- formatC(id_veredas$CODIGO_VER, width = 8, format = "d", flag = "0")

files_tablas <- list.files("./Datos/Comp2/TablasPorVereda/",pattern = ".dbf$")
names_tablas <- gsub(".dbf","",files_tablas)
l_tablas <- map(files_tablas,function(x) read.dbf(paste0("./Datos/Comp2/TablasPorVereda/",x)))

l_tablas_veredas <- map(l_tablas, function(x) left_join(x,id_veredas,by=c("FID_"="FID")))
l_tablas_veredas <- map(l_tablas_veredas, function(x) x %>% mutate(COD_VEREDA=formatC(CODIGO_VER, width = 8, format = "d", flag = "0")))

l_tablas_veredas_est1 <- map(l_tablas_veredas[c(-2,-3,-4,-5)], function(y) y %>% dplyr::select(COD_VEREDA,MIN:MEDIAN) %>% pivot_longer(cols = MIN:MEDIAN,names_to="Estad",values_to="Valores"))
names(l_tablas_veredas_est1)<-names_tablas[c(-2,-3,-4,-5)]
d_tablas_veredas_est1_lab <- l_tablas_veredas_est1 %>% bind_rows(.id="Variable")

l_tablas_veredas_est2 <- map(l_tablas_veredas[c(2,3,5)], function(y) y %>% dplyr::select(COD_VEREDA,VALUE_0,VALUE_1) %>% pivot_longer(cols = VALUE_0:VALUE_1,names_to="Estad",values_to="Valores"))
names(l_tablas_veredas_est2)<-names_tablas[c(2,3,5)]
d_tablas_veredas_est2_lab <- l_tablas_veredas_est2 %>% bind_rows(.id="Variable")

l_tablas_veredas_est3 <- map(l_tablas_veredas[4], function(y) y %>% dplyr::select(COD_VEREDA,VALUE_0:VALUE_16) %>% pivot_longer(cols = VALUE_0:VALUE_16,names_to="Estad",values_to="Valores"))
names(l_tablas_veredas_est3)<-names_tablas[4]
d_tablas_veredas_est3_lab <- l_tablas_veredas_est3 %>% bind_rows(.id="Variable")

d_tablas_veredas_est <- rbind(d_tablas_veredas_est1_lab,d_tablas_veredas_est2_lab,d_tablas_veredas_est3_lab) %>% mutate(Var_Stat=paste(Variable,Estad,sep="_"))
d_tablas_veredas_est_wide <- d_tablas_veredas_est %>% ungroup() %>% dplyr::select(COD_VEREDA, Var_Stat, Valores) %>% pivot_wider(names_from=Var_Stat,values_from=Valores)

write.csv(d_tablas_veredas_est_wide,"./Datos/Comp2/d_tablas_veredas_est.csv")

d_tablas_veredas_est_wide <- read.csv("./Datos/Comp2/d_tablas_veredas_est.csv")[,-1]
d_tablas_veredas_est_wide <- d_tablas_veredas_est_wide %>% mutate(COD_VEREDA=as.character(formatC(COD_VEREDA, width = 8, format = "d", flag = "0")))


#-------------------------------------------------------------------------------
#Leer tablas Luis
#-------------------------------------------------------------------------------
files_tluis <- list.files("./Datos/Comp2/Datos_Luis_Romero/",pattern = "_2.csv$")
# files_tluis <- files_tluis[-grep("Cv.csv",files_tluis,value = F)]
names_tablas2 <- gsub("_2.csv","",files_tluis)

l_tluis <- map(files_tluis,function(x) read.csv(paste0("./Datos/Comp2/Datos_Luis_Romero/",x),dec=","))

map(l_tluis,names)
# ids <- c("Site","Site","polid","polid",rep("site",7),rep("Site",2))

l_tluis <- pmap(list(l_tluis,names_tablas2), function(x,y,z){
  # x <- x %>% dplyr::select(-X)
  # names(x) <- gsub(z,"Site",names(x))
  names(x) <- paste(y,names(x),sep="_")
  x %>% rename(OBJECTID=paste0(y,"_Site"))
})

map(l_tluis, nrow)

l_tluis_veredas <- map(l_tluis, function(x) inner_join(x %>% mutate(OBJECTID=as.character(OBJECTID)),id_veredas %>% dplyr::select(COD_VEREDA,OBJECTID) %>% mutate(OBJECTID=as.character(OBJECTID)),by="OBJECTID") %>% dplyr::select(-OBJECTID))# %>% filter(!is.na(COD_VEREDA)))
l_tluis_veredas <- map(l_tluis_veredas, function(x) x %>% mutate(COD_VEREDA=formatC(COD_VEREDA, width = 8, format = "d", flag = "0")))

map(l_tluis_veredas, nrow)

d_tablas_degr_int <- l_tluis_veredas %>% reduce(inner_join,by="COD_VEREDA")
# d_tablas_degr_int_2 <- l_tluis_veredas[5:11] %>% reduce(inner_join,by="COD_VEREDA") # estaban partidos porque estos tenian menos datos

write.csv(d_tablas_degr_int,"./Datos/Comp2/d_tablas_degr_int.csv")

d_tablas_degr_int <- read.csv("./Datos/Comp2/d_tablas_degr_int.csv")
d_tablas_degr_int <- d_tablas_degr_int %>% mutate(COD_VEREDA=as.character(formatC(COD_VEREDA, width = 8, format = "d", flag = "0")))

# write.csv(d_tablas_degr_int_2,"./Datos/Comp2/d_tablas_degr_int_2.csv")
# 
# d_tablas_degr_int_2 <- read.csv("./Datos/Comp2/d_tablas_degr_int_2.csv")
# d_tablas_degr_int_2 <- d_tablas_degr_int_2 %>% mutate(COD_VEREDA=as.character(formatC(COD_VEREDA, width = 8, format = "d", flag = "0")))


# #---------------------------------------------------------------
# #Grupos
# #---------------------------------------------------------------
# sf_CAR_ZonifH <- st_read("./Datos/Capas/CAR_ZonifH.shp")
# sf_CAR_ZonifH <- sf_CAR_ZonifH %>% st_transform(sf_crs)
# 
# sf_CAR <- st_read(paste0(ruta_datos,"maps/AutoridadesAmb/environmental_authorities.shp"))
# sf_CAR <- sf_CAR %>% st_transform(sf_crs)
# 
# sf_ZonifH <- st_read(paste0(ruta_datos,"maps/Zonificacion_hidrografica_2013/Zonificacion_hidrografica_2013.shp"))
# sf_ZonifH <- sf_ZonifH %>% st_transform(sf_crs)
# 
# sf_areas_regiones <- st_intersection(sf_areas2,regiones)
# sf_areas_regiones_CAR <- st_intersection(sf_areas_regiones,sf_CAR)
# sf_areas_regiones_CAR_ZonifH <- st_intersection(sf_areas_regiones_CAR,sf_ZonifH)
# 
# st_write(sf_areas_regiones_CAR_ZonifH,"./Datos/Capas/sf_areas_regiones_CAR_ZonifH.shp")
# d_areas_regiones_CAR_ZonifH <- sf_areas_regiones_CAR_ZonifH %>% st_drop_geometry()


#----------------------------------------------------------
# Join all
#----------------------------------------------------------
d_All_Data <- dCNA_Categoriestab_wide %>% 
  left_join(dCNA2014_areas_activ_VerBST_categ,by="COD_VEREDA") %>%# 8136 ubicacion por cuantiles del total de area por UPA segun proporcion entre cada cuantil
  left_join(dCNA2014_areas_activ_VerBST_tot,by="COD_VEREDA") %>% #8154 veredas Proporcion de area por categoria de tipo de cobertura
  left_join(d_CNA2014_S6Areas_BST_indic_div_cultiv %>% dplyr::select(COD_VEREDA, n_cultivos, Total_Area_Sem, D, H, Dmg),by="COD_VEREDA") %>% #7602 veredas indices de diversidad de cultivos
  left_join(d_MODIS_1218_verBST_sum_prop %>% dplyr::select(COD_VEREDA, MODIS_2012:MODIS_2018),by="COD_VEREDA") %>% #8368 veredas Proporción de área quemada según MODIS
  left_join(dDef_Prop_Def12_17BST %>% dplyr::select(COD_VEREDA=CODIGO_VER,starts_with("Prop_Def")),by="COD_VEREDA") %>% #8368 veredas Proporción de área deforestada
  left_join(d_data_eros %>% ungroup() ,by="COD_VEREDA") %>% #8362 veredas proporcion de área en categoría de erosión de suelo y categoría dominante
  left_join(d_usos_suel_ver_wide ,by="COD_VEREDA") %>% #8368 veredas proporción de área en categoría de conflicto de uso del suelo
  left_join(pend_verBST_stats2 %>% dplyr::select(-CODIGO_VER),by="COD_VEREDA") %>% #8369 veredas estadistico de resumen de pendiente por vereda
  left_join(d_VerBST_runap_to2014,by="COD_VEREDA") %>% #8368 veredas estadísticos de Distancia a áreas protegidas
  left_join(d_VerBST_dstr_riego,by="COD_VEREDA") %>% #8368 veredas estadísticos de distancia a distritos de riego
  left_join(d_data_muni %>% dplyr::select(COD_VEREDA=CODIGO_VER,GINI_2011:PARTICIPACIÓN_Por),by="COD_VEREDA") %>% #8715 veredas Variables por municipio como Gini, Tendencias en Quemas, inversión en biodiversidad
  mutate_if(is.numeric, replace_na, 0) %>%
  left_join(d_tablas_veredas_est_wide,by="COD_VEREDA") %>% #8772 veredas estadisticos de integridad de bosque y degradación
  left_join(d_tablas_degr_int ,by="COD_VEREDA") #%>% # 8369
  #left_join(d_tablas_degr_int_2 ,by="COD_VEREDA") # 865

d_All_Data <- d_All_Data %>% mutate(across(where(is.character), as.numeric),COD_VEREDA=formatC(COD_VEREDA, width = 8, format = "d", flag = "0"))

write.csv(d_All_Data,"./salidas/d_All_Data.csv",row.names = F)
  