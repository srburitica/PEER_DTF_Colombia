library(sf)
library(rgdal)
library(tidyverse)
library(raster)

#ruta_datos <- "/home/marce/Documentos/MArce/AnalisisR/"

ruta_datos <- "G:/PEER_LastMile/DatosPropiosDelProyecto/GIS_Base/"

#Defino CRS de trabajo
epsg <- make_EPSG()
epsg_crs <- CRS(na.exclude(epsg$prj4[epsg$code=="3116"])[1])
sf_crs <- st_crs(3116)

#Cargo mapa veredas BST
sf_areas <- st_read("G:/PEER_LastMile/DatosPropiosDelProyecto/GIS_Base/VeredasBST.shp", options = "ENCODING=latin9")
sf_areas <- sf_areas %>% mutate(CODIGO_VER=formatC(CODIGO_VER, width = 8, format = "d", flag = "0") , DPTOMPIO=formatC(DPTOMPIO, width = 5, format = "d", flag = "0"))
st_crs(sf_areas) <- st_crs(4326)
sf_areas <- st_transform(sf_areas,sf_crs)

sf_areas2 <- sf_areas %>% distinct(CODIGO_VER, .keep_all=T) #elimino veredas duplicadas
# stringi::stri_enc_detect(sf_areas$NOMB_MPIO[grep(52838,sf_areas$DPTOMPIO)][1])
d_areas_ver <- sf_areas2 %>% st_drop_geometry() %>% dplyr::select(CODIGO_VER,AREA_HA)

res <- 100
#Creo raster
sf_ex<-extent(sf_areas2)
r_frame <- raster(sf_ex,resolution=res,crs=epsg_crs)
values(r_frame) <- rep(1,ncell(r_frame))

#Cargo mapa veredas DANE
sf_dane <- st_read(paste0(ruta_datos,"maps/DANE/CRVeredas_2017/CRVeredas_2017.shp")) %>% st_transform(sf_crs)
sf_dane$CODIGO_VER <- formatC(sf_dane$CODIGO_VER, width = 8, format = "d", flag = "0")

sf_dane$NOMB_MPIO[grep(52838,sf_dane$DPTOMPIO)] <- "TÚQUERRES"
sf_dane$NOMB_MPIO[grep(66594,sf_dane$DPTOMPIO)] <- "QUINCHÍA"
sf_dane$NOMB_MPIO[grep(99001,sf_dane$DPTOMPIO)] <- "PUERTO CARREÑO"
sf_dane$NOMB_MPIO <- factor(sf_dane$NOMB_MPIO)

sf_dane_BST <- sf_dane %>% filter(CODIGO_VER %in% sf_areas2$CODIGO_VER)

sf_deptos_munis <- sf_dane_BST %>% mutate(Area_m2=st_area(.)) %>% group_by(DPTOMPIO,NOM_DEP,NOMB_MPIO) %>% summarise(n=n(),Area_Muni=sum(Area_m2), .groups = 'drop') 
dAreasMuni <- sf_deptos_munis %>% group_by(NOM_DEP,DPTOMPIO,NOMB_MPIO) %>% summarise(Area_Ha=sum(Area_Muni)/10000, .groups = 'drop')

#Regiones Colombia
regiones <- st_read(paste0(ruta_datos,"maps/IGAC/RegionFis/RegionFis_Col_WGS84.shp")) %>% st_transform(sf_crs)
regiones$Region <- regiones$NOM_REGIóN
regiones$Region[regiones$NOM_SUBREG=="Sierra_Nevada_Sta._Marta"] <- "Caribe"
regiones$Region[regiones$NOM_SUBREG=="Depresión_Catatumbo"] <- "Andina"
regiones$Region[regiones$NOM_SUBREG=="Valle_Medio_Río_Magdalena"] <- "Andina"
regiones$Region[regiones$NOM_SUBREG=="Valle_Alto_Río_Magdalena"] <- "Andina"
regiones$Region[regiones$NOM_SUBREG=="Valle_Cauca"] <- "Andina"
regiones$Region[regiones$NOM_SUBREG=="Depresión_Patía"] <- "Andina"
regiones$Region[regiones$NOM_SUBREG=="Serranía_Macarena"] <- "Amazonia"

regiones_simp <- regiones %>% group_by(Region) %>% summarise(n=n())

# st_write(regiones_simp, dsn="Datos/Capas/RegionFis_Col_reclass.shp")
regiones_simp <- st_read("Datos/Capas/RegionFis_Col_reclass.shp")

#Cruzo con mapa de veredas para extraer sólo estas áreas
sf_areas_reg <- st_intersection(sf_areas2,regiones_simp)
sf_areas_reg2 <- sf_areas_reg %>% mutate(Area=st_area(.)) %>% group_by(CODIGO_VER) %>% slice(which.max(Area))

temp1 <- sf_areas_reg %>% group_by(CODIGO_VER) %>% summarise(n=n())
temp2 <- sf_areas %>% group_by(CODIGO_VER) %>% summarise(n=n())

cod <- temp2$CODIGO_VER[!temp2$CODIGO_VER %in% temp1$CODIGO_VER]

sf_areas_codreg <- sf_areas %>% filter(CODIGO_VER %in% cod) %>% mutate(Region="Insular",n=6) %>% mutate(Area=st_area(.))
head(sf_areas_codreg)

#agrego áreas de providencia que no cruzan en mapa por diferencia de ubicación
sf_areas_reg2 <- sf_areas_reg2 %>% bind_rows(sf_areas_codreg)

rm(regiones,temp1,temp2,cod)
cod_reg <- sf_areas_reg2 %>% select(COD_VEREDA=CODIGO_VER,Region)

reg <- unique(cod_reg$Region)
