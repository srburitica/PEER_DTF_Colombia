library(tidyverse)
library(sf)
library(ggmap)
library(corrr)
library(rstatix)
library(ggcorrplot)

#Cargo informacion
d_All_Data <- read_csv("./salidas/2021/d_All_Data.csv")
sf_VeredasBST_categ <- st_read("./Datos/Capas/Grupos/VeredasBST.shp")

# setwd(file.path("G:","GdriveMV","PEER_SEM"))
# d_All_Data <- read_csv(file.path("G:","GdriveMV","PEER_SEM","d_All_Data2.csv"))
# #define group variables
# dataG<-colnames(d_All_Data)
# write.csv(dataG, file.path("G:","Dropbox","Humboldt","2_Documentos de trabajo","16_PEER_LastMile2019","4_CNA_SEM","VariablesSEM.csv"))
write.csv(names(d_All_Data),"./Datos/VariablesSEM_G2.csv")

#tabla con variables para incluir en analisis y grupos
dataG<-read_csv("./Datos/VariablesSEM_G2.csv")
selc<-dataG$Var[!is.na(dataG$Grupo)]
dataG<-dataG[!is.na(dataG$Grupo),]

# sf_VeredasBST_categ <- st_read(file.path("G:","GdriveMV","PEER_SEM", "VeredasBST.shp"))

#fixed table
sf_VeredasBST_categ$SelSEM[is.na(sf_VeredasBST_categ$SelSEM)]<-0
sf_VeredasBST_categ$Grupo[is.na(sf_VeredasBST_categ$Grupo)]<-0

d_VeredasBST_categ_2 <- sf_VeredasBST_categ %>% dplyr::select(COD_VEREDA=CODIGO_VER,Grupo,CatPotEtt,SelSEM,AREA_HA) %>% st_drop_geometry()
d_All_Data_categ <- d_All_Data[,selc] %>% left_join(d_VeredasBST_categ_2,by="COD_VEREDA")
f1 <- function(x,area) (x/10000)/area # CLCP parecen estar en m2
f2 <- function(x,area) (x*300*300)/area
d_All_Data_categ <- d_All_Data_categ %>% mutate_at(vars(matches(c("CLCP"))), .funs=f1 , area=quote(AREA_HA)) %>% 
  mutate_at(vars(matches(c("ESA"))), .funs=f2 , area=quote(AREA_HA)) %>% 
  dplyr::select(-AREA_HA)

vars <- names(d_All_Data_categ)[2:(ncol(d_All_Data_categ)-3)]
vars_g<-dataG[dataG$Var%in%vars,]
grupos <- sort(unique(d_All_Data_categ$Grupo))
#lista con grupos de correlaciones
corrG<-list(c("C","R"),c("C","V"),c("C","M"),c("C","I"),c("V","M"),c("V","I"),
            c("V","R"),c("R","M"),c("R","I"),c("E","V"),c("E","R"),c("E","I"),c("E","M"),
            c("L","R"),c("L","V"),c("L","M"),c("L","I"))
RedG<-list("1","2")

#creo lista con tablas por grupo de datos VeredaBST
l_All_Data_categ <- map(grupos, function(x) d_All_Data_categ %>% filter(Grupo==x) %>% na.omit())
names(l_All_Data_categ) <- grupos
#corregir grupo 0 para incluir todas las veredas
l_All_Data_categ[[1]]<-d_All_Data_categ  %>% filter(Grupo>=0)%>%na.omit()
l_All_Data_categ_completos <- keep(l_All_Data_categ, ~ nrow(.x) > 0)
grupos2 <- names(l_All_Data_categ_completos)

#------------------------------------------
#Correlaciones
#------------------------------------------
l_all_data_corr_g <- map(l_All_Data_categ_completos, function(x) correlate(x %>% dplyr::select(-COD_VEREDA, -Grupo,-CatPotEtt,-SelSEM)))

#Para generar los gráficos por grupos y por variables
vars_gg<-map(corrG, function(i) map(RedG,function (j){
  vars<-vars_g$Var[vars_g$Grupo%in%i & vars_g$Redun==j]}))

names(vars_gg)<-corrG<-lapply(corrG,paste,collapse="")

l_img_all_data_corr_g <- map(l_all_data_corr_g, 
                             function(x) map(vars_gg, function(y)
                               map(y, function (j) map(j, function (h)
                                 {x %>% cor_select(j) %>% rename(Var=h) %>% 
                                   mutate(term = reorder(rowname, Var)) %>% filter(abs(Var)>0.1) %>%
                                   ggplot(aes(term, Var)) +
                                   geom_col() + coord_flip() +
                                   theme_bw() + ylab(h) +xlab("") + ylim(-1,1)}))))

#network graphs
l_netw_all_data_corr_g <- map(l_all_data_corr_g,
                              function(x) map(vars_gg, 
                                              function(y) map(y, function(j){
                                                xx<-x%>% cor_select(j)
                                                xx[is.na(xx)]<-0
                                                try(n1<-network_plot(xx[,-1],min_cor=0.2))
                                                try(n2<-network_plot(xx[,-1],min_cor=0.4))
                                                list(g.2=n1,g.4=n2)
                                                })))

#Guardar gráficos generados por grupos con datos    
map2(l_img_all_data_corr_g, grupos2, function(x,y) map2(x,corrG, function (i,j)
  {
  h<-vars_gg[[j]]
  map2(i,h,function(z,k) map2(z,gsub("/","_",k), function(p,r)
    ggsave(paste0("./salidas/2021/g",y,
                                    "/img_",r,"_g",y,"_cor",j,
                                    ".pdf"),p,height=20,width=15)))}))


map2(l_netw_all_data_corr_g, grupos2, 
     function(x,y) map2(x,unlist(corrG), 
                        function (i,j) map2(i,c(1,2),
                                            function(r,p)  map2(r,c("g.2","g.4"),function(z,k){
                                              pdf(file=paste0("./salidas/2021/g",y,
                                                              "/img_",k,"_g",y,"_cor",j,p,".pdf"),height=20,width=20)
                                              try(plot(z))
                                              dev.off()
                                            }))))



     




#-----------------------código que puede servir-----------------------------
# pdf(file="./salidas/2021/img_all_data_corr_g0.pdf",width=45,height = 40)
# rplot(all_data_corr_g0)
# dev.off()#
# 
# x<-l_all_data_corr_g[[1]]
# y<-vars_gg[[1]][[1]]
# test<-x %>% cor_select(y)
# network_plot(test[,-1],min_cor=0.2)
# x2<-x
# x2[, -1] <- x[, -1] + rnorm(cumprod(dim(x[, -1]))[-1], sd = 0.001) # You can change the sd value
# network_plot(x2,min_cor=0.6)
# 
# imgCorrPlot_all_vars_g0 <- ggcorrplot(cor(l_All_Data_categ[[1]] %>% dplyr::select(-COD_VEREDA)), hc.order = TRUE, lab = TRUE,lab_size = 2.5,digits = 2,type = "upper")
# ggsave(paste0("./salidas/CorrPlot_all_vars.pdf"),imgCorrPlot_all_vars,width=30,height=30)
# 
# 
# 
# all_data_corr <- correlate(all_data_cont)
# all_data_corr %>% 
#   network_plot(min_cor = 0.8)
# 
# all_data_corr %>% focus(No_Eventos_Quemas) %>%
#   mutate(rowname = reorder(term, No_Eventos_Quemas)) %>%
#   ggplot(aes(rowname, No_Eventos_Quemas)) +
#   geom_col() + coord_flip() +
#   theme_bw()
# 
# imgCorrPlot_all_vars <- ggcorrplot(cor(all_data_cont), hc.order = TRUE, lab = TRUE,lab_size = 2.5,digits = 2,type = "upper")
# ggsave(paste0("./salidas/CorrPlot_all_vars.pdf"),imgCorrPlot_all_vars,width=30,height=30)
# 
# d_corr_all_data_LRE_EN_CR_aTR <- all_data_LRE_EN_CR_aTR_corr %>%  
#   gather(-term, key = "colname", value = "cor") %>% 
#   filter(abs(cor) > 0.6) %>% arrange(term)
# 
# write.csv(d_corr_all_data_LRE_EN_CR_aTR,"./salidas/d_corr_all_data_LRE_EN_CR_aTR.csv")
# lstUnique <- function(x, depth = 4) {
#   if (depth == 0) class(x)
#   else if (is.list(x)) lapply(x, lstUnique, depth-1)
#   else x
# }
# unlist(lstUnique(l_netw_all_data_corr_g), use.names=F)