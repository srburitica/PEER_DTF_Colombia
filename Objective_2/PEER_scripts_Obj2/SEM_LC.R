library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(dslabs)
library(viridis)
library(ggExtra)
library(ggridges)
library(sf)
library(lavaan)
library(lavaanPlot)
library(tidyr)
library(knitr)
library(mvnormalTest)
library("PerformanceAnalytics")
library(MASS)
library("blavaan")
library("semPlot")
library("qgraph")
library("nloptr")
library(tidySEM)
library(semptools)

WD<-file.path('G:','My Drive','PEER_LastMile','DatosPropiosDelProyecto',
              'Objetivo_2_HistoriaUso',
              'SEM')
setwd(WD)

BASE_SEM_NORM <- read_csv("Producto1_BASE_SEM_NORM.csv")

## Modelo 12----
#El modelo 12  se compone de dos variables latentes (Prácticas y Riesgo) conformadas 
#por tres variables exógenas cada una, y 8 variables exógenas adicionales 
#y una variable endógena (SCI 2019)


model12 <- '
  # Variables latentes
    Practicas =~ P_S11P124_Fnatural + P_S11P125_No_protegen + P_S11P125_Prac_Cons_Rest
    Riesgo =~ P_S6P71_Enfermedad + P_S6P71_Plaga + P_S6P71_Inundacion
   # regressions
    Practicas ~ LHFI00_MEAN + pend_mean + Bosq_Nat
    SCI_19_Mean ~ Practicas +  P_S6P61_SP_Autoconsumo + P_S7P84A_Total_hembras
    SCI_19_Mean ~ G_VPrecipitacionQuanCvW_sigma + pend_mean
    SCI_19_Mean ~ LHFI00_MEAN + Riesgo
    SCI_19_Mean ~ P_S6P70_No_Sist_Riego
    P_S6P70_No_Sist_Riego ~ Bosq_Nat + G_VPrecipitacionQuanCvW_sigma  
    
  # Covarianzas
    Practicas ~~ Riesgo
    '
fit12 <- sem(model = model12, data = BASE_SEM_NORM, meanstructure=TRUE)
summary(fit12, fit.measures =TRUE, standardized = TRUE, rsquare=TRUE) 

modindices(fit12, sort = TRUE, maximum.number = 10)

semPaths(fit12,"std", "est",fade = FALSE, rotation = 2, theme= "colorblind", layout = "tree3",intercepts = FALSE, residuals = FALSE, thresholds = FALSE, equalizeManifests = TRUE, 
         optimizeLatRes = TRUE, curvePivot = TRUE)

## Modelo 15----
#El modelos 15 esta compuesto de dos variables latentes (Practicas y Riesgo), 
#7 variables exógenas y una variable endógena (Diferencia entre SCI2019 y SCI2014) 

model15 <- '
# Variables latentes
    Practicas =~ P_S11P124_Fnatural + P_S11P125_No_protegen + P_S11P125_Prac_Cons_Rest
    Riesgo =~ P_S6P71_Enfermedad + P_S6P71_Plaga + P_S6P71_Inundacion
   # regressions
    Practicas ~ LHFI00_MEAN + Bosq_Nat 
    DIF_SCI ~ Practicas +  P_S6P61_SP_Autoconsumo + P_S7P83A_Total_machos
    DIF_SCI ~ G_VPrecipitacionQuanCvW_sigma + pend_mean
    DIF_SCI ~ LHFI00_MEAN + Riesgo
    DIF_SCI ~ P_S6P70_No_Sist_Riego
    P_S6P70_No_Sist_Riego ~ Bosq_Nat + G_VPrecipitacionQuanCvW_sigma  
    
  # Covarianzas
    Practicas ~~ Riesgo
    '
fit15 <- sem(model = model15, data = BASE_SEM_NORM, meanstructure=TRUE)
summary(fit15, fit.measures =TRUE, standardized = TRUE) 

modindices(fit15, sort = TRUE, maximum.number = 10)

semPaths(fit15,"std", "est",fade = FALSE, rotation = 2, theme= "colorblind", layout = "tree3",intercepts = FALSE, residuals = FALSE, thresholds = FALSE, equalizeManifests = TRUE, 
         optimizeLatRes = TRUE, curvePivot = TRUE) 

model12b <- '
  # Variables latentes
    Practicas =~ P_S11P124_Fnatural + P_S11P125_No_protegen + P_S11P125_Prac_Cons_Rest
    Riesgo =~ P_S6P71_Enfermedad + P_S6P71_Plaga + P_S6P71_Inundacion
   # regressions
    Practicas ~ LHFI00_MEAN + pend_mean + Bosq_Nat
    SCI_19_Mean ~ Practicas +  P_S6P61_SP_Autoconsumo + P_S7P84A_Total_hembras
    SCI_19_Mean ~ G_VPrecipitacionQuanCvW_sigma + pend_mean
    SCI_19_Mean ~ LHFI00_MEAN + Riesgo
    SCI_19_Mean ~ P_S6P70_No_Sist_Riego
    P_S6P70_No_Sist_Riego ~ Bosq_Nat + G_VPrecipitacionQuanCvW_sigma  
    
  # Covarianzas
    Practicas ~~ Riesgo
    P_S11P125_No_protegen ~~ P_S11P125_Prac_Cons_Rest
    '
fit12b <- sem(model = model12b, data = BASE_SEM_NORM, meanstructure=TRUE)
summary(fit12b, fit.measures =TRUE, standardized = TRUE, rsquare=TRUE) 

modindices(fit12b, sort = TRUE, maximum.number = 10)

p_f12b<-semPaths(fit12b,"std", "est",fade = FALSE, rotation = 2, 
                 theme= "colorblind", layout = "tree3",intercepts = FALSE, 
                 residuals = FALSE, thresholds = FALSE, 
                 equalizeManifests = TRUE, 
         optimizeLatRes = TRUE, curvePivot = TRUE, structural=F,reorder=T)
my_label_list <- list(list(node="P_S11P124",to="H2O_From_F"),
                       list(node="P_S11P125_N",to="No_Protect"),
                      list(node="P_S11P125_P",to="Cons_RestF"),
                      list(node="pn_", to="slope"),
                      list(node="B_N", to="%NatF"),
                      list(node="P_S6P70", to="No_Irrg"),
                      list(node="P_S6P71_E",to="Infect"),
                      list(node="P_S6P71_P",to="Plague"),
                      list(node="P_S6P71_I",to="flood"),
                      list(node="P_S6P6",to="autoconsump"),
                      list(node="P_S7",to="#Cows"),
                      list(node="G_V",to="SigmaPrecip"))


p_f12b_2 <- change_node_label(p_f12b, my_label_list)
plot(p_f12b_2)

graph_sem(model=fit12b)

