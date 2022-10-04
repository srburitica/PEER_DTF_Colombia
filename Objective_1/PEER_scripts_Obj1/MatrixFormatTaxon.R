#ESTE CODIGO ESTA HECHO PARA FUNCIONAR CON EL ARCHIVO TAXON Y DESCRIPTION
library(fossil)

setwd(
  "C:/Users/metor/Downloads")

taxon= read.delim("taxon.txt",header=T, row.names = NULL, sep = "\t")
desc= read.delim("distribution.txt",header=T, row.names = NULL, sep = "\t")
description=desc$description
location=desc$locality
Data=cbind(taxon,location)

#filtrar por rango taxonomico
DataAnimal= Data[ which(Data$kingdom=="Animalia"), ]
#Con esto se sabe que hay en cada columna
unique(DataAnimal$class)

DataPlantae= Data[ which(Data$kingdom=="Plantae"), ]
Plant= DataPlantae
unique(Plant$phylum)


Mammalia= DataAnimal[ which(DataAnimal$class=="Mammalia"), ]


#OCURRENCIA
unique(Plant$location)
#1 Crear la matriz de ocurencia con locality como columna y especie como fila
Plant=create.matrix(Plant, tax.name = "scientificName", locality = "location")
Plant=Plant[complete.cases(Plant),]
#1.5 Si la matriz genera una columna sin valores V1 correr este codigo para eliminar la primera columna
#Plant=Plant[,-1]
#2 Usar la función "t" para cambiar las columnas y las filas. deja como filas las locality y como columnas las especies 
Plant=as.data.frame(t(Plant))
#2.5 Si solo hay una localidad, nombrar con
row.names(Plant)="CARIBE COLOMBIANO"
colnames(Plant)="Sphagnum"


#ABUNDANCIA

#1 Crear la matriz de abundancia con locality como columna y especie como fila, abundand col como los registros de individuos
Plant=create.matrix(Plant, tax.name = "scientificName", locality = "description", abund=TRUE,abund.col="individualCount")
Plant=Plant[complete.cases(Plant),]
#1.5 Si la matriz genera una columna sin valores V1 correr este codigo para eliminar la primera columna
Plant=Plant[,-1]
#2 Usar la función "t" para cambiar las columnas y las filas. deja como filas las locality y como columnas las especies 
Plant=as.data.frame(t(Plant))
#2.5 Si solo hay una localidad, nombrar con
row.names(Plant)="EstaciÃ³n primates"


#Guardar la matriz de abundancia en formato txt separado por tabs

write.table(Plant, file = "OcurrPlant.txt", quote= FALSE, row.names = TRUE, col.names = NA, sep = "\t") # guarda un archivo txt
#Abrir los archivos guardados
xd <- read.delim("OcurrPlantTotal.txt", row.names=1)

