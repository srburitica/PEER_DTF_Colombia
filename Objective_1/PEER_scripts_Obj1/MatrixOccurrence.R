####################################################################

#matriz de ocurrencias
#ocurrencias=create.matrix(fdata.list,tax.name="species",locality="locality")    
#toma la lista de las especies y su ocurrencias, y las convierte en una matriz de especies (filas) y localidades (columnas).


#matriz de abundancia
#abundancias=create.matrix(fdata.list,tax.name="species",locality="locality", abund=TRUE,abund.col="abundance") 
#crea una matriz de abundancias, se trata de la misma función anterior pero ahora incluye la opción abund=TRUE y el argumento abund.col que corresponde a la columna de los valores de abundancia.
#para este ejemplo, el resultado es el mismo que data(fdaa.mat)

#matriz de coordenadas
#coordenadas=create.lats(fdata.list,loc="locality",long="longitude", lat="latitude") 
#extrae las coordenadas de los sitios, eliminando aquellas entradas que se repiten.

#write.table(MagdalenaAbundancia, file = "Matriz abundancia magdalena.txt", quote= FALSE, row.names = TRUE, col.names = NA, sep = "\t") # guarda un archivo txt

#



#ESTE CODIGO ESTA HECHO PARA FUNCIONAR CON EL ARCHIVO OCCURENCE
library(fossil)

setwd(
  "C:/Users/metor/Downloads")

#Los archivos se deben abrir primero con excel y luego volverse a guardar como texto para que el codigo funcione
#Abrir el archivo
Data <- read.delim("occurrence.txt",header=T, row.names = NULL, sep = "\t")

#filtrar por rango taxonomico
DataAnimal= Data[ which(Data$kingdom=="Animalia"), ]

unique(DataAnimal$class)

DataPlant= Data[ which(Data$kingdom=="Plantae"), ]
#Con esto se sabe que hay en cada columna
unique(DataPlant$phylum)
Plant= DataPlant

#ABUNDANCIA

Aves= DataAnimal[ which(DataAnimal$class=="Aves"), ]

#1 Crear la matriz de abundancia con locality como columna y especie como fila, abundand col como los registros de individuos

unique(Plant$locality)
Plant=create.matrix(Plant, tax.name = "scientificName", locality = "locality", abund=TRUE,abund.col="individualCount")
Plant=Plant[complete.cases(Plant),]
#1.5 Si la matriz genera una columna sin valores V1 correr este codigo para eliminar la primera columna
Plant=Plant[,-1]
#2 Usar la función "t" para cambiar las columnas y las filas. deja como filas las locality y como columnas las especies 
Plant=as.data.frame(t(Plant))
#2.5 Si solo hay una localidad, nombrar con
row.names(Plant)="EstaciÃ³n primates"

#OCURRENCIA


#1 Crear la matriz de ocurencia con locality como columna y especie como fila

Plant=create.matrix(Plant, tax.name = "scientificName", locality = "locality")
Plant=Plant[complete.cases(Plant),]
#1.5 Si la matriz genera una columna sin valores V1 correr este codigo para eliminar la primera columna
Plant=Plant[,-1]
#2 Usar la función "t" para cambiar las columnas y las filas. deja como filas las locality y como columnas las especies 
Plant=as.data.frame(t(Plant))
#2.5 Si solo hay una localidad, nombrar con
row.names(Plant)="Reserva Ciudad antigua"

#Guardar la matriz de abundancia en formato txt separado por tabs

write.table(Plant, file = "OcurrPlant.txt", quote= FALSE, row.names = TRUE, col.names = NA, sep = "\t") # guarda un archivo txt
#Abrir los archivos guardados
xd <- read.delim("OcPlantMar.txt", row.names=1)
