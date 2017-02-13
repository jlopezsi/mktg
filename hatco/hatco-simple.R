#leer hatco.completo.csv
hatco <- read.csv2("hatco.completo.csv")
#load("hatco.RData")
#Mostramos las 6 primeras líneas del fichero de datos
head(hatco)

#De todo el fichero, separamos las variables que utilizaremos como bases de segmentación

bases<-data.frame(hatco[1:7])
#Mostramos las 6 primeras líneas del fichero de datos
library(ggplot2)
qplot(bases$DELSPEED)
qplot(bases$PRICELEV)
qplot(bases$PRICEFLE)
qplot(bases$MANUFIMA)
qplot(bases$SERVICE)
qplot(bases$SALESFOR)
qplot(bases$PRODUCTQ)

head(bases)
plot(bases)

#Exploramos la heterogeneidad

bases.hclust<-hclust(dist(bases, method="euclidean"), method="ward")
#Mostramos el resultado de la agrupación
plot(bases.hclust)
rect.hclust(bases.hclust, k=2, border="red") 
#Ahora calculamos los centros de los grupos formados durante elproceso de agrupación jerárquica.
source("marketing-models.R")

#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.bases<-centros.hclust(bases.hclust, bases, 2)
centros.bases

#Dividimos la muestra con kmeans
bases.kmeans2<-kmeans(bases, centros.bases)

#Para caracterizar a los segmentos utilizamos las medias de las variables
#originales en los segmentos formados
names(bases.kmeans2)
#[1] "cluster"      "centers"      "totss"        "withinss"    
#[5] "tot.withinss" "betweenss"    "size"         "iter"        
#[9] "ifault" 
#el objeto centers contiene la información que buscamos si hemos segmentado con las variables originales
bases.kmeans2$centers
#  DELSPEED PRICELEV PRICEFLE MANUFIMA  SERVICE SALESFOR PRODUCTQ
#1 4.382692 1.580769 8.900000 4.925000 2.957692 2.525000 5.903846
#2 2.575000 3.212500 6.804167 5.597917 2.870833 2.816667 8.127083

#Identificamos a los componentes de los grupos
#Para ello utilizamos las variables descriptoras de los segmentos
#The TableOne package is created by Kazuki Yoshida and Justin Bohn and is used to create the Table 1 in R. The ReporteRs package is created by David Gohel and in this post I use for exporting Table from R to Microsoft Word

names(hatco)
#install.packages("tableone")
library(tableone)

#Descriptores de los segmentos

library(tableone)
hatco$segmento <-bases.kmeans2$cluster
listVars <- c("TAMEMP", "USAGELEV", "SATISFLE", "ESPCOMPR", "ESTRCOMP", "INDUSTRI", "SITCOMP")
catVars<- c("TAMEMP", "ESPCOMPR", "ESTRCOMP", "INDUSTRI", "SITCOMP")
hatco.descriptores <- CreateTableOne(vars = listVars, data = hatco, factorVars = catVars, strata = "segmento")
hatco.descriptores <- print(hatco.descriptores)
class(hatco.descriptores)

