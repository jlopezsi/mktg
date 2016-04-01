#leer fichero R-pacificBrandsBases.csv 
#bases<-read.table(file.choose(),  header = TRUE)

#& R-pacificBrandsDisc.csv
#descrip<-read.table(file.choose(),  header = TRUE)

bases<-read.csv2("R-pacificBrandsBases.csv", row.names=1, header = TRUE)
head(bases)
descrip<-read.csv2("R-pacificBrandsDisc.csv", row.names=1, header = TRUE)
head(descrip)

#Comprobar si es neesario normalizar los datos
#Hacer la clasificación con las bases originales y las normalizadas
bases.hclust<-hclust(dist(bases), method="ward")
plot(bases.hclust)

#Ahora calculamos los centros de los grupos formados durante elproceso de agrupación jerárquica.
source("marketing-models.R")

#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.bases<-centros.hclust(bases.hclust, bases, 4)
options(digits=2)
centros.bases

bases.kmeans4<-kmeans(bases, centros.bases)
names(bases.kmeans4)
bases.kmeans4$size
t(bases.kmeans4$center)

tamaño.segmentos<-bases.kmeans4$size
tamaño.segmentos
tamaño.muestra<-sum(tamaño.segmentos)
tamaño.segmentos.p<-100*(tamaño.segmentos/tamaño.muestra)
tamaño.segmentos.p

centros.bases3<-centros.hclust(bases.hclust, bases, 3)
options(digits=2)
centros.bases3
bases.kmeans3<-kmeans(bases, centros.bases3)
t(bases.kmeans3$center)
#Descripción segmentos

install.packages("tableone")
library(tableone)
names(descrip)
listVars <- c("Fst2Try", "Fashnista", "PplThnkMe", "PplAskMe", "FashnMzne", "NoBgDeal", "NotBld", "OnlyBest", "ExclsvShop", "WhrFrndShop", "OtherPpl", "MagznNews", "Catlg", "Internet", "Excitbl","Thrfty","Contemp","Rationl", "Yuthfl", "Orthdx", "Modst","Age", "ApparelEx",  "Income", "Edu" )
descrip$segmento<-bases.kmeans4$cluster
descriptores <- CreateTableOne(vars = listVars, data = descrip, strata = "segmento")
descriptores
descriptores <- print(descriptores)

############
######COMPROBAR LA ADECUACIÓN DE LAS BASES PARA LA SEGMENTACIÓN#######
#Ahora comprobaríamos si el resultado obtenido con las bases de segmetnacion
#originales varia cuando las transformamos en sus componentes principales
####Comprobar que la correlación entre las bases de segmentación
#no suponga un problema para la segmentacion
#Visualizamos las correlaciones entre las bases de segmentación
cor(bases)
#Bartlett’s sphericity test
#The Bartlett’s test checks if the observed correlation matrix R diverges significantly from the identity matrix (theoretical matrix under H0: the variables are orthogonal).
bartlett.test(bases)
#KMO Measure of Sampling Adequacy (MSA)
#The KMO index has the same goal. It checks if we can factorize efficiently the original variables. But it is based on another idea.
#We know that the variables are more or less correlated, but the correlation between two variables can be influenced by the others. So, we use the partial correlation in order to measure the relation between two variables by removing the effect of the remaining variables
#The KMO index compares the values of correlations between variables and those of the partial correlations. If the KMO index is high ( 1), the PCA can act efficiently; if KMO is low ( 0), the PCA is not relevant.
install.packages("psych")
library(psych)
KMO(bases)
#Componentes principales

#Componentes principales

bases.acp<-princomp(bases, cor=T)

plot(bases.acp)
biplot(bases.acp)
names(bases.acp)
bases.acp$loadings
bases.puntos<-bases.acp$scores
head(bases.puntos)
cor(bases, bases.puntos)
cor(bases.puntos)
#Rehacemos el proceso con los components principales
bases.puntos.hclust<-hclust(dist(bases.puntos), method="ward")
plot(bases.puntos.hclust)

centros.bases.puntos <- centros.hclust(bases.puntos.hclust, bases.puntos, 4)
centros.bases.puntos

bases.puntos.kmeans4<-kmeans(bases.puntos, centros.bases.puntos)


tamaño.puntos.segmentos<-bases.puntos.kmeans4$size
t(bases.puntos.kmeans4$centers)
t(aggregate(bases,by=list(bases.puntos.kmeans4$cluster),FUN=mean))

tamaño.puntos.segmentos
tamaño.puntos.muestra<-sum(tamaño.puntos.segmentos)
tamaño.puntos.segmentos.p<-100*(tamaño.puntos.segmentos/tamaño.puntos.muestra)
tamaño.puntos.segmentos.p

table(Datos_originales=bases.kmeans4$cluster, datos_acp=bases.puntos.kmeans4$cluster)


library(tableone)
names(descrip)
listVars <- c("Fst2Try", "Fashnista", "PplThnkMe", "PplAskMe", "FashnMzne", "NoBgDeal", "NotBld", "OnlyBest", "ExclsvShop", "WhrFrndShop", "OtherPpl", "MagznNews", "Catlg", "Internet", "Excitbl","Thrfty","Contemp","Rationl", "Yuthfl", "Orthdx", "Modst","Age", "ApparelEx",  "Income", "Edu" )
descrip$segACP4<-bases.puntos.kmeans4$cluster
descriptores.seg.acp4 <- CreateTableOne(vars = listVars, data = descrip, strata = "segACP4")
descriptores.seg.acp4 
descriptores.seg.acp4  <- print(descriptores.seg.acp4 )

#To help interpreting
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(bases.acp, obs.scale = 1, var.scale = 1,
         groups = as.factor(bases.puntos.kmeans4$cluster), ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

