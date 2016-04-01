#leer fichero pdaBasDes2.txt
#pda<-read.table(file.choose(),  header = TRUE)
getwd()
pda<-read.table("pdaBasDes2.txt",  header = TRUE)
head(pda)
#leer fichero pdaBasDes2.csv
#pda.completo<-read.table(file.choose(), header = TRUE, sep = ";")
#head(pda.completo)
names(pda)
pda.bases<-pda[,-c(12:24)]
head(pda.bases)

#Hacer la clasificación con las bases originales y las normalizadas
pda.bases.hclust<-hclust(dist(pda.bases), method="ward")
plot(pda.bases.hclust)
#normalizar datos
pda.bases.norm<-scale(pda.bases)
pda.bases.norm.hclust<-hclust(dist(pda.bases.norm), method="ward")
plot(pda.bases.norm.hclust)

#Ahora calculamos los centros de los grupos formados durante elproceso de agrupación jerárquica.
source("marketing-models.R")

#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.pda.bases.norm<-centros.hclust(pda.bases.norm.hclust, pda.bases.norm, 4)
options(digits=2)
centros.pda.bases.norm

pda.bases.norm.kmeans4<-kmeans(pda.bases.norm, centros.pda.bases.norm)
names(pda.bases.norm.kmeans4)
pda.bases.norm.kmeans4$size

tamaño.segmentos<-pda.bases.norm.kmeans4$size
tamaño.segmentos
tamaño.muestra<-sum(tamaño.segmentos)
tamaño.segmentos.p<-100*(tamaño.segmentos/tamaño.muestra)
tamaño.segmentos.p

centros.pda.bases.norm3<-centros.hclust(pda.bases.norm.hclust, pda.bases.norm, 3)
options(digits=2)
centros.pda.bases.norm3
pda.bases.norm.kmeans3<-kmeans(pda.bases.norm, centros.pda.bases.norm3)


#Veamos, ahora, una comparación con el resultado que obtendríamos si no hubieramos normalizado las variables

pda.bases.kmeans4<-kmeans(pda.bases, 4)
table(Datos_norm=pda.bases.norm.kmeans4$cluster, Datos_bases=pda.bases.kmeans4$cluster)

# Media de las variables originales en los clusters
aggregate(pda.bases,by=list(pda.bases.norm.kmeans4$cluster),FUN=mean)
aggregate(pda.bases,by=list(pda.bases.norm.kmeans3$cluster),FUN=mean)

names(pda)
str(pda)
install.packages("tableone")
library(tableone)
listVars <- c("Age", "Eduaction", "Income", "Construction", "Emergency", "Sales", "Service", "Professnl", "PDA", "Bus_Week", "PC_Mag", "Fiel.Stream", "M_Gourmet")
pda$segmento<-pda.bases.norm.kmeans4$cluster
pda.descriptores <- CreateTableOne(vars = listVars, data = pda, strata = "segmento")
pda.descriptores
pda.descriptores <- print(hatco.descriptores)
####solución con 3
pda$segmento3<-pda.bases.norm.kmeans3$cluster
pda.descriptores3 <- CreateTableOne(vars = listVars, data = pda, strata = "segmento3")
pda.descriptores3




######COMPROBAR LA ADECUACIÓN DE LAS BASES PARA LA SEGMENTACIÓN#######
#Ahora comprobaríamos si el resultado obtenido con las bases de segmetnacion
#originales varia cuando las transformamos en sus componentes principales
####Comprobar que la correlación entre las bases de segmentación
#no suponga un problema para la segmentacion
#Visualizamos las correlaciones entre las bases de segmentación
cor(pda.bases)
#Bartlett’s sphericity test
#The Bartlett’s test checks if the observed correlation matrix R diverges significantly from the identity matrix (theoretical matrix under H0: the variables are orthogonal).
bartlett.test(pda.bases)
#KMO Measure of Sampling Adequacy (MSA)
#The KMO index has the same goal. It checks if we can factorize efficiently the original variables. But it is based on another idea.
#We know that the variables are more or less correlated, but the correlation between two variables can be influenced by the others. So, we use the partial correlation in order to measure the relation between two variables by removing the effect of the remaining variables
#The KMO index compares the values of correlations between variables and those of the partial correlations. If the KMO index is high ( 1), the PCA can act efficiently; if KMO is low ( 0), the PCA is not relevant.
install.packages("psych")
library(psych)
KMO(pda.bases)
#Componentes principales

cor(pda.bases)
pda.bases.acp<-princomp(pda.bases, cor=T)
plot(pda.bases.acp)
biplot(pda.bases.acp)
names(pda.bases.acp)
pda.bases.acp$loadings
pda.bases.puntos<-pda.bases.acp$scores
cor(pda.bases, pda.bases.puntos)

#prueba con la función prcomp()
?prcomp
pda.bases.prcomp<-prcomp(pda.bases, scale=T)
head(pda.bases)
plot(pda.bases.prcomp)
biplot(pda.bases.prcomp)
names(pda.bases.prcomp)
pda.bases.prcomp$rotation
pda.bases.scores<-pda.bases.prcomp$rotation
cor(pda.bases, pda.bases.puntos)

#Rehacemos el proceso con los components principales
pda.bases.puntos.hclust<-hclust(dist(pda.bases.puntos), method="ward")
plot(pda.bases.puntos.hclust)

centros.pda.bases.puntos <- centros.hclust(pda.bases.puntos.hclust, pda.bases.puntos, 4)
centros.pda.bases.puntos

pda.bases.puntos.kmeans4<-kmeans(pda.bases.puntos, centros.pda.bases.puntos)
table(Datos_nomalizados=pda.bases.norm.kmeans4$cluster, datos_acp=pda.bases.puntos.kmeans4$cluster)

###### Predictive use of findings

#La funciokn discriminante
names(pda)
pda.des<-pda[,c(12:24)]
names(pda.des)
#cargamos el packete MASS que nos ofrece la función disciminante, lda()
library(MASS)
#Como argumentos tenemos que dar las bases de segmentación y la clasificacion que hemos realizado
pda.des.lda<-lda(pda.des, pda.bases.norm.kmeans4$cluster)

summary(pda.des.lda)
pda.des.lda
plot(pda.des.lda, dimen=2)
?lda
lda.arrows(pda.des.lda)

#Comprobamos la calidad dela prediccion realizada por la función discriminante

options(digits=4)
pda.des.predict<-predict(pda.des.lda, pda.des)$class
table(segmento=pda.bases.norm.kmeans4$cluster, lda=pda.des.predict)

#para interpretar el significado de las funciones discriminantes
#estimamos su correlación con los descriptores

pda.des.puntos<-predict(pda.des.lda, pda.des)$x
cor(pda.des, pda.des.puntos)
#Ahora vamos a calcular los valores medios de las variables en los grupos
#Observemos primero los valores medios. Con esta información ya sería suficiente 
#para caracterizar a los grupos. Su representación en el espacio de las funciones 
#discriminante nos permiten visualizar la tabla de medias y ver su asociación con los grupos.
t(pda.des.lda$means)
#Para visualizar los segmentos
#También podemos representar gráficamente los centros de los grupos en el espacio de las funciones discriminantes. 
#Para ello tenemos, primero, la función predict que toma como argumento un objeto de la clase lda y utilizar 
#las funciones discriminantes obtenidas para predecir los valores de un conjunto de valores, 
#concretamente nos interesa conocer la puntuación de las medias en el espacio de las funciones discriminantes.
predict(pda.des.lda, pda.des.lda$means)$x
#Vamos ya podemos representar visualmente la caracterización de los grupos. 
#Para ello utilizamos la función biplot con dos grupos de datos, 
#la puntuación de los centros de los grupos en las funciones discriminantes 
#y la correlación de las variables discriminantes con las funciones discriminantes.
pda.des.cor<-cor(pda.des, pda.des.puntos)

biplot(predict(pda.des.lda, pda.des.lda$means)$x, pda.des.cor)
################

######### Who to impove solution #########

####3 o 4 CLUSTERS?

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
clusplot(pda.bases.norm, pda.bases.norm.kmeans4$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
#sin el paquete cluster tendríamos que hacer lo siguiente:
#pda.bases.acp<-princomp(pda.bases, cor=T)
#names(pda.bases.acp)
#pda.bases.puntos<-pda.bases.acp$scores
#plot(pda.bases.puntos[,1:2], col=pda.bases.norm.kmeans4$cluster)

pda.bases.norm.pam2<-pam(dist(pda.bases.norm), 2)
plot(pda.bases.norm.pam2)
pda.bases.norm.pam3<-pam(dist(pda.bases.norm), 3)
plot(pda.bases.norm.pam3)
pda.bases.norm.pam4<-pam(dist(pda.bases.norm), 4)
plot(pda.bases.norm.pam4)
names(pda.bases.norm.pam4)
pda.bases.norm.pam4$silinfo

