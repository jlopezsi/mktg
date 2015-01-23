#leer fichero pdaBasDes2.txt
pda<-read.table(file.choose(),  header = TRUE)
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
centros.pda.bases.norm<-tapply(pda.bases.norm, list(rep(cutree(pda.bases.norm.hclust, 4), ncol(pda.bases.norm)),col(pda.bases.norm)),mean)
options(digits=2)
centros.pda.bases.norm
pda.bases.norm.kmeans4<-kmeans(pda.bases.norm, centros.pda.bases.norm)
names(pda.bases.norm.kmeans4)
tamaño.segmentos<-pda.bases.norm.kmeans4$size
tamaño.segmentos
tamaño.muestra<-sum(tamaño.segmentos)
tamaño.segmentos.p<-100*(tamaño.segmentos/tamaño.muestra)
tamaño.segmentos.p
names(pda.bases.norm.kmeans4)
names(pda.bases)
aggregate(pda.bases,by=list(pda.bases.norm.kmeans4$cluster),FUN=min)
aggregate(pda.bases,by=list(pda.bases.norm.kmeans4$cluster),FUN=mean)


centros.pda.bases.norm3<-tapply(pda.bases.norm, list(rep(cutree(pda.bases.norm.hclust, 3), ncol(pda.bases.norm)),col(pda.bases.norm)),mean)
options(digits=2)
pda.bases.norm.kmeans3<-kmeans(pda.bases.norm, centros.pda.bases.norm3)
names(pda.bases.norm.kmeans3)
pda.bases.norm.kmeans3$tot.withinss

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


#Veamos, ahora, una comparación con el resultado que obtendríamos si no hubieramos normalizado las variables
pda.bases.kmeans4<-kmeans(pda.bases, 4)
table(Datos_acp=pda.bases.puntos.kmeans4$cluster, Datos_bases=pda.bases.kmeans4$cluster)
#pda$segmento<-pda.bases.puntos.kmeans4$cluster

# Media de las variables originales en los clusters
aggregate(pda.bases,by=list(pda.bases.norm.kmeans4$cluster),FUN=mean)
aggregate(pda.bases,by=list(pda.bases.norm.kmeans3$cluster),FUN=mean)
aggregate(pda.bases,by=list(pda.bases.norm.kmeans3$cluster),FUN=min)

#Identificar a los individuso que forman los clusters
aggregate(pda[,-c(1:11)],by=list(pda.bases.norm.kmeans4$cluster),FUN=mean)

#La funciokn discriminante
names(pda)
pda.des<-pda[,-c(1:11)]
names(pda.des)
#cargamos el packete MASS que nos ofrece la función disciminante, lda()
library(MASS)
#Como argumentos tenemos que dar las bases de segmentación y la clasificacion que hemos realizado
pda.des.lda<-lda(pda.des, pda.bases.norm.kmeans4$cluster)

summary(pda.des.lda)
pda.des.lda
plot(pda.des.lda, dimen=2)
?lda
lda.arrows <- function(x, myscale = 1, tex = 0.75, choices = c(1,2), ...){
  ## adds `biplot` arrows to an lda using the discriminant function values
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], ...)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex)
}
lda.arrows(pda.des.lda)
detach(pda.completo)

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
#Componentes principales
cor(pda.bases.norm)
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
pda.bases.scores<-pda.bases.prcomp$
  cor(pda.bases, pda.bases.puntos)

#Rehacemos el proceso con los components principales
pda.bases.puntos.hclust<-hclust(dist(pda.bases.puntos), method="ward")
plot(pda.bases.puntos.hclust)
centros.pda.bases.puntos<-tapply(pda.bases.puntos, list(rep(cutree(pda.bases.puntos.hclust, 4),ncol(pda.bases.puntos)),col(pda.bases.puntos)),mean)
centros.pda.bases.puntos
pda.bases.puntos.kmeans4<-kmeans(pda.bases.puntos, centros.pda.bases.puntos)
table(Datos_nomalizados=pda.bases.norm.kmeans4$cluster, datos_acp=pda.bases.puntos.kmeans4$cluster)
