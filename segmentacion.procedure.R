#clasificación jerárquica

ejemplo<-c(2,5,9,10,15)
ejemplo
ejemplo.dist<-dist(ejemplo)
ejemplo.dist
ejemplo.hclust<-hclust(ejemplo.dist, method="ward")
ejemplo.hclust
par(mfrow=c(1,2))
plot(ejemplo.hclust)
labels<-c("A", "B", "C", "D", "E")
ejemplo.hclust$labels<-labels
plot(ejemplo.hclust)
par(mfrow=c(1,1))

#Partición con kmeans

peces<-read.csv("peces.csv", row.names=1, header=T)
peces
peces.dist<-dist(peces)
peces.dist

#############
peces.hclust<-hclust(peces.dist, method="ward")
par(mfrow=c(1,1))
plot(peces.hclust)
par(mfrow=c(1,1))

peces.kmeans<-kmeans(peces, 3, trace=T)
?kmeans
names(peces.kmeans)
peces.kmeans$centers
peces.kmeans$cluster
peces.kmeans$iter

plot(peces[1:3], col=peces.kmeans$cluster)
points(peces[1:3])
points(peces.kmeans$centers, col=1:3, pch=8,cex=2)

#datos simulados
sim<-rbind(cbind(rnorm(100,0,0.5), rnorm(100,2,1.5)), 
           cbind(rnorm(150,3.5,0.5), rnorm(15,4,2.5)))
head(sim)
par(mfrow=c(1,2))
plot(sim)


#Partición de lal muestra con kmeans
#library(MASS)
sim.kmeans<-kmeans(sim, 2)
#eqscplot(sim, type="n", xlab="x1", ylab="x2")
plot(sim, type="n", xlab="x1", ylab="x2")
text(sim, labels=sim.kmeans$cluster)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
sim.kmeans3<-kmeans(sim, 3)
plot(sim, type="n", xlab="x1", ylab="x2")
text(sim, labels=sim.kmeans3$cluster)

sim.kmeans4<-kmeans(sim, 4)
plot(sim, type="n", xlab="x1", ylab="x2")
text(sim, labels=sim.kmeans4$cluster)
par(mfrow=c(1,1))

#ahora utilizaremos los centros para partir el mercado

sim.hclust<-hclust(dist(sim), method = "ward")
plot(sim.hclust)
centros.h<-tapply(sim, list(rep(cutree(sim.hclust, 2),ncol(sim)), col(sim)),mean)
centros.h
sim.kmeans.centros<-kmeans(sim, centros.h)

centros.h3<-tapply(sim, list(rep(cutree(sim.hclust, 3), ncol(sim)), col(sim)), mean)
centros.h3 
sim.kmeans.centros3<-kmeans(sim, centros.h3)
centros.h4<-tapply(sim, list(rep(cutree(sim.hclust, 4), ncol(sim)), col(sim)), mean)
centros.h4
sim.kmeans.centros4<-kmeans(sim, centros.h4)
table(sim.kmeans$cluster, sim.kmeans.centros$cluster)
table(sim.kmeans3$cluster, sim.kmeans.centros3$cluster)
table(sim.kmeans4$cluster, sim.kmeans.centros4$cluster)

#repetimos el análisis con el paquete cluster

library(cluster)
sim.clara<-clara(sim, 2)
sim.clara3<-clara(sim, 3)
sim.clara4<-clara(sim, 4)
par(mfrow=c(2,2))
plot(sim, type="n", xlab="x1", ylab="x2")
text(sim, labels=sim.clara$clustering)
plot(sim, type="n", xlab="x1", ylab="x2")
text(sim, labels=sim.clara3$clustering)
plot(sim, type="n", xlab="x1", ylab="x2")
text(sim, labels=sim.clara4$clustering)
par(mfrow=c(1,1))
#Alternativa para visualizar el resultado
par(mfrow=c(2,2))
clusplot(sim.clara, color=TRUE, shade=TRUE, labels=2, lines=0)
#plot(sim.clara)
clusplot(sim.clara3, color=TRUE, shade=TRUE,labels=2, lines=0)
#plot(sim.clara3)
clusplot(sim.clara4, color=TRUE, shade=TRUE,labels=2, lines=0)
par(mfrow=c(1,1))
#plot(sim.clara4)

#Comparamos los resultados

table(clara=sim.clara$clustering, kmeans=sim.kmeans.centros$cluster)
table(clara=sim.clara3$clustering, kmeans=sim.kmeans.centros3$cluster)
table(clara=sim.clara4$clustering, Kmeans=sim.kmeans.centros4$cluster)

#Ejemplos con arrestos

mydata = USArrests
mydata<- na.omit(mydata) # listwise deletion of missing
mydata.orig = mydata #save original data copy
mydata <- scale(mydata) # standardize variables

d <- dist(mydata, method = "euclidean") # distance matrix
fit<- hclust(d, method="ward")

plot(fit) # display dendogram
k1 = 2 
# eyeball the no. of clusters
# cut tree into k1 clusters
groups = cutree(fit, k=k1)
rect.hclust(fit, k=2, border="red")

# Determina el número de segmentos
#Calcula la variación dentro de los segmentos
wss<- (nrow(mydata)-1)*sum(apply(mydata,2,var))
#Calcula la variación dentro de los segmentos para 14 particiones, 
#de 2 hasta 15
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i) $withinss) 
plot(1:15, wss, type="b", xlab="Número de segmentos", ylab="wss")
# Busca un 'codo' en la distribución de la wss
# Ese 'codo' nos indicará el número adecuado de segmentos a formar, en este caso
k1=2

fit <- kmeans(mydata, k1) # k1 es el número de segmentos a formar
fit$centers 
#Obtenemos los centros
aggregate(mydata.orig,by=list(fit$cluster),FUN=mean)
# Añadimos el resultado de la partición a la base de datos
mydata0<- data.frame(mydata.orig, fit$cluster)
# Mostramso el resultado
library(cluster)
clusplot(mydata0, fit$cluster, color=TRUE, shade=TRUE,labels=2,
         lines=0)
##Componentes principales
# Examinando la correlación entre los delitos cometidos en las ciudades de los EEUU
cor(mydata)

# Principal Components Analysis
# from the correlation matrix
#fit.princomp<- princomp(mydata, cor=TRUE)
fit.prcomp <-prcomp(mydata, scale=T, retx=T)
summary(fit.prcomp)

# print variance accounted for
# pc loadings
#loadings(fit.princomp) 
fit.prcomp$rotation

# scree plot
par(mfrow=c(1,2))
plot(fit.prcomp,type="lines") 
biplot(fit.prcomp)
par(mfrow=c(1,1))

head(fit.prcomp$x[, c(1,2)])
tail(fit.prcomp$x[, c(1,2)])
cor(fit.prcomp$x[, c(1,2)])
cor(scale(mydata), fit.prcomp$x[, c(1,2)])

library(psych)
library(GPArotation)
fit.principal<- principal(mydata, nfactors=2, rotate="varimax")
fit.principal 

#Model bases clustering

library(mclust)
fit.Mclust<- Mclust(mydata)
fit.Mclust 
names(fit.Mclust)
# view solution summary
fit.Mclust$BIC 
# lookup all the options attempted
classif <- fit.Mclust$classification 
# classifiation vector
mydata1 <- cbind(mydata.orig, classif) 
# append to dataset
# draw dendrogram with red borders around the k1 clusters
mydata1[1:10,] 
#view top 10 rows
table(fit$cluster, classif)

#write.table(mydata1,file.choose()) 
#save output
fit1=cbind(classif)
rownames(fit1)=rownames(mydata)
library(cluster)
clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0)

cmeans=aggregate(mydata.orig,by=list(classif),FUN=mean); cmeans

############Identificación
getwd()
#Identificación de los individuos que forman los segmentos
#leer fichero pdaBasDes2.txt
pda<-read.table("pda/pdaBasDes2.txt",  header = TRUE)
head(pda)
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
pda.bases.norm.kmeans4$size

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
correlaciones<-cor(pda.des, pda.des.puntos)
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

biplot(predict(pda.des.lda, pda.des.lda$means)$x, pda.des.cor)

#Anexo: fichero simulado



