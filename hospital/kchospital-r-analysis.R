
#lecutra de datos
getwd()
#"hospital/hospital.txt"
kchospital <- read.table('hospital/hospital.txt', header=T)
#"../hospital/hospital.prefs.txt"
kcprefs <- read.table("hospital/hospital.prefs.txt", header=T)
names(kchospital)
head(kchospital)
names(kcprefs)
#Selección bases
bases <- kchospital[,c(10:20)]
names(bases)

#Aglomeración jerárquica
library(cluster)
bases.hclust<-hclust(dist(bases), method="ward")
plot(bases.hclust)

plot(bases.hclust, main="Dendrograma",ylab="distancia",xlab="objetos clasificados",frame.plot=TRUE)
rect.hclust(bases.hclust, k=3, border=2)

plot(bases.hclust, main="Dendrograma",ylab="distancia",xlab="objetos clasificados",frame.plot=TRUE)
rect.hclust(bases.hclust, k=4, border=2)
#partición
bases.kmeans3<-kmeans(bases, 3)

#Visualización del resultado

library(MASS)
bases.pca<-princomp(bases, cor=T)
bases.puntos<-predict(bases.pca)
plot(bases.puntos[,1:2], col=bases.kmeans3$cluster)
#repetimos con 4 grupos
bases.kmeans4<-kmeans(bases, 4)
plot(bases.puntos[,1:2], col=bases.kmeans4$cluster)
centros4<-tapply(as.matrix(bases), list(rep(cutree(bases.hclust, 4), ncol(as.matrix(bases))),col(as.matrix(bases))),mean)
options(digits=3)
centros4
bases.kmeans4<-kmeans(bases, centros4)
plot(bases.puntos[,1:2], col=bases.kmeans4$cluster)

##¿Existe correlación entre las variables? Cambio de variables o construcción de las nuevas bases de segmentación

cor(bases)

library(MASS)
bases.pca<-princomp(bases, cor=T)
summary(bases.pca)
bases.pca
bases.puntos<-predict(bases.pca)
bases.pca$loadings
screeplot(bases.pca)
bases.puntos<-predict(bases.pca)
eqscplot(bases.puntos, xlab="primer componente", ylab="segundo componente")


kchospital$PC1<-bases.puntos[,1]
kchospital$PC2<-bases.puntos[,2]
kchospital$PC3<-bases.puntos[,3]

#Repetimos el proceso con las nuevas bases de segmentación

bases.puntos.hclust<-hclust(dist(bases.puntos[,1:3]), method="ward")
plot(bases.puntos.hclust)

cutree(bases.puntos.hclust, 4)

bases.puntos3<-bases.puntos[,1:3]
head(bases.puntos3)

plot(bases.puntos.hclust, main="Dendrograma PCA",labels=FALSE,ylab="distancia",xlab="objetos clasificados",frame.plot=TRUE)
rect.hclust(bases.puntos.hclust, k=4, border=2)

?kmeans
bases.puntos.kmeans<-kmeans(bases.puntos3, 4)
bases.puntos.kmeans$cluster

plot(bases.puntos[,1:2], col=bases.puntos.kmeans$cluster)
points(bases.puntos.kmeans$centers, col=1:2,pch=8)

##¿Podemos mejorar la partición utilizando los centros iniciales proporcionados por el algoritmo de aglomeración jerárquica?

centros.puntos4<-tapply(bases.puntos3, list(rep(cutree(bases.puntos.hclust, 4),ncol(bases.puntos3)),col(bases.puntos3)),mean)
centros.puntos4

bases.puntos.kmeans4.c<-kmeans(bases.puntos3, centros.puntos4)

plot(bases.puntos[,1:2], col=bases.puntos.kmeans4.c$cluster)
points(bases.puntos.kmeans4.c$centers,col=1:2, pch=8)






