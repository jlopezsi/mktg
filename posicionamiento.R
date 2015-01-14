getwd()
g20per<-read.table("g20per.txt", header=TRUE)
names(g20per)
g20<-data.frame(t(g20per))
names(g20)
options(digits=1)
cor(g20)
####Hacer la descomposición signular de la matriz de correlaciones
X<-scale(g20)
X
##matriz de correlaciones, X'X
cor=X%*%t(X)
cor
#Calcular los componentes principales P or t(P)
E=eigen(cor,TRUE)
E
A<-E$vectors
A
I=A%*%t(A)
I
var<-E$values
#La puntuación en los componentes o factors principales 
newdata = P %*% X
options(digits=6)
newdata
La dsviación estandar de cada atributo de la matriz rotada
sdev = sqrt(diag((1/(dim(X)[2]-1)* P %*% A %*% t(P))))
sdev
#######
g20.pca<-prcomp(g20, cor=T)
summary(g20.pca)
names(g20.pca)
g20.pca$sdev
g20.pca$rotation
g20.pca$center
g20.pca$scale
g20.pca$X
biplot(g20.pca, pc.biplot=T, cex=0.7, ex=0.8)
g20.puntos <- predict(g20.pca)
scale(g20)
g20.puntos
##########mapas conjuntos
##analysis interno
#Leer fichero g20seg.txt
g20seg<-read.table("g20seg.txt", header=T)
g20.seg<-data.frame(t(g20seg))
names(g20.seg)

##analysis interno
#Leer fichero g20seg.txt
g20seg<-read.table("g20seg.txt", header=T)
head(g20seg)
g20.seg<-data.frame(t(g20seg))
names(g20.seg)
head(g20.seg)
g20.seg.pca <- prcomp(g20.seg, cor=TRUE)
plot(g20.seg.pca)
biplot(g20.seg.pca , pc.biplot=T, cex=0.7, ex=0.8)
biplot(g20.seg.pca , choices=c(1,3), pc.biplot=T, cex=0.7, ex=0.8)
biplot(g20.seg.pca , choices=c(2,3), pc.biplot=T, cex=0.7, ex=0.8)



