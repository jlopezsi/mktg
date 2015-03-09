
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
bases <- kchospital[,c(11:20)]
names(bases)

#Aglomeración jerárquica
#library(cluster)
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

head(cutree(bases.puntos.hclust, 4))

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

#Procedimiento paquete cluster
library(cluster)
bases.cluster<-clara(bases, 2, metric="euclidean")
plot(bases.cluster)
clusplot(bases.cluster)

bases.cluster3<-clara(bases, 3, metric="euclidean")
bases.cluster4<-clara(bases, 4, metric="euclidean")
bases.cluster5<-clara(bases, 5, metric="euclidean")

names(bases.cluster)
bases.cluster$silinfo$avg.width
bases.cluster3$silinfo$avg.width
bases.cluster4$silinfo$avg.width
bases.cluster5$silinfo$avg.width

#con componentes principales

bases.puntos.cluster<-clara(bases.puntos[,1:3],2,metric="euclidean")

bases.puntos.cluster3<-clara(bases.puntos[,1:3],3,metric="euclidean")

bases.puntos.cluster4<-clara(bases.puntos[,1:3],4,metric="euclidean")

bases.puntos.cluster5<-clara(bases.puntos[,1:3],5,metric="euclidean")

clusplot(bases.puntos.cluster)
clusplot(bases.puntos.cluster3)
clusplot(bases.puntos.cluster4)
clusplot(bases.puntos.cluster5)

bases.puntos.cluster$silinfo$avg.width
bases.puntos.cluster3$silinfo$avg.width
bases.puntos.cluster4$silinfo$avg.width
bases.puntos.cluster5$silinfo$avg.width

kchospital$clusterpca<-bases.puntos.cluster4$clustering

plot(bases.puntos[,1:2], col=bases.cluster4$clustering)
plot(bases.puntos[,1:2], col=bases.puntos.cluster4$clustering)

plot(bases.puntos[,1:2], col=bases.cluster3$clustering)
plot(bases.puntos[,1:2], col=bases.puntos.cluster3$clustering)


write.table(kchospital, "kcclas.dat", quote = FALSE, sep = ",", row.names=F)

#identificación  segmentos

names(kchospital)

library(plyr)
library(dplyr)
descriptores<-kchospital[,c(43:47)]
head(descriptores) 

class(descriptores$SEXO)
descriptores$SEXO<-as.factor(descriptores$SEXO)
summary(descriptores$SEXO)

class(descriptores$ESTADOMA)
descriptores$ESTADOMA<-as.factor(descriptores$ESTADOMA)
summary(descriptores$ESTADOMA)

class(descriptores$EDAD)
descriptores$EDAD<-as.factor(descriptores$EDAD)
summary(descriptores$EDAD)

class(descriptores$NIVELEDU)
descriptores$NIVELEDU<-as.factor(descriptores$NIVELEDU)
summary(descriptores$NIVELEDU)

class(descriptores$INGRESOS)
descriptores$INGRESOS<-as.factor(descriptores$INGRESOS)
summary(descriptores$INGRESOS)

descriptores$SEXO <- revalue(descriptores$SEXO, c("0" ="Mujer", "1" = "Hombre"))
descriptores$ESTADOMA <- revalue(descriptores$ESTADOMA, c("0" ="Casado", "10"="Soltero", "1" = "Otro"))
descriptores$EDAD <- revalue(descriptores$EDAD, c("0" ="<=25 años", "1"="55+", "10"="41-55", "100" = "26-40"))
descriptores$NIVELEDU <- revalue(descriptores$NIVELEDU, c("0" ="ESO", "1"="Graduado Univ.", "10"="Bachillerato +", "100" = "Bachillerato"))
descriptores$INGRESOS <- revalue(descriptores$INGRESOS, c("0" ="<=$100000", "1"="$60000+", "10"="$40001-60000", "100" = "$30001-40000", "1000"="$20001-30000", "10000"= "$10001-20000"))

#Mostrar datos según segmento
#descriptores %>%
#  group_by(bases.puntos.cluster4$clustering) %>% #no es necesario especificar los descriptores
#  summarize(Sexo= mean(SEXO), Estado.Marital = mean(ESTADOMA), Edad = mean(EDAD), Nivel.Educativo = mean(NIVELEDU), N = length(descriptores))

options(digits=3)
descritores.tabla.sexo<-t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$SEXO)))
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$ESTADOMA)))
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$EDAD)))
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$NIVELEDU)))
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$INGRESOS)))
library(xtable)
xtabs(bases.puntos.cluster4$clustering, descriptores$ESTADOMA)

# Revisar, no funciona
descriptores.tabla <-rbind(c(
  t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$SEXO))),
  t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$ESTADOMA))),
  t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$EDAD))),
  t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$NIVELEDU))),
  t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$INGRESOS)))
  )
  )
descriptores.tabla

descriptores.tabla<- list( 
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$SEXO))),
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$ESTADOMA))),
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$EDAD))),
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$NIVELEDU))),
t(prop.table(table(bases.puntos.cluster4$clustering, descriptores$INGRESOS)))
)
descriptores.tabla[2]
do.call(rbind, descriptores.tabla)

library(DiagrammeR)
diagrama.causalidad <-DiagrammeR("
  graph BT;
    Cuidado.especializado --> Ventaja.competitiva;
    Ventaja.competitiva --> Reconocimiento;
    Reconocimiento -->Preferencia;
    Preferencia --> Tasa.de.ocupacion;
    Tasa.de.ocupacion--> Beneficio;
")
diagrama.causalidad

names(kchospital)
hospital.utilizado <- kchospital[,c(2:10)]
str(hospital.utilizado)
hospital.utilizado %>%
summarize(Kc= sum(KCMATER), B = sum(HB), C = sum(HC), D = sum(HD))

