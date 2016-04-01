#leer hatco.completo.csv
flip <-  read.delim("R-FlipEncodedData.txt", row.names=1)

#Mostramos las 6 primeras líneas del fichero de datos
head(flip)
names(flip)
#De todo el fichero, separamos las variables que utilizaremos como bases de segmentación

bases<-data.frame(flip[13:19])
descrip <- data.frame(flip[1:12])
#Mostramos las 6 primeras líneas del fichero de datos
head(bases)
head(descrip)

#Agrupamos a los clientes
bases.hclust<-hclust(dist(bases, method="euclidean"), method="ward")

#Mostramos el resultado de la agrupación
plot(bases.hclust)

#Ahora calculamos los centros de los grupos formados durante elproceso de agrupación jerárquica.
source("marketing-models.R")

#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.bases<-centros.hclust(bases.hclust, bases, 4)
centros.bases

#Dividimos la muestra con kmeans
bases.kmeans4<-kmeans(bases, centros.bases)
names(bases.kmeans4)
t(bases.kmeans4$centers)
bases.kmeans4$size

#Descriptores
names(descrip)
install.packages("tableone")
library(tableone)
listVars <- c("BS.Reputation1", "BS.Reputation2", "TWorkExperience", "RWorkExperience", "MaSpecialization1", "MaSpecialization2", "MiSpecialization1", "MiSpecialization2", "UGDegree1", "UGDegree2","PercentileClass1","PercentileClass2")

flip$segmento3<-bases.kmeans3$cluster
flip.descriptores <- CreateTableOne(vars = listVars, data = flip,  strata = "segmento3")
flip.descriptores

########
#La función discriminante: Hay que preparar los descriptores, son colineales)
names(bases)
names(descrip)
#cargamos el packete MASS que nos ofrece la función disciminante, lda()
library(MASS)
#Como argumentos tenemos que dar las bases de segmentación y la clasificacion que hemos realizado
names(descrip)
cor(descrip)
descrip2<-descrip[,-7]
head(descrip2)
flip.des.lda<-lda(descrip2, bases.kmeans3$cluster)
summary(flip.des.lda)
flip.des.lda
plot(flip.des.lda, dimen=2)
?lda
lda.arrows(flip.des.lda)

#Comprobamos la calidad dela prediccion realizada por la función discriminante

options(digits=4)
flip.des.predict<-predict(flip.des.lda, descrip2)$class
table(segmento=bases.kmeans2$cluster, lda=flip.des.predict)

#para interpretar el significado de las funciones discriminantes
#estimamos su correlación con los descriptores

flip.des.puntos<-predict(flip.des.lda, descrip2)$x
cor(descrip, flip.des.puntos)
#Ahora vamos a calcular los valores medios de las variables en los grupos
#Observemos primero los valores medios. Con esta información ya sería suficiente 
#para caracterizar a los grupos. Su representación en el espacio de las funciones 
#discriminante nos permiten visualizar la tabla de medias y ver su asociación con los grupos.
t(flip.des.lda$means)
#Para visualizar los segmentos
#También podemos representar gráficamente los centros de los grupos en el espacio de las funciones discriminantes. 
#Para ello tenemos, primero, la función predict que toma como argumento un objeto de la clase lda y utilizar 
#las funciones discriminantes obtenidas para predecir los valores de un conjunto de valores, 
#concretamente nos interesa conocer la puntuación de las medias en el espacio de las funciones discriminantes.
predict(flip.des.lda, flip.des.lda$means)$x
#Vamos ya podemos representar visualmente la caracterización de los grupos. 
#Para ello utilizamos la función biplot con dos grupos de datos, 
#la puntuación de los centros de los grupos en las funciones discriminantes 
#y la correlación de las variables discriminantes con las funciones discriminantes.
flip.des.cor<-cor(descrip2, flip.des.puntos)

biplot(predict(flip.des.lda, flip.des.lda$means)$x, flip.des.cor)
################