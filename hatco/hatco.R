#leer hatco.completo.csv
hatco <- read.csv(file.choose(), sep=";", dec=",")

#Mostramos las 6 primeras líneas del fichero de datos
head(hatco)

#De todo el fichero, separamos las variables que utilizaremos como bases de segmentación
bases<-data.frame(hatco[1:7])

#Mostramos las 6 primeras líneas del fichero de datos
head(bases)

#Comprobar que las bases están medidas en la misma escala
#si no lo estuvieran, entonces normalizar las bases de segmentación
#utilizando la función scale()
#bases.norm<-scale(bases)

#Agrupamos a los clientes
bases.hclust<-hclust(dist(bases, method="euclidean"), method="ward")
#Mostramos el resultado de la agrupación
plot(bases.hclust)
#Ahora calculamos los centros de los grupos formados durante elproceso de agrupación jerárquica.
centros.bases<-tapply(as.matrix(bases), list(rep(cutree(bases.hclust, 2), ncol(as.matrix(bases))),col(as.matrix(bases))),mean)
#Visualizamos el resultado
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

table(hatco$TAMEMP, bases.kmeans2$cluster)
#Como USAGELEV es una variable metrica podemos calcular las medias 
#con la función t.test() o con la función lm()
t.test(hatco$USAGELEV ~ bases.kmeans2$cluster)
summary(lm(hatco$USAGELEV ~ bases.kmeans2$cluster))
#Y ver las medias en un gráfico
boxplot(hatco$USAGELEV ~ bases.kmeans2$cluster)
#para las variables categóricas utilizamos la función table()
table(hatco$ESPCOMPR, bases.kmeans2$cluster)
table(hatco$ESTRCOMP, bases.kmeans2$cluster)
table(hatco$INDUSTRI, bases.kmeans2$cluster)
table(hatco$SITCOMP, bases.kmeans2$cluster)

#Ahora compropbariamos si el resultado obtenido con las bases de segmetnacion
#originales varia cuando las transformamos en sus componentes principales

####Comprobar que la correlación entre las bases de segmentación
#no suponga un problema para la segmentacion
#Visualizamos las correlaciones entre las bases de segmentación
cor(bases)
#Si la correlación es elevada, transformamos las bases de segmentación
#en unas nuevas variables 
#Ahora vamos a calcular los componentes principales para comprobar si el resultado cambia
bases.acp<-princomp(bases, cor=T)
#Podemos visualizar la varianza explicada por cada componente principal
#al utilizar las correlación (acotadas entre -1 y 1) la variacion explicada  por
#los componentes principales es igual al número de variables originales
#y la varianza explicada por cada componentes estará entre 0 y 7
plot(bases.acp)
summary(bases.acp)
#vemos que con cuatro cuatro componentes explicamos el 90% de la variación
#Utilizamos la puntuación de los clientes en los nuevos componentes principales
#Esa puntuación está recogida en el objeto scores de la lista bases.acp
#La asignamos al objeto bases.puntos

bases.puntos<-bases.acp$scores
#si queremos visualizar el resultado de la clasificacion que hemos realizado antes
#podemos utilizar los componentes de esta forma:

plot(bases.puntos[,1:2], col=bases.kmeans2$cluster)

#Volvemos a realizar la agrupación jerárquica
bases.puntos.hclust<-hclust(dist(bases.puntos), method="ward")
#visualizamos la heterogeneidad y la comparamos con el la mostrada con los datos originales
plot(bases.puntos.hclust)
#Calculamos los centros de los grupos
centros.bases.puntos<-tapply(bases.puntos, list(rep(cutree(bases.puntos.hclust, 2), ncol(bases.puntos)),col(bases.puntos)),mean)
#visualizamos los centros
centros.bases.puntos
#De nuevo partimos la muestra con kmeans
bases.puntos.kmeans2<-kmeans(bases.puntos, centros.bases.puntos)
plot(bases.puntos[,1:2], col=bases.puntos.kmeans2$cluster)
#Comparamos el resultado
table(bases.kmeans2$cluster, bases.puntos.kmeans2$cluster)
bases.puntos.kmeans2
options(digits=3)
#Mostramos el valor medio de las bases de segmentación en los grupos
t(aggregate(bases, list(Segmento = bases.puntos.kmeans2$cluster), mean))

#finite mixture regression
head(bases)
head(hatco)
set.seed(2807)
library("flexmix")

Model <- FLXMRglm(SATISFLE~ DELSPEED + PRICELEV+PRICEFLE+MANUFIMA+SERVICE+SALESFOR+PRODUCTQ)
fittedModel <- stepFlexmix(y ~ 1, model = Model, nrep = 3, k = 3, data = hatco)
#fittedModel <- relabel(fittedModel, "model", "x1")
summary(refit(fittedModel))
BIC(fittedModel)
fittedModel2 <- stepFlexmix(y ~ 1, model = Model, nrep = 3, k = 2, data = hatco)
summary(refit(fittedModel2))
BIC(fittedModel2)

fittedModel4 <- stepFlexmix(y ~ 1, model = Model, nrep = 4, k = 4, data = hatco)
summary(refit(fittedModel4))
BIC(fittedModel4)

fittedModel5 <- stepFlexmix(y ~ 1, model = Model, nrep = 5, k = 5, data = hatco)
summary(refit(fittedModel5))
BIC(fittedModel4)
