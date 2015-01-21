
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

