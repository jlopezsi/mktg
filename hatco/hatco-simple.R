#leer hatco.completo.csv
hatco <- read.csv2("hatco.completo.csv")

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
source("marketing-models.R")

#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.bases<-centros.hclust(bases.hclust, bases, 2)
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
#The TableOne package is created by Kazuki Yoshida and Justin Bohn and is used to create the Table 1 in R. The ReporteRs package is created by David Gohel and in this post I use for exporting Table from R to Microsoft Word

names(hatco)
install.packages("tableone")
library(tableone)
listVars <- c("TAMPEMP", "USAGELEV", "SATISFLE", "ESPCOMPR", "ESTRCOMP", "INDUSTRI", "SITCOMP")
catVars<- c("TAMPEMP", "ESPCOMPR", "ESTRCOMP", "INDUSTRI", "SITCOMP")
hatco$segmento<-bases.kmeans2$cluster
hatco.descriptores <- CreateTableOne(vars = listVars, data = hatco, factorVars = catVars, strata = "segmento")
hatco.descriptores
hatco.descriptores <- print(hatco.descriptores)

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

#Si la correlación es elevada, transformamos las bases de segmentación
#en unas nuevas variables 
#Ahora vamos a calcular los componentes principales para comprobar si el resultado cambia
bases.acp<-princomp(bases, cor=T)

#Podemos visualizar la varianza explicada por cada componente principal
#al utilizar las correlación (acotadas entre -1 y 1) la variacion explicada  por
#los componentes principales es igual al número de variables originales
#y la varianza explicada por cada componentes estará entre 0 y 7

#vemos que con cuatro cuatro componentes explicamos el 90% de la variación
#Utilizamos la puntuación de los clientes en los nuevos componentes principales
#Esa puntuación está recogida en el objeto scores de la lista bases.acp
#La asignamos al objeto bases.puntos

bases.puntos<-bases.acp$scores
#si queremos visualizar el resultado de la clasificacion que hemos realizado antes
#podemos utilizar los componentes de esta forma:

cor(bases.puntos[,1:3], bases)

plot(bases.puntos[,1:2], col=bases.kmeans2$cluster)

#Volvemos a realizar la agrupación jerárquica
bases.puntos.hclust<-hclust(dist(bases.puntos), method="ward")
#visualizamos la heterogeneidad y la comparamos con el la mostrada con los datos originales
plot(bases.puntos.hclust)
#Calculamos los centros de los grupos
centros.bases.puntos<- centros.hclust(bases.puntos.hclust, bases.puntos, 2)
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
names(bases.puntos.kmeans2)
bases.puntos.kmeans2$centers
t(aggregate(bases, list(Segmento = bases.puntos.kmeans2$cluster), mean))

hatco$cluster <- bases.puntos.kmeans2$cluster
table(hatco$segmento, hatco$cluster)
#Descriptores de los segmentos

library(tableone)
listVars <- c("TAMEMP", "USAGELEV", "SATISFLE", "ESPCOMPR", "ESTRCOMP", "INDUSTRI", "SITCOMP")
catVars<- c("TAMEMP", "ESPCOMPR", "ESTRCOMP", "INDUSTRI", "SITCOMP")
hatco.descriptores <- CreateTableOne(vars = listVars, data = hatco, factorVars = catVars, strata = "cluster")
hatco.descriptores <- print(hatco.descriptores)

########OPTATIVO##############

##OPTATIVO
install.packages("dplyr")
library(dplyr)
bases.d<-select(hatco, DELSPEED:PRODUCTQ)
names(bases.d)


#OPTATIVO
install.packages("ggdendro")
library(ggdendro)
ggdendrogram(bases.hclust, rotate = FALSE, size = 2)


#OPTATIVO con dplyr
names(bases)
head(rep(cutree(bases.hclust, 2)))
centros.bases.d<- bases %>%
  group_by(rep(cutree(bases.hclust, 2))) %>%
  summarise(
    n=n(),
    DELSPEED=mean(DELSPEED),
    PRICELEV=mean(PRICELEV),
    PRICEFLE=mean(PRICELEV),
    MANUFIMA=mean(MANUFIMA),
    SERVICE=mean(SERVICE),
    SALESFOR=mean(SALESFOR),
    PRODUCTQ=mean(PRODUCTQ)
  )

#####OPTATIVO
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
###OPTATIVO PARA MEJORAR LA TABLA DE DESCRIPTORES
# Load the packages 
library(ReporteRs) 
library(magrittr)
docx( ) %>% addFlexTable(hatco.descriptores %>% FlexTable(header.cell.props = cellProperties( background.color = "#003366"), header.text.props = textBold( color = "white" ), add.rownames = TRUE ) %>% setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>% writeDoc(file = "table1.docx")

######optativo para mejorar la tabla de descriptores
# Load the packages 
library(ReporteRs) 
library(magrittr)
docx( ) %>% addFlexTable(hatco.descriptores %>% FlexTable(header.cell.props = cellProperties( background.color = "#003366"), header.text.props = textBold( color = "white" ), add.rownames = TRUE ) %>% setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>% writeDoc(file = "table1.docx")


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
?`BIC`
