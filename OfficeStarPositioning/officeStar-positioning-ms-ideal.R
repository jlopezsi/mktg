##Reading data OfficeStar_per.txt
source("marketing-models.R")

os_per <- read.table("OfficeStar_per.txt", sep = "\t", dec = "," , row.names=1, header=T)
head(os_per)
perceptions<-t(os_per) #view the table read
head(perceptions)

#read prferences OfficeStar_prefs
preferences.segments <- read.table("OfficeStar_prefs.txt", sep = "\t", dec = "," , row.names=1, header=T)
head(preferences.segments)
preferences<-preferences.segments[,-5]
head(preferences)

########plot the map of perceptions (the structure of the market)
#Compute principal components analysis with prcomp (princomp doesn't alow to perform pca with less rows than colomnes)
perceptions.pca<-prcomp(perceptions, cor=T)
names(perceptions.pca)
#Check the findings, how pc are produced from original indicators
perceptions.pca
plot(perceptions.pca)
#Check the variation explained by principal components
summary(perceptions.pca)
#Check all objects available in g20t.pca objects
names(perceptions.pca)
perceptions.puntos<-predict(perceptions.pca)
head(perceptions.puntos)
#Interpret the meaning of the principal components
cor(perceptions, perceptions.puntos)
#plot pc using biplot function
biplot(perceptions.pca, pc.biplot=T)
?biplot #seach for help
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model from an external analysis##########
#we use the correlation between the preferences data frame and the pc plotted
dim(t(preferences))
dim(perceptions.puntos)

preferences.puntos<-cor(t(preferences), perceptions.puntos)
#add preferences as arrows to the biplot
arrows(0, 0, preferences.puntos[,1], preferences.puntos[,2]) 
#add the indentity of consumers
text (preferences.puntos[,1:2], labels = row.names(preferences))
#####
addVectorialPrefs(perceptions.puntos, preferences, 1, 2)

#####repeat the process again for pc1 and pc3
biplot(perceptions.pca, choice=c(1,3))
preferences.puntos<-cor(t(preferences), perceptions.puntos)
arrows(0, 0, preferences.puntos[,1], preferences.puntos[,3]) 
text (preferences.puntos[,1:3], labels = row.names(preferences))

addVectorialPrefs(perceptions.puntos, preferences, 1, 3)

#And for compontens 2 and 3
biplot(perceptions.pca, choice=c(2,3))
preferences.puntos<-cor(t(preferences), perceptions.puntos)
arrows(0, 0, preferences.puntos[,2], preferences.puntos[,3]) 
text (preferences.puntos[,2:3], labels = row.names(preferences))

addVectorialPrefs(perceptions.puntos, preferences, 2, 3)

####Segment preferences and plot arrows' colours  according to the classification
pref.hclust <- hclust(dist(preferences), method="ward")
plot(pref.hclust)

#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.pref2<-centros.hclust(pref.hclust, preferences, 2)
options(digits=2)
centros.pref2
pref.kmeans2<-kmeans(preferences, centros.pref2)
names(pref.kmeans2)
pref.kmeans2$size
#First plot
biplot(perceptions.pca, pc.biplot=T)
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
#preferences.puntos<-cor(t(preferences), perceptions.puntos)
#add preferences as arrows to the biplot
#arrows(0, 0, preferences.puntos[,1], preferences.puntos[,2]) 
#add the indentity of consumers
#text (preferences.puntos[,1:2], labels = row.names(preferences))


########add preferences from a verctor model##########
biplot(perceptions.pca, pc.biplot=T)
?biplot
names(perceptions.pca)
abline(h=0); abline(v=0)
#identify individuals classified in each segment
arrows(0, 0, preferences.puntos[,1], preferences.puntos[,2], col=pref.kmeans2$cluster) 
#points(pref.puntos[,1:2]) #mirar de graficar en color el segmento
text (preferences.puntos[,1:2], labels = row.names(preferences))
addVectorialPrefs(perceptions.puntos, preferences, 1, 2, pref.kmeans2$cluster)

#We can add the clusters' means instead of the individual preferences
pref.segments <- pref.kmeans2$centers
pref.segments
biplot(perceptions.pca, pc.biplot=T)
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 
########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
pref.segments.puntos<-cor(t(pref.segments), perceptions.puntos)
head(pref.segments.puntos)
#add preferences as arrows to the biplot
arrows(0, 0, pref.segments.puntos[,1], pref.segments.puntos[,2], col='green') 
#add the indentity of consumers
text (pref.segments.puntos[,1:2], labels = row.names(pref.segments))
addVectorialPrefs(perceptions.puntos, pref.segments, 1, 2, pref.kmeans2$cluster)

###########some extra code to plot beatiful joint maps
#library(devtools)
#install_github("vqv/ggbiplot")

######biplot with 
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
###ggplot2 needs a data.frame as argument
preferences.puntos<-as.data.frame(preferences.puntos)
names(preferences.puntos)
ggscreeplot(perceptions.pca)
os.ggbiplot<-ggbiplot(perceptions.pca,  labels=rownames(perceptions),choice=c(1,2))
os.ggbiplot + geom_segment(data = preferences.puntos,
                           aes(x = 0, y = 0, xend = PC1, yend = PC2 ),
                           arrow = arrow(length = unit(1/2, 'picas')), 
                           color = muted('green')) + 
  geom_text(data=preferences.puntos, aes(PC1, PC2, label = row.names(preferences)), size=4)

###Add the results of clustering preferences

os.ggbiplot<-ggbiplot(perceptions.pca,  labels=rownames(perceptions),choice=c(1,2))
os.ggbiplot + geom_segment(data = preferences.puntos,
                           aes(x = 0, y = 0, xend = PC1, yend = PC2),
                           arrow = arrow(length = unit(1/2, 'picas')), 
                           color = pref.kmeans2$cluster) + 
  geom_text(data=preferences.puntos, aes(PC1, PC2, label = row.names(preferences)), size=4)
####################################################
JointVectorialMap(perceptions, preferences, 1, 2, pref.kmeans2$cluster)

##############Market shares #######################
#Vectorial preferences. market share in the segments. 
#Rule: share of preferences (frequent buy)
dim(perceptions.puntos) #Check matrix dimensions
dim(pref.segments.puntos) #Check matrix dimensions
utilidad<-perceptions.puntos %*% t(pref.segments.puntos) #Projections of brands on individual preferences
utilidad 
utilidad.exp <- exp(utilidad)
utilidad.exp
suma.utilidad.exp<-colSums(utilidad.exp)
probalidad.segmentos<-utilidad.exp/suma.utilidad.exp
probalidad.segmentos
msSharePrefs(perceptions.puntos, pref.segments.puntos)

#market.share in individuals
#regla de la cuota de preferencia
dim(perceptions.puntos)
dim(preferences.puntos)
utilidad.ind<-perceptions.puntos %*% t(preferences.puntos) #brand projection on individuals' prefererences
head(utilidad.ind) #shows preferences of individuals about brands
utilidad.ind.exp <- exp(utilidad.ind)
head(utilidad.ind.exp)
suma.utilidad.ind.exp<-colSums(utilidad.ind.exp)
probalidad.ind<-utilidad.ind.exp/suma.utilidad.ind.exp
head(probalidad.ind)
rowMeans(probalidad.ind) #market share of brands
sum(rowMeans(probalidad.ind)) #Check that they sum to 1
#All in a function
msSharePrefs(perceptions.puntos, preferences.puntos)

#regla de la primera elección
dim(perceptions.puntos)
dim(preferences.puntos)
utilidad.ind<-perceptions.puntos %*% t(preferences.puntos) #brand projection on individuals' prefererences
head(utilidad.ind) #shows preferences of individuals about brands
individualElections <- apply(utilidad.ind, 2, which.max) # cols. identificamos la máxima utilidad
individualElections #visualizamos el resultado
length(individualElections[individualElections==1]) #sumamos para cada marca los inviduos que la prefieren
length(individualElections[individualElections==2])
length(individualElections[individualElections==3])
length(individualElections[individualElections==4])
utilidad.ind <- as.data.frame(utilidad.ind) #transformamos la matriz en un base datos para poder añadir las cuotas de mercado
utilidad.ind
ms<-vector() #creamos el vector de cuotas de mercado
ms[1]<-length(individualElections[individualElections==1])/length(individualElections) #calculamos la cuota de mercado para la marca 1 (OfficeStar)
ms[2]<-length(individualElections[individualElections==2])/length(individualElections)
ms[3]<-length(individualElections[individualElections==3])/length(individualElections)
ms[4]<-length(individualElections[individualElections==4])/length(individualElections)
ms
utilidad.ind$ms<-ms
utilidad.ind
#all in one function
msFirstElectionPrefs(perceptions.puntos, preferences.puntos)

######################Ideal preferences
#read prferences officeStar_ideal
preferences.ideal <- read.table("OfficeStar_ideal.txt", sep = "\t", dec = "," , row.names=1, header=T)
head(preferences.ideal) #visualizar preferencias ideales
head(perceptions) #visualizar percepciones
names(preferences.ideal) <- c("Large choice", "Low prices", "Service quality", "Product quality", "Convenience") #homogeneizamos los nombres de las propiedades de los productos
head(preferences.ideal) #visualizar
#add preferences as arrows to the biplot
biplot(perceptions.pca, pc.biplot=T)
#?biplot #seach for help
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

preferences.ideal.puntos<-predict(perceptions.pca, preferences.ideal)
points(preferences.ideal.puntos[,1], preferences.ideal.puntos[,2]) 
#add the indentity of consumers
text (preferences.ideal.puntos[,1:2], labels = row.names(preferences.ideal))
addIdealPrefs(perceptions.pca, preferences.ideal, 1, 2)

JointIdealMap(perceptions, preferences.ideal, 1, 2)
#market.share ideal preferences in individuals
#######completed
###elección de la marca más preferida
dim(perceptions.puntos) #comprobamos las dimensiones de las matrices
dim(preferences.ideal.puntos) #comprobamos las dimensiones de las matrices
install.packages("pdist") #paquete para calcular distancias entre dos matrices
library(pdist) #cargamos el paquete
utilidad.ideal.ind<-pdist(perceptions.puntos,  preferences.ideal.puntos)  #distances among brnads and individuals
rownames(perceptions.puntos)
rownames(preferences.ideal.puntos)
utilidad.ideal.ind <- as.matrix(utilidad.ideal.ind) #transform the data type
rownames(utilidad.ideal.ind) <- rownames(perceptions.puntos) #asignamos nombres a filas y columnas de la matriz
colnames(utilidad.ideal.ind) <- rownames(preferences.ideal.puntos) #asignamos nombres a filas y columnas de la matriz
utilidad.ideal.ind
individualElections <- apply(utilidad.ideal.ind, 2, which.min) # cols. identificamos la mínima distancia
individualElections #visualizamos el resultado
length(individualElections[individualElections==1]) #sumamos para cada marca los inviduos que la prefieren
length(individualElections[individualElections==2])
length(individualElections[individualElections==3])
length(individualElections[individualElections==4])
utilidad.ideal.ind <- as.data.frame(utilidad.ideal.ind) #transformamos la matriz en un base datos para poder añadir las cuotas de mercado
utilidad.ideal.ind
ms<-vector() #creamos el vector de cuotas de mercado
ms[1]<-length(individualElections[individualElections==1])/length(individualElections) #calculamos la cuota de mercado para la marca 1 (OfficeStar)
ms[2]<-length(individualElections[individualElections==2])/length(individualElections)
ms[3]<-length(individualElections[individualElections==3])/length(individualElections)
ms[4]<-length(individualElections[individualElections==4])/length(individualElections)
ms
utilidad.ideal.ind$ms<-ms
utilidad.ideal.ind
msFirstElectionIdealPrefs(perceptions.puntos,  preferences.ideal.puntos)

#market share repetitive buy (cuota de preferencia)
utilidad.ideal.ind<-pdist(perceptions.puntos,  preferences.ideal.puntos) 
#distances among brnads and individuals
utilidad.ideal.ind <- as.matrix(utilidad.ideal.ind) #transform the data type
rownames(perceptions.puntos) #Let's add  names to the rows and columns of the matrix
rownames(preferences.ideal.puntos)
rownames(utilidad.ideal.ind) <- rownames(perceptions.puntos) #add  names to the rows  of the matrix
colnames(utilidad.ideal.ind) <- rownames(preferences.ideal.puntos) #add  names to the columns of the matrix
utilidad.ideal.ind #See the result
utilidad.ideal.ind.inv<-1/utilidad.ideal.ind
utilidad.ideal.ind.inv
suma.utilidad.ideal.ind.inv<-colSums(utilidad.ideal.ind.inv) #sumamos las columnas ocn el objeto de ver cómo se distribuyen las preferencias por las marcas en cada individuo
suma.utilidad.ideal.ind.inv
probalidad.ideal.ind.inv<-utilidad.ideal.ind.inv/suma.utilidad.ideal.ind.inv #calculamos la probabilidad de compra de cada marca en cada individuo
head(probalidad.ideal.ind.inv) #visualizamos el resultado
rowMeans(probalidad.ideal.ind.inv) #calculamos la media de las marcas en los individuos

msShareIdealPrefs(perceptions.puntos, preferences.ideal.puntos)

#market share in the segments 
#######Follow the same process but change the matrix of individuals' ideal products by the segments' ideal products



