#lectura de datos g20per.txt con análisis interno
library(MASS)
#this code install packages if they aren't already installed
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot)#load the packege
}

source("marketing-models.R")
# leer fichero g20per.txt
#g20<-read.table(file.choose(), header=T)
g20<-read.table("g20per.txt", header=T)
#transpose data so that brands are rows and attributes, columnes
g20t<-data.frame(t(g20))
#Check whether the data frame is ok.
head(g20t)
#Compute principal components analysis with prcomp (princomp doesn't alow to perform pca with less rows than colomnes)
g20t.pca<-prcomp(g20t, cor=T)
#Check the findings, how pc are produced from original indicators
g20t.pca
#Check the variation explained by principal components
summary(g20t.pca)
#Check all objects available in g20t.pca objects
names(g20t.pca)
g20t.puntos<-predict(g20t.pca)
#Interpret the meaning of the principal components
cor(g20t, g20t.puntos)
#plot pc using biplot function
biplot(g20t.pca, pc.biplot=T)
?biplot #seach for help
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
pref.puntos<-cor(t(pref), g20t.puntos)
#add preferences as arrows to the biplot
arrows(0, 0, pref.puntos[,1], pref.puntos[,2]) 
#add the indentity of consumers
text (pref.puntos[,1:2], labels = row.names(pref))

#####repeat the process again for pc1 and pc3
biplot(g20t.pca, choice=c(1,3))
pref.puntos<-cor(t(pref), g20t.puntos)
arrows(0, 0, pref.puntos[,1], pref.puntos[,3]) 
text (pref.puntos[,1:3], labels = row.names(pref))

#And for compontens 2 and 3
biplot(g20t.pca, choice=c(2,3))
pref.puntos<-cor(t(pref), g20t.puntos)
arrows(0, 0, pref.puntos[,2], pref.puntos[,3]) 
text (pref.puntos[,2:3], labels = row.names(pref))

####Segment preferences and plot arrows' colours  according to the classification
pref.hclust <- hclust(dist(pref), method="ward")
plot(pref.hclust)
source("marketing-models.R")
#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.pref3<-centros.hclust(pref.hclust, pref, 3)
options(digits=2)
centros.pref3
pref.kmeans3<-kmeans(pref, centros.pref3)
names(pref.kmeans3)
pref.kmeans3$size
#First plot
biplot(g20t.pca, pc.biplot=T)

# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
pref.puntos<-cor(t(pref), g20t.puntos)
#add preferences as arrows to the biplot
arrows(0, 0, pref.puntos[,1], pref.puntos[,2]) 
#add the indentity of consumers
text (pref.puntos[,1:2], labels = row.names(pref))


########add preferences from a verctor model##########
biplot(g20t.pca, pc.biplot=T)
?biplot
names(g20t.pca)
abline(h=0); abline(v=0)
#pref.puntos<-cor(t(pref), g20t.puntos)
arrows(0, 0, pref.puntos[,1], pref.puntos[,2], col=pref.kmeans3$cluster) 
#points(pref.puntos[,1:2]) #mirar de graficar en color el segmento
text (pref.puntos[,1:2], labels = row.names(pref))

#We can plot the clusters' means instead of the individual preferences
pref.segments <- pref.kmeans3$centers
pref.segments
biplot(g20t.pca, pc.biplot=T)
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 
########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
head(pref.segments.puntos)
pref.segments.puntos<-cor(t(pref.segments), g20t.puntos)
#add preferences as arrows to the biplot
arrows(0, 0, pref.segments.puntos[,1], pref.segments.puntos[,2], col='green') 
#add the indentity of consumers
text (pref.segments.puntos[,1:2], labels = row.names(pref.segments))

###########some extra code to plot clusters into de pc
#library(devtools)
#install_github("vqv/ggbiplot")

######biplot with ggplot
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
###ggplot2 needs a data.frame as argument
pref.puntos<-as.data.frame(pref.puntos)
names(pref.puntos)
ggscreeplot(g20t.pca)
g20.ggbiplot<-ggbiplot(g20t.pca,  labels=rownames(g20t),choice=c(1,2))
g20.ggbiplot + geom_segment(data = pref.puntos,
             aes(x = 0, y = 0, xend = PC1, yend = PC2 ),
             arrow = arrow(length = unit(1/2, 'picas')), 
             color = muted('green')) + 
  geom_text(data=pref.puntos, aes(PC1, PC2, label = row.names(pref)), size=4)

###Add the results of clustering preferences

g20.ggbiplot<-ggbiplot(g20t.pca,  labels=rownames(g20t),choice=c(1,2))
g20.ggbiplot + geom_segment(data = pref.puntos,
                            aes(x = 0, y = 0, xend = PC1, yend = PC2),
                            arrow = arrow(length = unit(1/2, 'picas')), 
                            color = pref.kmeans3$cluster) + 
  geom_text(data=pref.puntos, aes(PC1, PC2, label = row.names(pref)), size=4)

###Add the segments summary instead of individual preferences

###It doesn't work with few brand names, In cas pda.positioning it works fine
# mapas conjuntos con análisis interno
#mapas conjuntos, percepciones y preferencias
#Leer las preferencias, modelo vectorial con análisis externo
#leer g20pref.txt
#install.packages("XLConnect")
#require(XLConnect)
#read g20_completo.xlsx
#g20 <- loadWorkbook(file.choose())
#g20 <- loadWorkbook("g20_completo.xlsx")
#mydata <- readWorksheet(g20, rownames=1, sheet = "per", header = TRUE)
#?readWorksheet
#head(mydata)
#mydata = t(mydata) #transposing to ease analysis
#mydata #view the table read
# extract brand and attribute names #
#Step 3b: Read into R the preferences table into 'prefs'.
# -- Read in preferences table -- #
#pref <- readWorksheet(g20, rownames=1, sheet = "prefs", header = TRUE)
#head(pref)
#dim(pref) #check table dimensions
#pref[1:10,] #view first 10 rows



