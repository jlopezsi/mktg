#lectura de datos g20per.txt con análisis interno
library(MASS)
#this code install packages if they aren't already installed
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot)#load the packege
}

source("marketing-models.R")
# -- Read in Average Perceptions table -- #
require(XLConnect)
heineken <- loadWorkbook("heinekenData.xlsx")
heineken.per <- readWorksheet(heineken, rownames=1, sheet = "perceptions", header = TRUE)
head(heineken.per)
tail(heineken.per)

perceptions<-t(heineken.per)
dim(perceptions)
perceptions<-(perceptions[,-24])
head(perceptions)
# -- Read in Average preferences table -- #
preferences.plus <- readWorksheet(heineken, rownames=1, sheet = "preferences", header = TRUE)
head(preferences.plus)
preferences <-preferences.plus[,-c(11,12)]
head(preferences)

#read in average pref by greographical Region
geographical <- readWorksheet(heineken, rownames=1, sheet = "AveragePrefByGeographicalRegion", header = TRUE)

head(geographical)

#read in avergae preferences by segment

pref.segments <- readWorksheet(heineken, rownames=1, sheet = "AveragePrefsBySegment", header = TRUE)

head(pref.segments)

#read in atributes description

brands.atributs <- readWorksheet(heineken, sheet = "hoja1", header = TRUE)

head(brands.atributs)

########plot perceptions
#Compute principal components analysis with prcomp (princomp doesn't alow to perform pca with less rows than colomnes)
perceptions.pca<-prcomp(perceptions, cor=T)
#Check the findings, how pc are produced from original indicators
perceptions.pca
plot(perceptions.pca)
#Check the variation explained by principal components
summary(perceptions.pca)
#Check all objects available in g20t.pca objects
names(perceptions.pca)
perceptions.puntos<-predict(perceptions.pca)
#Interpret the meaning of the principal components
cor(perceptions, perceptions.puntos)
#plot pc using biplot function
biplot(perceptions.pca, pc.biplot=T)
?biplot #seach for help
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
dim(t(preferences))
dim(perceptions.puntos)
preferences.puntos<-cor(t(preferences), perceptions.puntos)
#add preferences as arrows to the biplot
arrows(0, 0, preferences.puntos[,1], preferences.puntos[,2]) 
#add the indentity of consumers
text (preferences.puntos[,1:2], labels = row.names(preferences))

#####repeat the process again for pc1 and pc3
biplot(perceptions.pca, choice=c(1,3))
preferences.puntos<-cor(t(preferences), perceptions.puntos)
arrows(0, 0, preferences.puntos[,1], preferences.puntos[,3]) 
text (preferences.puntos[,1:3], labels = row.names(preferences))

#And for compontens 2 and 3
biplot(perceptions.pca, choice=c(2,3))
preferences.puntos<-cor(t(preferences), perceptions.puntos)
arrows(0, 0, preferences.puntos[,2], preferences.puntos[,3]) 
text (preferences.puntos[,2:3], labels = row.names(preferences))

####Segment preferences and plot arrows' colours  according to the classification
pref.hclust <- hclust(dist(preferences), method="ward")
plot(pref.hclust)
source("marketing-models.R")
#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.pref9<-centros.hclust(pref.hclust, preferences, 9)
options(digits=2)
centros.pref9
pref.kmeans9<-kmeans(preferences, centros.pref9)
names(pref.kmeans9)
pref.kmeans9$size
#First plot
biplot(perceptions.pca, pc.biplot=T)

# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
preferences.puntos<-cor(t(preferences), perceptions.puntos)
#add preferences as arrows to the biplot
arrows(0, 0, preferences.puntos[,1], preferences.puntos[,2]) 
#add the indentity of consumers
text (preferences.puntos[,1:2], labels = row.names(preferences))

########add preferences from a verctor model##########
biplot(perceptions.pca, pc.biplot=T)
?biplot
names(perceptions.pca)
abline(h=0); abline(v=0)
#pref.puntos<-cor(t(pref), g20t.puntos)
arrows(0, 0, preferences.puntos[,1], preferences.puntos[,2], col=pref.kmeans9$cluster) 
#points(pref.puntos[,1:2]) #mirar de graficar en color el segmento
text (preferences.puntos[,1:2], labels = row.names(preferences))

#We can plot the clusters' means instead of the individual preferences
pref.segments <- pref.kmeans9$centers
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

###########some extra code to plot clusters into de pc
#library(devtools)
#install_github("vqv/ggbiplot")

######biplot with ggplot
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
###ggplot2 needs a data.frame as argument
preferences.puntos<-as.data.frame(preferences.puntos)
names(preferences.puntos)
ggscreeplot(perceptions.pca)
heineken.ggbiplot<-ggbiplot(perceptions.pca,  labels=rownames(perceptions),choice=c(1,2))
heineken.ggbiplot + geom_segment(data = preferences.puntos,
                            aes(x = 0, y = 0, xend = PC1, yend = PC2 ),
                            arrow = arrow(length = unit(1/2, 'picas')), 
                            color = muted('green')) + 
  geom_text(data=preferences.puntos, aes(PC1, PC2, label = row.names(preferences)), size=4)

###Add the results of clustering preferences

heineken.ggbiplot<-ggbiplot(perceptions.pca,  labels=rownames(perceptions),choice=c(1,2))
heineken.ggbiplot + geom_segment(data = preferences.puntos,
                            aes(x = 0, y = 0, xend = PC1, yend = PC2),
                            arrow = arrow(length = unit(1/2, 'picas')), 
                            color = pref.kmeans9$cluster) + 
  geom_text(data=preferences.puntos, aes(PC1, PC2, label = row.names(preferences)), size=4)

