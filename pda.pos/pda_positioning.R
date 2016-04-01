library(MASS)
#this code install packages if they aren't already installed
if(!require(c("ggplot2", "XLConnect")){
  install.packages("ggplot2", "XLConnect")
  library(ggplot)#load the packege
}
source("marketing-models.R")
require(XLConnect)
#pda <- loadWorkbook(file.choose())
#perceptions <- readWorksheet(pda, rownames=1, sheet = "perceptions", header = TRUE)
#?readWorksheet
perceptions <- read.table("pda_per.txt", sep = "\t", dec = "," , row.names=1, header=T)
head(perceptions)

mydata = t(perceptions) #transposing to ease analysis

mydata #view the table read

# -- Read in preferences table -- #
#pda.pref.txt
#pref = read.table(file.choose())
#pda_pref <- readWorksheet(pda, rownames=1, sheet = "preferences", header = TRUE)
preferences <- read.table("pda_pref.txt", sep = "\t", dec = "," , row.names=1, header=T)
head(preferences)
dim(preferences) #check table dimensions

preferences[1:10,] #view first 10 rows

#Compute principal components analysis with prcomp (princomp doesn't alow to perform pca with less rows than colomnes)
perceptions.pca<-prcomp(mydata, cor=T)
summary(perceptions.pca)#Check the variation explained by principal components
perceptions.pca #Check the findings, how pc are produced from original indicators
#Check all objects available in g20t.pca objects
names(perceptions.pca)
perceptions.puntos<-predict(perceptions.pca)
#Interpret the meaning of the principal components
cor(mydata, perceptions.puntos)
#plot pc using biplot function
biplot(perceptions.pca, pc.biplot=T)
?biplot #seach for help
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
pref.puntos<-cor(t(preferences), perceptions.puntos)
#add preferences as arrows to the biplot
arrows(0, 0, pref.puntos[,1], pref.puntos[,2]) 
#add the indentity of consumers
text (pref.puntos[,1:2], labels = row.names(preferences))

#####repeat the process again for pc1 and pc3
biplot(perceptions.pca, choice=c(1,3))
arrows(0, 0, pref.puntos[,1], pref.puntos[,3]) 
text (pref.puntos[,1:3], labels = row.names(preferences))

#And for compontens 2 and 3
biplot(perceptions.pca, choice=c(2,3))
arrows(0, 0, pref.puntos[,2], pref.puntos[,3]) 
text (pref.puntos[,2:3], labels = row.names(preferences))

####Segment preferences and plot arrows' colours  according to the classification
pref.hclust <- hclust(dist(pref), method="ward")
plot(pref.hclust)
source("marketing-models.R")
#centros-hclust, calcula las medias en los segmentos obtenidos con hclust
centros.pref3<-centros.hclust(pref.hclust, preferences, 3)
options(digits=2)
centros.pref3
pref.kmeans3<-kmeans(preferences, centros.pref3)
names(pref.kmeans3)
pref.kmeans3$size
#First plot
biplot(perceptions.pca, pc.biplot=T)

# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
#add preferences as arrows to the biplot
arrows(0, 0, pref.puntos[,1], pref.puntos[,2]) 
#add the indentity of consumers
text (pref.puntos[,1:2], labels = row.names(preferences))

########add preferences from a verctor model##########
biplot(perceptions.pca, pc.biplot=T)
?biplot
names(perceptions.pca)
abline(h=0); abline(v=0)
#pref.puntos<-cor(t(pref), g20t.puntos)
arrows(0, 0, pref.puntos[,1], pref.puntos[,2], col=pref.kmeans3$cluster) 
#points(pref.puntos[,1:2]) #mirar de graficar en color el segmento
text (pref.puntos[,1:2], labels = row.names(preferences))

#We can plot the clusters' means instead of the individual preferences
pref.segments <- pref.kmeans3$centers
pref.segments
biplot(perceptions.pca, pc.biplot=T)
# draw horitzontal and vertical lines
abline(h=0); abline(v=0) # draw horiz and vertical axes 

########add preferences from a vector model##########
#we use the correlation between the preferences data frame and the pc plotted
head(pref.segments.puntos)
pref.segments.puntos<-cor(t(pref.segments), perceptions.puntos)
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
pref.puntos<-as.data.frame(pref.puntos)
names(pref.puntos)
ggscreeplot(perceptions.pca)
pda.ggbiplot<-ggbiplot(perceptions.pca,  labels=rownames(mydata),choice=c(1,2))
pda.ggbiplot + geom_segment(data = pref.puntos,
                            aes(x = 0, y = 0, xend = PC1, yend = PC2 ),
                            arrow = arrow(length = unit(1/2, 'picas')), 
                            color = muted('green')) + 
  geom_text(data=pref.puntos, aes(PC1, PC2, label = row.names(preferences)), size=4)

###Add the results of clustering preferences

pda.ggbiplot<-ggbiplot(perceptions.pca,  labels=rownames(mydata),choice=c(1,2))
pda.ggbiplot + geom_segment(data = pref.puntos,
                            aes(x = 0, y = 0, xend = PC1, yend = PC2),
                            arrow = arrow(length = unit(1/2, 'picas')), 
                            color = pref.kmeans3$cluster) + 
  geom_text(data=pref.puntos, aes(PC1, PC2, label = row.names(preferences)), size=4)

###Add the segments summary instead of individual preferences


