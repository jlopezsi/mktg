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
La desviación estandar de cada atributo de la matriz rotada
sdev = sqrt(diag((1/(dim(X)[2]-1)* P %*% A %*% t(P))))
sdev
#######
g20.pca<-prcomp(g20, cor=T)
summary(g20.pca)
g20.puntos<-predict(g20.pca)
class(g20)
cor(g20, g20.puntos)
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
##Data
#g20_completo.xlsx
require(XLConnect)
#read g20_completo.xlsx
g20 <- loadWorkbook("g20_completo.xlsx")
g20.prefs <- readWorksheet(g20, rownames=1, sheet = "prefs", header = TRUE)
?readWorksheet
head(g20.prefs)
g20.prefs.hclust<-hclust(dist(g20.prefs, method="euclidean"), method="ward")
#Mostramos el resultado de la agrupación
plot(g20.prefs.hclust)
g20.prefs.hclust.centros<-tapply(as.matrix(g20.prefs), list(rep(cutree(g20.prefs.hclust, 3), ncol(as.matrix(g20.prefs))),col(as.matrix(g20.prefs))),mean)
#Visualizamos el resultado
g20.prefs.hclust.centros
g20.prefs.kmeans3<-kmeans(g20.prefs, g20.prefs.hclust.centros)
names(g20.prefs.kmeans3)
g20.prefs.kmeans3$centers
g20per
g20.int.seg<-rbind(g20per, g20.prefs.kmeans3$centers)
g20.int.seg
g20.int.seg.t<-data.frame(t(g20.int.seg))
g20.int.pca <- prcomp(g20.int.seg.t, cor=TRUE)
plot(g20.int.pca)
biplot(g20.int.pca , pc.biplot=T, cex=0.7, ex=0.8)
biplot(g20.int.pca , choices=c(1,3), pc.biplot=T, cex=0.7, ex=0.8)
biplot(g20.int.pca , choices=c(2,3), pc.biplot=T, cex=0.7, ex=0.8)

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

###analisis externo

## --- Build func to run simple perceptual maps --- ##
JSM <- function(inp1, prefs){
  
  # inp1 = perception matrix with row and column headers
  # brands in rows and attributes in columns
  # prefs = preferences matrix
  
  par(pty="s") # set square plotting region
  
  fit = prcomp(inp1, scale.=TRUE) # extract prin compts
  
  plot(fit$rotation[,1:2], # use only top 2 prinComps
       type ="n",xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms
       main ="Joint Space map - Home-brew on R") # plot title
  
  abline(h=0); abline(v=0) # build horiz & vert axes
  
  attribnames = colnames(inp1)
  brdnames = rownames(inp1)
  
  # <-- insert attrib vectors as arrows--
  for (i1 in 1:nrow(fit$rotation)){
    arrows(0,0, x1=fit$rotation[i1,1]*fit$sdev[1], y1=fit$rotation[i1,2]*fit$sdev[2], col="blue", lwd=1.5);
    text(x=fit$rotation[i1,1]*fit$sdev[1],y=fit$rotation[i1,2]*fit$sdev[2], labels=attribnames[i1],col="blue", cex=1.1)}
  
  # <--- make co-ords within (-1,1) frame #
  
  fit1=fit
  fit1$x[,1]=fit$x[,1]/apply(abs(fit$x),2,sum)[1]
  fit1$x[,2]=fit$x[,2]/apply(abs(fit$x),2,sum)[2]
  points(x=fit1$x[,1], y=fit1$x[,2], pch=19, col="red")
  text(x=fit1$x[,1], y=fit1$x[,2], labels=brdnames,col="black", cex=1.1)
  
  # --- add preferences to map ---#
  k1 = 2; #scale-down factor
  pref=data.matrix(prefs)# make data compatible
  pref1 = pref %*% fit1$x[,1:2]
  for (i1 in 1:nrow(pref1)){segments(0,0, x1=pref1[i1,1]/k1,y1=pref1[i1,2]/k1, col="maroon2", lwd=1.25)}
  # voila, we're done! #
  
}

##Data
#g20_completo.xlsx
require(XLConnect)
#read g20_completo.xlsx
g20 <- loadWorkbook("g20_completo.xlsx")
mydata <- readWorksheet(g20, rownames=1, sheet = "per", header = TRUE)
?readWorksheet
head(mydata)

mydata = t(mydata) #transposing to ease analysis

mydata #view the table read

# extract brand and attribute names #

brdnames = rownames(mydata);

attribnames = colnames(mydata)
#Step 3b: Read into R the preferences table into 'prefs'.

# -- Read in preferences table -- #
pref <- readWorksheet(g20, rownames=1, sheet = "prefs", header = TRUE)
head(pref)


dim(pref) #check table dimensions

pref[1:10,] #view first 10 rows

#g20pref<-read.table(file.choose(), row.names=1, header=T)
#head(g20pref)
## --- Build func to run simple perceptual maps --- ##
pref0 = pref*0; rownames(pref0) = NULL
source("JSM.R")
JSM(mydata, pref0)
JSM(mydata, pref)

#####MDS datos hospital
kcpref<-read.table("hospital.prefs.txt", header=T)
summary(kcpref)
head(kcpref)
dist(t(kcpref))
kcpref.dist<-dist(t(kcpref))
kcpref.dist
library(MASS)
kcpref.mds<-isoMDS(kcpref.dist)
names(kcpref.mds)
summary(kcpref.mds)
kcpref.mds$stress
kcpref.mds$points
##Plot
plot(kcpref.mds$points, type="n")
text(kcpref.mds$points, labels=names(kcpref))
eqscplot(kcpref.mds$points, type="n")
text(kcpref.mds$point, labels=names(kcpref))
kcpref.sh <- Shepard(kcpref.dist, kcpref.mds$points)
names(kcpref.sh)
plot(kcpref.sh)
lines(kcpref.sh$x, kcpref.sh$yf, type="S")
install.packages("DiagrammeR")
library(DiagrammeR)
diagram <- "
graph
  A-->B
  A-->C
 
"

DiagrammeR(diagram)
DiagrammeR(diagram)

boxes_and_rectangles <- "
digraph boxes_and_rectangles {
node [shape = box]
A
B
C
D
E
F
node [shape = circle, fixedsize = true, width = 0.9]
1
2
3
4
5
6
7
8

A->1
B->2
B->3
B->4
C->A
1->D
E->A
2->4
1->5
1->F
E->6
4->6
5->7
6->7
3->8

overlap = true
fontsize = 10;
}
"

grViz(boxes_and_rectangles)
####
library(smacof)
data('breakfast')
head(breakfast)
res.rect <- smacofRect(breakfast, itmax = 1000)
plot(res.rect, joint = TRUE, xlim = c(-10, 10), asp = 1)
plot(res.rect, plot.type = "Shepard", asp = 1)
data("bread")
res.uc <- smacofIndDiff(bread)
res.uc
res.id <- smacofIndDiff(bread, constraint = "identity")
res.id
plot(res.uc, main = "Group Configurations Unconstrained", xlim = c(-1.2, 1), ylim = c(-1.2, 1), asp = 1)
plot(res.id, main = "Group Configurations Identity", xlim = c(-1.2, 1), ylim = c(-1.2, 1), asp = 1)
plot(res.uc, plot.type = "resplot", main = "Residuals Unconstrained", xlim = c(2, 14), ylim = c(-3, 3), asp = 1)
plot(res.id, plot.type = "resplot", main = "Residuals Indentity", xlim = c(2, 14), ylim = c(-3, 3), asp = 1)

######
library(SensoMineR)
data(chocolates)
#There are three data frames: 
#- sensochoc: a data frame with 348 rows and 19 columns: 
# 5 qualitative variables (Panelist, Session, Form, Rank, Product) 
#and 14 sensory descriptors;
#- hedochoc: a data frame with 6 rows and 222 columns: 
#each row corresponds to a chocolate 
#and each column to the hedonic scores given by one of the 
#222 consumers participating in the study;
#- sensopanels: a data frame with 6 rows and 98 columns: 
#each row corresponds to a chocolate and each column to the mean over 
#the panelists of a given panel according to a sensory descriptor.

names(sensochoc)
head(sensochoc)
class(chocolates)
boxprod(sensochoc, col.p = 4, firstvar = 5, numr = 2, numc = 2)
#averagetable Computes a (products,descriptors) matrix
resaverage<-averagetable(sensochoc, formul = "~Product+Panelist",
                         firstvar = 5)
resaverage
coltable(magicsort(resaverage), level.upper = 6,level.lower = 4,
         main.title = "Average by chocolate")
res.pca = PCA(resaverage, scale.unit = TRUE)
res.pca
resdecat<-decat(sensochoc, formul = "~Product+Panelist", firstvar = 5,
                graph = FALSE)
resdecat
## Not run:
barrow(resdecat$tabT)
barrow(resdecat$coeff, color = "orange")
boxprod(sensochoc, col.p = 4, firstvar = 5, numr = 2, numc = 2)

#Performs preference mapping techniques based on multidimensional exploratory data analysis.
## Example 1: carto for the sensory descriptors
data(cocktail)
#The data used here refer to 16 cocktails.
#There are 3 files corresponding to the composition of the cocktails; 
#the sensory description of the cocktails; the hedonic scores.
#- For the composition of the cocktails: The mango, banana, 
#orange and lemon concentration are known;
#- For the sensory description: each cocktail was evaluated by 12 panelists 
#according to 13 sensory descriptors (only the average of each cocktail are given). 
#- For the hedonic data: each cocktail was evaluated on a structured scale 
#from 0 to 10, by 100 consumers, according to their liking (0) or disliking (10).
#format
#There are three data frames: - compo.cocktail: a data frame with 16 rows and 4 columns: 
#the composition of each cocktail is given for the 4 ingredients;
#- senso.cocktail: a data frame with 16 rows and 13 columns: 
#each cocktail was evaluated by 12 panelists according to 13 sensory descriptors;
#hedo.cocktail: a data frame with 16 rows and 100 columns: each cocktail 
#was evaluated on a structured scale from 0 to 10, by 100 consumers, 
#according to their liking (0) or disliking (10).


res.pca <- PCA(senso.cocktail)
res.carto <- carto(res.pca$ind$coord[,1:2], hedo.cocktail)
## Example 2
data(cocktail)
class(cocktail)
mydata<-cbind.data.frame(senso.cocktail,compo.cocktail)
head(mydata)
res.mfa <- MFA(cbind.data.frame(senso.cocktail,compo.cocktail),
               group=c(ncol(senso.cocktail),ncol(compo.cocktail)),
               name.group=c("senso","compo"))
res.carto <- carto(res.mfa$ind$coord[,1:2], hedo.cocktail)
#Performs preference mapping techniques based on multidimensional exploratory data analysis and segmentation of consumers.
## Not run:
## Example 1: carto on the sensory descriptors
data(cocktail)
res.pca <- PCA(senso.cocktail)
results1 <- cartoconsumer(res.pca, hedo.cocktail)
results2 <- cartoconsumer(res.pca, hedo.cocktail,
                          graph.hcpc=TRUE,graph.group=TRUE)
## End(Not run)
## Example 2
## Not run:
data(cocktail)
res.mfa <- MFA(cbind.data.frame(senso.cocktail,compo.cocktail),
               group=c(ncol(senso.cocktail),ncol(compo.cocktail)),
               name.group=c("senso","compo"))
results3 <- cartoconsumer(res.mfa, hedo.cocktail)

#cpa Consumers’ Preferences Analysis

## Not run:
data(cocktail)
res.cpa = cpa(cbind(compo.cocktail, senso.cocktail), hedo.cocktail)
## If you prefer a graph in black and white and with 3 clusters
res.cpa = cpa(cbind(compo.cocktail, senso.cocktail), hedo.cocktail,
              name.panelist = TRUE, col = gray((50:1)/50), nb.clusters = 3)
## End(Not run)

## End(Not run)
#chocolates
#The data used here refer to six varieties of chocolates sold in France.
#- For the sensory description: each chocolate was evaluated twice by 29 panelists according to 14 sensory descriptors;
#- For the hedonic data: each chocolate was evaluated on a structured scale from 0 to 10, by 222 consumers, according to their liking (0) or disliking (10);
#- For the sensory panels description: each chocolate was evaluated by 7 panels according to 14 sensory descriptors.
#There are three data frames: - sensochoc: a data frame with 348 rows and 19 columns: 5 qualitative variables (Panelist, Session, Form, Rank, Product) and 14 sensory descriptors;
#- hedochoc: a data frame with 6 rows and 222 columns: each row corresponds to a chocolate and each column to the hedonic scores given by one of the 222 consumers participating in the study;
#- sensopanels: a data frame with 6 rows and 98 columns: each row corresponds to a chocolate and each column to the mean over the panelists of a given panel according to a sensory descriptor.
data(chocolates)
decat(sensochoc, formul = "~Product+Panelist", firstvar = 5, graph = FALSE)
#cocktail Cocktail data
#The data used here refer to 16 cocktails.
#There are 3 files corresponding to the composition of the cocktails; the sensory description of the cocktails; the hedonic scores.
#- For the composition of the cocktails: The mango, banana, orange and lemon concentration are known;
#- For the sensory description: each cocktail was evaluated by 12 panelists according to 13 sensory descriptors (only the average of each cocktail are given). - For the hedonic data: each cocktail was evaluated on a structured scale from 0 to 10, by 100 consumers, according to their liking (0) or disliking (10).

#There are three data frames: - compo.cocktail: a data frame with 16 rows and 4 columns: the composition of each cocktail is given for the 4 ingredients;
#- senso.cocktail: a data frame with 16 rows and 13 columns: each cocktail was evaluated by 12 panelists according to 13 sensory descriptors;
#hedo.cocktail: a data frame with 16 rows and 100 columns: each cocktail was evaluated on a structured scale from 0 to 10, by 100 consumers, according to their liking (0) or disliking (10).
## Example 1
data(chocolates)
resdecat<-decat(sensochoc, formul = "~Product+Panelist", firstvar = 5,
                graph = FALSE)
resaverage<-averagetable(sensochoc, formul = "~Product+Panelist",
                         firstvar = 5)
resaverage.sort = resaverage[rownames(magicsort(resdecat$tabT)),
                             colnames(magicsort(resdecat$tabT))]
coltable(resaverage.sort, magicsort(resdecat$tabT),
         level.lower = -1.96, level.upper = 1.96,
         main.title = "Average by chocolate")
## Example 3
## Not run:
data(chocolates)
resperf<-paneliperf(sensochoc,
                    formul = "~Product+Panelist+Product:Panelist",
                    formul.j = "~Product", col.j = 1, firstvar = 5, lastvar = 12,
                    synthesis = FALSE, graph = FALSE)
resperfprob<-magicsort(resperf$prob.ind, method = "median")
coltable(resperfprob, level.lower = 0.05, level.upper = 1,
         main.title = "P-value of the F-test (by panelist)")
resperfr2<-magicsort(resperf$r2.ind, method = "median",
                     ascending = FALSE)
coltable(resperfr2, level.lower = 0.00, level.upper = 0.85,
         main.title = "Adjusted R-square (by panelist)")
## End(Not run)

## Not run:

#ConsistencyIdeal Sensory and Hedonic consistency of the ideal data 
data(perfume_ideal)
res <- ConsistencyIdeal(perfume_ideal, col.p=2, col.j=1,
                        col.lik=ncol(perfume_ideal), id.recogn="id_",
                        type="both", nbsim=100)
## End(Not run)

#construct.axes Coordinates of individuals and illustrative individuals for PCA or MFA
￼￼
## Example1: PCA
data(chocolates)
donnee <- cbind.data.frame(sensochoc[,c(1,4,5:18)])
axe <- construct.axes(donnee, scale.unit = TRUE)
## Example2: MFA (two groups of variables)
data(chocolates)
donnee <- cbind.data.frame(sensochoc[,c(1,4,5:18)])
axe <- construct.axes(donnee, group = c(6,8),
                      name.group = c("A-F","T-S"),scale.unit = TRUE)

#cpa Consumers’ Preferences Analysis
#Performs preference mapping techniques based on multidimensional exploratory data analysis. 
#This methodology is oriented towards consumers’ preferences; 
#here consumers are pictured according only to their preferences. 
#In this manner, the distance between two consumers is very natural and easy to interpret, 
#and a clustering of the consumers is also very easy to obtain.
## Not run:
data(cocktail)
res.cpa = cpa(cbind(compo.cocktail, senso.cocktail), hedo.cocktail)
## If you prefer a graph in black and white and with 3 clusters
res.cpa = cpa(cbind(compo.cocktail, senso.cocktail), hedo.cocktail,
              name.panelist = TRUE, col = gray((50:1)/50), nb.clusters = 3)
## End(Not run)

#cream_id Cream Ideal Data
#The data used here refer to the sensory description of 9 dessert chocolate creams.
#Each cream was evaluated once by 86 French consumers and described on 13 attributes according to the Ideal Profile Method.
#Both perceived and ideal intensities were asked. In addition, the overall liking is asked.

## Not run:
data(cream_id)
decat(cream_id, formul = "~product+user", firstvar = 3, graph = FALSE)
###IdMapConsumer function
data(cream_signa)
res.idmap <- IdMapConsumer(craem_id, cream_signa, col.p=2, col.j=1, col.lik=29, num.col.var.signa=c(1:12), conf.level=0.90, id.recogn="id_", color = FALSE, simusigni = 500)

#IdMap Ideal Mapping (IdMap) Description

## Not run:
data(perfume_ideal)
#! For the IdMap
res.IdMap <- IdMap(perfume_ideal, col.p=2, col.j=1,
                   col.lik=ncol(perfume_ideal), id.recogn="id_")
plot.IdMap(res.IdMap, xlim=c(-7,10), ylim=c(-5,7), levels.contour=NULL, color=TRUE)
#! For the wIdMap
res.wIdMap <- IdMap(perfume_ideal, col.p=2, col.j=1, col.lik=ncol(perfume_ideal),
                    id.recogn="id_", cons.eq=TRUE)
## End(Not run)