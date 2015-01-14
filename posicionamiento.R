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
La dsviación estandar de cada atributo de la matriz rotada
sdev = sqrt(diag((1/(dim(X)[2]-1)* P %*% A %*% t(P))))
sdev
#######
g20.pca<-prcomp(g20, cor=T)
summary(g20.pca)
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

JSM(mydata, pref0)
JSM(mydata, pref)


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
