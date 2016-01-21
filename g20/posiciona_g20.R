#lectura de datos g20per.txt con análisis interno
library(MASS)
# leer fichero g20.per.txt
g20<-read.table(file.choose(), header=T)
g20t<-data.frame(t(g20))
head(g20t)
g20t.pca<-prcomp(g20t, cor=T)
g20t.pca
summary(g20t.pca)
loadings(g20t.pca)
names(g20t.pca)
g20t.pca$rotation
g20t.puntos<-predict(g20t.pca)
cor(g20t, g20t.puntos)
biplot(g20t.pca, pc.biplot=T)
?biplot
abline(h=0); abline(v=0) # draw horiz and vertical axes 
biplot(g20t.pca, choice=c(1,3))
biplot(g20t.pca, choice=c(2,3))
# mapas conjuntos con análisis interno
#mapas conjuntos, percepciones y preferencias
#Leer las preferencias, modelo vectorial con análisis externo
#leer g20pref.txt
require(XLConnect)
#read g20_completo.xlsx
g20 <- loadWorkbook(file.choose())
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

## --- Build func to run simple perceptual maps --- ##

JSM = function(inp1, prefs){ #JSM() func opens
  
  # inp1 = perception matrix with row and column headers
  # brands in rows and attributes in columns
  # prefs = preferences matrix
  
  par(pty="s") # set square plotting region
  
  fit = prcomp(inp1, scale=TRUE) # extract prin compts
  
  plot(fit$rotation[,1:2], # use only top 2 prinComps
       
       type ="n", xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms
       
       main ="Joint Space map - Home-brew on R") # plot title
  
  abline(h=0); abline(v=0) # build horiz and vert axes
  
  attribnames = colnames(inp1);
  
  brdnames = rownames(inp1)
  
  # -- insert attrib vectors as arrows --
  
  for (i1 in 1:nrow(fit$rotation)){
    
    arrows(0,0, x1 = fit$rotation[i1,1]*fit$sdev[1],
           
           y1 = fit$rotation[i1,2]*fit$sdev[2], col="blue", lwd=1.5);
    
    text(x = fit$rotation[i1,1]*fit$sdev[1], y = fit$rotation[i1,2]*fit$sdev[2],
         
         labels = attribnames[i1],col="blue", cex=1.1)}
  
  # --- make co-ords within (-1,1) frame --- #
  
  fit1=fit; fit1$x[,1]=fit$x[,1]/apply(abs(fit$x),2,sum)[1]
  
  fit1$x[,2]=fit$x[,2]/apply(abs(fit$x),2,sum)[2]
  
  points(x=fit1$x[,1], y=fit1$x[,2], pch=19, col="red")
  
  text(x=fit1$x[,1], y=fit1$x[,2], labels=brdnames, col="black", cex=1.1)
  
  # --- add preferences to map ---#
  
  k1 = 2; #scale-down factor
  
  pref = data.matrix(prefs)# make data compatible
  
  pref1 = pref %*% fit1$x[,1:2];
  
  for (i1 in 1:nrow(pref1)){
    
    segments(0, 0, x1 = pref1[i1,1]/k1, y1 = pref1[i1,2]/k1, col="maroon2", lwd=1.25);
    
    points(x = pref1[i1,1]/k1, y = pref1[i1,2]/k1, pch=19, col="maroon2");
    
    text(x = pref1[i1,1]/k1, y = pref1[i1,2]/k1, labels = rownames(pref)[i1], adj = c(0.5, 0.5), col ="maroon2", cex = 1.1)}
  
  # voila, we're done! #
  
} # JSM() func ends

  JSM(mydata, pref0)
JSM(mydata, pref)
