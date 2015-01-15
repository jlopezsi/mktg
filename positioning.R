#rm(list = ls()) # clear workspace

install.packages("MASS") # install MASS package

mydata = USArrests # USArrests is an inbuilt dataset

pc.cr = princomp(mydata, cor=TRUE) # princomp() is core func summary(pc.cr) # summarize the pc.cr object

biplot(pc.cr) # plot the pc.cr object

abline(h=0); abline(v=0) # draw horiz and vertical axes
##############
## --- Build func to run simple perceptual maps --- ##
## --- Build func to run simple perceptual maps --- ##
JSM = function(inp1, prefs){ #JSM() func opens
  
  # inp1 = perception matrix with row and column headers
  # brands in rows and attributes in columns
  # prefs = preferences matrix
  
  par(pty="s") # set square plotting region
  
  fit = prcomp(inp1, scale.=TRUE) # extract prin compts
  
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
  ########################
  # -- Read in Average Perceptions table -- #
  # mydata = read.table(file.choose(), header = TRUE)
  
# -- Read in Average Perceptions table -- #
require(XLConnect)
os <- loadWorkbook(file.choose())
os_per <- readWorksheet(os, rownames=1, sheet = "per", header = TRUE)
?readWorksheet
head(os_per)


  mydata = t(os_per) #transposing to ease analysis
  
  mydata #view the table read
  
  # extract brand and attribute names #
  
  brdnames = rownames(mydata);
  
  attribnames = colnames(mydata)
  
  # -- Read in preferences table -- #
  #pref = read.table(file.choose())
# -- Read in preferences table -- #
os_pref <- readWorksheet(os, rownames=1, sheet = "prefs", header = TRUE)
head(os_pref)
pref<-os_pref
  
  dim(pref) #check table dimensions
  
  pref[1:10,] #view first 10 rows
  # creating empty pref dataset
  pref0 = pref*0; rownames(pref0) = NULL
  
  JSM(mydata, pref0) # p-map without prefs information
  
JSM(mydata, os_pref) # for p-map with preference data
  
  ##################
  # --------------------- #
  ### --- MDS code ---- ###
  # --------------------- #
  rm(list = ls()) # clear workspace
  
  mydata = read.table(file.choose(), header = TRUE) # 'mds car data raw.txt'
  mydata<-kcpref
  dim(mydata) # view dimension of the data matrix
  
  brand.names = colnames(kcpref)
#c("Hyundai", "Honda", "Fiat", "Ford", "Chevrolet", "Toyota", "Nissan", "TataMotors", "MarutiSuzuki")
  ### --- build user define func run.mds --- ###
  run.mds = function(mydata, brand.names){
    
    # build distance matrix # k = length(brand.names)
    
    dmat = matrix(0, k, k)
    
    for (i1 in 1:(k-1)){ a1 = grepl(brand.names[i1], colnames(mydata));
                         
                         for (i2 in (i1+1):k){a2 = grepl(brand.names[i2], colnames(mydata));
                                              # note use of Regex here
                                              
                                              a3 = a1*a2;
                                              
                                              a4 = match(1, a3);
                                              
                                              dmat[i1, i2] = mean(mydata[, a4]);
                                              
                                              dmat[i2, i1] = dmat[i1, i2] } #i2 ends
                         
    } # i1 ends
    
    colnames(dmat) = brand.names;
    
    rownames(dmat) = brand.names
    
    ### --- run metric MDS --- ###
    
    d = as.dist(dmat)
    
    # Classical MDS into k dimensions #
    
    fit = cmdscale(d,eig=TRUE, k=2)  # cmdscale() is core MDS func
    
    fit # view results
    
    # plot solution #
    
    x = fit$points[,1];
    y = fit$points[,2];
    
    plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Metric MDS", xlim = c(floor(min(x)), ceiling(max(x))), ylim = c(floor(min(y)), ceiling(max(y))), type="p",pch=19, col="red");
    
    text(x, y, labels = rownames(fit$points), cex=1.1, pos=1);
    
    abline(h=0); abline(v=0)# horiz and vertical lines drawn
    
  }	# run.mds func ends
  # run MDS on raw data (before segmenting)
  run.mds(mydata, brand.names)
  
#read file KCPREFS.DAT
kcpref<-read.table(file.choose(), header=T)
summary(kcpref)
head(kcpref)
kcpref
kcpref.dist<-dist(t(kcpref))
library(MASS)
kcpref.mds<-isoMDS(kcpref.dist)
names(kcpref.mds)
summary(kcpref.mds)
kcpref.mds$stress
kcpref.mds$points
#Para la prepresentación gráfica del espacio reducido de dos dimensiones 
#podemos utilizar las funciones gráficas primitivas de R, plot y text.
plot(kcpref.mds$points, type="n")
text(kcpref.mds$points, labels=names(kcpref))

#O bien las funciones gráficas que ofrece el paquete mass, 
#concretamente en este caso eqscplot, donde las escalas de ambos ejes son iguales 
#-eqscplot significa equal scale plot.
eqscplot(kcpref.mds$points, type="n")
text(kcpref.mds$point, labels=names(kcpref))

kcpref.sh <- Shepard(kcpref.dist, kcpref.mds$points)
names(kcpref.sh)
plot(kcpref.sh)
lines(kcpref.sh$x, kcpref.sh$yf, type="S")
