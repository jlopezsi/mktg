## --- Build func to run simple perceptual maps --- ##
JSM <- function(inp1, prefs){
  
  # inp1 = perception matrix with row and column headers
  # brands in rows and attributes in columns
  # prefs = preferences matrix
  
  par(pty="s") # set square plotting region
  
  fit = prcomp(inp1, scale.=TRUE) # extract prin compts
  
  plot(fit$rotation[,1:2], # use only top 2 prinComps
       type ="n",xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms
       main ="Joint Space map on R") # plot title
  
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