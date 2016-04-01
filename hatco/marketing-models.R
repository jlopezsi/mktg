#marketing models functions
#Segmentation
##centros-hclust, returns the means values of the segmentation bases on the segments obtained by hclust

centros.hclust <- function (bases.hclust, bases, s) {
  centros.bases<-tapply(
    as.matrix(bases), 
    list(rep(cutree(bases.hclust, s),
             ncol(as.matrix(bases))),
         col(as.matrix(bases))),
    mean)
  return(centros.bases)
}
medias.kmeans <- function(bases.puntos.kmeans, bases) {
  medias <-t(aggregate(bases, list(Segmento = bases.puntos.kmeans2$cluster), mean))
  return(medias)
}
##Función discriminante

lda.arrows <- function(x, myscale = 1, tex = 0.75, choices = c(1,2), ...){
  ## adds `biplot` arrows to an lda using the discriminant function values
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], ...)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex)
}
