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


RFM <- function(BBBClub.holdout){
  BBBClub.holdout$R <- ifelse(BBBClub.holdout$last<3, 25, ifelse(BBBClub.holdout$last<6 | BBBClub.holdout$last>=3, 20, ifelse(BBBClub.holdout$last<9 | BBBClub.holdout$last>=6, 10, ifelse(BBBClub.holdout$last<18 |BBBClub.holdout$last>=9, 5, 0))))
head(BBBClub.holdout$R)

BBBClub.holdout$F <- ifelse(BBBClub.holdout$freq<10, 10, ifelse(BBBClub.holdout$freq<20 | BBBClub.holdout$freq>=10, 20, ifelse(BBBClub.holdout$freq<30 | BBBClub.holdout$freq>=20, 30, ifelse(BBBClub.holdout$freq<40 | BBBClub.holdout$freq>=30, 40, 50))))
head(BBBClub.holdout$F)

BBBClub.holdout$M <- ifelse(BBBClub.holdout$amount<=50, 10, ifelse(BBBClub.holdout$amount<=150 , 20, ifelse(BBBClub.holdout$amount<=250 , 30, ifelse(BBBClub.holdout$amount<=350 , 40, 50))))
return(BBBClub.holdout)
}  

confusion.glm <- function(data, model) {
  prediction <- ifelse(predict(model, data, type='response') > 0.5, TRUE, FALSE)
  confusion  <- table(prediction, as.logical(model$y))
  confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
  confusion  <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  confusion
}

reverse.quartile <- function (BBBClub.holdout.bypglm) {
  
  
  #Reverse order of quartiles
    BBBClub.holdout.bypglm$decil <- ifelse(
      BBBClub.holdout.bypglm$quartile==10, 1, 
                                         ifelse(
                                           BBBClub.holdout.bypglm$quartile==9, 2, 
                                                ifelse(
                                                  BBBClub.holdout.bypglm$quartile==8, 3, 
                                                       ifelse(
                                                         BBBClub.holdout.bypglm$quartile==7, 4, 
                                                              ifelse(
                                                                BBBClub.holdout.bypglm$quartile==6, 5, 
                                                                     ifelse(
                                                                       BBBClub.holdout.bypglm$quartile==5, 6, 
                                                                            ifelse(
                                                                              BBBClub.holdout.bypglm$quartile==4, 7, 
                                                                                   ifelse(
                                                                                     BBBClub.holdout.bypglm$quartile==3, 8, 
                                                                                          ifelse(
                                                                                            BBBClub.holdout.bypglm$quartile==2, 9, 10)
                                                                                   )
                                                                            )
                                                                            
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
    )
    return(BBBClub.holdout.bypglm)
}

########Function for the multiple choice model#########

prob.of.choice<-function(abb.clogit, indiv){
  
  #La función predict() nos predice la utilidad que cada inidividuo obtiene
  
  abb.predict<-predict(abb.clogit)
  head(abb.predict)
  abb.predict.exp<-exp(abb.predict)
  eutil<-abb.predict.exp
  head(abb.predict.exp)
  #sumamos el exp() de la utilidad para cada individuo
  sum.exp<-by(abb.predict.exp, abb$id, sum)
  suma<-sum.exp
  
  #suma, eutil, inviv son los argumentos de la función
  n<-0
  #Crea un vector con tantos elementos como el producto entre 
  #lis individuos y las marcas
  p<-1:indiv*4
  #Para cada individuo
  for (i in 1:indiv) {
    #para cada marca
    for (j in 1:4) {
      #construye un índice
      n<-n+1
      #calcula la probabilidad de que el individuo i compre la #marca j
      p[n]<-eutil[n]/suma[i]
    }
  }
  #Devuelve el vector de probabilidades
  return(p)
  
}

segments<-function(p, indiv){
  # p es el vector de probabilidades
  # in es el número de individuos 
  s<-1:indiv*4
  j<-0
  for (i in 1:indiv) {
    #para cada individuo
    j=j+4
    #Leales
    if (p[j-3]>0.8) {s[j-3]<-"Leal"; s[j-2]<-"Leal"; s[j-1]<-"Leal"; s[j]<-"Leal"}
    #Competitivos
    if (p[j-3]<=0.8 & p[j-3]>0.5) {s[j-3]<-"Competitivo"; s[j-2]<-"Competitivo"; s[j-1]<-"Competitivo"; s[j]<-"Competitivo"}
    #Apropiables
    if (p[j-3]<=0.5 & p[j-3]>0.15) {s[j-3]<-"Apropiable"; s[j-2]<-"Apropiable"; s[j-1]<-"Apropiable"; s[j]<-"Apropiable"}
    #Perdidos
    if (p[j-3]<=0.15) {s[j-3]<-"Perdido"; s[j-2]<-"Perdido"; s[j-1]<-"Perdido"; s[j]<-"Perdido"}
  }
  #Devuelve el resultado de la función
  return(s)
}
############
############ functin for joint maps  -prectual and preferences
## --- Build func to run simple perceptual maps --- ##
JSM = function(inp1, prefs){ #JSM() func opens
  
  # inp1 = perception matrix with row and column headers
  # brands in rows and attributes in columns
  # prefs = preferences matrix
  
  par(pty="s") # set square plotting region
  
  fit = prcomp(inp1, cor=T) # extract prin compts
  
  plot(fit$rotation[,1:2], # use only top 2 prinComps
       
       type ="n", xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms
       
       main ="Joint Space map on R") # plot title
  
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

### --- func to build inputs to JSM subsets ---- ###
JSMsubset <- function(mydata, prefs, attribs, brands, k1){
  
  a1 = apply(mydata[k1,], 2, mean) 
  a2 = seq(from=1, to=length(a1), by=length(brands))
  
  # define a new inp1
  inp1 = matrix(0, nrow=length(attribs), ncol=length(brands))
  for (i1 in 1:nrow(inp1)){ inp1[i1,] = a1[a2[i1]:(a2[i1]+ncol(inp1)-1)]}
  colnames(inp1) = brands; rownames(inp1) = attribs; inp1
  
  inp1 = t(inp1) # transpose for convenience
  prefs1 = prefs[k1,]
  
  outp = list(inp1, prefs1)
  
  outp }
#######ggbiplot function


ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE, ...)
{
  # 
  #  ggbiplot.r
  #  
  #  Copyright 2011 Vincent Q. Vu.
  # 
  #  This program is free software; you can redistribute it and/or
  #  modify it under the terms of the GNU General Public License
  #  as published by the Free Software Foundation; either version 2
  #  of the License, or (at your option) any later version.
  #  
  #  This program is distributed in the hope that it will be useful,
  #  but WITHOUT ANY WARRANTY; without even the implied warranty of
  #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  #  GNU General Public License for more details.
  #  
  #  You should have received a copy of the GNU General Public License
  #  along with this program; if not, write to the Free Software
  #  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
  # 
  
  #' Biplot for Principal Components using ggplot2
  #'
  #' @param pcobj           an object returned by prcomp() or princomp()
  #' @param choices         which PCs to plot
  #' @param scale           covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
  #' @param obs.scale       scale factor to apply to observations
  #' @param var.scale       scale factor to apply to variables
  #' @param pc.biplot       for compatibility with biplot.princomp()
  #' @param groups          optional factor variable indicating the groups that the observations belong to. If provided the points will be colored according to groups
  #' @param ellipse         draw a normal data ellipse for each group?
  #' @param ellipse.prob    size of the ellipse in Normal probability
  #' @param labels          optional vector of labels for the observations
  #' @param labels.size     size of the text used for the labels
  #' @param alpha           alpha transparency value for the points (0 = transparent, 1 = opaque)
  #' @param circle          draw a correlation circle? (only applies when prcomp was called with scale = TRUE and when var.scale = 1)
  #' @param var.axes        draw arrows for the variables?
  #' @param varname.size    size of the text for variable names
  #' @param varname.adjust  adjustment factor the placement of the variable names, >= 1 means farther from the arrow
  #' @param varname.abbrev  whether or not to abbreviate the variable names
  #'
  #' @return                a ggplot2 plot
  #' @export
  #' @examples
  #'   data(wine)
  #'   wine.pca <- prcomp(wine, scale. = TRUE)
  #'   print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))
  #'
  #library(ggplot2)
  #library(plyr)
  #library(scales)
  #library(grid)
  #if(!require(c("ggplot2", "plyr", "scales", "grid")){
  #  install.packages(c("ggplot2", "plyr", "scales", "grid")) }
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  stopifnot(length(choices) == 2)
  
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }
  
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }
  
  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }
  
  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'), 
                         size = 1/2, alpha = 1/3)
    }
    
    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = muted('red'))
  }
  
  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    } else {
      g <- g + geom_point(alpha = alpha)      
    }
  }
  
  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    
    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  
  # Label the variable axes
  if(var.axes) {
    g <- g + 
      geom_text(data = df.v, 
                aes(label = varname, x = xvar, y = yvar, 
                    angle = angle, hjust = hjust), 
                color = 'darkred', size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }
  
  # TODO: Add a second set of axes
  
  return(g)
}


library(ggplot2)

PCbiplot <- function(PC, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames), color=colors[1])
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2, color=colors[2])
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
  plot
}

###ggscreeplot function

ggscreeplot <- function(pcobj, type = c('pev', 'cev')) 
{
  # 
  #  ggscreeplot.r
  #
  #  Copyright 2011 Vincent Q. Vu.
  # 
  #  This program is free software; you can redistribute it and/or
  #  modify it under the terms of the GNU General Public License
  #  as published by the Free Software Foundation; either version 2
  #  of the License, or (at your option) any later version.
  #  
  #  This program is distributed in the hope that it will be useful,
  #  but WITHOUT ANY WARRANTY; without even the implied warranty of
  #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  #  GNU General Public License for more details.
  #  
  #  You should have received a copy of the GNU General Public License
  #  along with this program; if not, write to the Free Software
  #  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
  # 
  
  #' Screeplot for Principal Components
  #'
  #' @param pcobj          an object returned by prcomp() or princomp()
  #' @param type           the type of scree plot.  'pev' corresponds proportion of explained variance, i.e. the eigenvalues divided by the trace. 'cev' corresponds to the cumulative proportion of explained variance, i.e. the partial sum of the first k eigenvalues divided by the trace.
  #' @export
  #' @examples
  #'   data(wine)
  #'   wine.pca <- prcomp(wine, scale. = TRUE)
  #'   print(ggscreeplot(wine.pca))
  #'
  type <- match.arg(type)
  d <- pcobj$sdev^2
  yvar <- switch(type, 
                 pev = d / sum(d), 
                 cev = cumsum(d) / sum(d))
  
  yvar.lab <- switch(type,
                     pev = 'proportion of explained variance',
                     cev = 'cumulative proportion of explained variance')
  
  df <- data.frame(PC = 1:length(d), yvar = yvar)
  
  ggplot(data = df, aes(x = PC, y = yvar)) + 
    xlab('principal component number') + ylab(yvar.lab) +
    geom_point() + geom_path()
}
