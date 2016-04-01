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

