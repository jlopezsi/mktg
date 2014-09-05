#read data
#el paquete repis contiene funciunes para acceder a datos de dropbox
#library(repmis)
#?repmis
#abb2<-source_DropboxData("https://www.dropbox.com/s/rc7jxtsm3ia71te/abb.txt",  key="rc7jxtsm3ia71te",header=T)
#la opción file.chose() en la función read.table nos permite escoger un fichero de datos guardado en el ordenador local)
#read abb-R.txt
abb<-read.table(file.choose(), header=T)
abb<-read.table("abb-r.txt", header=T)
#la función head()nos permite visualizar las primer seis líneas de un objeto de datos
head(abb)
#la funcion names() muestra los nombres de las variables 
names(abb)
#El paquete survival continene la función clogit para estimar el moledo de elección discreta
library(survival)
#Estimacion del modelo con la función clogit.  la opción strata() con el argumento
#id controlar las observaciones repetidas para cada individuo
#abb.clogit es el objeto que contiene el resultado de la estimación
abb.clogit<-clogit(choice~price+energy_loss+maintenance+warranty
                   +spare_parts+ease_install+problem_solving+quality+DA+DB+DC+strata(id), data=abb)
#La funcion summary()  muestra los resultados de la estimacion del modelo
summary(abb.clogit)
#La función predict() nos predice la utilidad que cada inidividuo obtiene
abb.predict<-predict(abb.clogit)

abb.predict.exp<-exp(abb.predict)
#sumamos el exp() de la utilidad para cada individuo
sum.exp<-by(abb.predict.exp, abb$id, sum)

head(sum.exp)
#Calculamos la probabilidad de elección de cada marca. Para ello definimos una función
#que llamaremos prob.eleccion()
prob.eleccion<-function(suma, eutil, indiv){
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
#calculamos la probalidad de elección
eleccion<-prob.eleccion(sum.exp,abb.predict.exp, 88)
head(eleccion)
#Guardamos el resultado en fichero de datos abb
abb$eleccion<-eleccion
#Para segmentos a los clientes en función de su probilidad de escoger nuestra marca
#creamos la función segmentos
segmentos<-function(p, indiv){
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
#Ahora utilizamos la nueva funcion segmentos() para segmentar la
#muestra de clientes según su probablidad de cambio
abb.segmentos<-segmentos(eleccion, 88)
abb$segmentos<-abb.segmentos
#Ahora utilizamos la nueva funcion para ordenar la base de datos
#según el volmen de ventas de cada cliente, de mayor a menor
abb.ordenado<-order(abb$volume, decreasing=TRUE)
#una vez ordenado componemos una tabla con los datos de ventas, la probabilidad de elección y la clasificación
abb.ordenado.volumen<-cbind(abb$volume[abb.ordenado], abb$eleccion[abb.ordenado], abb$segmentos[abb.ordenado])

head(abb.ordenado.volumen)

