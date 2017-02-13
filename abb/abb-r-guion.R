#read data

#la opción file.chose() en la función read.table nos permite escoger un fichero de datos guardado en el ordenador local)
#read abb-R.txt
#abb<-read.table(file.choose(), header=T)
abb<-read.table("abb-r.txt", header=T)
#la función head()nos permite visualizar las primer seis líneas de un objeto de datos
head(abb)
#la funcion names() muestra los nombres de las variables 
names(abb)
#install.packages("dplyr")
library(dplyr)
#El paquete survival continene la función clogit para estimar el moledo de elección discreta
install.packages("survival")
library(survival)
#Estimacion del modelo con la función clogit.  la opción strata() con el argumento
#id controlar las observaciones repetidas para cada individuo
#abb.clogit es el objeto que contiene el resultado de la estimación
abb.clogit<-clogit(choice~price+energy_loss+maintenance+warranty
                   +spare_parts+ease_install+problem_solving+quality+DA+DB+DC+strata(id), data=abb)
#La funcion summary()  muestra los resultados de la estimacion del modelo
summary(abb.clogit)

#install.packages("stargazer")
library(stargazer)
stargazer(abb.clogit, no.space = TRUE, type='text',  title="Regression")

source("marketing-models.R")


eleccion<-prob.of.choice(abb.clogit, 88)

round(head(eleccion), digits=2)

#Guardamos el resultado en fichero de datos abb
abb$eleccion<-eleccion
#Para segmentos a los clientes en función de su probilidad de escoger nuestra marca
#creamos la función segmentos

#Ahora utilizamos la nueva funcion segmentos() para segmentar la
#muestra de clientes según su probablidad de cambio
abb.segmentos<-segments(eleccion, 88)
head(abb.segmentos)
abb$segmentos<-abb.segmentos
#Ahora utilizamos la nueva funcion para ordenar la base de datos
#según el volmen de ventas de cada cliente, de mayor a menor
head(abb)
abb.ordenado<-order(abb$volume, decreasing=TRUE)
#una vez ordenado componemos una tabla con los datos de ventas, la probabilidad de elección y la clasificación
abb.ordenado.volumen<-data.frame(id=abb$id[abb.ordenado], choice=abb$Alternatives[abb.ordenado], volume=abb$volume[abb.ordenado], volumeExpected=abb$volume[abb.ordenado]*abb$eleccion[abb.ordenado], prob=abb$eleccion[abb.ordenado], segment=abb$segmentos[abb.ordenado])
options(digits=3)
class(abb.ordenado.volumen)
head(abb.ordenado.volumen)
unique(abb.ordenado.volumen$id)

library(dplyr)

arrange(abb.ordenado.volumen, segment)

group_by(abb.ordenado.volumen, segment) %>%
  summarise(
    n=n(),
    volume=sum(volume),
    volumenExprected=sum(volumeExpected)
  )

########Recortes introducidos en la funcion prob.of.choice.()

