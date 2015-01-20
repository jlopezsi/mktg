# Cargar las bibliotecas necesarias en el siguiente
#lectura de datos
install.packages('s20x')
install.packages('car')
library(s20x)
library(car)
#read the dataset from an existing .csv file
url<-"http://www.dataapple.net/wp-content/uploads/2013/04/grapeJuice.csv"
df <- read.csv(url,header=T)
#list the name of each variable (data column) and the first six rows of the dataset
head(df)
# basic statistics of the variables
str(df)
summary(df)
#exploración datos

#Fijar dos gráficos por fila
par(mfrow = c(1,2))

# boxplot: comprobar la presencia de observciones distorsionantes
boxplot(df$sales,horizontal = TRUE, xlab="sales", main="Sales variation")
# histogram: comprobar la distribución de las variables
hist(df$sales,main="",xlab="sales",prob=T)
lines(density(df$sales),lty="dashed",lwd=2.5,col="red")
par(mfrow = c(1,1))
#Análisis de la eficacia del anuncio
#Dividimos la base de datos en dos, según el tipo de anuncio
  
sales_ad_nature = subset(df,ad_type==0)
sales_ad_family = subset(df,ad_type==1)

#calculate the mean of sales with different ad_type
 mean(sales_ad_nature$sales)
#[1] 186.6667
mean(sales_ad_family$sales)
#[1] 246.6667

#test de diferencias
#Fijar dos gráficos por fila
par(mfrow = c(1,2))

# histogram: explorar normalidad
hist(sales_ad_nature$sales,main="",xlab="Nature production ad",prob=T)
lines(density(sales_ad_nature$sales),lty="dashed",lwd=2.5,col="red")

hist(sales_ad_family$sales,main="",xlab="Family health ad",prob=T)
lines(density(sales_ad_family$sales),lty="dashed",lwd=2.5,col="red")
par(mfrow = c(1,1))

#test de normalidad de las variables
shapiro.test(sales_ad_nature$sales)
shapiro.test(sales_ad_family$sales)

#Test de diferencias de medias
t.test(sales_ad_nature$sales,sales_ad_family$sales)

# función de las ventas
pairs(df,col="blue",pch=20)
pairs20x(df)

sales.reg<-lm(sales~price+ad_type+price_apple+price_cookies,df)
summary(sales.reg)

#Supuestos del modelo de regresión lineal
# visualizando la relación entre los residuos y otras variables
#fijar los gráficos
par(mfrow=c(2,2))
plot(sales.reg)
par(mfrow=c(1,1))
#controlar la presencia de multicolinearidad
vif(sales.reg)

# Decisiones óptimas

f = function(x) -51.24*x^2 + 1028.84*x - 3863.2
optimize(f,lower=0,upper=20,maximum=TRUE)

# predicir ventas
inputData <- data.frame(price=10,ad_type=1,price_apple=7.659,price_cookies=9.738)
predict(sales.reg,inputData,interval="p")

# load the Oracle R Enterprise library and connect to Oracle Database
install.packages('ore')
library(ore)
ore.connect(user = "<DBUser>",sid = "orcl",host = "<host name>",password = "<password>",port = 1521,all = TRUE)
# regression by ore.lm
sales.reg <- ore.lm(SALES ~ PRICE+AD_TYPE+PRICE_APPLE+PRICE_COOKIES, data = GRAPEJUICE)
summary(sales.reg)





