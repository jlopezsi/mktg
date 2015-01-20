library(gdata)
getwd()
offers <- read.xls("wine/clustering-vanilla.xls", sheet=1, header=TRUE)
transactions <- read.xls("wine/clustering-vanilla.xls", sheet=2, header=TRUE)
head(offers)
head(transactions)
#Load Library
library(reshape)

# Melt transactions, cast offer by customers
pivot<-melt(transactions[1:2])
pivot<-(cast(pivot,value~Customer.Last.Name,fill=0,fun.aggregate=function(x) length(x)))

# Bind to offers, we remove the first column of our new pivot because it's redundant. 
pivot<-cbind(offers,pivot[-1])
#We can output the pivot table into a new CSV file called pivot.
#write.csv(file="pivot.csv",pivot)
# Load library
install.packages('fpc')
library(fpc)

# Only use customer transaction data and we will rotate the matrix
cluster.data<-pivot[,8:length(pivot)]
cluster.data<-t(cluster.data)

# We will run KMeans using pamk (more robust) with 4 clusters. 
cluster.kmeans<-pamk(cluster.data,k=4)

# Use this to view the clusters
View(cluster.kmeans$pamobject$clustering)

#Merge Data
cluster.deals<-merge(transactions[1:2],cluster.kmeans$pamobject$clustering,by.x = "Customer.Last.Name", by.y = "row.names")
colnames(cluster.deals)<-c("Name","Offer","Cluster")

# Melt, cast, and bind
cluster.pivot<-melt(cluster.deals,id=c("Offer","Cluster"))
cluster.pivot<-cast(cluster.pivot,Offer~Cluster,fun.aggregate=length)
cluster.topDeals<-cbind(offers,cluster.pivot[-1])

#We can then reproduce the excel version by writing to a csv file:
#write.csv(file="topdeals.csv",cluster.topDeals,row.names=F)

##Data Smart

library(gdata)
getwd()
input.file <- "wine/clustering-vanilla.xls"
deals <- read.xls(input.file, sheet=1, header=TRUE)
offers <- read.xls("wine/clustering-vanilla.xls", sheet=2, header=TRUE)
head(offers)
str(offers)
head(deals)
colnames(offers) <-c("name", "offer")
offers$value <- 1
offers.data <- cast(offers, name~offer, sum)[,2:33]
rownames(offers.data) <- cast(offers, name~offer, sum)[,1]
set.seed(1)
km.out                <- kmeans(offers.data,4,nstart=25)
deals.temp            <- cbind(deals, (t(km.out$centers)))
deals.temp[order(deals.temp$"1",decreasing = TRUE),1:6][1:10,]
deals.temp[order(deals.temp$"2",decreasing = TRUE),1:6][1:10,]
deals.temp[order(deals.temp$"3",decreasing = TRUE),1:6][1:10,]
deals.temp[order(deals.temp$"4",decreasing = TRUE),1:6][1:10,]

#As one can see, the above analysis does not given any conclusive results. Instead one can look at deal counts in each cluster

cluster               <-  data.frame(name = (rownames(offers.data)),
                                     cluster = km.out$cluster)
deals.by.cluster      <- merge(offers, cluster, all.x= T)
temp                  <- cast(deals.by.cluster, offer~cluster, sum)
temp                  <- cbind(deals,temp)

#The first cluster is small timers
temp[order(temp$"1",decreasing = TRUE),1:6][1:10,]
#The second cluster is not clear
temp[order(temp$"2",decreasing = TRUE),1:6][1:10,]
#The third cluster is Pinot Noir variety
temp[order(temp$"3",decreasing = TRUE),1:6][1:10,]

#The fourth cluster seems to like August Champaign
temp[order(temp$"4", decreasing = TRUE), 1:6][1:10,]

#One can try K means for varying K and pick one of the k values. 
#The chapter suggests another way to compare the K means across various k values,
#i.e by computing a score for your clusters called the silhouette. 
#The following R code gives the metric. 
#You can also use the silhouette function from the cluster package.

silhouette.rk <- function(cluster,dist.euclidean){
clusters <- sort(unique(cluster$cluster))
silh  <- numeric()
  for(i in cluster$id){
    temp              <- subset(cluster, id!=i)
    temp.cluster      <- subset(cluster, id==i)$cluster
    same.cluster      <- subset(temp, cluster == temp.cluster)
    diff.cluster      <- subset(temp, cluster != temp.cluster)
    i.star            <- pmin(i,same.cluster$id)
    j.star            <- pmax(i,same.cluster$id)
    within            <- mean(dist.euclidean[ n*(i.star-1) -
                                                i.star*(i.star-1)/2 + j.star-i.star ])
    neighbor          <- min( sapply( clusters[-temp.cluster],function(j)
    {
      i.star  <- pmin(i,subset(diff.cluster, cluster== j)$id)
      j.star  <- pmax(i,subset(diff.cluster, cluster== j)$id)
      mean(dist.euclidean[ n*(i.star-1) - i.star*(i.star-1)/2 + j.star-i.star ])
    }
    ))
    silh <- c(silh , (neighbor-within)/max(within, neighbor))
  }
  mean(silh) }

#For K = 4 clusters, one can calculate silhouette as follows:
set.seed(1)
dist.euclidean    <- dist(offers.data)
n                 <- attr(dist.euclidean, "Size")
km.out            <- kmeans(offers.data,4,nstart=25)
cluster           <-  data.frame(name = (rownames(offers.data)),
                                 cluster = km.out$cluster, id = 1:nrow(cluster) )
print(silhouette.rk(cluster, dist.euclidean))
## [1] 0.1243 
library(cluster)
print((summary(silhouette(km.out$cluster, dist.euclidean)))$avg.width) 
## [1] 0.1243
#For K = 5 clusters, one can calculate silhouette as follows:
set.seed(1)
km.out                <- kmeans(offers.data,5,nstart=25)
cluster           <-  data.frame(name = (rownames(offers.data)),
                                 cluster = km.out$cluster, id = 1:nrow(cluster) )
print(silhouette.rk(cluster,dist.euclidean))
## [1] 0.1231 
print((summary(silhouette(km.out$cluster,dist.euclidean)))$avg.width) 
## [1] 0.1231
#The above metric shows that 5 clusters is no better than 4 clusters.
#The chapter subsequently introduces a diffrent way to do K means clustering, 
#i.e. Spherical K means. This is a method where the dissimilarity measure is 
#based on correlation-based distance. 
#The package in R that does spherical K means is skmeans.
library(skmeans)
set.seed(1)
sk.out                <- skmeans(as.matrix(offers.data), 5, method="genetic")
cluster               <-  data.frame(name = (rownames(offers.data)),
                                     cluster = sk.out$cluster, id = 1:nrow(cluster) )
deals.by.cluster      <- merge(offers, cluster, all.x= T)
temp                  <- cast(deals.by.cluster, offer~cluster, sum)
temp                  <- cbind(deals,temp)
temp                  <- cbind(deals,temp)
#The first cluster is Pinot Noir gang
temp[order(temp$"1",decreasing = TRUE),1:6][1:10,]
#The second cluster looks like small timers
temp[order(temp$"2",decreasing = TRUE),1:6][1:10,]
#The third cluster is is high volume deals segment
temp[order(temp$"3",decreasing = TRUE),1:6][1:10,]
#The fourth cluster is France buyer segment
temp[order(temp$"4",decreasing = TRUE),1:6][1:10,]

#The fifth cluster are those who buy only sparkling wine
temp[order(temp$"5",decreasing = TRUE),1:6][1:10,]
