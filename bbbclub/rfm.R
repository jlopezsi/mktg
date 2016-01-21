#read CDNOW_sample.txt
CDNOW_sample <- read.table(file.choose(), quote="\"")
getwd()
source("bbbclub/rfm-CDNOW.R")

# construct a data frame with the necessary columns of customer ID, 
#transaction date, and money amount paid by a customer per transaction
df <- as.data.frame(cbind(CDNOW_sample[,1],CDNOW_sample[,3],CDNOW_sample[,5]))
# add appropriate column names for the above three column and
names <- c("ID", "Date", "Amount")
names(df) <- names
#tranfer the the text column type to date type
df[,2] <- as.Date(as.character(df[,2]),"%Y%m%d")
head(df)

dim(df)
#remove the rows with the duplicated IDs to see how many customers in total
uid <- df[!duplicated(df[,"ID"]),]
dim(uid)
# set the startDate and endDate, we will only analysis the records in this date range
startDate <- as.Date("19970101","%Y%m%d")
endDate <- as.Date("19980701","%Y%m%d")

df <- getDataFrame(df,startDate,endDate)
head(df)

############

                                            
########


df1 <-getIndependentScore(df)
head(df1[-(2:3)])
#Draw the histograms in the R, F, and M dimensions so that we can see the distribution of customers in each RFM cell.
drawHistograms(df1[-c(5:6)])

S500<-df1[df1$Total_Score>500,]
dim(S500)

S400<-df1[df1$Total_Score>400,]
dim(S500)
par(mfrow = c(1,3))
hist(df$Recency)
hist(df$Frequency)
hist(df$Monetary)
par(mfrow = c(1,1))
# set the Recency ranges as 0-120 days, 120-240 days, 240-450 days, 450-500days, and more than 500days.
r <-c(120,240,450,500)
# set the Frequency ranges as 0 – 2times, 2-5 times,5-8 times, 8-10 times, and more than 10 times.
f <-c(2,5,8,10)
# set the Monetary ranges as 0-10 dollars, 10-20 dollars, and so on.
m <-c(10,20,30,100)
df2<-getScoreWithBreaks(df,r,f,m)
drawHistograms(df2)

#We can also calculate how many customers have a total score of more than 500 or 400.
S500<-df2[df2$Total_Score>500,]
dim(S500)

drawHistograms(df2)
S400<-df2[df2$Total_Score>400,]
dim(S400)
target <- df2[df2$Total_Score>=441,]
dim(target)


