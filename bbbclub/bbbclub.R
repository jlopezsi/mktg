## data, packages, random seed
#data("BBBClub", package = "evtree")
#Read data patch: ~/Documents/github/mktg-models-projects/bbbclub/
BBBClub.choice<-read.delim("R-Bookbinders Book Club Data (Customer Choice).txt")
BBBClub.holdout<-read.delim("R-Bookbinders Book Club Data (Customer Choice) Holdout Sample.txt")

head(BBBClub.choice)
head(BBBClub.holdout)

#RFM model

###1st we apply the RFM model to the new data

source("marketing-models.R")
BBBClub.holdout <- RFM(BBBClub.holdout)


head(BBBClub.holdout)

BBBClub.holdout$RFM<-BBBClub.holdout$R +BBBClub.holdout$F+BBBClub.holdout$M

head(BBBClub.holdout)

hist(BBBClub.holdout$R)
hist(BBBClub.holdout$F)
hist(BBBClub.holdout$M)
hist(BBBClub.holdout$RFM)

##Now we calibrate the model using a Linear regression model. We use the choice model

BBBClub.lm <- lm(choice~gender+amount+freq+last+first+child+youth+cook+diy+art, data=BBBClub.choice)

install.packages("stargazer")
library(stargazer)
stargazer(BBBClub.lm, type="text")

#test model with holdout sample (we predict the probability of buying the art book using de model from choice data)

BBBClub.holdout$plm <- predict(BBBClub.lm, BBBClub.holdout)
head(BBBClub.holdout)

###########################################################
#test model with holdout sample (we predict the probability of buying the art book using de model from choice data using a Logistic regression model)

BBBClub.glm<-glm(choice~gender+amount+freq+last+first+child+youth+cook+diy+art, family=binomial(), data=BBBClub.choice)
stargazer(BBBClub.glm, type="text")

BBBClub.holdout$pglm <- predict(BBBClub.glm, BBBClub.holdout, type="response")
head(BBBClub.holdout)

quantile(BBBClub.holdout$RFM, probs=seq(.1,1,.1))
plot(quantile(BBBClub.holdout$plm, probs=seq(.1,1,.1)))

#Test accuracy of prediction

confusion.glm(BBBClub.choice, BBBClub.glm)

##use logist regression model for decision

#Number of people that bought the book in the choice sample

exit<-sum(BBBClub.holdout$choice)
exit

install.packages("dplyr")
library(dplyr)

#order the data base for logistic model

BBBClub.holdout.bypglm<-arrange(BBBClub.holdout, desc(pglm))
head(BBBClub.holdout.bypglm)
plot(BBBClub.holdout.bypglm$plm, type="h")

#Prepare the report for logistic model
source("marketing-models.R")

#save quartiles
BBBClub.holdout.bypglm$quartile <- ntile(BBBClub.holdout.bypglm$pglm, 10) 

BBBClub.holdout.bypglm <- reverse.quartile(BBBClub.holdout.bypglm)
plot(BBBClub.holdout.bypglm$quartile, BBBClub.holdout.bypglm$plm)
plot(BBBClub.holdout.bypglm$plm)
#check

head(BBBClub.holdout.bypglm)

#Cummulative distribution of exit (what would have happened if we had used glm to target segments)

BBBClub.holdout.bypglm$choice2<-BBBClub.holdout.bypglm$choice/sum(BBBClub.holdout.bypglm$choice)
BBBClub.holdout.bypglm$acumul<-cumsum(BBBClub.holdout.bypglm$choice2)
head(BBBClub.holdout.bypglm)
#by quartile count the exit ratio

table1<-BBBClub.holdout.bypglm %>%
  group_by(decil) %>%
  summarize(
    count=n(),
    mean.choice=sum(choice)/204
  )
table1
#Add the cumulative distribution 
table1$acumul<-cumsum(table1$mean.choice)
table1
#Add the cumulative mailing to be sent
units.mailed<- c(5000,10000,15000,20000,25000,30000,35000,40000,45000,50000)
units.mailed
#add them to table
table1$mailed<-units.mailed
#Add cost
table1$cost<-table1$mailed*0.65
#Add market potential
market.potential<-50000*(exit/2300)
#Add units sold
table1$sold<-table1$acumul*(market.potential)
table1
#Add profit
margin<-31.95-1.45*15
margin
table1$profit<-table1$sold*margin-table1$cost
table1
plot(table1$profit)

sum(table1$profit)
sum(table1$cost)
total.profit1<-sum(table1$profit) - sum(table1$cost)
total.profit1
rendimiento1<-total.profit1/sum(table1$cost)
rendimiento1

########### linear model##############
#order the data base for linear model

BBBClub.holdout.byplm<-arrange(BBBClub.holdout, desc(plm))
head(BBBClub.holdout.byplm)

#Prepare the report for logistic model
source("marketing-models.R")

#save quartiles
BBBClub.holdout.byplm$quartile <- ntile(BBBClub.holdout.byplm$plm, 10) 

BBBClub.holdout.byplm <- reverse.quartile(BBBClub.holdout.byplm)

#check

head(BBBClub.holdout.byplm)

#Cummulative distribution of exit

BBBClub.holdout.byplm$choice2<-BBBClub.holdout.byplm$choice/sum(BBBClub.holdout.byplm$choice)
BBBClub.holdout.byplm$acumul<-cumsum(BBBClub.holdout.byplm$choice2)

#by quartile count the exit ratio

table.lm<-BBBClub.holdout.byplm %>%
  group_by(decil) %>%
  summarize(
    count=n(),
    mean.choice=sum(choice)/204
  )
table.lm
#Add the cumulative distribution 
table.lm$acumul<-cumsum(table.lm$mean.choice)
table.lm
#Add the cumulative mailing to be sent
units.mailed<- c(5000,10000,15000,20000,25000,30000,35000,40000,45000,50000)
units.mailed
#add them to table
table.lm$mailed<-units.mailed
#Add cost
table.lm$cost<-table.lm$mailed*0.65
#Add market potential
market.potential<-50000*(exit/2300)
#Add units sold
table.lm$sold<-table.lm$acumul*(market.potential)
table.lm
#Add profit
margin<-31.95-1.45*15
margin
table.lm$profit<-table.lm$sold*margin-table.lm$cost
table.lm
plot(table.lm$profit)

sum(table.lm$profit)
sum(table.lm$cost)
total.profit.lm<-sum(table.lm$profit) - sum(table.lm$cost)
total.profit.lm
rendimiento.lm<-total.profit.lm/sum(table.lm$cost)
rendimiento.lm

######RFM model report #########

#order the data base for RFM model

BBBClub.holdout.byRFM<-arrange(BBBClub.holdout, desc(RFM))
head(BBBClub.holdout.byRFM)

#Prepare the report for logistic model
source("marketing-models.R")

#save quartiles
BBBClub.holdout.byRFM$quartile <- ntile(BBBClub.holdout.byRFM$RFM, 10) 

BBBClub.holdout.byRFM <- reverse.quartile(BBBClub.holdout.byRFM)

#check

head(BBBClub.holdout.byRFM)

#Cummulative distribution of exit

BBBClub.holdout.byRFM$choice2<-BBBClub.holdout.byRFM$choice/sum(BBBClub.holdout.byRFM$choice)
BBBClub.holdout.byRFM$acumul<-cumsum(BBBClub.holdout.byRFM$choice2)

#by quartile count the exit ratio

table.RFM<-BBBClub.holdout.byRFM %>%
  group_by(decil) %>%
  summarize(
    count=n(),
    mean.choice=sum(choice)/204
  )
table.RFM
#Add the cumulative distribution 
table.RFM$acumul<-cumsum(table.RFM$mean.choice)
table.RFM
#Add the cumulative mailing to be sent
units.mailed<- c(5000,10000,15000,20000,25000,30000,35000,40000,45000,50000)
units.mailed
#add them to table
table.RFM$mailed<-units.mailed
#Add cost
table.RFM$cost<-table.RFM$mailed*0.65
#Add market potential
market.potential<-50000*(exit/2300)
#Add units sold
table.RFM$sold<-table.RFM$acumul*(market.potential)
table.RFM
#Add profit
margin<-31.95-1.45*15
margin
table.RFM$profit<-table.RFM$sold*margin-table.RFM$cost
table.RFM
plot(table.RFM$profit)

sum(table.RFM$profit)
sum(table.RFM$cost)
total.profit.RFM<-sum(table.RFM$profit) - sum(table.RFM$cost)
total.profit.RFM
rendimiento.RFM<-total.profit.RFM/sum(table.RFM$cost)
rendimiento.RFM
