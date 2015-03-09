## data, packages, random seed
#data("BBBClub", package = "evtree")
BBBClub.choice<-read.delim("~/Documents/github/mktg-models-projects/bbbclub/R-Bookbinders Book Club Data (Customer Choice).txt")
BBBClub.holdout<-read.delim("~/Documents/github/mktg-models-projects/bbbclub/R-Bookbinders Book Club Data (Customer Choice) Holdout Sample.txt")

head(BBBClub.choice)
summary(BBBClub.holdout$last)
#RFM model
BBBClub.holdout$R <- ifelse(BBBClub.holdout$last<3, 25, ifelse(BBBClub.holdout$last<6 | BBBClub.holdout$last>=3, 20, ifelse(BBBClub.holdout$last<9 | BBBClub.holdout$last>=6, 10, ifelse(BBBClub.holdout$last<18 |BBBClub.holdout$last>=9, 5, 0))))
head(BBBClub.holdout$R)

BBBClub.holdout$F <- ifelse(BBBClub.holdout$freq<10, 10, ifelse(BBBClub.holdout$freq<20 | BBBClub.holdout$freq>=10, 20, ifelse(BBBClub.holdout$freq<30 | BBBClub.holdout$freq>=20, 30, ifelse(BBBClub.holdout$freq<40 | BBBClub.holdout$freq>=30, 40, 50))))
head(BBBClub.holdout$F)

BBBClub.holdout$M <- ifelse(BBBClub.holdout$amount<=50, 10, ifelse(BBBClub.holdout$amount<=150 , 20, ifelse(BBBClub.holdout$amount<=250 , 30, ifelse(BBBClub.holdout$amount<=350 , 40, 50))))
head(BBBClub.holdout)

BBBClub.holdout$RFM<-BBBClub.holdout$R +BBBClub.holdout$F+BBBClub.holdout$M
head(BBBClub.holdout)

##Regression
BBBClub.lm <- lm(choice~gender+amount+freq+last+first+child+youth+cook+diy+art, data=BBBClub.choice)
library(stargazer)
stargazer(BBBClub.lm, type="text")

BBBClub.holdout$plm <- predict(BBBClub.lm, BBBClub.holdout)
head(BBBClub.holdout)

BBBClub.glm<-glm(choice~gender+amount+freq+last+first+child+youth+cook+diy+art, family=binomial(), data=BBBClub.choice)
stargazer(BBBClub.glm, type="text")

BBBClub.holdout$pglm <- predict(BBBClub.glm, BBBClub.holdout, type="response")
head(BBBClub.holdout)

quantile(BBBClub.holdout$RFM, probs=seq(.1,1,.1))
quantile(BBBClub.holdout$plm, probs=seq(.1,1,.1))
