## data, packages, random seed
#data("BBBClub", package = "evtree")
#Read data
BBBClub.choice<-read.delim("~/Documents/github/mktg-models-projects/bbbclub/R-Bookbinders Book Club Data (Customer Choice).txt")
BBBClub.holdout<-read.delim("~/Documents/github/mktg-models-projects/bbbclub/R-Bookbinders Book Club Data (Customer Choice) Holdout Sample.txt")

head(BBBClub.choice)
Head(BBBClub.holdout)

#RFM model

BBBClub.holdout$R <- ifelse(BBBClub.holdout$last<3, 25, ifelse(BBBClub.holdout$last<6 | BBBClub.holdout$last>=3, 20, ifelse(BBBClub.holdout$last<9 | BBBClub.holdout$last>=6, 10, ifelse(BBBClub.holdout$last<18 |BBBClub.holdout$last>=9, 5, 0))))
head(BBBClub.holdout$R)

BBBClub.holdout$F <- ifelse(BBBClub.holdout$freq<10, 10, ifelse(BBBClub.holdout$freq<20 | BBBClub.holdout$freq>=10, 20, ifelse(BBBClub.holdout$freq<30 | BBBClub.holdout$freq>=20, 30, ifelse(BBBClub.holdout$freq<40 | BBBClub.holdout$freq>=30, 40, 50))))
head(BBBClub.holdout$F)

BBBClub.holdout$M <- ifelse(BBBClub.holdout$amount<=50, 10, ifelse(BBBClub.holdout$amount<=150 , 20, ifelse(BBBClub.holdout$amount<=250 , 30, ifelse(BBBClub.holdout$amount<=350 , 40, 50))))

head(BBBClub.holdout)

BBBClub.holdout$RFM<-BBBClub.holdout$R +BBBClub.holdout$F+BBBClub.holdout$M
head(BBBClub.holdout)

hist(BBBClub.holdout$R)
hist(BBBClub.holdout$F)
hist(BBBClub.holdout$M)
hist(BBBClub.holdout$RFM)

##Linear regression
BBBClub.lm <- lm(choice~gender+amount+freq+last+first+child+youth+cook+diy+art, data=BBBClub.choice)

library(stargazer)
stargazer(BBBClub.lm, type="text")

#test model with holdout sample
BBBClub.holdout$plm <- predict(BBBClub.lm, BBBClub.holdout)
head(BBBClub.holdout)

###########################################################
#Logistic regression

BBBClub.glm<-glm(choice~gender+amount+freq+last+first+child+youth+cook+diy+art, family=binomial(), data=BBBClub.choice)
stargazer(BBBClub.glm, type="text")
print(anova(BBBClub.glm, test="Chisq"))

#Plot predicted probabilities


BBBClub.holdout$pglm <- predict(BBBClub.glm, BBBClub.holdout, type="response")
head(BBBClub.holdout)

pdf(file = "bbbclub/bbbclub-fig_predicting_choice_density_evaluation.pdf", 
    width = 8.5, height = 8.5)
plotting_object <- densityplot( ~ pglm | choice, 
                                data = BBBClub.holdout, 
                                layout = c(1,2), aspect=1, col = "darkblue", 
                                plot.points = "rug",
                                strip=function(...) strip.default(..., style=1),
                                xlab="Predicted Probability of buying book") 
print(plotting_object) 
dev.off()

quantile(BBBClub.holdout$RFM, probs=seq(.1,1,.1))
quantile(BBBClub.holdout$plm, probs=seq(.1,1,.1))

#Test accuracy of prediction

confusion.glm <- function(data, model) {
  prediction <- ifelse(predict(model, data, type='response') > 0.5, TRUE, FALSE)
  confusion  <- table(prediction, as.logical(model$y))
  confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
  confusion  <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  confusion
}

confusion.glm(BBBClub.choice, BBBClub.glm)

##use logist regression model for decision

#Number of people that baught the book in the holdout sample

exit<-sum(BBBClub.holdout$choice)
exit

library(dplyr)

#order the data base

BBBClub.holdout.bypglm<-arrange(BBBClub.holdout, desc(pglm))
head(BBBClub.holdout.bypglm)

#save quartiles
BBBClub.holdout.bypglm$quartile <- ntile(BBBClub.holdout.bypglm$pglm, 10) 

#Reverse order of quartiles

BBBClub.holdout.bypglm$decil <- ifelse(BBBClub.holdout.bypglm$quartile==10, 1, 
                  ifelse(BBBClub.holdout.bypglm$quartile==9, 2, 
                         ifelse(BBBClub.holdout.bypglm$quartile==8, 3, 
                                ifelse(BBBClub.holdout.bypglm$quartile==7, 4, 
                                       ifelse(BBBClub.holdout.bypglm$quartile==6, 5, 
                                              ifelse(BBBClub.holdout.bypglm$quartile==5, 6, 
                                                     ifelse(BBBClub.holdout.bypglm$quartile==4, 7, 
                                                            ifelse(BBBClub.holdout.bypglm$quartile==3, 8, 
                                                                   ifelse(BBBClub.holdout.bypglm$quartile==2, 9, 10)
                                                                   )
                                                            )
                                                  
                                              )
                                       )
                                )
                         )
                  )

#check

head(BBBClub.holdout.bypglm)

#Cummulative distribution of exit

BBBClub.holdout.bypglm$choice2<-BBBClub.holdout.bypglm$choice/sum(BBBClub.holdout.bypglm$choice)
BBBClub.holdout.bypglm$acumul<-cumsum(BBBClub.holdout.bypglm$choice2)

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

