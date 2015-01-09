
require(ggplot2)
require(dplyr)
train <- read.csv("https://raw.githubusercontent.com/campbwa/R-videos/master/train.csv")
head(train)
#key function
#select select columns from a dataframe
#filter selec rows form a adata frame based upon criteria
#group_by group by a afactor variable
#summarize allos you to do summary stats based upon the grouped variable
#arrange a better way to order the data set

train.select <- select(train, Survived, Pclas, Sex, Age, Sibsp, Parch, Fare)
#Everything but PassangerId
train.select = select(train, -PassengerId)

#From suvived to age
train.select = select(train, Survived:Age)
#Only first class passangers
FirstClass <- filter(train, Pclass == 1)
#Only frist class male pasagners
FirstClass <- filter(train, Pclass == 1, Sex =="male")
#Ordering the rows

train <- arrange(train, Fare, Pclass)

#average fare by passenger
select(train, Pclass, Fare, Survived) %>%
  group_by(Pclass) %>% #don't have to specify the data ser a second time
  summarize(AvgFare= mean(Fare), Probsurvived = mean(Survived), N = length(Fare))

#proabbility of survival by gender
select(train, Sex, Survived) %>%
  group_by(Sex) %>%
  summarize(ProbSurived = mean(Survived))

#both categories at the same time!
#group by two variables
A= select(train, Pclass, Sex, Fare, Survived) %>%
  group_by(Pclass, Sex) %>%
  summarize(AvgFare = mean(Fare), ProbSurvived = man(Survived), N = length(Fare))
A

#dpllyr makes it easier to produce goruped plots in ggplot

ggplot(A, aes(x=Pclass, y=ProbSurvived)) +
  geom_point(aes(size = N)) +
  geom_line(aes(by = Sex, color = Sex))

#use of the mutate function

train = mutate(train
               , AgeByFare = Age * Fare
               , Age1 = Age^2)
#alows you to create variables in one step
#without specifying the data frame a bunch of times


