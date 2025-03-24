rm(list=ls()) ##removes all variables stored previously
library(Hmisc) #import

data <- read.csv("C:/Users/Admin/Downloads/COVID19_line_list_data.csv")
describe(data) #Hmisc command

##cleaned up data
data$death_dummy <-as.integer(data$death != 0)#if the death not equal to zero then the personn died 

##death rate
sum(data$death_dummy)/nrow(data)

#age
#claim: people who die are older
dead =subset(data,death_dummy == 1)
alive =subset(data,death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

#is this statistically significant? 
t.test(alive$age,dead$age,alternative = "two.sided",conf.level = 0.95)
#normally,if p value < 0.05,we reject null hypothesis
# here p value ~ 0,so we reject null hypotheseis


#gender
#claim: gender has no effect
men =subset(data,gender == "male")
women =subset(data,gender == "female")
mean(men$death_dummy, na.rm = TRUE)#8.5%
mean(women$death_dummy, na.rm = TRUE)#3.7%

#is this statistically significant? 
t.test(men$death_dummy,women$death_dummy,alternative = "two.sided",conf.level = 0.95)
# 95% confidence interval: men have from 1.7% to 7.8% higher chance of dying
# p value= 0.002 <0.05, so this is significant


