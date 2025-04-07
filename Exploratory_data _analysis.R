################################
#       Loading data set
################################

library(datasets)
data("iris")

################################
#       Display Summary
################################

head(iris,4)
tail(iris,4)

#summary
summary(iris)
summary(iris$Sepal.Length)

#check if ther any missing values
sum(is.na(iris))

#skimr()-expands on summary() by providing larger set of statistic
# installl.packages(skimr)
library(skimr)
skim(iris) #perform skim to display summary statistic

##group data by species them perform skim
iris %>%
  dplyr::group_by(Species) %>%
  skim()


################################
# Quick data visulization
#
# R base plot()
################################


#panel plots
plot(iris)
plot(iris,col = "green" )

#scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)
plot(iris$Sepal.Length,iris$Sepal.Width, col = "red") # makes the red circles

plot(iris$Sepal.Length,iris$Sepal.Width, col = "red", xlab = "Sepal length",ylab = "Sepal width")#add axis labels

#histogram
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col="red")

#feature plot
install.packages("caret")
library(caret)
featurePlot(x= iris[,1:4],
            iris$Species,
            plot= "box",
            strip=strip.custom(par.strip.text= list(cex =.7)),
            scales = list(x= list(relation = "free"),
                          y= list(relation = "free")))


      

