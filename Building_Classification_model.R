#####Building a classification data Model#######

library(datasets) #contains the iris data set
library(caret) #package for ML algorithms 

data(iris) #Import the iris data set

sum(is.na(iris)) #checking is there any missing values in the data set
set.seed(100) #To achieve reproducible model;set the random seed number

#performance stratified random split of the data set
Trainingindex <-createDataPartition(iris$Species,p=0.8,list = FALSE)
TrainingSet <-iris[Trainingindex,] #Training set
TestingSet <-iris[-Trainingindex,] #Test set

#compare scatter plot of the for 80 and 20 data subsets

install.packages("cowplot")
library(ggplot2)
library(cowplot)

p1 <- ggplot(TrainingSet) + 
  geom_point(aes(x=Sepal.Length,y=Petal.Length,color=Species)) +
  scale_color_manual(values=c("#135029", "#E1AD01", "#E2725B")) +
  ggtitle("Training Set")

p2 <- ggplot(TestingSet) + 
  geom_point(aes(x=Sepal.Length,y=Petal.Length,color=Species)) +
  scale_color_manual(values=c("#135029", "#E1AD01", "#E2725B")) +
  ggtitle("Testing Set")

# Combine the plots using cowplot::plot_grid() then display it
multi_plot <- plot_grid(p1, p2, ncol = 2)
multi_plot



###SVM Model(polynomial Kernal)###

#Build training Model
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale", "center"),
               trControl = trainControl(method = "none"),
               tuneGrid = data.frame(degree = 1, scale = 1, C = 1))

#Build CV Model

Model.cv <-train(Species~.,data = TrainingSet,
                 method = "svmPoly",
                 na.action = na.omit,
                 preProcess=c("scale","center"),
                 trControl = trainControl(method ="cv",number=10),
                 tuneGrid = data.frame(degree = 1,scale=1,C=1))

#Apply model for prediction 
Model.training <-predict(Model,TrainingSet)#Apply model to make prediction on TrainingSet
Model.testing <-predict(Model,TestingSet)#Apply model to make prediction on TestingSet
Model.cv <-predict(Model.cv,TrainingSet)

#Model Performance(Display confusion and matrix and statistics)
Model.training.confsion <- confusionMatrix(Model.training,TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing,TestingSet$Species)
Model.cv.cofusion <-confusionMatrix(Model.cv,TrainingSet$Species)

print(Model.training.confsion)
print(Model.testing.confusion)
print(Model.cv.cofusion)

#feature Importance
Importance <-varImp(Model)
plot(Importance,col="red")
