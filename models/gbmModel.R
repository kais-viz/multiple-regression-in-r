###Creating a linear model (Gradient boosing models)
library(ggplot2, quietly = T)
library(caret, quietly = T)
library(readr, quietly = T)
library(C50, quietly = T)
library(mlbench, quietly = T)
library(bda, quietly = T)
library(zoo, quietly = T)
library(dplyr, quietly=T)
library(corrplot, quietly=T)
library(reshape2, quietly=T)
library(e1071, quietly=T)
library(gbm)

set.seed(2333)

existingDf <- read.csv("existingproductattributes2017.csv")
tempdf <- dummyVars(" ~ .", data = existingDf)
existingData <- data.frame(predict(tempdf, newdata = existingDf))
existingData$sumReviews <- rowSums(existingData[,c("x5StarReviews", "x4StarReviews", "x3StarReviews", "x2StarReviews", "x1StarReviews")])
existingData %>% select(c(PositiveServiceReview, sumReviews, Volume, ProductType.PC)) -> ExistingSub
#tell the system not to plot the outliers
outliers <- boxplot(ExistingSub$Volume, plot=FALSE)$out
ExistingSub <- ExistingSub[-which(ExistingSub$Volume %in% outliers),]

###Creating a linear model (Random Forest)
inTraining <- createDataPartition(ExistingSub$Volume, p = .75, list = FALSE)
training <- ExistingSub[inTraining,]
testing <- ExistingSub[-inTraining,]

#10 fold cross validation repeated 1 times
gbmControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)

#Grid to define our own parameters for this classifier
gbmGrid <-  expand.grid(interaction.depth = c(3,5), 
                        n.trees = 10, 
                        shrinkage = c(0.1),
                        n.minobsinnode = 10)


#Set seed to know the random order
set.seed(998)

#Train GBM Regression model
gbmModel <- train(Volume~., data = training, 
                  method = "gbm", 
                  trControl = gbmControl, 
                  verbose = FALSE, 
                  ## specifying the exact models 
                  ## to evaluate:
                  tuneGrid = gbmGrid,
                  importance = T)

#training results
gbmModel

#varimp
varImp(gbmModel)

#predict on testing
pred <- predict(gbmModel, newdata = testing)
summary(pred)

postResample(pred, testing$Volume)


dataList=list("Hello", c(1, 0.1, 1.5), c("India", "Blue", "76"))
for(i in dataList){
  if(i >)
  print(i)
  }