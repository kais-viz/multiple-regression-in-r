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

set.seed(66)

existingDf <- read.csv("existingproductattributes2017.csv")
tempdf <- dummyVars(" ~ .", data = existingDf)
existingData <- data.frame(predict(tempdf, newdata = existingDf))
existingData$sumReviews <- rowSums(existingData[,c("x5StarReviews", 
                                                   "x4StarReviews", 
                                                   "x3StarReviews", 
                                                   "x2StarReviews", 
                                                   "x1StarReviews")])
existingData %>% select(c(PositiveServiceReview, 
                          NegativeServiceReview, 
                          sumReviews, 
                          ProductType.Smartphone,
                          Volume)) -> ExistingSub
#tell the system not to plot the outliers
outliers <- boxplot(ExistingSub$Volume, plot=FALSE)$out
ExistingSub <- ExistingSub[-which(ExistingSub$Volume %in% outliers),]

#Normalise/scale attributes except the dependent feature
ExistingSub[,c(1:4)] <- lapply(ExistingSub[,c(1:4)] , scale)

###Creating a linear model (Random Forest)
inTraining <- createDataPartition(ExistingSub$Volume, p = .75, list = FALSE)
training <- ExistingSub[inTraining,]
testing <- ExistingSub[-inTraining,]

#filtering test set to include only netbook products
testingLimited <- testing %>% 
  dplyr::filter(testing$ProductType.Smartphone > 1)

set.seed(299)

SVMControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 3,
                           search="random")
SVMgrid <- expand.grid(C = c(1,5,10,18,50))

SVMModel <- train(Volume ~., data = training, method = "svmLinear",
                  trControl=SVMControl,
                  tuneGrid = SVMgrid)
SVMModel

varImp(SVMModel)

pred <- predict(SVMModel, newdata = testingLimited)
summary(pred)

postResample(pred, testingLimited$Volume)
