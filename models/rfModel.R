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

set.seed(150)

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
                          ProductType.Netbook,
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
  dplyr::filter(testing$ProductType.Netbook > 1)

rfControl <- trainControl(method = "repeatedcv", 
                          number = 10, 
                          repeats = 3, 
                          search="random")

#rfGrid <- expand.grid(mtry=c(9,10,11,12,13))

#Set seed to know the random order
set.seed(33)

#Creating RF model
rfModel <- train(Volume~., data = training, 
                 method = "rf",
                 trControl=rfControl, 
                 #tuneGrid=rfGrid, 
                 #classProbs = TRUE,
                 tuneLength=10, 
                 importance=T)

rfModel
varImp(rfModel)
pred <- predict(rfModel, newdata = testingLimited)
postResample(pred, testingLimited$Volume)

newDf <- read.csv("newproductattributes2017.csv")
tempdf <- dummyVars(" ~ .", data = newDf)
newData <- data.frame(predict(tempdf, newdata = newDf))
newData$sumReviews <- rowSums(newData[,c("x5StarReviews", "x4StarReviews", "x3StarReviews", "x2StarReviews", "x1StarReviews")])
newData %>% select(c(PositiveServiceReview, NegativeServiceReview, sumReviews, ProductType.Netbook,Volume)) -> newSub

#Normalise/scale attributes except the dependent feature
newSub[,c(1:4)] <- lapply(newSub[,c(1:4)] , scale)

#filtering test set to include only netbook products
newDataLimited <- newSub %>% 
  dplyr::filter(newSub$ProductType.Netbook > 1)


newDataLimited$Volume <- predict(rfModel, newdata = newDataLimited)


