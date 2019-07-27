#load needed libraries
if(!require(pacman)) install.packages("pacman")
p_load(ggplot2. dplyr, caret, readr, C50, mlbench, bda, zoo, corrplot, reshape2, e1071)

#Loading data
existingDf <- read.csv("data/existingproductattributes2017.csv")

#add dummy data
tempdf <- dummyVars(" ~ .", data = existingDf)
existingData <- data.frame(predict(tempdf, newdata = existingDf))

#check if all data types are int
str(existingData)

#check for NA's
summary(existingData)

#feature engineering, sumReviews
existingData$sumReviews <- rowSums(existingData[,c("x5StarReviews", "x4StarReviews", "x3StarReviews", "x2StarReviews", "x1StarReviews")])

#delete bestsellerrank because it contains NA's
existingData$BestSellersRank <- NULL

#select subset of items, save to new var
#existingData %>% select(-c(ShippingWeight:ProfitMargin, ProductNum)) -> ExistingSub
existingData %>% select(c(PositiveServiceReview, sumReviews, Volume, ProductType.Laptop, ProductType.Smartphone, ProductType.Netbook, ProductType.PC)) -> ExistingSub

##boxplot before removing outliers
boxplot(existingData$Volume)

#tell the system not to plot the outliers
outliers <- boxplot(ExistingSub$Volume, plot=FALSE)$out
ExistingSub <- ExistingSub[-which(ExistingSub$Volume %in% outliers),]

#boxplot after removing outliers
boxplot(ExistingSub$Volume)

#create correlation matrix
corrMatrix <- cor(ExistingSub)

#create heatmap
melted_cormat <- melt(corrMatrix)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#bad visual
corrplot(corrMatrix)

###Creating a linear model (Random Forest)
inTraining <- createDataPartition(ExistingSub$Volume, p = .75, list = FALSE)
training <- ExistingSub[inTraining,]
testing <- ExistingSub[-inTraining,]

rfControl <- trainControl(method = "repeatedcv", number = 20, repeats = 3)

rfGrid <- expand.grid(mtry=c(2,3,4,5,6,7))

#Set seed to know the random order
set.seed(998)

#Creating RF model
rfModel <- train(Volume~., data = training, method = "rf", trControl=rfControl, tuneGrid=rfGrid, tuneLength=2, importance=T)

#view model
rfModel

#varimp
varImp(rfModel)

#predict on testing
pred <- predict(rfModel, newdata = testing)
summary(pred)

postResample(pred, testing$Volume)

###Creating a linear model (Gradient boosing models)
#10 fold cross validation repeated 1 times
gbmControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#Grid to define our own parameters for this classifier
gbmGrid <-  expand.grid(interaction.depth = c(3,5), 
                        n.trees = (1:4)*25, 
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
                 tuneGrid = gbmGrid)

#training results
gbmModel


###knn model
knnControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
set.seed(998)
knnModel <- train(Volume ~., data = training, method = "knn",
 knnControl=trctrl,
 preProcess = c("center", "scale"),
 tuneLength = 5)

knnModel

#SVM model

SVMControl <- trainControl(method = "repeatedcv", number = 40, repeats = 3)
#SVMgrid <- expand.grid(C = c(0.05, 0.1 0.5, 0.75, 1. 1.5))
SVMgrid <- expand.grid(C = c(0.25,0.5,1, 1.5, 1.25, 1.75))

set.seed(3233)

SVMModel <- train(Volume ~., data = training, method = "svmLinear",
                 trControl=SVMControl,
                 preProcess = c("scale"),
                 tuneGrid = SVMgrid)
SVMModel

#Using a different lib
SVMModel <-svm(Volume~., data=training)
Prediction <- predict (SVMModel, testing)


