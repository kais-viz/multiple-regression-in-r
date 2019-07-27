# External script to load vars needed for Netbook study case

## @knitr rfModels
#Set seed to remember the randomization
rfControl <- trainControl(method = "repeatedcv", 
                          number = 10, 
                          repeats = 3, 
                          search="random")

set.seed(33)
#Creating RF model
rfModelNetbook <- train(Volume~., data = trainingNetbook, 
                 method = "rf",
                 trControl=rfControl, 
                 tuneLength=10, 
                 importance=T)

set.seed(616)
#Creating RF model
rfModelPC <- train(Volume~., data = trainingPC, 
                        method = "rf",
                        trControl=rfControl, 
                        tuneLength=10, 
                        importance=T)

set.seed(44)
#Creating RF model
rfModelLaptop <- train(Volume~., data = trainingLaptop, 
                        method = "rf",
                        trControl=rfControl, 
                        tuneLength=10, 
                        importance=T)

set.seed(150)
#Creating RF model
rfModelSmartphone <- train(Volume~., data = trainingSmartphone, 
                        method = "rf",
                        trControl=rfControl, 
                        tuneLength=10, 
                        importance=T)

#get predictions
predNetbook <- predict(rfModelNetbook, newdata = newSubNetbook)
predPC <- predict(rfModelPC, newdata = newSubPC)
predLaptop <- predict(rfModelLaptop, newdata = newSubLaptop)
predSmartphone <- predict(rfModelSmartphone, newdata = newSubSmartphone)

#combine predictions
predVolumes <-  rbind(predNetbook, predPC, predLaptop, predSmartphone)

## @knitr rfNetbook
#Set seed to remember the randomization
set.seed(99)

#Creating subset of the dataset
existingData %>% select(c(Volume,
                          PositiveServiceReview, 
                          NegativeServiceReview, 
                          sumReviews, 
                          ProductType.Netbook)) -> ExistingSub

#Normalise/scale attributes except the dependent feature
ExistingSub[,c(2:5)] <- lapply(ExistingSub[,c(2:5)] , scale)

#Splitting data into training set and testing set for cross validation
inTraining <- createDataPartition(ExistingSub$Volume, p = .75, list = FALSE)
trainingNetbook <- ExistingSub[inTraining,]
testingNetbook <- ExistingSub[-inTraining,]

#create subset
newData %>% select(c(PositiveServiceReview, NegativeServiceReview, sumReviews, ProductType.Netbook,Volume)) -> newSub

#Normalise/scale attributes except the dependent feature
newSub[,c(1:4)] <- lapply(newSub[,c(1:4)] , scale)

#filtering test set to include only Netbook products
newSubNetbook <- newSub %>% 
  dplyr::filter(newSub$ProductType.Netbook > 1)

## @knitr rfPC
#Set seed to remember the randomization
set.seed(666)

#Creating subset of the dataset
existingData %>% select(c(Volume,
                          PositiveServiceReview, 
                          NegativeServiceReview, 
                          sumReviews, 
                          ProductType.PC)) -> ExistingSub

#Normalise/scale attributes except the dependent feature
ExistingSub[,c(2:5)] <- lapply(ExistingSub[,c(2:5)] , scale)

#Splitting data into training set and testing set for cross validation
inTraining <- createDataPartition(ExistingSub$Volume, p = .75, list = FALSE)
trainingPC <- ExistingSub[inTraining,]
testingPC <- ExistingSub[-inTraining,]

#create subset
newData %>% select(c(PositiveServiceReview, NegativeServiceReview, sumReviews, ProductType.PC,Volume)) -> newSub

#Normalise/scale attributes except the dependent feature
newSub[,c(1:4)] <- lapply(newSub[,c(1:4)] , scale)

#filtering test set to include only PC products
newSubPC <- newSub %>% 
  dplyr::filter(newSub$ProductType.PC > 1)

## @knitr rfLaptop
#Set seed to remember the randomization
set.seed(77)

#Creating subset of the dataset
existingData %>% select(c(Volume,
                          PositiveServiceReview, 
                          NegativeServiceReview, 
                          sumReviews, 
                          ProductType.Laptop)) -> ExistingSub

#Normalise/scale attributes except the dependent feature
ExistingSub[,c(2:5)] <- lapply(ExistingSub[,c(2:5)] , scale)

#Splitting data into training set and testing set for cross validation
inTraining <- createDataPartition(ExistingSub$Volume, p = .75, list = FALSE)
trainingLaptop <- ExistingSub[inTraining,]
testingLaptop <- ExistingSub[-inTraining,]

#create subset
newData %>% select(c(PositiveServiceReview, NegativeServiceReview, sumReviews, ProductType.Laptop,Volume)) -> newSub

#Normalise/scale attributes except the dependent feature
newSub[,c(1:4)] <- lapply(newSub[,c(1:4)] , scale)

#filtering test set to include only Laptop products
newSubLaptop <- newSub %>% 
  dplyr::filter(newSub$ProductType.Laptop > 1)


## @knitr rfSmartphone
#Set seed to remember the randomization
set.seed(66)

#Creating subset of the dataset
existingData %>% select(c(Volume,
                          PositiveServiceReview, 
                          NegativeServiceReview, 
                          sumReviews, 
                          ProductType.Smartphone)) -> ExistingSub

#Normalise/scale attributes except the dependent feature
ExistingSub[,c(2:5)] <- lapply(ExistingSub[,c(2:5)] , scale)

#Splitting data into training set and testing set for cross validation
inTraining <- createDataPartition(ExistingSub$Volume, p = .75, list = FALSE)
trainingSmartphone <- ExistingSub[inTraining,]
testingSmartphone <- ExistingSub[-inTraining,]

#create subset
newData %>% select(c(PositiveServiceReview, NegativeServiceReview, sumReviews, ProductType.Smartphone,Volume)) -> newSub

#Normalise/scale attributes except the dependent feature
newSub[,c(1:4)] <- lapply(newSub[,c(1:4)] , scale)

#filtering test set to include only Smartphone products
newSubSmartphone <- newSub %>% 
  dplyr::filter(newSub$ProductType.Smartphone > 1)


## @knitr modelNetbook
#Set seed to remember the randomization
set.seed(99)

#Creating subset of the dataset
existingData %>% select(c(Volume,
                          PositiveServiceReview, 
                          NegativeServiceReview, 
                          sumReviews, 
                          ProductType.Netbook)) -> ExistingSub

#Normalise/scale attributes except the dependent feature
ExistingSub[,c(2:5)] <- lapply(ExistingSub[,c(2:5)] , scale)

#Splitting data into training set and testing set for cross validation
inTraining <- createDataPartition(ExistingSub$Volume, p = .75, list = FALSE)
training <- ExistingSub[inTraining,]
testing <- ExistingSub[-inTraining,]

#filtering test set to include only netbook products
testingLimited <- testing %>% 
  dplyr::filter(testing$ProductType.Netbook > 1)