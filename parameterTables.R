# External script to create tables of parameters for models

## @knitr tableNetbook
Parameters <- c('Folds','repeats','tuneLength', 'tuneGrid','Best mtry/k/C','RMSE','R Squared','RMSE','R Squared')
RandomForest <- c(10,3,10,'None','mtry = 4',124.2,0.969,7.2, 'NA')
kNN <- c(10,3,'None',"[1:5]",'k = 3',185.3,0.915,23.2, 'NA')
SupportVectorMachines <- c(10,3,'None','(1,5,10,18,50)','C = 50',174.4,0.96,14.9, 'NA')

metrics <- data.frame(Parameters, RandomForest, kNN, SupportVectorMachines)
kable(metrics) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),fixed_thead = T) %>%
  pack_rows("Training Metrics", 6, 7) %>%
  pack_rows("Testing Metrics", 8, 9)

## @knitr tablePC
Parameters <- c('Folds','repeats','tuneLength', 'tuneGrid','Best mtry/k/C','RMSE','R Squared','RMSE','R Squared')
RandomForest <- c(10,3,10,'None','mtry = 4',128.5,0.964,3.6, 1)
kNN <- c(10,3,'None',"[1:5]",'k = 3',184.4,0.908,26.2, 1)
SupportVectorMachines <- c(10,3,'None','(1,5,10,18,50)','C = 50',161.3,0.966,31.8, 1)

metrics <- data.frame(Parameters, RandomForest, kNN, SupportVectorMachines)
kable(metrics) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),fixed_thead = T) %>%
  pack_rows("Training Metrics", 6, 7) %>%
  pack_rows("Testing Metrics", 8, 9)

## @knitr tableLaptop
Parameters <- c('Folds','repeats','tuneLength', 'tuneGrid','Best mtry/k/C','RMSE','R Squared','RMSE','R Squared')
RandomForest <- c(10,3,10,'None','mtry = 3',144.5,0.949,3.5, 'NA')
kNN <- c(10,3,'None',"[1:5]",'k = 4',196.8,0.912,62, 'NA')
SupportVectorMachines <- c(10,3,'None','(1,5,10,18,50)','C = 1',457.7,0.87,17.9, 'NA')

metrics <- data.frame(Parameters, RandomForest, kNN, SupportVectorMachines)
kable(metrics) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),fixed_thead = T) %>%
  pack_rows("Training Metrics", 6, 7) %>%
  pack_rows("Testing Metrics", 8, 9)

## @knitr tableSmartphone
Parameters <- c('Folds','repeats','tuneLength', 'tuneGrid','Best mtry/k/C','RMSE','R Squared','RMSE','R Squared')
RandomForest <- c(10,3,10,'None','mtry = 4', 142.6, 0.95, 44.0, 1)
kNN <- c(10,3,'None',"[1:10]",'k = 6',224.4,0.87, 217.7, 1)
SupportVectorMachines <- c(10,3,'None','(1,5,10,18,50)','C = 1',463.6, 0.837, 66.7, 1)

metrics <- data.frame(Parameters, RandomForest, kNN, SupportVectorMachines)
kable(metrics) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),fixed_thead = T) %>%
  pack_rows("Training Metrics", 6, 7) %>%
  pack_rows("Testing Metrics", 8, 9)