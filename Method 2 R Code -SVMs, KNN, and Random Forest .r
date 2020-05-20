# Load the required packages.
require("caret")
require("e1071")
require("dplyr")
require("rpart")
require("stringr")
require("randomForest")
require("ggplot2")
require("dplyr")

#import datasets
setwd("C:\\Users\\parinppatel\\⁨Documents⁩\\Portfolio⁩\\IST-707 (Data Analytics)⁩\⁩\HW7- SVMs, kNN, and Random Forest Algorithms ( Handwriting Recognition )")


#import train
filename='Kaggle-digit-train-sample-small-1400.csv'
digit_train<-read.csv(filename,header = TRUE, stringsAsFactors = TRUE)
#import test
filename1='Kaggle-digit-test-sample1000.csv'
digit_test<-read.csv(filename1,header = TRUE, stringsAsFactors = TRUE)


#explore- 
dim(digit_train)
summary(digit_train[, 1:10])
str(digit_train[, 1:10])


3Training and testing the models.

#convert label data type to factor
digit_train$label <- factor(paste0('X', digit_train$label), levels = c('X0', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X8'))
digit_test$label <- factor(paste0('X', digit_test$label), levels = c('X0', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X8'))


#kNN : 3-fold CV
set.seed(238)
t <- trainControl(method = 'repeatedcv', repeats = 3, classProbs = TRUE)
Train.kNN <- train(label ~ ., data = digit_train, method = 'knn', preProcess = c('center', 'scale'), trControl = x, metric = 'ROC', tuneLength = 3)

#Analyze Model 
Train.kNN
plot(Train.kNN)


#Testing- kNN 
Pred.kNN <- predict(Train.kNN, digit_test, type = 'prob')
Pred.kNN <- as.data.frame(Pred.kNN)
Pred.kNN.df<- data.frame(apply(Pred.kNN, 1, which.max) - 1) 
colnames(Pred.kNN.df) <- 'prediction'

install.packages("tidyverse")
library("tidyverse")
require("stringr")

knnResults <- digit_test %>% select(label) %>% mutate(real = str_remove(label, 'X')) %>% bind_cols(Pred.kNN.df) %>% mutate(real = as.factor(real), prediction = as.factor(prediction))

confusionMatrix(knnResults$real, knnResults$prediction)

str(digit_test)
head(digit)

#svm  - training
svmTrain <- svm(label ~ ., data = digit_train, type = 'C', kernel = 'linear', cross = 3, probability = TRUE)
summary(svmTrain)

svmPred <- predict(svmTrain, digit_test, type = 'C')
svmPred <- as.data.frame(svmPred)
colnames(svmPred) <- 'results'

# Build the data frame to compare the results. 
svmResults <- testSet %>% select(label) %>% bind_cols(svmPred) %>% mutate(real = factor(as.character(str_remove(label, 'X'))), prediction = factor(as.character(str_remove(results, 'X'))))




#Random Forest
x <- trainControl(method = 'repeatedcv', number = 3, repeats = 3)
rfTrain <- train(label ~ ., data = digit_train, method = 'rf', metric = 'Accuracy', trControl = x, type = 'C')

rfTrain

#test rf 
rfPred <- predict(rfTrain, digit_test, type = 'prob')
rfPred <- as.data.frame(rfPred)

rfPredictedValues <- data.frame(apply(rfPred, 1, which.max) - 1) 
colnames(rfPredictedValues) <- 'results'

#Rf results and confusion matrix
rfResults <- testSet %>% select(label) %>% bind_cols(rfPredictedValues) %>% mutate(real = factor(as.character(str_remove(label, 'X'))), prediction = factor(results))
confusionMatrix(rfResults$real, rfResults$prediction)

