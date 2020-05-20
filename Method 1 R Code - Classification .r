install.packages("e1071")
install.packages("naivebayes")
install.packages("rpart")
install.packages("rattle")

library("e1071")
library("naivebayes")
library("rpart")
library("rattle")

install.packages("e1071")
install.packages("naivebayes")
install.packages("rpart")
install.packages("rattle")

library("e1071")
library("naivebayes")
library("rpart")
library("rattle")



##import datasets
setwd("C:\\Users\\parinppatel\\⁨Documents⁩\\Portfolio⁩\\IST-707 (Data Analytics)⁩\\HW6 - Naiive Bayes + Decision Trees (Handwriting Recognition)")

#import train
filename='Kaggle-digit-train-sample-small-1400.csv'
digit_train<-read.csv(filename,header = TRUE, stringsAsFactors = TRUE)
#import test
filename1='Kaggle-digit-test-sample1000.csv'
digit_test<-read.csv(filename1,header = TRUE, stringsAsFactors = TRUE)


str(digit_train)
str(digit_train)

#clean:
#change data type from int to factor
digit_train$label <- as.factor(digit_train$label)
str(digit_train$label)

##Decision Tree Prep
digit_cleaned <- digit_train
digit_cleaned2 <- digit_train
digit_cleaned <- replace(digit_cleaned[,-1],digit_cleaned[,-1] < 100, 0)
digit_cleaned$label <- digit_train$label
digit_cleaned2 <- replace(digit_cleaned2[,-1],digit_cleaned2[,-1] < 150, 0)
digit_cleaned2$label <- digit_train$label

fit <- rpart(digit_cleaned$label ~ ., data = digit_cleaned, method="class")
fit2 <- rpart(digit_train$label ~ ., data = digit_train, method="class")
fit3 <- rpart(digit_cleaned2$label ~ ., data = digit_cleaned2, method="class")
fancyRpartPlot(fit)
fancyRpartPlot(fit2)
fancyRpartPlot(fit3)

predict_tree= predict(fit,digit_test, type="class")


###############################################################
#############  Create k-folds for k-fold validation ###########
###############################################################


# Number of observations
N <- nrow(digit_train)
# Number of desired splits
kfolds <- 5
# Generate indices of holdout observations
# Note if N is not a multiple of folds you will get a warning, but is OK.
holdout <- split(sample(1:N), 1:kfolds)


#####  Run training and Testing for each of the k-folds
AllResults<-list()
AllLabels<-list()
for (k in 1:kfolds){
  
  Kdigit_Test=digit_train[holdout[[k]], ]
  Kdigit_Train=digit_train[-holdout[[k]], ]
  ## View the created Test and Train sets
  (head(Kdigit_Train))
  (table(Kdigit_Test$label))
  
  ## Make sure you take the labels out of the testing data
  (head(Kdigit_Test))
  Kdigit_Test_noLabel<-Kdigit_Test[-c(1)]
  Kdigit_Test_justLabel<-Kdigit_Test$label
  (head(Kdigit_Test_noLabel))
  
  
  #### e1071
  ## formula is label ~ x1 + x2 + .  NOTE that label ~. is "use all to create model"
  NB<-naiveBayes(label~., data=Kdigit_Train, laplace=0, na.action = na.pass)
  NB_Pred <- predict(NB, Kdigit_Test_noLabel)
  NB
  
  ## Accumulate results from each fold
  AllResults<- c(AllResults,NB_Pred)
  AllLabels<- c(AllLabels, Kdigit_Test_justLabel)
  
}
### end crossvalidation -- present results for all folds   
table(unlist(AllResults),unlist(AllLabels))
#####  Run training and Testing for each of the k-folds
AllResults2<-list()
AllLabels2<-list()
for (k in 1:kfolds){
  
  Kdigit_Test=digit_train[holdout[[k]], ]
  Kdigit_Train=digit_train[-holdout[[k]], ]
  ## View the created Test and Train sets
  
  
  ## Make sure you take the labels out of the testing data
  (head(Kdigit_Test))
  Kdigit_Test_noLabel<-Kdigit_Test[-c(1)]
  Kdigit_Test_justLabel<-Kdigit_Test$label
  
  
  
  #### e1071
  ## formula is label ~ x1 + x2 + .  NOTE that label ~. is "use all to create model"
  NB2<-naive_bayes(label~., data=Kdigit_Train, laplace=0, na.action = na.pass)
  NB_Pred2 <- predict(NB, Kdigit_Test_noLabel)
  
  
  ## Accumulate results from each fold
  AllResults2<- c(AllResults2,NB_Pred2)
  AllLabels2<- c(AllLabels2, Kdigit_Test_justLabel)
  
}

table(unlist(AllResults2),unlist(AllLabels2))

printcp(fit)
printcp(fit2)
printcp(fit3)






##import datasets
setwd("C:\\Users\\parinppatel\\⁨Documents⁩\\Portfolio⁩\\IST-707 (Data Analytics)⁩\\HW6 - Naiive Bayes + Decision Trees (Handwriting Recognition)")

#import train
filename='Kaggle-digit-train-sample-small-1400.csv'
digit_train<-read.csv(filename,header = TRUE, stringsAsFactors = TRUE)
#import test
filename1='Kaggle-digit-test-sample1000.csv'
digit_test<-read.csv(filename1,header = TRUE, stringsAsFactors = TRUE)


str(digit_train)
str(digit_train)

#clean:
#change data type from int to factor
digit_train$label <- as.factor(digit_train$label)
str(digit_train$label)

##Decision Tree Prep
digit_cleaned <- digit_train
digit_cleaned2 <- digit_train
digit_cleaned <- replace(digit_cleaned[,-1],digit_cleaned[,-1] < 100, 0)
digit_cleaned$label <- digit_train$label
digit_cleaned2 <- replace(digit_cleaned2[,-1],digit_cleaned2[,-1] < 150, 0)
digit_cleaned2$label <- digit_train$label

fit <- rpart(digit_cleaned$label ~ ., data = digit_cleaned, method="class")
fit2 <- rpart(digit_train$label ~ ., data = digit_train, method="class")
fit3 <- rpart(digit_cleaned2$label ~ ., data = digit_cleaned2, method="class")
fancyRpartPlot(fit)
fancyRpartPlot(fit2)
fancyRpartPlot(fit3)

predict_tree= predict(fit,digit_test, type="class")


###############################################################
#############  Create k-folds for k-fold validation ###########
###############################################################


# Number of observations
N <- nrow(digit_train)
# Number of desired splits
kfolds <- 5
# Generate indices of holdout observations
# Note if N is not a multiple of folds you will get a warning, but is OK.
holdout <- split(sample(1:N), 1:kfolds)


#####  Run training and Testing for each of the k-folds
AllResults<-list()
AllLabels<-list()
for (k in 1:kfolds){
  
  Kdigit_Test=digit_train[holdout[[k]], ]
  Kdigit_Train=digit_train[-holdout[[k]], ]
  ## View the created Test and Train sets
  (head(Kdigit_Train))
  (table(Kdigit_Test$label))
  
  ## Make sure you take the labels out of the testing data
  (head(Kdigit_Test))
  Kdigit_Test_noLabel<-Kdigit_Test[-c(1)]
  Kdigit_Test_justLabel<-Kdigit_Test$label
  (head(Kdigit_Test_noLabel))
  
  
  #### e1071
  ## formula is label ~ x1 + x2 + .  NOTE that label ~. is "use all to create model"
  NB<-naiveBayes(label~., data=Kdigit_Train, laplace=0, na.action = na.pass)
  NB_Pred <- predict(NB, Kdigit_Test_noLabel)
  NB
  
  ## Accumulate results from each fold
  AllResults<- c(AllResults,NB_Pred)
  AllLabels<- c(AllLabels, Kdigit_Test_justLabel)
  
}
### end crossvalidation -- present results for all folds   
table(unlist(AllResults),unlist(AllLabels))
#####  Run training and Testing for each of the k-folds
AllResults2<-list()
AllLabels2<-list()
for (k in 1:kfolds){
  
  Kdigit_Test=digit_train[holdout[[k]], ]
  Kdigit_Train=digit_train[-holdout[[k]], ]
  ## View the created Test and Train sets
  
  
  ## Make sure you take the labels out of the testing data
  (head(Kdigit_Test))
  Kdigit_Test_noLabel<-Kdigit_Test[-c(1)]
  Kdigit_Test_justLabel<-Kdigit_Test$label
  
  
  
  #### e1071
  ## formula is label ~ x1 + x2 + .  NOTE that label ~. is "use all to create model"
  NB2<-naive_bayes(label~., data=Kdigit_Train, laplace=0, na.action = na.pass)
  NB_Pred2 <- predict(NB, Kdigit_Test_noLabel)
  
  
  ## Accumulate results from each fold
  AllResults2<- c(AllResults2,NB_Pred2)
  AllLabels2<- c(AllLabels2, Kdigit_Test_justLabel)
  
}

table(unlist(AllResults2),unlist(AllLabels2))

printcp(fit)
printcp(fit2)
printcp(fit3)






