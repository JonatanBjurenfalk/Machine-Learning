#Lab 1 
#Assignment 1

#Loading the data from spambase.csv. 
data <- read.csv2(file="spambase.csv", header=TRUE)

data$Spam <- factor(data$Spam)

#Compute misclassification rate. 
missclass = function(X, X1) {
  
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
  
}

#Dividing data into training and test sets. 
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#Model for logistic regression. 
logisticModel <- glm(Spam ~ ., data=train, family="binomial")

#Computing the probability. 
trainProbability <- predict(logisticModel, newdata = train, type = "response")
testProbability <- predict(logisticModel, newdata = test, type = "response")

#Changing values in array depending on probability.
trainProbability0.5 <- ifelse(trainProbability > 0.5, 1, 0)
testProbability0.5 <- ifelse(testProbability > 0.5, 1, 0)

#Confusion matrix
confusionMatrixTrain0.5 <- table(Actual_value = train$Spam, Predicted_value = trainProbability0.5)
confusionMatrixTest0.5 <- table(Actual_value = test$Spam, Predicted_value = testProbability0.5)

#Computing misclassification rates.
missratetrain0.5 <- missclass(train$Spam, trainProbability0.5)
missratetest0.5 <- missclass(test$Spam, testProbability0.5)
#------------------------------------------------------------------------------
#Changing values in array depending on probability.
trainProbability0.9 <- ifelse(trainProbability > 0.9, 1, 0)
testProbability0.9 <- ifelse(testProbability > 0.9, 1, 0)

#Confusion matrix
confusionMatrixTrain0.9 <- table(Actual_value = train$Spam, Predicted_value = trainProbability0.9)
confusionMatrixTest0.9 <- table(Actual_value = test$Spam, Predicted_value = testProbability0.9) 

#Computing misclassification rates.
missratetrain0.9 <- missclass(train$Spam, trainProbability0.9)
missratetest0.9 <- missclass(test$Spam, testProbability0.9)
#-----------------------------------------------------------------------------
library("kknn")

#Model for kknn.
kknnSpamTrain30 <- kknn(Spam ~ ., train = train, test = train, k = 30)
kknnSpamTest30 <- kknn(Spam ~ ., train = train, test = test, k = 30)

kknnSpamTrain1 <- kknn(Spam ~ ., train = train, test = train, k = 1)
kknnSpamTest1 <- kknn(Spam ~ ., train = train, test = test, k = 1)

#Computing misclassification rates.
missclassTrain30 <- missclass(train$Spam, kknnSpamTrain30$fitted.values)
missclassTest30 <- missclass(test$Spam, kknnSpamTest30$fitted.values)

missclassTrain1 <- missclass(train$Spam, kknnSpamTrain1$fitted.values)
missclassTest1 <- missclass(test$Spam, kknnSpamTest1$fitted.values)




