#Lab 2
#Assignment 2. Analysis of credit scoring

#Loading the data from creditscoring.csv. 
data <- read.csv(file="creditscoring.csv", sep=";")

missclass = function(X, X1) {
  
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
  
}

# 1. 
#Dividing data into training, test and validation sets.
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

# 2. 

library("tree")
n=dim(train)[1]
# Deviance
fitDeviance=tree(good_bad ~ ., data=train, split = c("deviance"))
plot(fitDeviance)
text(fitDeviance, pretty=0)
fitDeviance
summary(fitDeviance)

fitTrainDeviance = predict(fitDeviance, newdata=train, type="class")
table(actual = train$good_bad, predicted = fitTrainDeviance)
missclass(train$good_bad, fitTrainDeviance)

fitTestDeviance = predict(fitDeviance, newdata=test, type="class")
table(actual = test$good_bad, predicted = fitTestDeviance)
missclass(test$good_bad, fitTestDeviance)

# Gini index
fitGini=tree(good_bad ~ ., data=train, split = c("gini"))
plot(fitGini)
text(fitGini, pretty=0)
fitGini
summary(fitGini)

fitTrainGini = predict(fitGini, newdata=train, type="class")
table(actual = train$good_bad, predicted = fitTrainGini)
missclass(train$good_bad, fitTrainGini)

fitTestGini = predict(fitGini, newdata=test, type="class")
table(actual = test$good_bad, predicted = fitTestGini)
missclass(test$good_bad, fitTestGini)


# 3. 

fit=tree(good_bad~., data=train, split = c("deviance"))
cv.res=cv.tree(fit)
plot(fit)
text(fit, pretty=0)
par(mfrow=c(1,2))
#plot(cv.res$size, cv.res$dev, type="b", col="red")
#plot(log(cv.res$k), cv.res$dev, type="b", col="red")

trainScore = rep(0,9)
testScore = rep(0,9)
for(i in 2:9) {
  prunedTree = prune.tree(fit, best=i)
  pred = predict(prunedTree, newdata=valid, type="tree")
  trainScore[i] = deviance(prunedTree)
  testScore[i] = deviance(pred)
}
plot(2:9, trainScore[2:9], type="b", col="red", ylim=c(280,570))
points(2:9, testScore[2:9], type="b", col="blue")


finalTree = prune.tree(fit, best=4)
YfitValid = predict(finalTree, newdata=valid, type="class")
table(valid$good_bad,YfitValid)
plot(finalTree)
text(finalTree, pretty=0)

YfitTest = predict(finalTree, newdata = test, type="class")
missclass(test$good_bad, YfitTest)

# 4. 
library("MASS")
library("e1071")

fitNaive = naiveBayes(good_bad ~ ., data=train)
fitNaive
YfitNaiveTrain = predict(fitNaive, newdata = train)
YfitNaiveTest = predict(fitNaive, newdata = test)

table(YfitNaiveTrain, train$good_bad)
missclass(train$good_bad, YfitNaiveTrain)

table(YfitNaiveTest, test$good_bad)
missclass(test$good_bad, YfitNaiveTest)

# 5. 
sekvens <- seq(0, 1, 0.05)

predNaiveBayes = predict(fitNaive, newdata=test, type="raw")
predFinalTree = predict(finalTree, newdata=test)

NaiveScore = matrix(NA, nrow = nrow(predNaiveBayes), ncol = length(sekvens))
finalTreeScore = matrix(NA, nrow = nrow(predFinalTree), ncol = length(sekvens))

for(i in 1:length(sekvens)) {
  
  NaiveScore[,i] <- ifelse(predNaiveBayes[,2] > sekvens[i], 1, 0)
  finalTreeScore[,i] <- ifelse(predFinalTree[,2] > sekvens[i], 1, 0)
  
}

# TPR
TPR = function(actual, labels) {
  nDataPoints = dim(labels)[1]
  nPointsInGraph = dim(labels)[2]
  TP = 0
  TPR = as.numeric()
  for (i in 1:nPointsInGraph) { # for each column
    for(j in 1:nDataPoints) { #loop through rows
      if(actual[j] == 1 && labels[j,i] == 1) {
        TP = TP + 1
      }
        
    }
    print(TP)
    print(sum(actual == 1))
    TPR[i] = TP / sum(actual == 1)
    TP = 0
  }
  return(TPR)
}
TPRBayes = TPR(as.numeric(test$good_bad) - 1, NaiveScore)
TPRFinalTree = TPR(as.numeric(test$good_bad) - 1, finalTreeScore)

# FPR
FPR = function(actual, labels) {
  nDataPoints = dim(labels)[1]
  nPointsInGraph = dim(labels)[2]
  FP = 0
  FPR = as.numeric()
  for (i in 1:nPointsInGraph) { # for each column
    for(j in 1:nDataPoints) { #loop through rows
           if(actual[j] == 0 && labels[j,i] == 1) {
             FP = FP + 1
           }
    }
    print(FP)
    print(sum(actual == 0))
    FPR[i] = FP / sum(actual == 0)
    FP = 0
  }
  return(FPR)
}
FPRBayes = FPR(as.numeric(test$good_bad) - 1, NaiveScore)
FPRFinalTree = FPR(as.numeric(test$good_bad) - 1, finalTreeScore)

# ROC-curve
plot(x = FPRBayes, y = TPRBayes, type = 'l', col="blue", xlab = 'FPR', ylab = 'TPR', main = 'ROC-curve', xlim = c(0,1), ylim = c(0,1))
lines(x = FPRFinalTree, y = TPRFinalTree, type = 'l', col="red")

# 6.

model.bayes <- naiveBayes(good_bad~., data=train)

trainLoss <- predict(model.bayes, newdata=train, type="raw")
trainLoss <- ifelse(trainLoss[,1]*10 > trainLoss[,2], "bad", "good")
table(Predicted = trainLoss, Actual = train$good_bad)
print("Train misclassification rate: ")
missclass(trainLoss, train$good_bad)

testLoss <- predict(model.bayes, newdata=test, type="raw")
testLoss <- ifelse(testLoss[,1]*10 > testLoss[,2], "bad", "good")
table(Predicted = testLoss, Actual = test$good_bad)
print("Test misclassification rate: ")
missclass(testLoss, test$good_bad)


