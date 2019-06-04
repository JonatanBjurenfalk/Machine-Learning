#Lab 1
#Assignment 4

#Loading the data from tecator.csv. 
data <- read.csv2(file="tecator.csv", header=TRUE)

#Checking if a linear model makes sense.
plot(data$Moisture ~ data$Protein) #Yes

#Dividing data into training and test sets. 
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]


#Creating m1-m6 with different degrees. Maybe do a loop?
m1 <- lm(Moisture ~ poly(Protein, degree = 1, raw = TRUE), data = train)
m2 <- lm(Moisture ~ poly(Protein, degree = 2, raw = TRUE), data = train)
m3 <- lm(Moisture ~ poly(Protein, degree = 3, raw = TRUE), data = train)
m4 <- lm(Moisture ~ poly(Protein, degree = 4, raw = TRUE), data = train)
m5 <- lm(Moisture ~ poly(Protein, degree = 5, raw = TRUE), data = train)
m6 <- lm(Moisture ~ poly(Protein, degree = 6, raw = TRUE), data = train)

#Calculating MSE for the training data.
mseTrain1 <- mean(m1$residuals^2)
mseTrain2 <- mean(m2$residuals^2)
mseTrain3 <- mean(m3$residuals^2)
mseTrain4 <- mean(m4$residuals^2)
mseTrain5 <- mean(m5$residuals^2)
mseTrain6 <- mean(m6$residuals^2)

mseTrain <- c(mseTrain1, mseTrain2, mseTrain3, mseTrain4, mseTrain5, mseTrain6)

#Predicting with the test data and calculating the MSE. 
p1 <- predict(m1, newdata = test, type = "response")
mseTest1 <- mean((p1 - test$Moisture)^2)
p2 <- predict(m2, newdata = test, type = "response")
mseTest2 <- mean((p2 - test$Moisture)^2)
p3 <- predict(m3, newdata = test, type = "response")
mseTest3 <- mean((p3 - test$Moisture)^2)
p4 <- predict(m4, newdata = test, type = "response")
mseTest4 <- mean((p4 - test$Moisture)^2)
p5 <- predict(m5, newdata = test, type = "response")
mseTest5 <- mean((p5 - test$Moisture)^2)
p6 <- predict(m6, newdata = test, type = "response")
mseTest6 <- mean((p6 - test$Moisture)^2)

mseTest <- c(mseTest1, mseTest2, mseTest3, mseTest4, mseTest5, mseTest6)

#Plot booth MSE-arrays. 
plot(mseTrain, type="b", col="blue", pch="o", lty=1, xlim=c(0, 7), ylim=c(30, 35))
points(mseTest, type ="b", col="red", pch="*")
#-------------------------------------------------------------------
library("MASS")

channelFat <- data[, 2:102]
model <- lm(Fat ~ ., data = channelFat)

#stepAIC with how many coefficients are chosen. 
step <- stepAIC(model, direction="both")

step$rank
#-------------------------------------------------------------------
library("glmnet")

#Ridge
covariates=scale(data[, 2:101])
response=scale(data[, 102])

ridge <- glmnet(as.matrix(covariates), response, alpha=0, family="gaussian")
#-------------------------------------------------------------------
#Lasso
lasso <- glmnet(as.matrix(covariates), response, alpha=1, family="gaussian")

#Plot lasso and ridge together. 
par(mfrow=c(1,2))
plot(lasso, xvar="lambda", label=TRUE, xlim=c(-7, 0), ylim=c(-3, 5))
plot(ridge, xvar="lambda", label=TRUE, xlim=c(-3, 7), ylim=c(-0.18, 0.35))
#-------------------------------------------------------------------
#Cross-validation 
cvLasso <- cv.glmnet(as.matrix(covariates), response, alpha=1, family="gaussian")

cvLasso$lambda.min
plot(cvLasso)
coef(cvLasso, s="lambda.min")






