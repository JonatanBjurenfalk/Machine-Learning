#Lab 2
#Assignment 1. LDA and logistic regression

#Loading the data from australian-crabs.csv. 
data <- read.csv(file="australian-crabs.csv")

missclass = function(X, X1) {
  
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
  
}

# 1. 
plot(data$RW, data$CL, main = "CL versus RW",
     xlab = "Rear width", ylab = "Carapace length",
     col = data$sex, pch=19) 
legend("topleft", pch = c(19, 19), 
       col = c("black", "red"), 
       legend = c("Female", "Male"))


# 2.
# Linear Discriminant Analysis 
library("MASS")

lda <- lda(sex ~ RW + CL, data = data)
ldaPredict <- predict(lda, newdata = data)
print(lda)

plot(data$RW, data$CL,
     col= ldaPredict$class, pch=19,
     main="Prediction",
     xlab = "Rear width", ylab = "Carapace length")
legend("topleft", pch = c(19, 19), 
       col = c("black", "red"), 
       legend = c("Female", "Male"))

table(Predicted_value = ldaPredict$class,Actual_value = data$sex)

missclass(data$sex, ldaPredict$class)


# 3.
# Lda with a prior. 
ldaPrior <- lda(sex ~ RW + CL, prior = c(0.1, 0.9), data = data)
ldaPriorPredict <- predict(ldaPrior, newdata = data)

plot(data$RW, data$CL,
             col= ldaPriorPredict$class, pch=19,
             main="Prior Prediction",
             xlab = "Rear width", ylab = "Carapace length")
legend("topleft", pch = c(19, 19), 
       col = c("black", "red"), 
       legend = c("Female", "Male"))
table(Predicted_value = ldaPriorPredict$class, Actual_value = data$sex)

missclass(data$sex, ldaPriorPredict$class)


# 4. 

glm <- glm(sex ~ RW + CL, data = data, family = "binomial")
glmPredict <- predict(glm, newdata = data, type = "response")

glmPredict0.5 = ifelse(glmPredict < 0.5, 0, 1)
glmPredict0.5 = factor(glmPredict0.5)

plot(data$RW, data$CL, col = glmPredict0.5,
     pch=19,
     main="GLM Prediction",
     xlab = "Rear width", ylab = "Carapace length")
legend("left", pch = c(19, 19), 
       col = c("black", "red"), 
       legend = c("Female", "Male"))

table(Predicted_value = data$sex, Actual_value = glmPredict0.5)

missclass(data$sex, glmPredict0.5)

intercept = - glm$coefficients[1] / glm$coefficients[3]
slope = - glm$coefficients[2] / glm$coefficients[3]

abline(intercept, slope)



