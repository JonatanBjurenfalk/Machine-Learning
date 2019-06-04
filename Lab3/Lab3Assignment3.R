# Assignment 3. Neural networks.
library("neuralnet")
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)

optimalSum <- 1
predSum <- vector(length=10)
  
for(i in 1:10) {
    
  nn <- neuralnet(Sin ~ Var, tr, hidden = c(10), threshold = i/1000, startweights = winit)
  
  # Predicting validating set with nn and calculating the error value
  pred <- compute(nn, va[1])
  predSum[i] <- sum(abs((pred$net.result-va[2])))
  
  # If new error is better then we change threshold
  if (optimalSum > predSum[i]) {
    optimalSum <- predSum[i]
    optimalThreshold <- i
  }
      
}

plot(predSum)
optimalThreshold

# Plot the network with the optimal threshold
plot(nn <- neuralnet(Sin ~ Var, trva, hidden = c(10), threshold = optimalThreshold/1000, 
                     startweights = winit), rep="best")
  
# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1)
points(trva, col = "red")
