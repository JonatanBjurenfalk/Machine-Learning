#Lab 1
#Assignment 2

#Loading the data from machines.csv. 
data <- read.csv2(file="machines.csv", header=TRUE)

#Function for calculating the loglikelihood. 
calculateProbability = function(x, theta) {
  
  n = dim(x)[1]
  probability = n*log(theta) - theta*sum(x)
  return(probability)
  
}

#Select x number of values between minData and maxData.
theta = seq(min(data), max(data), length.out = 1000)

#loglikelihood. 
logLikelihood = calculateProbability(data, theta)
bestThetaLogLikelihood = theta[which.max(logLikelihood)]

#6 first observations. 
firstSix = as.data.frame(data[1:6, ])
logLikelihoodSix = calculateProbability(firstSix, theta)

plot(theta, logLikelihood, type="l", ylim = c(-150, 0), xlab="Theta", ylab="Log Likelihood", main="Log Likelihood and theta", col="red")
lines(theta, logLikelihoodSix, col="blue")
#-------------------------------------------------------------------
#Prior + theta
bayesian <- logLikelihood + log(10) - 10*theta 

lines(theta, bayesian, col="green")

bestThetaBayesian <- theta[which.max(bayesian)]
#-------------------------------------------------------------------
set.seed(12345)
newObservations <- rexp(50, rate = bestThetaLogLikelihood)

#Histograms. 
par(mfrow=c(1,2))
hist(data$Length, breaks=15)
hist(newObservations, breaks = 15)
par(mfrow=c(1,1))



