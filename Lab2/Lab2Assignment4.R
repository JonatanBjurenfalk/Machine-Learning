#Lab 2
#Assignment 4. Principal components

#Loading the data from NIRspectra.csv. 
data <- read.csv2(file="NIRSpectra.csv")

# 1. 
res = prcomp(data[,1:126])
lambda=res$sdev^2
#eigenvalues
# lambda
#proportion of variation
sprintf("%2.3f",lambda/sum(lambda)*100)
screeplot(res)

U=res$rotation
# head(U)

plot(res$x[,1], res$x[,2])

# 2. 
#U=loadings(res) rotation = loadings?
par(mfrow=c(1,2))
plot(U[,1], main="Traceplot, PC1")
plot(U[,2],main="Traceplot, PC2")

# 3. 
library(fastICA)

# a. 
set.seed(12345)
fastICAres = fastICA(data, n.comp = 2)
W = fastICAres$K%*%fastICAres$W

plot(W[,1], main= "Traceplot")
plot(W[,2], main= "Traceplot")

# b.

library(fastICA)
#fICA = fastICA(as.matrix(featSpace),)
#X = as.matrix()

# a. Compute W' = K * W
set.seed(12345)
fICAResult = fastICA(as.matrix(data), n.comp = 2)
W_prime = fICAResult$K%*%fICAResult$W

plot(W_prime[,1], xlab = "Variable", ylab = "W'")
plot(W_prime[,2], xlab = "Variable", ylab = "W'")



