setwd("C:\\Users\\Dell\\Desktop\\R - Kod\\Machine Learning\\Lab1")
dataset <-scale(read.csv("parkinsons.csv", header=T))
set.seed(123)
ind <- sample(nrow(dataset), floor(nrow(dataset)*0.6))
train <- dataset[ind,5:ncol(dataset)]
test <- dataset[-ind,5:ncol(dataset)]


loglikelihood <- function(params, data){
  sigma <- params[1]
  w <- params[2:ncol(data)]
  y <- data[,1]
  X <- data[, 2:ncol(data)]
  n <- nrow(X)
  llh <- -n/2 * log(2*pi*sigma^2) - 1/(2*sigma^2) * sum((y - X %*% w)^2) 
  return(-llh)
}

ridge <- function(params, data, lambda){
 w <- params[2:length(params)]
 loglik <- loglikelihood(params, data) + lambda*sum(w^2)
 return(loglik)
}

ridgeOpt <- function(lambda, data){
  opt <- optim(rep(1, ncol(data)), fn=ridge, data=data, lambda=lambda, method="BFGS")
  return(opt$par)
}
ridgeOpt(1, train)


df <- function(lambda, X){
  H <- X %*% solve(t(X) %*% X+ lambda * diag(ncol(X))) %*% t(X)
  return(sum(diag(H)))
}

# Remember that the first paramter is Sigma
optParams <- sapply(c(1, 100, 1000), ridgeOpt,data=train)[2:ncol(train),]

# Predicting the train & testing data for each value of lambda
y_hat_train <- sapply(1:ncol(optParams), function(x) train[, 2:ncol(train)] %*% optParams[,x])
y_hat_test <- sapply(1:ncol(optParams), function(x) test[, 2:ncol(test)] %*% optParams[,x])

# Training & testing MSE for each value of lambda
MSE_train <- sapply(1:ncol(y_hat_train), function(x) 1/nrow(y_hat_train) * sum((train[,1] - y_hat_train[,x])^2))
MSE_test <- sapply(1:ncol(y_hat_test), function(x) 1/nrow(y_hat_test) * sum((test[,1] - y_hat_test[,x])^2))
which.min(MSE_test)


Para <- sapply(c(1, 100, 1000), ridgeOpt,data=train)
AIC <- function(beta, data, lambda){
   return(-2*loglikelihood(beta, data) + 2*df(lambda, data[,2:ncol(data)]))
}

penalty <- c(1,100,1000)
trainaic <- sapply(1:ncol(Para), function(x) AIC(Para[,x], train, penalty[x]))
testaic <- sapply(1:ncol(Para), function(x) AIC(Para[,x], test, penalty[x]))
which.min(testaic)


