setwd("C:\\Users\\Dell\\Desktop\\R - Kod\\Machine Learning\\Lab3")
library(randomForest)

## Training data

Train_data <- function(task){

  x1 <- runif(100)
  x2 <- runif(100)
  
  if(task == "a"){
    train <- cbind(x1,x2)
    y <- as.numeric(x1<x2)
    trainlabs <- as.factor(y)
  }else if(task == "b"){
    train <- cbind(x1,x2)
    y <- as.numeric(x1<0.5)
    trainlabs <- as.factor(y)
  }else if(task == "c"){
    train <- cbind(x1,x2)
    y <- as.numeric(x1<0.5 & x2 < 0.5 | x1 > 0.5 & x2 > 0.5)
    trainlabs <- as.factor(y)
  }
  
  tr <- cbind(train, trainlabs)
  return(tr)
}

Train_data("c")

## Test data
set.seed(1234)
Test_data <- function(task){
  x1 <- runif(1000)
  x2 <- runif(1000)
  if(task == "a"){
    test <- cbind(x1,x2)
    y <- as.numeric(x1<x2)
    testlabs <- as.factor(y)
  }else if(task == "b"){
    test <- cbind(x1,x2)
    y <- as.numeric(x1<0.5)
    testlabs <- as.factor(y)
  }else if(task == "c"){
    test <- cbind(x1,x2)
    y <- as.numeric(x1<0.5 & x2 < 0.5 | x1 > 0.5 & x2 > 0.5)
    testlabs <- as.factor(y)
  }
  
  te <- cbind(test, testlabs)
  return(te)
}

MC <- function(y, yhat) 1 - sum(diag(table(y, yhat)))/length(y)

Forests <- function(task, nodes){
  
  MCMatrix <- matrix(NA, ncol=3, nrow=1000)
  trees <- c(1, 10, 100)
  for(j in 1:3){
    for(i in 1:1000){
      data <- Train_data(task)
      test_data <- Test_data(task)
      forest <- randomForest(as.factor(trainlabs) ~ x1 + x2, data = data, ntree = trees[j], nodesize = nodes, keep.forest = TRUE)
      p <- predict(forest, test_data[,1:2])
      MCMatrix[i,j] <-  MC(test_data[,3], p)
    }
  }
  M <- rbind(colMeans(MCMatrix), apply(MCMatrix, 2, var))
  row.names(M) <- c("Mean MC", "V[MC]")
  colnames(M) <- c("1", "10", "100")
  return(M)
}
Forests("c", 12) # Remember that task c requires 12 nodes







