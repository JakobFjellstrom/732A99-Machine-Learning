# Assignment 1

setwd("C:\\Users\\Dell\\Desktop\\R - Kod\\Machine Learning\\Lab1")
library("kknn")
data <- read.csv("optdigits.csv", header=F)

n <- dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.4)) 
train <- data[id,] 

id1 <- setdiff(1:n, id)
set.seed(12345)
id2 <- sample(id1, floor(n*0.3)) 
valid <- data[id2,]

id3 <- setdiff(id1,id2)
test <- data[id3,] 

###

trainKnn <- kknn(as.factor(train$V65) ~. , train = train, test = train, kernel = "rectangular", k=30)
testKnn <- kknn(as.factor(test$V65) ~. , train = test, test=test, kernel = "rectangular", k=30)

table(train$V65, trainKnn$fitted.values)
table(test$V65, testKnn$fitted.values)
1-sum(diag(table(train$V65, trainKnn$fitted.values)))/nrow(train)
1-sum(diag(table(test$V65, testKnn$fitted.values)))/nrow(test)

##

#best
heatmap(t(matrix(as.numeric(train[which(trainKnn$prob[,9] == 1)[1],-65]), ncol=8, nrow=8)), Colv=NA,Rowv=NA)
heatmap(t(matrix(as.numeric(train[which(trainKnn$prob[,9] == 1)[2],-65]), ncol=8, nrow=8)), Colv=NA,Rowv=NA)

#worst
heatmap(t(matrix(as.numeric(train[which(trainKnn$prob[,9] == 0)[1],-65]), ncol=8, nrow=8)), Colv=NA,Rowv=NA)
heatmap(t(matrix(as.numeric(train[which(trainKnn$prob[,9] == 0)[2],-65]), ncol=8, nrow=8)), Colv=NA,Rowv=NA)
heatmap(t(matrix(as.numeric(train[which(trainKnn$prob[,9] == 0)[3],-65]), ncol=8, nrow=8)), Colv=NA,Rowv=NA)


##
MC <- function(p, data){return(1-sum(diag(table(data$V65, p$fitted.values)))/nrow(data))}

ValMC <- rep(0, 30)
trainMC <- rep(0, 30)
for(i in 1:30){
  KNN <- kknn(as.factor(train$V65) ~. , train = train, test = train, kernel="rectangular", k=i)
  trainMC[i] <- MC(KNN, train)
  ValKNN <- kknn(as.factor(train$V65) ~. , train = train, test = valid, kernel="rectangular", k=i)
  ValMC[i] <- MC(ValKNN, valid)
}
plot(1:30, trainMC, type="l")
lines(1:30, ValMC, col="blue")

which.min(ValMC)
MC(kknn(as.factor(train$V65) ~. , train = train, test=test, kernel = "rectangular", k=which.min(ValMC)), test)


##

# -log(KNN$prob[j, valid$V65[j]+1] + 1e-15) * valid$V65[j]
# Above yields q(x) * p(x), where p(x) is the true distribution and q(x) is the predicted probability. Lets use row 1 as example;
# The true value - p(x) - is 1. We want to find the estimated probability of 1, i.e. q(x) = 1. That is row 1 and column 2 of
# KNN$prob (since q(0) is the first coiumn...). The use the formula -sum(log(q(x)) * p(x))

entropy <- rep(0,30)
for(i in 1:30){
  KNN <- kknn(as.factor(train$V65) ~. , train = train, test = valid, kernel = "rectangular", k = i)
  ent <- sapply(1:nrow(KNN$prob), function(j){-log(KNN$prob[j, valid$V65[j]+1] + 1e-15) * valid$V65[j]})
  entropy[i] <- mean(ent)
}
plot(1:30,entropy, "l")
which.min(entropy)

