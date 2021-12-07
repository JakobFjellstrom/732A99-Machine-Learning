setwd("C:\\Users\\Dell\\Desktop\\R - Kod\\Machine Learning\\Lab2")
data <- read.csv("bank-full.csv", header=T)

covs <- scan(text = colnames(data), sep = ".", what = "", comment.char = " ")
data <- tidyr::separate(data,1,covs,sep=";")
data <- data[-which(colnames(data) == "duration")]

data$age <- as.numeric(data[,1])   
data$job <- as.factor(data[,2])    
data$marital <- as.factor(data[,3])    
data$education <- as.factor(data[,4])    
data$default <- as.factor(data[,5])    
data$balance <- as.numeric(data[,6])   
data$housing <- as.factor(data[,7])     
data$loan <- as.factor(data[,8])     
data$contact <- as.factor(data[,9])     
data$day <- as.numeric(data[,10])  
data$month <- as.factor(data[,11])   
data$campaign <- as.numeric(data[,12])   
data$pdays <- as.numeric(data[,13])   
data$previous <- as.numeric(data[,14])   
data$poutcome <- as.factor(data[,15])
data$y <- as.factor(data$y)


n <- dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.4)) 
train <- data[id,] 

Xtrain <- train[, 1:(ncol(train)-1)]
ytrain <- train[, ncol(train)]

id1 <- setdiff(1:n, id)
set.seed(12345)
id2 <- sample(id1, floor(n*0.3)) 
valid <- data[id2,]

Xval <- valid[, 1:(ncol(valid)-1)]
yval <- valid[, ncol(valid)]

id3 <- setdiff(id1,id2)
test <- data[id3,] 

Xtest <- test[, 1:(ncol(test)-1)]
ytest <- test[, ncol(test)]



library(tree)


a <- tree(y ~ . , data = train)
b <- tree(y ~ . , data = train, control = tree.control(nobs = nrow(train), minsize = 7000) )
c <- tree(y ~ . , data = train, control = tree.control(nobs = nrow(train), mindev = 0.0005) )

1-sum(diag(table(ytrain,predict(a, Xtrain, "class"))))/nrow(train)
1-sum(diag(table(ytrain,predict(b, Xtrain, "class"))))/nrow(train)
1-sum(diag(table(ytrain,predict(c, Xtrain, "class"))))/nrow(train)

1-sum(diag(table(yval,predict(a, Xval, "class"))))/nrow(valid)
1-sum(diag(table(yval,predict(b, Xval, "class"))))/nrow(valid)
1-sum(diag(table(yval,predict(c, Xval, "class"))))/nrow(valid)



trainScore <- rep(0,50)
validScore <- rep(0,50)

for(i in 2:50){
  prunedTree=prune.tree(c,best=i)
  pred=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i]=deviance(prunedTree)
  validScore[i]=deviance(pred)
}
plot(2:50, trainScore[2:50], type="b", col="red", ylim = c(8000,12000))
points(2:50, validScore[2:50], type="b", col="blue")

which.min(validScore[2:length(validScore)]) # 21 nodes seems to be best

opTree <- prune.tree(c, best = which.min(validScore[2:length(validScore)]))
summary(opTree)[3]

PopTree <- predict(opTree, Xtest, type="class")
1-sum(diag(table(ytest, as.factor(PopTree))))/nrow(test) # Still ~10% MC rate


library(rpart)

LossTree <- rpart(formula = y ~., data = train, method = "class", parms = list(loss = matrix(data=c(0,1,5,0), nrow=2, byrow = T)))
p <- predict(LossTree, test, type="class")
table(ytest, as.factor(p))
1-sum(diag(table(ytest, as.factor(p))))/nrow(test) # ~14% MC when using the loss Matrix


library(e1071)

TP <- function(preds){
  T_P <- table(ytest, as.factor(preds), useNA = "always")[1,1]
  F_N <- table(ytest, as.factor(preds), useNA = "always")[1,2]
  return(T_P/(T_P + F_N))
}
FP <- function(preds){
  F_P <- table(ytest, as.factor(preds), useNA = "always")[2,1]
  T_N <- table(ytest, as.factor(preds), useNA = "always")[2,2]
  return(F_P/(F_P + T_N))
}

NB <- naiveBayes(y ~. , train)
PNB <- predict(NB, Xtest, "raw")
TreePreds <- predict(opTree, Xtest, type="vector")
pie <- seq(0.05, 0.95, 0.05)

NaiveDF <- data.frame("Pi" = rep(0,length(pie)), "TPR" = rep(0,length(pie)), "FPR" = rep(0,length(pie)))
TreeDF <- data.frame("Pi" = rep(0,length(pie)), "TPR" = rep(0,length(pie)), "FPR" = rep(0,length(pie)))

for(i in 1:length(pie)){
 
  pBayes <- ifelse(PNB[,2] > pie[i], "yes", "no")   
  pTree <-  ifelse(TreePreds[,2] > pie[i], "yes", "no")
  
  NaiveDF[i,] <- c(pie[i], TP(pBayes), FP(pBayes))
  TreeDF[i,] <- c(pie[i], TP(pTree), FP(pTree))
}
plot(TreeDF$FPR,TreeDF$TPR, type="l", col="red" )
lines(NaiveDF$FPR, NaiveDF$TPR, col="blue")

