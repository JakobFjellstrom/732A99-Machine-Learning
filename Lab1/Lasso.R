setwd("C:\\Users\\Dell\\Desktop\\R - Kod\\Machine Learning\\Lab1")
dataset <- read.csv("tecator.csv", header=T)
set.seed(12345)
ind <- sample(nrow(dataset), floor(nrow(dataset)*0.5))
train <- dataset[ind,-c(1,103,104)]
test <- dataset[-ind,-c(1,103,104)]

X <- scale(as.matrix(train[, 1:(ncol(train)-1)]))
y <- as.vector(train[, ncol(train)])
Xtest <- scale(as.matrix(test[, 1:(ncol(test)-1)]))
ytest <- as.vector(test$Fat)

trainMSE <- 1/nrow(train) * sum((y - predict(lm(Fat ~ ., data = train), train))^2)
testMSE <- 1/nrow(test) * sum((ytest - predict(lm(Fat ~ ., data = train), test))^2)


library(glmnet)

glmfit <- glmnet(X,y,alpha=1)
plot(glmfit, xvar = "lambda")
plot(log(glmfit$lambda),glmfit$df, type="l")

ridgefit <- glmnet(X,y,alpha=0)
plot(ridgefit, xvar = "lambda")
plot(log(ridgefit$lambda),ridgefit$df, type="l") # Coefficients never goes to 0 in ridge regression

CV <- cv.glmnet(X, y)
plot(CV)
CV$lambda.min
CV$index

opt_lasso <- glmnet(X,y,alpha=1, lambda = CV$lambda.min)
opt_lasso$df  # 12 coefficients for "optimal lambda". Note lambda.1se could be more robust to use

plot(CV$lambda, CV$cvm, type="l")
CV$cvm[which(CV$lambda == CV$lambda.min)] # Cross-validated error of ~14 for optimal lambda
CV$lambda.min < exp(-2) # Since the mean square error is monotonically increasing (in this case), 
                        # we know that since exp(-2) > lambda.min, it also has a higer MSE

Lasso_predictions <- predict(opt_lasso, Xtest)

plot(ytest, Lasso_predictions, xlab= "Predicted", ylab="Actual")
# Should be a straight diagonal line, i.e. predicted 30 when actual = 30. So this isnt looking that bad

lasso_mse <- 1/nrow(test) * sum((ytest - Lasso_predictions)^2)

generated_data <- rnorm(nrow(test), Lasso_predictions, sd(ytest - Lasso_predictions))
plot(ytest, generated_data, xlab="actual", ylab="Generated")


