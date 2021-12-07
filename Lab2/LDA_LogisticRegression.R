setwd("C:\\Users\\Dell\\Desktop\\R - Kod\\Machine Learning\\Lab2")
library(ggplot2)
data(iris)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, colour = Species)) + geom_point() + theme_bw()

SetosaLength <- iris$Sepal.Length[which(iris$Species == "setosa")]
VersicolorLength <- iris$Sepal.Length[which(iris$Species == "versicolor")]
VirignicaLength <- iris$Sepal.Length[which(iris$Species == "virginica")]

SetosaWidth <- iris$Sepal.Width[which(iris$Species == "setosa")]
VersicolorWidth <- iris$Sepal.Width[which(iris$Species == "versicolor")]
VirignicaWidth <- iris$Sepal.Width[which(iris$Species == "virginica")]

Class1 <- cbind(SetosaLength, SetosaWidth)
Class2 <- cbind(VersicolorLength, VersicolorWidth)
Class3 <- cbind(VirignicaLength, VirignicaWidth)

M1 <- as.vector(colMeans(Class1))
M2 <- as.vector(colMeans(Class2))
M3 <- as.vector(colMeans(Class3))

rep(1/3,3) #Prior probs

cov(Class1)
cov(Class2)
cov(Class3)

PooledCov <- 1/3 * (cov(Class1) + cov(Class2) + cov(Class3)) # Pooled


Coefficient1_Setosa <- solve(PooledCov) %*% M1   ### For the true discriminant function, take x^T %*% this
Coefficient2_Setosa <- -1/2 * t(M1) %*% solve(PooledCov) %*% M1 + log(1/3)

Coefficient1_Versicolor <- solve(PooledCov) %*% M2   ### For the true discriminant function, take x^T %*% this
Coefficient2_Versicolor <- -1/2 * t(M2) %*% solve(PooledCov) %*% M2 + log(1/3)

Coefficient1_Virginica <- solve(PooledCov) %*% M3   ### For the true discriminant function, take x^T %*% this
Coefficient2_Virginica <- -1/2 * t(M3) %*% solve(PooledCov) %*% M3 + log(1/3)

# Decision boundary between Setosa & Versicolor - however, the first term has an factor of X
(Coefficient1_Setosa - Coefficient1_Versicolor) + (Coefficient2_Setosa - Coefficient2_Versicolor)

# Decision boundary between Setosa & Virginica - however, the first term has an factor of X
(Coefficient1_Setosa - Coefficient1_Virginica) + (Coefficient2_Setosa - Coefficient2_Virginica)

# Decision boundary between Versicolor & Virginica - however, the first term has an factor of X
(Coefficient1_Versicolor - Coefficient1_Virginica) + (Coefficient2_Versicolor - Coefficient2_Virginica)

Predictions <- data.frame("Length" = iris$Sepal.Length, "Width" = iris$Sepal.Width,"Pred" = rep(0,nrow(iris)), 
                          "Species" = iris$Species) 

for(i in 1:nrow(iris)){
  x <- c(iris$Sepal.Length[i], iris$Sepal.Width[i])
  
  # Discriminant functions
  
  Setosa <- t(x) %*% Coefficient1_Setosa + Coefficient2_Setosa
  Versicolor <- t(x) %*% Coefficient1_Versicolor +  Coefficient2_Versicolor
  Virginica <- t(x) %*% Coefficient1_Virginica + Coefficient2_Virginica
  
  if(Setosa > Versicolor & Setosa > Virginica){Predictions$Pred[i] <- "Setosa"}
    else if(Versicolor > Setosa & Versicolor > Virginica) {Predictions$Pred[i] <- "Versicolor"}
      else{Predictions$Pred[i] <- "Virginica"}  
}

ggplot(Predictions, aes(x=Length, y=Width, colour = Pred)) + geom_point() + theme_bw()
table(as.factor(iris$Species), as.factor(Predictions$Pred))

1-sum(diag(table(as.factor(iris$Species), as.factor(Predictions$Pred))))/nrow(iris) # Missclassification rate = 20%


library(mvtnorm)
set.seed(12)
generated_data <- rbind(rmvnorm(50, mean= M1, sigma = cov(Class1)),rmvnorm(50, mean= M2, sigma = cov(Class2)),
      rmvnorm(50, mean= M3, sigma = cov(Class3)))

generated_df <- data.frame("Length" = generated_data[,1], "Width" = generated_data[,2], "Species" = iris$Species)
ggplot(generated_df, aes(x=Length, y=Width, colour = Species)) + geom_point() + theme_bw()

simulated_data <- data.frame("Length" = rep(0, 150), "Width" = rep(0, 150), "Species" = rep(0,150))
for(i in 1:nrow(iris)){
  
  flower <- sample(iris$Species, 1, T)
  
  if(flower == "setosa"){
    drw <- rmvnorm(1, mean= M1, sigma = cov(Class1))
    simulated_data[i, ] <- c(drw[,1],drw[,2], flower)
  }
    else if(flower == "versicolor"){
      drw <- rmvnorm(1, mean= M2, sigma = cov(Class2))
      simulated_data[i, ] <- c(drw[,1], drw[,2], flower)
    }
      else{
        drw <- rmvnorm(1, mean= M3, sigma = cov(Class3))
        simulated_data[i, ] <- c(drw[,1], drw[,2], flower)
      }
}
simulated_data$Species <- as.character(simulated_data$Species)
ggplot(simulated_data, aes(x=as.numeric(Length), y=as.numeric(Width), colour =Species)) + 
  geom_point() + theme_bw() + xlab("Length") + ylab("Width")


library(nnet)

logreg <- multinom(Species ~ Sepal.Width + Sepal.Length, data = iris)

logregdf <- data.frame("Length" = iris$Sepal.Length, "Width" = iris$Sepal.Width, "Preds"= predict(logreg, iris[c(1,2)], "class"))

ggplot(logregdf, aes(x=Length, y=Width, colour = Preds)) + 
  geom_point() + theme_bw() 

table(as.factor(iris$Species), as.factor(predict(logreg, iris[c(1,2)], "class")))
1-sum(diag(table(as.factor(iris$Species), as.factor(predict(logreg, iris[c(1,2)], "class")))))/nrow(iris)











