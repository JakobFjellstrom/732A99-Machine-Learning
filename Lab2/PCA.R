setwd("C:\\Users\\Dell\\Desktop\\R - Kod\\Machine Learning\\Lab2")
data <- read.csv("communities.csv", header = T)
X <- scale(data[, -which(colnames(data) == "ViolentCrimesPerPop")])
y <- data$ViolentCrimesPerPop
  
e <- eigen(cov(X))
e$values/sum(e$values)
which(cumsum(e$values/sum(e$values)) > 0.95)  ## To explain 95% of the variation, seem slike we need atleast 35 variables
cumsum(e$values/sum(e$values)) ## The proportion of variation explained by each of the first two principal components is 0.2501  & 0.4195

pc <- princomp(X)
plot(pc$loadings[,1], ylab="Contribution", xlab="Covariate")
sort(abs(pc$loadings[,1]), decreasing=T)[1:5] # 5 most contributing features

library(ggplot2)
df <- data.frame(pc$scores)
df$crime <- y

ggplot(df, aes(x =Comp.1, y =Comp.2, color=crime)) +  geom_point() + scale_color_gradient(low="blue", high="red")


preg <- lm(y ~ poly(df$Comp.1,2))
summary(preg)
df[,"yhat"] <- predict(preg)
ggplot(df, aes(x=Comp.1)) + geom_point(aes(y=crime), color="black") +geom_line(aes(y=yhat), color="red", size=2)      

library(boot)

mle <- preg

rng <- function(data, mle){
  data1 <- data.frame(crime=data$crime, Comp.1=data$Comp.1)
  n <- nrow(data1)
  
  # Generate new y
  data1$crime <- rnorm(n, mean = predict(mle, newdata=data1), sd(mle$residuals))
  return(data1)
}

CIbands <- function(data1){
  res <- lm(crime ~ poly(Comp.1,2), data=data1) #fit polynomial model with the new simulated data
  # But do predictions based on the original data, i.e., all the Comp.1 values from the original data frame
  crime <- predict(res, newdata=data2) 
  return(crime)
}

Pred_bands <- function(data1){
  res <- lm(crime ~ poly(Comp.1,2), data=data1) 
  yhat <- predict(res, newdata=data2) 
  n <- length(yhat)
  # Generate new y based on current estimate
  preds <- rnorm(n, yhat, sd(mle$residuals))
  return(preds)
}
data2 <- df
CIres <- boot(data2, statistic = CIbands, R=1000, mle = mle, ran.gen = rng, sim="parametric")
Predci <- boot(data2, statistic = Pred_bands, R=1000, mle = mle, ran.gen = rng, sim="parametric")

CIenv <- envelope(CIres)
Penv <- envelope(Predci)

df[,"cmax"] = CIenv$point[1,]
df[,"cmin"] = CIenv$point[2,]
df[,"pmax"] = Penv$point[1,]
df[,"pmin"] = Penv$point[2,]


ggplot(df, aes(x=Comp.1)) + geom_ribbon(aes(ymin=pmin, ymax=pmax), color="darkgreen", fill="green", alpha=0.3) + 
  geom_ribbon(aes(ymin=cmin, ymax=cmax), color="darkblue", fill="blue", alpha=0.5) + 
  geom_point(aes(y=crime), color="black") + geom_line(aes(y=yhat), color="red", size=0.7)





