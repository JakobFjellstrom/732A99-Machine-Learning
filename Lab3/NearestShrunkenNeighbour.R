setwd("C:\\Users\\Dell\\Desktop\\R - Kod\\Machine Learning\\Lab3")
data <- read.csv("geneexp.csv", header=T)
data <- data[,-1]

set.seed(12345)

ind <- sample(nrow(data), floor(0.7*nrow(data)))
train <- data[ind, ]
test <- data[-ind,]

which(colnames(train) == "CellType")
xtrain <- t(train[,-2086])
ytrain <- train[,2086]
traindata <- list(x = xtrain, y = as.factor(ytrain),
                  geneid=as.character(1:nrow(xtrain)), genenames=rownames(xtrain))


library(pamr)

model <- pamr.train(traindata, threshold=seq(0, 10, 0.1))
cvmodel <- pamr.cv(model, traindata)
pamr.plotcv(cvmodel)

which.min(cvmodel$error)
best <- cvmodel$threshold[which.min(cvmodel$error)]
cvmodel$size[which.min(cvmodel$error)]

pamr.plotcen(model, traindata, threshold=best)
pamr.listgenes(model, traindata, threshold=best, genenames = T)

pamr.confusion(model, threshold=best, extra=TRUE)
xtest = t(test[,-2086])
ytest = test[[2086]]

pred <- pamr.predict(model, xtest, threshold=best)

1 - sum(diag(table(Actual = ytest,Predicted = pred)))/sum(table(Actual = ytest,Predicted = pred))




library(kernlab)
train$CellType <- as.factor(train$CellType)
classifier <- ksvm(CellType~., data=train, kernel="vanilladot")

predi <- predict(classifier, t(xtest))
confusion_matrix_svm <- table(predi, t(ytest))
1-sum(diag(confusion_matrix_svm))/sum(confusion_matrix_svm)


cd4 <- data
cd4$CellType[(cd4$CellType == "CD8") | (cd4$CellType == "CD19")] = "OTHER"
cd8 <- data
cd8$CellType[(cd8$CellType == "CD8") | (cd8$CellType == "CD19")] = "OTHER"
cd19 <- data
cd19$CellType[(cd19$CellType == "CD8") | (cd19$CellType == "CD19")] = "OTHER"


predictors_to_test <- colnames(data)[-2086]

cd4_pvals <- sapply(cd4[-2086], function(x) t.test(x ~ cd4$CellType)$p.value)
cd8_pvals <- sapply(cd4[-2086], function(x) t.test(x ~cd8$CellType)$p.value)
cd19_pvals <- sapply(cd4[-2086], function(x) t.test(x ~
                                                     cd19$CellType)$p.value)





cd4_sorted_pvals <- cd4_pvals[order(unlist(cd4_pvals),
                                   decreasing=FALSE)]
cd8_sorted_pvals <- cd8_pvals[order(unlist(cd8_pvals), 
                                   decreasing=FALSE)]
cd19_sorted_pvals <- cd19_pvals[order(unlist(cd19_pvals), 
                                     decreasing=FALSE)]

# Dropping NA values returned from t.test()
cd4_sorted_pvals <- cd4_sorted_pvals[!sapply(cd4_sorted_pvals,
                                            is.nan)]
cd8_sorted_pvals <- cd8_sorted_pvals[!sapply(cd8_sorted_pvals, 
                                            is.nan)]
cd19_sorted_pvals <- cd19_sorted_pvals[!sapply(cd19_sorted_pvals, 
                                              is.nan)]

alpha <- 0.05
benjamini_hochberg <- function(pvals, alpha) {
  M <- length(pvals)
  bh_vals <- c()
  for (ind in 1:M) {
    critical_val <- (ind / M) * alpha
    bh_vals <- c(bh_vals, critical_val)
  }
  return(bh_vals)
}

cd4 <- data.frame(cbind(cd4_sorted_pvals,
                       benjamini_hochberg(cd4_sorted_pvals, alpha),
                       c(1:length(cd4_sorted_pvals))))



colnames(cd4) <- c("p_values", "bh_values", "rank")
cd8 <- data.frame(cbind(cd8_sorted_pvals,
                       benjamini_hochberg(cd8_sorted_pvals, alpha), 
                       c(1:length(cd8_sorted_pvals))))
colnames(cd8) <- c("p_values", "bh_values", "rank")
cd19 <- data.frame(cbind(cd19_sorted_pvals, 
                        benjamini_hochberg(cd19_sorted_pvals, alpha), 
                        c(1:length(cd19_sorted_pvals))))
colnames(cd19) <- c("p_values", "bh_values", "rank")

# Find maximum p_value that is smaller than bh_value. 
#Reject everything below that value.

cd4_max_pval <- cd4[as.numeric(cd4$p_values) < as.numeric(cd4$bh_values), ]

cd8_max_pval <- cd8[as.numeric(cd8$p_values) < as.numeric(cd8$bh_values), ]

cd19_max_pval <- cd19[as.numeric(cd19$p_values) < as.numeric(cd19$bh_values), ] 


cd4_max_pval <- max(as.numeric(cd4_max_pval$p_values))
cd8_max_pval <- max(as.numeric(cd8_max_pval$p_values))
cd19_max_pval <- max(as.numeric(cd19_max_pval$p_values))

cd4_rejected_genes <- cd4_sorted_pvals[cd4_sorted_pvals <  cd4_max_pval]
cd8_rejected_genes <- cd8_sorted_pvals[cd8_sorted_pvals < cd8_max_pval]
cd19_rejected_genes <- cd19_sorted_pvals[cd19_sorted_pvals < cd19_max_pval]


cd4_plot <- ggplot(cd4, aes(rank, p_values, title="CD4")) + 
  geom_point(color="blue", size=1.5) + 
  geom_vline(xintercept = length(cd4_rejected_genes),
             linetype="dashed", color = "red", size=1.5) + ggtitle("CD4 Rejection Area")

cd8_plot <- ggplot(cd8, aes(rank, p_values, title="CD8")) + geom_point(color="blue", size=1.5) + 
  geom_vline(xintercept = length(cd8_rejected_genes),
             linetype="dashed", color = "red", size=1.5) + 
  ggtitle("CD8 Rejection Area")

cd19_plot <- ggplot(cd4, aes(rank, p_values)) + geom_point(color="blue", size=1.5) + 
  geom_vline(xintercept = length(cd19_rejected_genes),
             linetype="dashed", color = "red", size=1.5) + ggtitle("CD19 Rejection Area")

grid.arrange(cd4_plot, cd8_plot, cd19_plot, nrow = 2)




