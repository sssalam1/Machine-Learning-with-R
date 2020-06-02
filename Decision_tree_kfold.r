library(rpart)
data <- iris
data<- data[sample(nrow(data)),]
Accuracy<- c()

folds<- cut(seq(1,nrow(data)),breaks=5,labels=FALSE) 
for(k in 1:5) {
  fold <- which(folds == k)
  data_train <- data[-fold,]
  data_test <- data[fold,]
  fit <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = data_train)
  plot(fit);  text(fit, use.n = TRUE) 
  class1<- c()
  predict <- round(predict(fit,data_test))
  result <- cbind(round(predict), data_test[5])
  for( i in 1:nrow(predict)) {
    class<- names (which.max(predict[i,]))
    class1<- c(class1,class)
  }
  acc<- (sum(class1==data_test$Species)/dim(data_test)[1])*100
  Accuracy<- c(Accuracy,acc)
}
Accuracy<- mean(Accuracy)
cat("The best Accuracy for 5 fold is:",Accuracy)