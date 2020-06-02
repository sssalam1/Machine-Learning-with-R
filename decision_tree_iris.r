#Decision tree
data1<-iris
data1<- data1[sample(nrow(data1)),]
# ind<-sample(2, nrow(data1),replace = TRUE, prob = c(0.8,0.2))
folds<-cut(seq(1,nrow(data1)),breaks=5,labels=F)
library(rpart)
for (k in 1:5) {
  fold<-which(folds==1)
  data_train<-data1[-fold,]
  data_test<-data1[fold,]
  model<-rpart(Species ~., data = data_train)
  plot(model) ; text(model, use.n = T)
  class1<-c()
  pred<-predict(model,data_test)
  result <- cbind(round(pred), data_test[5])
  for( i in 1:nrow(pred)) {
    class<- names(which.max(pred[i,]))
    class1<- c(class1,class)
  }
  acc<-(sum(class1==data_test$Species)/dim(data_test)[1])*100
  Accuracy<- c(Accuracy,acc)
}
Accuracy<- mean(Accuracy)
cat("The best Accuracy for 5 fold is:",Accuracy)