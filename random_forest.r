#Decision Tree, Random Forest Code:

data<-read.csv("/nfs/cms/mtech18/salam.saudagar/Desktop/wdbc.csv")
data<-data[,-1]
library(rpart)
library(randomForest)
set.seed(800)
n_data<- data[sample(nrow(data)),]
folds<- cut(seq(1,nrow(n_data)),breaks=5,labels=FALSE) 

for(k in 1:5) {
  fold <- which(folds == k)
  data_train <- data[-fold,]
  data_test <- data[fold,]
  actual_class <- data_test[,1]
  ran_forest <- randomForest( M ~., data= data_train, mtry = 8, ntree= 200)
  predValue<-predict(ran_forest, data_test)
  table(predValue, actual_class)
  bestmtry <- tuneRF(data_train, data_train[,1], ntreeTry=200, stepFactor=1.2, improve=0.01, trace=T, plot=T)
  accuracy <- mean(as.numeric(predValue)==as.numeric(actual_class))*100
}