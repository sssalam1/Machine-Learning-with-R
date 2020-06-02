data1<-read.csv("/nfs/cms/mtech18/salam.saudagar/Desktop/ML/dataset/wdbc.csv",header = T)
data1<-data1[,-1]

x<-sample(2,nrow(data1),replace=T,c(0.8,0.2))
data_train<-data1[x==1,]
data_test<-data1[x==2,]

library(randomForest)
library(caret)
model<-randomForest(M ~., data=data_train, importance=T,proximity=T)
pred<-predict(model,data_test)
cf<-confusionMatrix(pred,data_test$M)
accuracy<-cf$overall["Accuracy"]
imp<-importance(model)