data1<-read.csv("/nfs/cms/mtech18/salam.saudagar/Desktop/ML/dataset/wdbc.csv",header = T)
data1<-data1[,-1]

x<-sample(2,nrow(data1),replace=T,c(0.8,0.2))
data_train<-data1[x==1,]
data_test<-data1[x==2,]

library(randomForest)
library(caret)
mtri<- seq(2,30,by=2)
ntre<-seq(50,500,by=50)

result<-c()
for ( i in mtri) {
  overall_acc<-c()
  for (j in ntre) {
    model<-randomForest(M ~., data=data_train, ntree= j, mtry=i, importance=T,proximity=T)
    pred<-predict(model,data_test)
    cf<-confusionMatrix(pred,data_test$M)
    accuracy<-as.numeric(cf$overall["Accuracy"])*100
    overall_acc<-c(overall_acc,accuracy)
  }
  result<- c(result,overall_acc)
}
res<-matrix(result,length(mtri),length(ntre),byrow = T)



imp<-importance(model)
# varImpPlot(model)
