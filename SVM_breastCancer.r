bcdata<-read.csv("/nfs/cms/mtech18/salam.saudagar/Desktop/ML/svm/BC_data.csv")
bcdata<-bcdata[,-c(1,33)]
# data<-sample(2,nrow(bcdata), replace=T,prob=c(0.8,0.2))
# train_data<-bcdata[data==1,]
# test_data<-bcdata[data==2,]
accuracy<-NULL
library(e1071)
#Distribution in train and test
#Applying CV Fold
cv<-cut(1:nrow(bcdata),breaks= 5,labels = F)
for ( i in 1:5) {
  fold<-which(cv== i)
  test_data<-bcdata[fold,]
  train_data<-bcdata[-fold,]
  
  #Applying SVM model
  my_model<- svm(formula= diagnosis ~., data= train_data, type = 'C-classification',
                 kernel = 'linear')
  pred<-predict(my_model, test_data)
  p<-as.character(pred)
  acc<-sum(p==test_data[,1])/length(p)
  accuracy<-c(accuracy,acc)
}
tot_acc<-mean(accuracy)*100

cat(sprintf("The Accuracy for Breast Cancer Data using SVM model is = "), tot_acc)
