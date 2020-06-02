train<-read.csv("/nfs/cms/mtech18/salam.saudagar/Desktop/ML/A_Vidya/loan_prediction/train_u6lujuX_CVtuZ9i.csv",header = T,na.strings =c("", NA))
test<-read.csv("/nfs/cms/mtech18/salam.saudagar/Desktop/ML/A_Vidya/loan_prediction/test_Y3wMUE5_7gLdaTN.csv",header = T, na.strings =c("", NA))

train[,4]<-as.character(train[,4])
k<-which(train[,4]== "3+") ; train[k,4]<-3
z<-which(train[,4]== "") ; train[z,4]<-0
train[,4]<-as.numeric(train[,4])

test[,4]<-as.character(test[,4])
m<-which(test[,4]== "3+") ; test[m,4]<-3
y<-which(test[,4]== "") ; test[y,4]<-0
test[,4]<-as.numeric(test[,4])

library(randomForest); library(e1071)

train<-na.roughfix(train)
test<-na.roughfix(test)

train_data<-train[,-1]
test_data<-test[,-1]

# ind<-sample(2, nrow(train_data),replace = TRUE, prob = c(0.9,0.1))
# trainData<-train_data[ind==1,]
# testData<-train_data[ind==2,]

accuracy<-c()
cv<-cut(1:nrow(train_data),breaks= 5,labels = F)
for ( i in 1:5) {
  fold<-which(cv== i)
  testData<-train_data[fold,]
  trainData<-train_data[-fold,]
  my_model<- svm(formula= Loan_Status ~., data= trainData, kernel = 'radial', 
                 gamma= 0.01, cost = 2)
  pred<-predict(my_model, testData)
  p<-as.character(pred)
  acc<-sum(p==testData[,12])/length(p)
  accuracy<-c(accuracy,acc)
}
