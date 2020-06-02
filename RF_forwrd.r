library(rpart); library(randomForest);library(caret)
data2<-read.csv("/nfs/cms/mtech18/salam.saudagar/Desktop/ML/data.csv")
data1<-data2[,-c(1,33)]
ind<-sample(2, nrow(data1),replace = TRUE, prob = c(0.8,0.2))
trainData<-data1[ind==1,]
testData<-data1[ind==2,]

rf=randomForest(diagnosis~.,data=trainData,importance=T,proximity=T)
pred<-predict(rf,testData[,-1])
cm=confusionMatrix(pred,testData$diagnosis)
imp<-importance(rf)
imp_sr=sort(imp[,3],decreasing = T)

accuracy<-c()
for (i in 1:30) {
  q<-paste(names(imp_sr)[1:i],collapse="+")
  r<- as.formula(paste(names(data1)[1], q, sep = " ~ "))
  model<-randomForest(r,data=trainData,importance = T,proximity = T)
  pred1<-predict(model,testData)
  cm1<-confusionMatrix(pred1,testData[,1])
  acc<-as.numeric(cm1$overall['Accuracy'])
  accuracy<-c(accuracy,acc)
}
max.acc=which.max(accuracy)
best_subset<-names(imp_sr[1:max.acc])



# Applying CV fold on best attributes

CV<-cut(1:nrow(data1),breaks=5, labels=FALSE)
ntre=seq(50,500,by=50)
c4=NULL
c5=NULL
ntr=NULL

for(i in best_subset)
{
  c3=NULL
  for(j in ntre)
  {
    
    c1=NULL
    
    for( m in 1:5) 
    {
      
      test1<- shuff[which(m==CV,arr.ind=T),]
      train1<- shuff[-(which(m==CV,arr.ind=T)),]
      
      var.predict1<-paste(names(sr)[1:length(best_subset)],collapse="+")
      rnft.formula1 <- as.formula(paste(names(mydata)[1], var.predict1, sep = " ~ "))
      rf1 <- randomForest(rnft.formula1 ,data = train1,ntree=j,mtry=i, importance = TRUE, proximity = TRUE)
      pred1 <- predict(rf1,test1)
      cm1 <- confusionMatrix(pred1,test1$diagnosis)
      over1=as.numeric(cm1$overall['Accuracy'])
      
      c1=c(c1,over1)
    }  
    
    c2=mean(c1)
    
    c3=c(c3,c2)
  }  
  c4=c(c4,max(c3))
  ntr=c(ntr,ntre[which.max(c3)])
}

zz=data.frame(bst,c4,ntr)
zx=which.max(zz[,2])
MTRY=zz[zx,1]
ACCURACY=zz[zx,2]
NTREE=zz[zx,3]
print("Best accuracy and its respective Mtry and Ntree are:")
print(data.frame(MTRY,ACCURACY,NTREE))               
print("No of important attributes are  :")
print(bst.mtr)
print("Important attributes are :")
print(data.frame(names(sr)[1:bst.mtr]))