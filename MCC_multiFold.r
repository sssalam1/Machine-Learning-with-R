M1<-NULL
mc<-function(k)
{
  iris_a<-iris
  iris_data<-iris_a[sample(150),] #shuffled data
  iris_test<- iris_data[1:30,] #test data set
  iris_train<-iris_data[31:150,] #traning data set
  
  dist<-NULL
  
  for (i in 1:30){
    for (j in 1:120){
      d<-sqrt(sum((iris_train[j,-5]-iris_test[i,-5])^2))
      dist<-c(dist,d)
    }
  }
  dd<- matrix(dist,30,120,byrow=T)
  
  D<-NULL
  
  
  for(m in 1:30)       #for-loop for finding class
  {
    c   = as.numeric(iris_train$Species[(order(dd[m,]))[1:k]])
    min = as.numeric(median(c)) 
    D   = c(D,min)
  }
  F       = as.numeric(iris_test$Species)    #compare the class of test set and predicted output
  mat<-as.matrix(table(F,D))
  
  TP=mat[1,1]
  TN=mat[2,2]
  FP=mat[1,2]
  FN=mat[2,1]
  MCC=((TP*TN)-(FP*FN))/sqrt((TP+FN)*(TP+FP)*(TN+FP)*(TN+FN))
  return(MCC)
}

cat(sprintf("The MCC for k =1 is = %03f \n", mc(1)))
cat(sprintf("The MCC for k =3 is = %03f \n", mc(3)))
cat(sprintf("The MCC for k =5 is = %03f \n", mc(5)))
cat(sprintf("The MCC for k =7 is = %03f \n", mc(7)))
cat(sprintf("The MCC for k =9 is = %03f \n", mc(7)))