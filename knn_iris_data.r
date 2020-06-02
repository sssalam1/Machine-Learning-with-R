# KNN Code for Iris Data Set- Binary
iris_a<-iris[51:150,] #remove 3rd class and 5th column of iris data set
rn<-sample(100)
iris_data<-iris_a[rn,] #shuffled data
iris_test<- iris_data[1:20,] #test data set
iris_train<-iris_data[21:100,] #traning data set
actual_species<-iris_test$Species #class/species of Test data
actual_class<-as.numeric(actual_species)

dist<-NULL
D<-NULL
accuracy  = NULL
accuracy1 = NULL

for (i in 1:20){
  for (j in 1:80){
    d<-sqrt(sum((iris_train[j,-5]-iris_test[i,-5])^2))
    dist<-c(dist,d)
  }
}

dd<- matrix(dist,20,80,byrow=T)


for( n in c( 1,3,5,7,9 ))     #use no of k 
{
  # D= NULL
  for(m in 1:20)       #for-loop for finding class
  {
    c   = as.numeric(iris_train$Species[(order(dd[m,]))[1:n]])
    min = as.numeric(summary(c)[3]) 
    D   = c(D,min)
  }
  F       = as.numeric(iris_test$Species)    #compare the class of test set and predicted output
  labels   <- (D==F)
  accuracy = ((sum(labels))/length(D))*100  # calculate accuracy
  accuracy1= c(accuracy1,accuracy)
}
print(accuracy1)


# # k<-c(1,3,5,7,9)
# for (z in 1:20) 
# {
#   n<-as.numeric(iris_train$Species[(order(dd[z,]))])
#   num<-c(num,n)
# }
# 
# num1<- matrix(num,20,80,byrow=T)
