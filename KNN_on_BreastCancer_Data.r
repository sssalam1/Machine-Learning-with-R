##Generalized Code of KNN/// Currently using Breast Cancer data set:

data<- read.csv("wdbc.csv") #Import the data
data<- data[,-1] #Removed the first column
n <- nrow(data) #no of rows in the data
data_s<- data[sample(1:n),] #shuffled the data

n_test<- floor(n*0.2); n_train<-n-n_test  #Nuber of rows for test and train
data_test<-data_s[1:n_test,]
data_train<-data_s[(n_test+1):n,]

actual_class<-data_s[1:n_test,1]

dist<-NULL
minimum<-NULL
acc<-NULL
accuracy<-NULL

for (i in 1:n_test){
  for (j in 1:n_train){
    d<-sqrt(sum((data_test[i,-1]-data_train[j,-1])^2))
    dist<-c(dist,d)
  }
}
distance<-matrix(dist, nrow=n_test, ncol = n_train, byrow = TRUE)

for(k in c(1,3,5,7,9))
{
  for(z in 1:n_test)
  {
    clas <- as.numeric(data_train$M[(order(distance[z,]))[1:k]])
    min<- as.numeric(median(clas))
    minimum<-c(minimum, min)
  }
  actual<-as.numeric(actual_class)
  check_label<-(minimum==actual)
  acc<-((sum(check_label))/length(minimum))*100
  accuracy<-c(accuracy,acc)
}
print(accuracy)





# k<-c(1,3,5,7,9)
# for(l in k) 
# {
#   for (m in 1:n_test)
#   {
#     distance[m,]<-sort(distance[m,])
#   }
#   
# }