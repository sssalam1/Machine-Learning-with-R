data_iris<-iris[,-5]
iris_class<-iris[,5]
data_iris<-as.matrix(data_iris)

cov_mat<-cov(data_iris)
tras_data_iris<-t(data_iris)
cov_mat<- tras_data_iris %*% data_iris
cov_mat<-matrix(as.numeric(cov_mat),4,4)
eig<-eigen(cov_mat)
new_data<-t(eig$vectors)%*%t(data_iris)
new_data<-t(new_data)
new_iris<-cbind(new_data,iris_class)


new_iris<-new_iris[sample(150),]
data <- sample(2,nrow(new_iris),replace=TRUE,prob=c(0.8,0.2))
train_data <- new_iris[data==1,]
cl<-train_data[,5]
test_data <- new_iris[data==2,]

library(class); library(caret)
knn_data<-knn(train_data[,1:4], test_data[,1:4], cl<-train_data[,5], k=3)
# cm<-confusionMatrix(as.integer(knn_data),as.integer(test_data[,5]))
a<-as.numeric(knn_data)
b<-as.numeric(test_data[,5])
acc<-sum(a==b)* 100 /length(b)

