data<-iris[51:150,]
nrFolds <- 10
accuracy<-NULL
folds <- rep_len(1:nrFolds, nrow(data)) #generate array containing fold-number for each sample (row)

# actual cross validation
for(k in 1:nrFolds) {
    fold <- which(folds == k)
    data_train <- data[-fold,]
    data_test <- data[fold,]

dist<-NULL
minimum<-NULL
acc<-NULL

for (i in 1:nrow(data_test)){
  for (j in 1:nrow(data_train)){
    d<-sqrt(sum((data_test[i,-5]-data_train[j,-5])^2))
    dist<-c(dist,d)
  }
}
distance<-matrix(dist, nrow=nrow(data_test), ncol = nrow(data_train), byrow = TRUE)

for(z in 1:nrow(data_test)) {
    clas <- as.numeric(data_train$Species[(order(distance[z,]))[1]])
    min<- as.numeric(median(clas))
    minimum<-c(minimum, min)
  }
actual<-as.numeric(data_test$Species)
check_label<-(minimum==actual)
acc<-((sum(check_label))/length(minimum))*100
}
accuracy<-c(accuracy,acc)
show(accuracy)