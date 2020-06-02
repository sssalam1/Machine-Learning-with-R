data1.var<-NULL
data2.var<-NULL

prob<-function(x,mu,varience)
{
  z<-exp(-(x-mu)^2/(2*varience))/sqrt(2*pi*varience)
  return(z)
}
#sigma(SD)=sqrt(var)
data<-iris[1:100,]
data1<-data[1:48,]
data2<-data[51:98,]

for( i in 1:4) {
var1<-var(data[1:48,i])
var2<-var(data[51:98,i])
data1.var<-append(data1.var,var1)
data2.var<-append(data2.var,var2)
}
data1.mean<-as.numeric(colMeans(data[1:48,1:4]))
data2.mean<-as.numeric(colMeans(data[51:98,1:4]))

test<-data[49,-5] ##Test Sample

prob.class1<-NULL
prob.class2<-NULL

for( i in 1:4) {
  prob1<-prob(test[,i],data1.mean[i],data1.var[i])
  prob2<-prob(test[,i],data2.mean[i],data2.var[i])
  prob.class1<-append(prob.class1, prob1)
  prob.class2<-append(prob.class2, prob2)
}
bay.prob.class1<-prod(prob.class1)*0.5
bay.prob.class2<-prod(prob.class2)*0.5

if(bay.prob.class1>bay.prob.class2) {cat("The given Sample is belongs to setosa class")} else {
  cat("The given Sample is belongs to versicolor class")}


  
  
# priorProb <-length(which(data == 'setosa'))/nrow(data)
  
  # normalize<- function(y)
  # {
  #   return((y-min(y))/(max(y)-min(y)))
  # }
  # iris.data<-normalize(iris.data)
