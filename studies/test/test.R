library(e1071)

x<- read.csv("data.csv")

x<- x['appl']

# x<-rbind(matrix(rnorm(100,sd=0.3),ncol=2),
#          matrix(rnorm(100,mean=1,sd=0.3),ncol=2))
cl<-cmeans(x,3,10000000,verbose=FALSE,method="cmeans",m=2)

# print(cl)

# summary(x)
sort(cl$centers)
