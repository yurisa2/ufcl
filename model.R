library(e1071)

data<- read.csv("data/data.csv")

data<- data['appl']

cl<-cmeans(data,7,10000,verbose=FALSE,method="cmeans",m=2)

# print(cl)

# summary(data)
sort(cl$centers)

cl2<-cmeans(data,7,1000,verbose=FALSE,method="ufcl",m=2)
sort(cl2$centers)


WIP - ZERAR UMA DIMENSAO DO CMEANS NO PYTHON
