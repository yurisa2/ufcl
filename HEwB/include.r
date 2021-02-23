### Funcoes para Ewbank and Roveda (2018) ###
## Funcoes baseadas em (2007) Li and Chen

#Encontra a sequência linguística da FTS
findMax <- function(x){
  return(which(x==max(x)))
}

#Operator X to calculate R
xis <- function(x){
  d <- NULL
  for(j in 2:length(x)){
    d <- c(d, min(x[1],x[j]) )
  }
  return(d)
}

#Defuzzification method, Song and Chissom (1993) - Part 1
#k = ordem da previsao
defuzzifyLC2007P1 <- function(x, intervals=u, k=1){
  index <- which(x==max(x))
  if((max(index)-min(index))==(length(index)-1)){   #Case (1 and 2)
    values <- c(intervals[index],intervals[max(index)+1])
    if(is.na(values[length(values)])){
      values[length(values)] <- max(U)
    }
    yhat <- mean(values)
  } else {    #Case (3)
    allIntervals <- c(u,max(U))
    midpoint <- NULL
    for(i in 1:(length(allIntervals)-1)) midpoint <- c(midpoint, mean(allIntervals[i:(i+1)]))
    std <- x/sum(x)
    yhat <- sum(midpoint * std)
  }
  return(yhat)
}

#Root Mean Square Error
RMSE <- function(y,yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  rmse <- sqrt(sum((y-yhat)^2)/length(y))
  return(rmse)
}

#Absolute Percentage Error
APE <- function(y, yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  100*abs(yhat-y)/y
}

#Mean Absolute Percentage Error
MAPE <- function(y,yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  sum(APE(y,yhat))/length(y)
}

#Squared Error
SE <- function(y, yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  (yhat-y)^2
}

#Mean Squared Error
MSE <- function(y,yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  sum(SE(y,yhat))/length(y)
}


#Mean Absolute Percentage Error
#Returns a percentage
MAPE <- function(y,yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  mape <- 100*sum(abs(y-yhat)/y)/length(y)
  return(mape)
}

#Theil's U (Economy)
#forecast quality U2
Uqual <- function(y, yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  sqrt(sum((yhat-y)^2))/sqrt(sum(y^2))
}

#Theil's U (Economy)
#forecast accuracy U1
Uacc <- function(y, yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  sqrt(mean((yhat-y)^2))/(sqrt(mean(y^2)) + sqrt(mean(yhat^2)))
}



########################### New Stuff


##### STEP 1
clusteringUBuild <- function(dataPoints) {
  Dmin <- min(dataPoints)
  Dmax <- max(dataPoints)
  D1 <- 0 #55
  D2 <- 0   #663  #Para dar um máximo de 7000
  U <- c(Dmin-D1, Dmax+D2)

  return(U)
}

clusteringPartitionU <- function(U, centers) {
  u <- min(U)
  for(i in 1:(k0-1)) u <- c(u, mean(al.cl$center[i:(i+1)]))

  return(u)
}

clusteringInitial <- function (dataPoints, k0, centers, method = "ufcl") {

  initialClustering <- cmeans(dataPoints,k0, centers=centers, method=method)
  #Ordered fuzzy sets are obtained according to the ascending ordered centers
  initialClustering$membership <- initialClustering$membership[,order(initialClustering$centers)]
  initialClustering$size[order(initialClustering$centers)]
  initialClustering$centers <- sort(initialClustering$centers)


  return(initialClustering)
}

#####
##### Step 2

lingTermsA2 <- function(LINGUISTIC.TERMS, k0) {

  A2 <- matrix(0, nrow=LINGUISTIC.TERMS, ncol=k0)
  for(i in 1:LINGUISTIC.TERMS){
    A2[i,i] <- 1
    if((i-1)>0){
      A2[i,(i-1)] <- 0.5
    }
    if((i+1)<k0){
      A2[i,(i+1)] <- 0.5
    }
  }
  rownames(A2) <- paste("A",1:k0,sep="")
  colnames(A2) <- paste("u",1:k0,sep="")

  return(A2)
}

lingTermsFTS <- function(clusteringInitial) {
  mu <- al.cl$membership
  rownames(mu) <- 1:length(dataPoints)

  fts <- apply(mu, 1, findMax)    #FTS somente com números dos termos linguísticos

  return(fts)
}



#####
