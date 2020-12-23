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