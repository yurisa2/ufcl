
rm(list=ls())

library(e1071)

set.seed(1)

PATH <- "/home/yurisa2/lampstack-8.0.3-0/apache2/htdocs/ufcl"

#ORDEM n+1 para calcularmos até ordem n
ORDEM <- 9    #High-order FTS. Maximum of 9th order
# PODE GERAR PROBLEMAS SE FOR POUCO - OPTIM = 15

k0 <- 7  #Number of intervals
LINGUISTIC.TERMS <- 7 ## Numero de termos linguisticos
##tem que ser igual a numero de intervalos???
fcmMethod <- "ufcl" #"ufcl"  #or "cmeans"

errorcols <- NULL
custerCenters <- NULL

setwd(PATH)
source("HEwB/include.r")

dados <- read.csv("data/contral.csv", header=TRUE)
dataPoints <- dados$appl

dataPoints <- tail(dataPoints, 50)



### 20210125 - Inicialização única para todas as rodadas
# centers <- c(0, sample(min(dataPoints[dataPoints!=0]):max(dataPoints),k0-1))
# centers <- as.matrix(centers)

forecasting <- NULL

### 20210125 Inicialização dos centros a cada rodada (iteração)
centers <- c(0, sample(min(dataPoints[dataPoints!=0]):max(dataPoints),k0-1))
centers <- as.matrix(centers)

### 20210508 NEED THIS Silva, P. C. L., Sadaei, H. J., & Guimaraes, F. G. (2016). Interval forecasting with Fuzzy Time Series. 2016 IEEE Symposium Series on Computational Intelligence (SSCI). doi:10.1109/ssci.2016.7850010



al.cl <- clusteringInitial(dataPoints, k0, centers, fcmMethod)
U <- clusteringUBuild(dataPoints)
u <- clusteringPartitionU(U, al.cl$centers, k0)
A2 <- lingTermsA2(LINGUISTIC.TERMS, k0)
fts <- lingTermsFTS(dataPoints, al.cl)

supPrec <- calcPrec(fts, ORDEM)

prec <- defPrec(supPrec, ORDEM)
frg <- fuzzyRelGroups(ORDEM, prec)
P <- idCertainTransitions(supPrec, frg)


supPrec[["prec9"]]

Pprocess <- head(P[[length(P)]], -1) # GET ALL MINUS NA Delisting IT


searchVec <- function(PprocessValue, Pvalue) {
  ret <- FALSE
  inc <- 0
  for(i in head(Pvalue, -1)) {
    inc <- inc + 1

    i <- as.vector(i)

    iB <- head(i, -1)
    if(all(iB == PprocessValue) && length(iB) == length(PprocessValue)) {
      print(inc)
      print("ACHEI")
      print(i)

      ret <- inc
    }
  }

  return(ret)
}

searchVec(tail(Pprocess, -1) , P)
length(dataPoints)
length(P)

searchVec(Pprocess , P)


CTest <-

as.character(unique(supPrec[["prec9"]][,2]))


rs <- defuzRle1(P)
yhat <- prediction(dataPoints, fts, A2, U, P, rs)
forecasting <- cbind(forecasting, yhat)
