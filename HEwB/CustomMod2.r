
rm(list=ls())

library(e1071)

set.seed(1)

PATH <- "/home/yurisa2/lampstack-8.0.3-0/apache2/htdocs/ufcl"
#ORDEM n+1 para calcularmos até ordem n
ORDEM <- 15    #High-order FTS. Maximum of 9th order
k0 <- 7  #Number of intervals
LINGUISTIC.TERMS <- 7 ## Numero de termos linguisticos
##tem que ser igual a numero de intervalos???
REPETITIONS <- 30   ##Monte Carlo simulation repetitions
fcmMethod <- "ufcl" #"ufcl"  #or "cmeans"

errorcols <- NULL
custerCenters <- NULL

setwd(PATH)
source("HEwB/include.r")

dados <- read.csv("data/contral.csv", header=TRUE)
dataPoints <- dados$appl

dataPoints <- head(dataPoints, 50)

### 20210125 - Inicialização única para todas as rodadas
# centers <- c(0, sample(min(dataPoints[dataPoints!=0]):max(dataPoints),k0-1))
# centers <- as.matrix(centers)

forecasting <- NULL

for(mcs in 1:REPETITIONS){
  print(paste("Monte Carlo Simulation #",mcs,sep=""))

  ### 20210125 Inicialização dos centros a cada rodada (iteração)
  centers <- c(0, sample(min(dataPoints[dataPoints!=0]):max(dataPoints),k0-1))
  centers <- as.matrix(centers)

  al.cl <- clusteringInitial(dataPoints, k0, centers, fcmMethod)
  U <- clusteringUBuild(dataPoints)
  u <- clusteringPartitionU(U, al.cl$centers, k0)
  A2 <- lingTermsA2(LINGUISTIC.TERMS, k0)
  fts <- lingTermsFTS(dataPoints, al.cl)
  supPrec <- calcPrec(fts, ORDEM)
  prec <- defPrec(supPrec, ORDEM)
  frg <- fuzzyRelGroups(ORDEM, prec)
  P <- idCertainTransitions(supPrec, frg)
  rs <- defuzRle1(P)
  yhat <- prediction(dataPoints, fts, A2, U, P, rs)
  forecasting <- cbind(forecasting, yhat)

  errorcols <- cbind(errorcols, round(MSE(head(dataPoints, -1),head(yhat, -1)),0))
  custerCenters <- cbind(custerCenters, sort(centers))

} # END of REPETITIONS

forecasting.median <- apply(forecasting, 1, median)

#Metricas
o <- dataPoints[-1]
d <- forecasting.median[-length(forecasting.median)]  #Remover "[]" se não prever futuro

mse <- round(MSE(o,d),0)
