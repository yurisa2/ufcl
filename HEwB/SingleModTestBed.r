
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

dataPoints <- tail(dataPoints, 70)

### 20210125 - Inicialização única para todas as rodadas
# centers <- c(0, sample(min(dataPoints[dataPoints!=0]):max(dataPoints),k0-1))
# centers <- as.matrix(centers)

forecasting <- NULL

### 20210125 Inicialização dos centros a cada rodada (iteração)
centers <- c(0, sample(min(dataPoints[dataPoints!=0]):max(dataPoints),k0-1))
centers <- as.matrix(centers)

### 20210508 NEED THIS Silva, P. C. L., Sadaei, H. J., & Guimaraes, F. G. (2016). Interval forecasting with Fuzzy Time Series. 2016 IEEE Symposium Series on Computational Intelligence (SSCI). doi:10.1109/ssci.2016.7850010


al.cl <- clusteringInitial(dataPoints, k0, centers, fcmMethod)
Uval <- clusteringUBuild(dataPoints)
uval <- clusteringPartitionU(Uval, al.cl$centers, k0)
A2val <- lingTermsA2(LINGUISTIC.TERMS, k0)
ftsVal <- lingTermsFTS(dataPoints, al.cl)
supPrec <- calcPrec(ftsVal, ORDEM)
precVal <- defPrec(supPrec, ORDEM)
frgVal <- fuzzyRelGroups(ORDEM, precVal)
Pval <- idCertainTransitions(supPrec, frgVal)
Pval <- fixCertainTransitions(Pval)
rsVal <- defuzRle1(Pval)
yhatVal <- prediction(dataPoints, ftsVal, A2val, Uval, Pval, rsVal, uval)
