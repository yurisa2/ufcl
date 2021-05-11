
rm(list=ls())

library(e1071)

# set.seed(1)

PATH <- "/home/yurisa2/lampstack-8.0.3-0/apache2/htdocs/ufcl"

#ORDEM n+1 para calcularmos até ordem n
ORDEM <- 15    #High-order FTS. Maximum of 9th order
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

dataPoints <- head(dataPoints, 79)


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

yhat2 <- c(NA, yhat)
dataPoints2 <- c(dataPoints, NA)


cbind(dataPoints2, yhat2)
