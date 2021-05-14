
rm(list=ls())

library(e1071)

set.seed(1)

PATH <- "/home/yurisa2/Documents/UFCL"

#ORDEM n+1 para calcularmos até ordem n
ORDER <- 15    #High-order FTS. Maximum of 9th order
# PODE GERAR PROBLEMAS SE FOR POUCO - OPTIM = 15

intervalos <- 7  #Number of intervals
termos <- 7 ## Numero de termos linguisticos
##tem que ser igual a numero de intervalos???
# fcmMethod <- "ufcl" #"ufcl"  #or "cmeans"

setwd(PATH)
source("HEwB/include.r")

dados <- read.csv("data/contral.csv", header=TRUE)
data <- dados$appl

# data <- head(data, 80)

windowSize <- 80


forePlus1 <- NULL

for (i in windowSize:length(data)){
  dataWindowed <- data[(i-windowSize+1):(i+windowSize)]

  forecasting <- runMCS(dataWindowed, intervalos,  "ufcl", ORDER, termos, 3)
  forecasting.median <- apply(forecasting, 1, median)
  forePlus1 <- c(forePlus1, as.numeric(tail(forecasting.median, 1)))


  print(paste("Iteration:",i,Sys.time()))
 }


forePlus1



getMetrics(data,forecasting.median)
