
rm(list=ls())

library(e1071)

set.seed(1)

PATH <- "/home/yurisa2/lampstack-8.0.3-0/apache2/htdocs/ufcl"

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

# data <- head(data, 79)


centersVal <- c(0, sample(min(data[data!=0]):max(data),k0-1))
centersVal <- as.matrix(centersVal)

yhat <- runModel(data, intervalos, centersVal, "ufcl", ORDER, termos)

getMetrics(data,yhat)
