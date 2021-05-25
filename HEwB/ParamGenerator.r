rm(list=ls())

set.seed(1)

PATH <- "/home/yurisa2/lampstack-7.3.7-1/apache2/htdocs/UFCL"

#ORDEM n+1 para calcularmos até ordem n
ORDER <- 15    #High-order FTS. Maximum of 9th order
# PODE GERAR PROBLEMAS SE FOR POUCO - OPTIM = 15

intervalos <- 7  #Number of intervals
termos <- 7 ## Numero de termos linguisticos
##tem que ser igual a numero de intervalos???
# fcmMethod <- "ufcl" #"ufcl"  #or "cmeans"


intsvalsTerms <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
orderi <- c(10, 12, 15, 20, 25, 30)
method <- c("ufcl", "cmeans")

allParams <- expand.grid(intsvalsTerms, orderi, method)
allParams[2,]


write.csv(allParams,"allparms.csv")
