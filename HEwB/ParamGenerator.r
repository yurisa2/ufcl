rm(list=ls())

set.seed(1)

PATH <- "/home/yurisa2/lampstack-8.0.3-0/apache2/htdocs/ufcl/"

#ORDEM n+1 para calcularmos até ordem n
ORDER <- 15    #High-order FTS. Maximum of 9th order
# PODE GERAR PROBLEMAS SE FOR POUCO - OPTIM = 15

intervalos <- 7  #Number of intervals
termos <- 7 ## Numero de termos linguisticos
##tem que ser igual a numero de intervalos???
# fcmMethod <- "ufcl" #"ufcl"  #or "cmeans"


intsvalsTerms <- c(4,5,6,7,8,9,10,11)
orderi <- c(10, 12, 15, 20)
method <- c("ufcl", "cmeans")

allParams <- expand.grid(intsvals, orderi, termosi, method)
allParams[2,]


write.csv(allParams,"allparms.csv")
