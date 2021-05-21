rm(list=ls())

library(foreach)
library(doParallel)

cl<-makeCluster(7)
registerDoParallel(cl)

set.seed(1)

PATH <- "C:/inetpub/wwwroot/ufcl"

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
data <- tail(dados$appl, 110)
# data <- dados$appl

windowSize <- 106


# data <- head(data, 80)


intsvals <- c(4,5,6,7,8,9)
orderi <- c(10, 12, 15, 20)
termosi <- c(4,5,6,7,8,9)



parCombs <- list()
iterator <- 1
for (forIntervals in intsvals) for (forOrder in orderi) for (forTermos in termosi) {
  parCombs[[iterator]] <- c(forIntervals, forOrder, forTermos)
  iterator <- iterator + 1
}


getParameters <- function(indexPar, parList, whichPar) {
  return(parList[[indexPar]][whichPar])
  
}



results <- list()
counter <- 1
foreach(valPar = 1:length(parCombs)) %dopar% {
  library(e1071)
  
  print(counter, Sys.time())
  results[[counter]] <- runRWindows(data,getParameters(counter, parCombs, 1),"ufcl",getParameters(counter, parCombs, 2),getParameters(counter, parCombs, 2),30, windowSize)
  
  counter <- counter + 1
}




stopImplicitCluster()

