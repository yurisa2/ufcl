rm(list=ls())


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
#data <- tail(dados$appl, 110)
data <- dados$appl


# data <- head(data, 80)

windowSize <- 111

forePlus1off <- runRWindows(data,intervalos,"ufcl",ORDER,termos,30, windowSize)
getRWMSE(data, forePlus1off, windowSize)

write.csv(forePlus1off, "foreplus1.csv")

