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

setwd(PATH)
source("HEwB/include.r")

dados <- read.csv("data/contral.csv", header=TRUE)
#data <- tail(dados$appl, 110)
data <- dados$appl


# data <- head(data, 80)

fixll <- TRUE
segsearch <- TRUE

windowSize <- 220

forePlus1off <- runRWindows(data,
                            intervalos,
                            "ufcl",
                            ORDER,
                            termos,
                            3,
                            windowSize,
                            fixll,
                            segsearch,
                            'ss_fixll_180'
                          )
getRWMSE(data, forePlus1off, windowSize)

write.csv(forePlus1off, "foreplus1.csv")
