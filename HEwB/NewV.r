###############################################################################
# Ewbank and Roveda (2018)
# Zero-inflated fuzzy time series
#
# FCM (Li, Cheng and Lin, 2008) for intervals
# Backtracking (Li and Chen, 2007)
# How to apply FTS to zero-inflated time series
# Application: inventory management, raw material prediction, manufacturing management
#
# FCM: mesmos centroides iniciais para todas as rodadas
#
###############################################################################

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

### 20210125 - Inicialização única para todas as rodadas
# centers <- c(0, sample(min(dataPoints[dataPoints!=0]):max(dataPoints),k0-1))
# centers <- as.matrix(centers)

tic <- proc.time()
forecasting <- NULL
mcs<-1
for(mcs in 1:REPETITIONS){
  print(paste("Monte Carlo Simulation #",mcs,sep=""))


  ### 20210125 Inicialização dos centros a cada rodada (iteração)
  centers <- c(0, sample(min(dataPoints[dataPoints!=0]):max(dataPoints),k0-1))
  centers <- as.matrix(centers)

  ### Step 1 - FCM ###

  al.cl <- clusteringInitial(dataPoints, k0, centers, fcmMethod)

  #Building U and u

  U <- clusteringUBuild(dataPoints)

  u <- clusteringPartitionU(U, al.cl$centers)

  ### Step 2 - Linguistic Terms ###

  A2 <- lingTermsA2(LINGUISTIC.TERMS, k0)

  fts <- lingTermsFTS(al.cl)


  # Identifica todas as precedencias - Ordem nth
  # ----------

  prec <- vector("list",ORDEM)
  for(n in 1:ORDEM) assign(paste("prec",n,sep=""), c(0,fts[1:n]))     #Iniciando variaveis com NULL, prec1, prec2, prec3...
  for(i in 2:length(fts)){
    prec1 <- rbind(prec1, c(fts[(i-1):i+1]))   #Acrescenta NA
    for(k in 2:min(i,ORDEM)){
      assign(paste("prec",k,sep=""), rbind(get(paste("prec",k,sep="")), fts[(i-k+1):(i+1)]) )
    }
  }

  for(n in 1:length(prec)){
    aux <- get(paste("prec",n,sep=""))
    aux <- unique(aux)
    aux <- aux[order(aux[,1],aux[,2]),]
    assign(paste("prec",n,sep=""),aux)
    prec[[n]] <- aux
  }


  #####


  #group
  frg <- fuzzyRelGroups(ORDEM, prec)
  #####

  P <- idCertainTransitions(prec1, frg, aux)

  #### Li and Cheng (2007) Defuzzifying ####
  #rs :: vetor com tamanhos das respostas
  #w :: tamanho máximo de uma subsequência de uma certain transition
  #

  rs <- defuz1w(P)

  predictions <- prediction(dataPoints, fts, P, rs)

  forecasting <- cbind(forecasting, predictions)


  # 20210327 NEED URGENTLY TO FIX PREDICTIONS AND DATAPOINTS INDEXES, THEY ARE MISALIGNED


  ### 20210125 Calcula o erro de cada rodada/coluna/inicialização
  ### 20210125 Adiciona à lista errorcols
  errorcols <- cbind(errorcols, round(MSE(head(dataPoints, -2),head(predictions, -1)),0))
  custerCenters <- cbind(custerCenters, sort(centers))



  print(proc.time()-tic)
} # END of REPETITIONS
(tac <- proc.time()-tic)

aggregate <- rbind(errorcols, custerCenters, forecasting)

### 20210125 Menor erro calculado de cada um dos 30 modelos
### 20210125  - MSE MINIMO DOS 30 MODELOS 4117521 (!!!)
min(errorcols)
# write.csv(aggregate, "data/exports/agg.csv")

# FCM fdsOUT
# user  system elapsed
#75.77    0.31   77.55

# UFCL fdsOUT
# user  system elapsed
#74.51    0.28   76.04
forecasting

forecasting.median <- apply(forecasting, 1, median)

length(forecasting.median)

length(dataPoints)

MSE(head(dataPoints, -2),head(forecasting.median, -1)) # HERE ARE THE CHIST


#Metricas
o <- dataPoints[-1]
d <- forecasting.median[-length(forecasting.median)]  #Remover "[]" se não prever futuro
mape <- round(MAPE(o, d),2)
(mape.text <- paste(mape,"%",sep=""))
(rmse <- round(RMSE(o, d),1))
sum(SE(o,d))
(mse <- round(MSE(o,d),0))   #61555


error <- o - d
mean(error^2)
Uacc(o,d)
Uqual(o,d)


  ### END ###
