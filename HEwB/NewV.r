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

PATH <- "/home/yurisa2/lampstack-7.4.10-0/apache2/htdocs/ufcl"
#ORDEM n+1 para calcularmos até ordem n
ORDEM <- 15    #High-order FTS. Maximum of 9th order
k0 <- 7  #Number of intervals
LINGUISTIC.TERMS <- 7 ## Numero de termos linguisticos
##tem que ser igual a numero de intervalos???
REPETITIONS <- 3   ##Monte Carlo simulation repetitions
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


  #Identifica todas as precedencias - Ordem nth
  #----------


  lingTermsPrec <- function(order, builtFTS) {
    prec <- vector("list",order)
    for(n in 1:order) assign(paste("prec",n,sep=""), c(0,builtFTS[1:n]), envir = .GlobalEnv)     #Iniciando variaveis com NULL, prec1, prec2, prec3...

    for(i in 2:length(builtFTS)){
      prec1 <- rbind(prec1, c(builtFTS[(i-1):i+1]))   #Acrescenta NA
      for(k in 2:min(i,order)){
        assign(paste("prec",k,sep=""), rbind(get(paste("prec",k,sep=""), envir = .GlobalEnv), builtFTS[(i-k+1):(i+1)]) , envir = .GlobalEnv)
      }
    }

    for(n in 1:length(prec)){
      aux <- get(paste("prec",n,sep=""), envir = .GlobalEnv)
      aux <- unique(aux)
      aux <- aux[order(aux[,1],aux[,2]),].GlobalEnv
      assign(paste("prec",n,sep=""),aux, envir = .GlobalEnv)
      prec[[n]] <- aux
    }

    return(c(prec,prec1))
  }


  precbeta <- lingTermsPrec(ORDEM, fts)[1]
  prec1beta <- lingTermsPrec(ORDEM, fts)[2]


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


  ### Step 3 ###
  #Divide the derived fuzzy logical relationships into groups based on
  #the current states of the enrollments of fuzzy logical relationships
  #----------
  #frg = fuzzy relationship groups
  #frg[[ordem]][[Au]]
  frg <- vector("list",ORDEM)
  for(n in 1:length(frg)){
    origin <- unique(prec[[n]][,1:(ncol(prec[[n]])-1)])   #unique(prec[[n]][,1])
    group <- NULL
    if(is.vector(origin)==TRUE){
      for(i in 1:length(origin)){
        group[[i]] <- prec[[n]][prec[[n]][,1]==origin[i],]
        if(is.vector(group[[i]])) group[[i]] <- t(as.matrix(group[[i]]))
      }
    } else {
      for(i in 1:nrow(origin)){
        index <- apply(prec[[n]][,1:n],1,function(x,y) identical(x,y), y=origin[i,])
        group[[i]] <- prec[[n]][index,]
        if(is.vector(group[[i]])) group[[i]] <- t(as.matrix(group[[i]]))
      }
    }

    frg[[n]] <- group
    frg[[n]]$prec <- as.matrix(origin)

  }
  #group
  frg
  #####



  #### Identifying all Certain Transitions ####

  #Initializing C
  C <- as.character(unique(prec1[,2]))  #Quem eu quero prever
  P <- NULL
  #C <- "NA"
  while(length(C)>0){
    C.length <- length(C)
    w <- length(P)

    f <- as.numeric(strsplit(C[1]," ")[[1]]) #Primeiro elemento de C, vetor
    n <- length(f)

    #LOOP
    while(w==length(P) && (C.length==length(C))) {
      frg[[n]]
      aux <- get(paste("prec",n,sep=""))

      if(is.na(f[length(f)])){
        S <- NA
        #Vetor que indica onde previsto igual NA
        vetor <- is.na(aux[,ncol(aux)])
        antecedentes <- aux[vetor,-ncol(aux)]
        #Vetor que identifica quais(e quantas) ocorrências iguais a antecedentes de NA
        vetor <- NULL
        for(linha in 1:nrow(aux)){
          vetor <- c(vetor, all(aux[linha,-ncol(aux)]==antecedentes))
        }
        origem <- aux[vetor,-ncol(aux)]
        #aux[aux[,1]==aux[vetor,1],1]
        destino <- aux[vetor,ncol(aux)]
        if(length(unique(destino))>1){
          C <- C[-1]
          C <- c(paste(paste(unique(origem),collapse=" "),S),C)
          w <- Inf   #Sai do inner WHILE
          next()
        } else {
          S <- as.character(S)
          P[[w+1]] <- as.numeric(c(origem, destino))
          C <- C[-1]
          #message(paste("****\nP:",paste(P,collapse="/"),"\nC:",paste(C,collapse="/"),"\n****\n"))
        }
      } else {


        #Removendo linha cuja última coluna seja NA
        if(any(is.na(aux[,ncol(aux)]))){
          aux <- aux[-which(is.na(aux[,ncol(aux)])),]
        }

        #A1 prevê A1 e A2. Logo, uncertain transition
        #Portanto, A1, deve ir para backtracking
        #Caso contrário, essa relação unica vai para P
        vetor <- NULL
        for(linha in 1:nrow(aux)){
          vetor <- c(vetor, all(aux[linha,-ncol(aux)]==f))
        }

        #Identificando o(s) próximo(s) valor(es)
        S <- aux[vetor,ncol(aux)]

        #From Table 6 (Li and Cheng, 2007)
        if(length(S)==0) {
          P[[w+1]] <- as.numeric(c(strsplit(C[1], " ")[[1]], NA))
          C <- C[-1]
          R <- NULL
        }
        if(length(S)==1){
          S <- as.character(S)
          P[[w+1]] <- as.numeric(c(strsplit(C[1], " ")[[1]], strsplit(S, " ")[[1]]))
          C <- C[-1]
        }
        if(length(S)>1){
          C <- C[-1]

          #Backtracking: série de antecedentes
          nome.var <- paste("prec",n+1,sep="")
          vars <- get(nome.var)
          vars <- na.omit(vars)
          vetor <- NULL
          for(i in 1:nrow(vars)){
            vetor <- c(vetor, all(vars[i,-c(1,ncol(vars))]==f))
          }
          vars <- vars[vetor,-ncol(vars)]
          vars <- unique(vars)
          R <- NULL
          for(i in 1:nrow(vars)){
            R[i] <- paste(vars[i,], collapse=" ")
          }
          C <- c(R, C)
          if(length(R)==1){
            w <- Inf
          }
        }
      }
    }
  }


  #### Li and Cheng (2007) Defuzzifying ####
  #rs :: vetor com tamanhos das respostas
  #w :: tamanho máximo de uma subsequência de uma certain transition
  rs <- NULL
  w <- 0
  for(i in 1:length(P)){
    rs <- c(rs, length(P[[i]])-1)
    w <- max(w, length(P[[i]])-1)
  }
  #tamanho da fts
  q <- length(fts)


  FIRST.YEAR <- dados$year[1]+1
  LAST.YEAR <- dados$year[length(dados$year)] +1
  year <- 1993
  yhat <- NULL
  #Só pode prever um ano a frente
  for(year in FIRST.YEAR:LAST.YEAR){

    fts2 <- c(0, fts)
    #Se ano a ser previsto está dentro da série observada
    ultimo.ano <- as.numeric(names(fts))[length(fts)]
    if(year > ultimo.ano){
      fts2 <- c(fts2,NA)
      names(fts2)[length(fts2)] <- year
    }
    index <- which(as.numeric(names(fts2))==year)
    subFTS <- fts2[1:(index-1)]

    ok <- FALSE
    while(length(subFTS)>0 && ok==FALSE){
      Pindex <- which(rs==length(subFTS))
      S <- NULL
      S.index <- NULL
      for(i in Pindex){
        if(all(P[[i]][-length(P[[i]])] == subFTS)){
          S <- c(S, P[[i]][length(P[[i]])])
          S.index <- c(S.index, i)
        }
      }
      if(!is.null(S)){
        ok <- TRUE
      } else {
        subFTS <- subFTS[-1]
      }
    }
    S
    S.index

    #Defuzzifica resposta
    if(!is.na(S)){
      x <- A2[which(rownames(A2)==paste("A",S,sep="")),]
      ind <- which(x==max(x))
      values <- c(u[ind],u[max(ind)+1])
      if(is.na(values[length(values)])){    #Caso selecione o último valor de u
        values[length(values)] <- max(U)
      }
      yhat <- c(yhat, mean(values))
      names(yhat)[length(yhat)] <- year
    } else {
      #PREVER
      x <- A2[paste("A",na.omit(P[[S.index]]),sep=""),]
      if(is.vector(x)) x <- t(as.matrix(x))
      m <- NULL
      for(j in 1:nrow(x)){
        index <- which(x[j,]==max(x[j,]))
        values <- c(u[index],u[max(index)+1])
      }
        if(is.na(values[length(values)])){    #Caso selecione o último valor de u
          values[length(values)] <- max(U)
        m <- c(m, mean(values))
      }
      #Cheng (2002)
      weight <- 1:length(m)
      #Li and Cheng (2007)
      weight <- rep(1,length(m))
      yhat <- c(yhat, sum(weight * m)/sum(weight) )
      names(yhat)[length(yhat)] <- year
    }
  }
  yhat

  forecasting <- cbind(forecasting, yhat)

  ### 20210125 Calcula o erro de cada rodada/coluna/inicialização
  ### 20210125 Adiciona à lista errorcols
  errorcols <- cbind(errorcols, round(MSE(head(dataPoints, -1),head(yhat, -1)),0))
  custerCenters <- cbind(custerCenters, sort(centers))

  print(proc.time()-tic)
} # END of REPETITIONS
(tac <- proc.time()-tic)

aggregate <- rbind(errorcols, custerCenters, forecasting)

### 20210125 Menor erro calculado de cada um dos 30 modelos
### 20210125  - MSE MINIMO DOS 30 MODELOS 4117521 (!!!)
min(errorcols)
write.csv(aggregate, "data/exports/agg.csv")

# FCM fdsOUT
# user  system elapsed
#75.77    0.31   77.55

# UFCL fdsOUT
# user  system elapsed
#74.51    0.28   76.04
forecasting

forecasting.median <- apply(forecasting, 1, median)



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
