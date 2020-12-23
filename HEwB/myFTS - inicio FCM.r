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

SEM.FINAL.DE.SEMANA <- TRUE
PATH <- "/home/yurisa2/lampstack-7.4.10-0/apache2/htdocs/ufcl"
#ORDEM n+1 para calcularmos até ordem n
ORDEM <- 15    #High-order FTS. Maximum of 9th order
MISSING.FIRST <- 1    #1 if may start sequence with NA (tables 3-6 from paper). 0 otherwise
k0 <- 7  #Number of intervals
LINGUISTIC.TERMS <- 7 ## Numero de termos linguisticos
##tem que ser igual a numero de intervalos???
REPETITIONS <- 30   ##Monte Carlo simulation repetitions
fcm.method <- "ufcl" #"ufcl"  #or "cmeans"


setwd(PATH)
source("HEwB/funcoes.r")

dados <- read.csv("data/contral.csv", header=TRUE)
#Ordenando os dados por DATA
# dados <- dados[order(dados$ano,dados$mes,dados$dia),]

# ### RECEITAS DIARIAS ###
# #Calculando receita diaria
# receita.diaria <- tapply(dados$valor.Total, dados$data, sum)
# #ordenando por data
# receita.diaria <- receita.diaria[order(as.Date(names(receita.diaria), "%m/%d/%Y"))]
# receita.diaria.n0 <- receita.diaria[receita.diaria != 0]
# receita.diaria.ts <- ts(receita.diaria, freq=7, start=c(2017,1,1))
# ts.plot(receita.diaria.ts)
#
# dados <- as.data.frame(cbind(receita.diaria, 1:length(receita.diaria)))
# names(dados)
# alabama <- data.frame(appl=dados$receita.diaria, year=dados$V2)

alabama <- dados
####

### 2017 ###
if(SEM.FINAL.DE.SEMANA){
  dom <- seq(1,nrow(dados),7)
  #seg <- seq(2,nrow(dados),7)
  #ter <- seq(3,nrow(dados),7)
  #qua <- seq(4,nrow(dados),7)
  #qui <- seq(5,nrow(dados),7)
  #sex <- seq(6,nrow(dados),7)
  sab <- seq(7,nrow(dados),7)
  #Demanda de dois sábados para sexta-feira anterior
  alabama[13,] <- alabama[14,]
  alabama[139,] <- alabama[140,]
  alabama <- alabama[-c(dom,sab),]
  alabama$year <- 1:nrow(alabama)
}
### FIM 2017 ###

centers <- c(0, sample(min(alabama$appl[alabama$appl!=0]):max(alabama$appl),k0-1))
centers <- as.matrix(centers)

tic <- proc.time()
forecasting <- NULL
mcs<-1
for(mcs in 1:REPETITIONS){
  print(paste("Monte Carlo Simulation #",mcs,sep=""))

  #centers

  ### Step 1 - FCM ###
  al.cl <- cmeans(alabama$appl,k0, centers=centers, method=fcm.method)
  #Ordered fuzzy sets are obtained according to the ascending ordered centers
  al.cl$membership <- al.cl$membership[,order(al.cl$centers)]
  al.cl$size[order(al.cl$centers)]
  al.cl$centers <- sort(al.cl$centers)

  #Building U and u
  Dmin <- min(alabama$appl)
  Dmax <- max(alabama$appl)
  D1 <- 0 #55
  D2 <- 830   #663  #Para dar um máximo de 7000
  U <- c(Dmin-D1, Dmax+D2)

  # Partition U into several u intervals, based on FCM
  u<-min(U)
  al.cl$center
  for(i in 1:(k0-1)) u <- c(u, mean(al.cl$center[i:(i+1)]))
  u


  ### Step 2 - Linguistic Terms ###
  #Example
  #A1 = {u1/1, u2/0.5, u3/0, u4/0, u5/0, u6/0, u7/0}
  A2 <- matrix(0, nrow=LINGUISTIC.TERMS, ncol=k0)
  for(i in 1:LINGUISTIC.TERMS){
    A2[i,i] <- 1
    if((i-1)>0){
      A2[i,(i-1)] <- 0.5
    }
    if((i+1)<k0){
      A2[i,(i+1)] <- 0.5
    }
  }
  rownames(A2) <- paste("A",1:k0,sep="")
  colnames(A2) <- paste("u",1:k0,sep="")
  A2

  ##Gerado a partir do FCM
  #mu <- round(al.cl$membership,3)
  mu <- al.cl$membership
  rownames(mu) <- alabama$year
  mu

  ##Gerado a partir da experiência dos autores/especialistas
  ##Outros papers usam FCM
  #mu <- data.frame(year=1992:1971,
  #                 A1=c(0,0,0,0,0,0,0,0.2,0.2,0.2,0.2,0,0,0,0,0,0.2,0.2,0.8,1,1,1),
  #                 A2=c(0,0,0,0,0,0.1,0.2,0.8,0.8,0.8,0.8,0.2,0.1,0.1,0.5,0.6,0.8,0.8,1,0.9,0.8,0.5),
  #                 A3=c(0,0,0,0,0.1,0.5,1,1,1,1,1,0.8,0.5,0.5,1,1,1,1,0.8,0.2,0.1,0),
  #                 A4=c(0.2,0.3,0.3,0.25,0.5,1,0.7,0.2,0.2,0.2,0.2,1,1,1,0.7,0.6,0.2,0.2,0.1,0,0,0),
  #                 A5=c(0.5,0.5,0.5,0.55,0.8,0.8,0.2,0,0,0,0,0.5,0.9,0.9,0.2,0.1,0,0,0,0,0,0),
  #                 A6=c(1,0.8,0.8,1,1,0.1,0,0,0,0,0,0,0.2,0.2,0,0,0,0,0,0,0,0),
  #                 A7=c(0.8,1,1,0.8,0.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
  #rownames(mu) <- mu$year
  #mu <- mu[order(mu$year),-1]


  # Identifica todas as precedências

  # Encontra a sequencia linguistica da FTS
  fts <- apply(mu, 1, findMax)    #FTS somente com números dos termos linguísticos
  #fts <- paste("A",fts,sep="")   #FTS contendo as letras e números dos termos linguísticos

  #Identifica todas as precedencias - Ordem nth
  #----------
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
  prec
  fts
  paste("A",fts,sep="")
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
    #Inclusão de NA como primeiro Grupo, somente para n>=3, quando anterior inexiste
    #if(n>=1){
    #  for(i in length(group):1){
    #    group[[i+1]] <- group[[i]]
    #  }
    #  #group[[1]] <- matrix(c(NA, group[[1]][-length(group[[1]])]), nrow=1)
    #  group[[1]] <- matrix(c(NA, fts[1:n]), nrow=1)
    #  colnames(group[[1]]) <- colnames(group[[2]])
    #  if(n!=1){
    #    origin <- rbind(group[[1]][-ncol(group[[1]])],origin)
    #  } else {
    #    origin <- matrix(c(NA, origin), ncol=1)
    #  }
    #}
    frg[[n]] <- group
    frg[[n]]$prec <- as.matrix(origin)
    #Adaptando para incluir a ultima colunas com os valores previstos
    #frg[[n]]$prec <- as.matrix(cbind(origin,prec[[n]][,ncol(prec[[n]])]))
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


  FIRST.YEAR <- alabama$year[1]+1
  LAST.YEAR <- alabama$year[length(alabama$year)] +1
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
        if(is.na(values[length(values)])){    #Caso selecione o último valor de u
          values[length(values)] <- max(U)
        }
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
  print(proc.time()-tic)
} # END of REPETITIONS
(tac <- proc.time()-tic)

# FCM fdsOUT
# user  system elapsed
#75.77    0.31   77.55

# UFCL fdsOUT
# user  system elapsed
#74.51    0.28   76.04


forecasting.median <- apply(forecasting, 1, median)

final <- rbind(rep(NA,REPETITIONS+1), cbind(forecasting.median, forecasting))
rownames(final)[1] <- 1
final
colnames(final) <- c("median",1:REPETITIONS)
setwd("HEwB/resultados/")
write.csv(final, paste("contral - ",toupper(fcm.method)," - centroide igual fdsOUT.csv", sep=""), row.names=FALSE)


#Metricas
o <- alabama$appl[-1]
d <- forecasting.median[-length(forecasting.median)]  #Remover "[]" se não prever futuro
mape <- round(MAPE(o, d),2)
(mape.text <- paste(mape,"%",sep=""))
(rmse <- round(RMSE(o, d),1))
sum(SE(o,d))
(mse <- round(MSE(o,d),0))
error <- o - d
mean(error^2)
Uacc(o,d)
Uqual(o,d)

## Inicializando Centroides: FCM
#MAPE: 54.38%
#RMSE: 280.3
#MSE: 78591

## Inicializando Centroides: UFCL
#MAPE: 55.11%
#RMSE: 3035
#MSE: 92117

## SEM Inicializar Centroides: FCM
#MAPE: 52.93%
#RMSE: 240.5
#MSE: 57828

## SEM Inicializar Centroides: UFCL
#MAPE: 50.25%
#RMSE: 304.5
#MSE: 92699


df.final <- data.frame(observed=d,medians=o)
df.final[df.final[,1]==0,]
df.final[round(df.final[,2],4)==197.7714,]

#Figure 3: Li, Cheng and Lin (2008)
resposta <- NULL
for(j in 1:ncol(forecasting)){
  resposta <- rbind(resposta, cbind(as.numeric(rownames(forecasting)), forecasting[,j]))
}
resposta <- as.data.frame(resposta)
names(resposta) <- c("year", "yhat")

boxplot(yhat ~ year, data=resposta[resposta$year!=1993,], border=4,
        ylab="Number of Students", xlab="Year", main="University of Alabama Enrollments")
lines(alabama$appl[-1], type="b", lwd=2)
legend("topleft", c("Actual Enrollments", "Li and Cheng (2007) Forecasted Enrollments"),
       col=c(1,4), lty=1, lwd=c(2,2), cex=0.8)
text(19,14000,paste("MAPE = ",mape.text,"\nRMSE = ",rmse,"\nMSE = ",mse,sep=""))


#Figure 4: Curves of forecasting enfollments and actual enrollments with length 1000
#Chen's first order vs proposed model
year.start <- (alabama$year[1]+1)
ts.plot(alabama.ts, ts(forecasting.median,start=year.start), type="b", col=c(1,2), lty=c(1,2), lwd=c(1,2),
        ylab="Number of Students", xlab="Year", main="University of Alabama Enrollments")
legend("topleft", c("Actual Enrollments", "one-factor Li, Cheng and Lin (2008) Forecasted Enrollments"), col=c(1,2),
       lty=c(1,2), lwd=c(1,2), cex=0.8)
text(1988,14000,paste("MAPE = ",mape.text,"\nRMSE = ",rmse,"\nMSE = ",mse,sep=""))

#Figure 5: Comparison of residual scatter: Chen's first order vs proposed model
plot(error,ylim=c(-1500,2000), pch=4)
abline(h=seq(-1000,1500,500), lty=2)
abline(h=0)



### END ###
