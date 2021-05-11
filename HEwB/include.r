### Funcoes para Ewbank and Roveda (2018) ###
## Funcoes baseadas em (2007) Li and Chen

#Encontra a sequência linguística da FTS
findMax <- function(x){
  return(which(x==max(x)))
}

#Operator X to calculate R
xis <- function(x){
  d <- NULL
  for(j in 2:length(x)){
    d <- c(d, min(x[1],x[j]) )
  }
  return(d)
}

#Defuzzification method, Song and Chissom (1993) - Part 1
#k = ordem da previsao
# defuzzifyLC2007P1 <- function(x, intervals=u, k=1){
#   index <- which(x==max(x))
#   if((max(index)-min(index))==(length(index)-1)){   #Case (1 and 2)
#     values <- c(intervals[index],intervals[max(index)+1])
#     if(is.na(values[length(values)])){
#       values[length(values)] <- max(U)
#     }
#     yhat <- mean(values)
#   } else {    #Case (3)
#     allIntervals <- c(u,max(U))
#     midpoint <- NULL
#     for(i in 1:(length(allIntervals)-1)) midpoint <- c(midpoint, mean(allIntervals[i:(i+1)]))
#     std <- x/sum(x)
#     yhat <- sum(midpoint * std)
#   }
#   return(yhat)
# }

#Root Mean Square Error
RMSE <- function(y,yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  rmse <- sqrt(sum((y-yhat)^2)/length(y))
  return(rmse)
}

#Absolute Percentage Error
APE <- function(y, yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  100*abs(yhat-y)/y
}

#Mean Absolute Percentage Error
MAPE <- function(y,yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  sum(APE(y,yhat))/length(y)
}

#Squared Error
SE <- function(y, yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  (yhat-y)^2
}

#Mean Squared Error
MSE <- function(y,yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  sum(SE(y,yhat))/length(y)
}


#Mean Absolute Percentage Error
#Returns a percentage
MAPE <- function(y,yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  mape <- 100*sum(abs(y-yhat)/y)/length(y)
  return(mape)
}

#Theil's U (Economy)
#forecast quality U2
Uqual <- function(y, yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  sqrt(sum((yhat-y)^2))/sqrt(sum(y^2))
}

#Theil's U (Economy)
#forecast accuracy U1
Uacc <- function(y, yhat){
  if(!is.vector(y) || !is.vector(yhat)) stop("y and yhat must be vectors")
  if(length(y)!=length((yhat))) stop("y and yhat have different lengths")
  sqrt(mean((yhat-y)^2))/(sqrt(mean(y^2)) + sqrt(mean(yhat^2)))
}



########################### New Stuff


##### STEP 1
clusteringUBuild <- function(data) {
  Dmin <- min(data)
  Dmax <- max(data)
  D1 <- 0 #55
  D2 <- 700   #663  #Para dar um máximo de 7000
  UValue <- c(Dmin-D1, Dmax+D2)

  return(UValue)
}

clusteringPartitionU <- function(UValue, centers, intervals) {
  uValue <- min(UValue)
  for(i in 1:(intervals-1)) uValue <- c(uValue, mean(centers[i:(i+1)]))

  return(uValue)
}

clusteringInitial <- function (data, intervals, centers, method = "ufcl") {

  initialClustering <- cmeans(data,intervals, centers=centers, method=method)
  #Ordered fuzzy sets are obtained according to the ascending ordered centers
  initialClustering$membership <- initialClustering$membership[,order(initialClustering$centers)]
  initialClustering$size[order(initialClustering$centers)]
  initialClustering$centers <- sort(initialClustering$centers)


  return(initialClustering)
}

#####
##### Step 2

lingTermsA2 <- function(terms, intervals) {

  A2Value <- matrix(0, nrow=terms, ncol=intervals)
  for(i in 1:terms){
    A2Value[i,i] <- 1
    if((i-1)>0){
      A2Value[i,(i-1)] <- 0.5
    }
    if((i+1)<intervals){
      A2Value[i,(i+1)] <- 0.5
    }
  }
  rownames(A2Value) <- paste("A",1:intervals,sep="")
  colnames(A2Value) <- paste("u",1:intervals,sep="")

  return(A2Value)
}

lingTermsFTS <- function(data, clusteringInitial) {
  mu <- clusteringInitial$membership
  rownames(mu) <- 1:length(data)

  ftsValue <- apply(mu, 1, findMax)    #FTS somente com números dos termos linguísticos

  return(ftsValue)
}

### Step 3 ###
#Divide the derived fuzzy logical relationships into groups based on
#the current states of the enrollments of fuzzy logical relationships
#----------
#frg = fuzzy relationship groups
#frg[[ordem]][[Au]]

fuzzyRelGroups <- function(order, prec) {

  frgValue <- vector("list",order)
  for(n in 1:length(frgValue)){
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

    frgValue[[n]] <- group
    frgValue[[n]]$prec <- as.matrix(origin)

  }

  return(frgValue)
}


calcPrec <- function(ftsValue, order) {
  precVal <- list()

  for(n in 1:order) precVal[[paste("prec",n,sep="")]] <-  c(0,fts[1:n])  # STOPPED HERE
  for(i in 2:length(fts)){
    precVal[["prec1"]] <- rbind(precVal[["prec1"]], c(fts[(i-1):i+1]))   #Acrescenta NA

    for(k in 2:min(i,order)){
      precProb <- fts[(i-k+1):(i+1)]
      precVal[[paste("prec",k,sep="")]] <- rbind(precVal[[paste("prec",k,sep="")]]  , precProb)
    }
  }
  return(precVal)
}

defPrec <- function(precVal, order) {
  prec <- vector("list",order)
  for(n in 1:length(prec)){
    # n <- 1
    aux <- precVal[[paste("prec",n,sep="")]]
    aux <- unique(aux)
    aux <- aux[order(aux[,1],aux[,2]),]
    precVal[[paste("prec",n,sep="")]] <- aux
    prec[[n]] <- aux
  }
  return(prec)
}


idCertainTransitions <- function(precVal, frgValue) {
  #Initializing C
  C <- as.character(unique(precVal[["prec1"]][,2]))  #Quem eu quero prever
  # precVal[["prec1"]][,2]
  PValue <- NULL
  #C <- "NA"
  while(length(C)>0){
    C.length <- length(C)
    w <- length(PValue)

    f <- as.numeric(strsplit(C[1]," ")[[1]]) #PValuerimeiro elemento de C, vetor
    n <- length(f)

    #LOOPValue
    while(w==length(PValue) && (C.length==length(C))) {
      frgValue[[n]]
      # aux <- get(paste("prec",n,sep=""))
      aux <- data.frame(precVal[paste("prec",n,sep="")])

      if(is.na(f[length(f)])){

        # print("BREAKPONT CAIU NO IF")

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
          PValue[[w+1]] <- as.numeric(c(origem, destino))
          C <- C[-1]
          #message(paste("****\nPValue:",paste(PValue,collapse="/"),"\nC:",paste(C,collapse="/"),"\n****\n"))
        }
      } else {

        # print("BREAKPONT CAIU NO ELSE")


        #Removendo linha cuja última coluna seja NA
        if(any(is.na(aux[,ncol(aux)]))){
          aux <- aux[-which(is.na(aux[,ncol(aux)])),]
          # print("BREAKPONT REMOVER LINHA CUJA COLUNA SEJA NA")
        }

        ncol(aux)
        as.vector(aux)
        #A1 prevê A1 e A2. Logo, uncertain transition
        #PValueortanto, A1, deve ir para backtracking
        #Caso contrário, essa relação unica vai para PValue
        vetor <- NULL
        for(linha in 1:nrow(aux)){
          vetor <- c(vetor, all(aux[linha,-ncol(aux)]==f))
        }

        #Identificando o(s) próximo(s) valor(es)
        S <- aux[vetor,ncol(aux)]

        #From Table 6 (Li and Cheng, 2007)
        if(length(S)==0) {
          PValue[[w+1]] <- as.numeric(c(strsplit(C[1], " ")[[1]], NA))
          C <- C[-1]
          R <- NULL
        }
        if(length(S)==1){
          S <- as.character(S)
          PValue[[w+1]] <- as.numeric(c(strsplit(C[1], " ")[[1]], strsplit(S, " ")[[1]]))
          C <- C[-1]
        }

        # ISTO É ESTRANHO, NO RELATION TO PVALUE
        if(length(S)>1){
          C <- C[-1]

          # print("BREAKPONT BACKTRACKING")

          #Backtracking: série de antecedentes
          nome.var <- paste("prec",n+1,sep="")
          vars <-data.frame(precVal[nome.var])
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

  return(PValue)
}

defuzRle1 <- function(PValue) {
  #### Li and Cheng (2007) Defuzzifying ####
  #rs :: vetor com tamanhos das respostas
  #w :: tamanho máximo de uma subsequência de uma certain transition

  rsValue <- NULL
  for(i in 1:length(PValue)){
    rsValue <- c(rsValue, length(PValue[[i]])-1)
  }

  return(rsValue)
}

prediction <- function(data, ftsValue, A2Value, UValue, PValue, rsValue) {

    FIRST.ITEM <- 2
    LAST.ITEM <- length(data)+10
    # year <- 1993
    yhatValue <- NULL
    #Só pode prever um ano a frente
    for(year in FIRST.ITEM:LAST.ITEM){

      ftsValue2 <- c(0, ftsValue)
      #Se ano a ser previsto está dentro da série observada
      ultimo.ano <- as.numeric(names(ftsValue))[length(ftsValue)]
      if(year > ultimo.ano){
        ftsValue2 <- c(ftsValue2,NA)
        names(ftsValue2)[length(ftsValue2)] <- year
      }
      index <- which(as.numeric(names(ftsValue2))==year)
      subFTS <- ftsValue2[1:(index-1)]

      ok <- FALSE
      while(length(subFTS)>0 && ok==FALSE){
        Pindex <- which(rsValue==length(subFTS))
        S <- NULL
        S.index <- NULL
        for(i in Pindex){
          if(all(PValue[[i]][-length(PValue[[i]])] == subFTS)){
            S <- c(S, PValue[[i]][length(PValue[[i]])])
            S.index <- c(S.index, i)
          }
        }
        if(!is.null(S)){
          ok <- TRUE
        } else {
          subFTS <- subFTS[-1]
        }
      }
      # S
      # S.index

      #Defuzzifica resposta
      if(!is.na(S)){
        x <- A2Value[which(rownames(A2Value)==paste("A",S,sep="")),]
        ind <- which(x==max(x))
        values <- c(u[ind],u[max(ind)+1])
        if(is.na(values[length(values)])){    #Caso selecione o último valor de u
          values[length(values)] <- max(UValue)
        }
        yhatValue <- c(yhatValue, mean(values))
        names(yhatValue)[length(yhatValue)] <- year
      } else {
        #PREVER

        x <- A2Value[paste("A",na.omit(PValue[[S.index]]),sep=""),]

        if(is.vector(x)) x <- t(as.matrix(x))

        # print(paste("x", x))
        print(paste("u", u))

        m <- NULL

        for(j in 1:nrow(x)){
          index <- which(x[j,]==max(x[j,]))
          values <- c(u[index],u[max(index)+1])
        }

        print(paste("values", values))

        if(is.na(values[length(values)])){    #Caso selecione o último valor de u
          values[length(values)] <- max(UValue)
          m <- c(m, mean(values))
        }

        print(paste("m", m))
        #Cheng (2002)
        weight <- 1:length(m)
        #Li and Cheng (2007)
        weight <- rep(1,length(m))
        print(paste("weight", weight))
        print(paste("year", year))
        yhatValue <- c(yhatValue, sum(weight * m)/sum(weight) )
        names(yhatValue)[length(yhatValue)] <- year
      }
    }

    yhatValue <- yhatValue[-length(yhatValue)] #Related to BREAKPOINT1

    return(yhatValue)
}
