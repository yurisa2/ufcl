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
defuzzifyLC2007P1 <- function(x, intervals=u, k=1){
  index <- which(x==max(x))
  if((max(index)-min(index))==(length(index)-1)){   #Case (1 and 2)
    values <- c(intervals[index],intervals[max(index)+1])
    if(is.na(values[length(values)])){
      values[length(values)] <- max(U)
    }
    yhat <- mean(values)
    } else {    #Case (3)
      allIntervals <- c(u,max(U))
      midpoint <- NULL
      for(i in 1:(length(allIntervals)-1)) midpoint <- c(midpoint, mean(allIntervals[i:(i+1)]))
      std <- x/sum(x)
      yhat <- sum(midpoint * std)
    }
    return(yhat)
  }

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
  clusteringUBuild <- function(dataPoints) {
    Dmin <- min(dataPoints)
    Dmax <- max(dataPoints)
    D1 <- 0 #55
    D2 <- 0   #663  #Para dar um máximo de 7000
    U <- c(Dmin-D1, Dmax+D2)

    return(U)
  }

  clusteringPartitionU <- function(U, centers) {
    u <- min(U)
    for(i in 1:(k0-1)) u <- c(u, mean(al.cl$center[i:(i+1)]))

    return(u)
  }

  clusteringInitial <- function (dataPoints, k0, centers, method = "ufcl") {

    initialClustering <- cmeans(dataPoints,k0, centers=centers, method=method)
    #Ordered fuzzy sets are obtained according to the ascending ordered centers
    initialClustering$membership <- initialClustering$membership[,order(initialClustering$centers)]
    initialClustering$size[order(initialClustering$centers)]
    initialClustering$centers <- sort(initialClustering$centers)


    return(initialClustering)
  }

  #####
  ##### Step 2

  lingTermsA2 <- function(LINGUISTIC.TERMS, k0) {

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

    return(A2)
  }

  lingTermsFTS <- function(clusteringInitial) {
    mu <- al.cl$membership
    rownames(mu) <- 1:length(dataPoints)

    fts <- apply(mu, 1, findMax)    #FTS somente com números dos termos linguísticos

    return(fts)
  }



  #####



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



    #### Identifying all Certain Transitions ####


    idCertainTransitions <- function(prec1, fuzzyRelGroups, aux) {

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
          fuzzyRelGroups[[n]]
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
          return(P)
        }



        prediction <- function(data, fts, P, rs) {

          yhat <- NULL
          #Só pode prever um ano a frente
          for(year in 2:length(data)+1){

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
              # S
              # S.index

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
              return(yhat)
            }
