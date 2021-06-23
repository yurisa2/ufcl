rm(list=ls())
library(MLmetrics)

set.seed(1)

PATH <- "C:/inetpub/wwwroot/ufcl"
setwd(PATH)

dados <- read.csv("data/contral.csv", header=TRUE)
dados <- tail(dados, 112)

dataPredicted <- read.csv("foreplus1.csv")

MSE( dd$dataPredicted.x,dd$dados.appl)


dd <- data.frame(dados$appl, dataPredicted$x)
dd[,1]
plot(dados$year,dd[,1],type="l",col="red")
lines(dados$year,dd[,2],col="green")