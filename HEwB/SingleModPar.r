rm(list=ls())
library(httr)

set.seed(1)

PATH <- "/home/yurisa2/lampstack-8.0.3-0/apache2/htdocs/ufcl/"


setwd(PATH)
source("HEwB/include.r")

dados <- read.csv("data/contral.csv", header=TRUE)
data <- tail(dados$appl, 110)
# data <- dados$appl

toString(dados)

windowSize <- 106



r <- GET("http://localhost:8080/ufcl/webservice/params.php")
htcontent <- content(r, "text")

params <- strsplit(htcontent, ",")[[1]]


toString(params)

params[1]


gotData <- TRUE
while (gotData = TRUE)  {
    params <- GET("http://localhost:8080/ufcl/webservice/params.php")
    print(counter, Sys.time())
    results <- runRWindows(data,params[1],"ufcl",getParameters(counter, parCombs, 2),getParameters(counter, parCombs, 2),30, windowSize)

    counter <- counter + 1
}
