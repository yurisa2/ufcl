rm(list=ls())
library(httr)

set.seed(1)

PATH <- "/home/yurisa2/lampstack-8.0.3-0/apache2/htdocs/ufcl/"


setwd(PATH)
source("HEwB/include.r")

dados <- read.csv("data/contral.csv", header=TRUE)
data <- tail(dados$appl, 110)
windowSize <- 106


# data <- dados$appl

# toString(dados)


# r <- GET("http://localhost:8080/ufcl/webservice/params.php")
# htcontent <- content(r, "text")
#
# params <- GET("http://localhost:8080/ufcl/webservice/params.php")
# htcontent <- content(params, "text")
# params <- strsplit(htcontent, ",")[[1]]

# intsvals, orderi, termosi, method
# data,interv,method,ord,term,mcsReps, windowSize

# toString(params)


gotData <- TRUE
counter <- 0

while (gotData == TRUE)  {
    params <- GET("http://localhost:8080/ufcl/webservice/params.php")
    htcontent <- content(params, "text")
    params <- strsplit(htcontent, ",")[[1]]

    print(params)

    # print(counter, Sys.time())
    print(counter)
    results <- try(runRWindows(data,as.integer(params[2]),params[4],as.integer(params[3]),as.integer(params[2]),30,windowSize))

    if(length(params) < 1) gotData <- FALSE
    counter <- counter + 1

}
