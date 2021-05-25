rm(list=ls())
library(httr)

set.seed(1)

PATH <- "/home/yurisa2/lampstack-7.3.7-1/apache2/htdocs/UFCL/"


setwd(PATH)
source("HEwB/include.r")

dados <- read.csv("data/contral.csv", header=TRUE)
data <- tail(dados$appl, 110)
windowSize <- 106

baseUrl <- "http://localhost:8080/UFCL/webservice"

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
    params <- GET(paste0(baseUrl,"/params.php"))
    htcontent <- content(params, "text")
    params <- strsplit(htcontent, ",")[[1]]

    # print(params)

    # print(counter, Sys.time())
    # print(counter)
    results <- try(runRWindows(data,as.integer(params[2]),params[4],as.integer(params[3]),as.integer(params[2]),30,windowSize))

    mseValue <- try(getRWMSE(dados$appl, results, windowSize))

    resut <- toString(results)
    body <- list(mse = mseValue, result = resut)
    r <- POST(paste0(baseUrl, "/results.php?id=",as.integer(params[1])), body = body, encode = "multipart")

    if(length(params) < 1) gotData <- FALSE
    counter <- counter + 1
}
