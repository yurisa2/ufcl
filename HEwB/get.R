library('httr')

r <- GET("http://localhost:8080/ufcl/webservice/params.php")
htcontent <- content(r, "text")

params <- strsplit(htcontent, ",")[[1]]


toString(params)

params[2]
