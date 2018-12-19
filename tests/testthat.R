library(testthat)
library(SnowFor)

test_check("SnowFor")

go_fun = function(x){
  Sys.sleep(1)
  x
}
a = snowFor(1:10,go_fun,cores = 2)




