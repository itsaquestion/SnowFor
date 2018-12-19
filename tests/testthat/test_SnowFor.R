context("Basic usage")
library(SnowFor)

go_fun = function(x){
  Sys.sleep(1)
  print(x)
  x
}
a = snowFor(1:6, go_fun,cores = 2)

test_that("Basic usage", {
  expect_equal(unlist(a), c(1,2,3,4,5,6))
})



test_that("export", {
  some_value = 123
  go_fun = function(x){
    some_value * x
  }
  a = SnowFor::snowFor(1:6, function(x){
    some_value * x
  },varlist = c("some_value"),cores = 2)
  expect_equal(unlist(a), (1:6) * 123)
})



