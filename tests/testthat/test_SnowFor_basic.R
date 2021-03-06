context("SnowFor_Base")

go_fun = function(x){
  Sys.sleep(1)
  print(x)
  x
}
a = snowFor(1:6, go_fun,cores = 2)

env = globalenv()
env$some_value = 123

test_that("Basic usage", {
  expect_equal(unlist(a), c(1,2,3,4,5,6))
})



some_value = 123
go_fun = function(x){
  some_value * x
}
a = snowFor(1:6, function(x){
  #if(x == 3){stop("adsf")}
  some_value * x
},varlist = c("some_value"),cores = 2)

test_that("export", {
  expect_equal(unlist(a), (1:6) * 123)
})



test_that("errors", {
  some_value = 123
  expect_warning({
    a = snowFor(1:6, function(x){
      if(x == 3){stop("adsf")}
      some_value * x
    },varlist = c("some_value"),cores = 2)
  })

  expect_equal(class(a[[3]]), "ThreadError")
})



