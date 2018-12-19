library(SnowFor)
library(testthat)
context("chunkSnowFor")

test_that("Basic usage", {
  a_list = 1:16

  go_fun = function(x) {
    Sys.sleep(0.5)
    c(x,x)
  }

  a = SnowFor::chunkSnowFor(a_list, go_fun , chunk_size = 4, cores = 2 )

  b = SnowFor::snowFor(a_list, go_fun, cores = 2)

  cat("a\n")
  print(unlist(a))
  cat("b\n")
  print(unlist(b))
  expect_true(all.equal(a,b))

})



test_that("Sleep", {
  a_list = 1:4

  go_fun = function(x) {
    c(x,x)
  }

  tt = system.time({
    a = SnowFor::chunkSnowFor(a_list, go_fun , chunk_size = 2, deley_sec = 5, cores = 2 )
  })

  expect_true(tt['elapsed'] > 5)

})


