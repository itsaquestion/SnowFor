library(SnowFor)
library(testthat)
context("chunkSnowFor")

test_that("Basic usage", {
  a_list = 1:16

  go_fun = function(x) {
    Sys.sleep(1)
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
