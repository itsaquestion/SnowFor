library(testthat)
context("chunkSnowFor_process")

test_that("Basic usage", {
  er = EasyRedis::ErInit()
  a_list = 1:20

  go_fun = function(x) {
    Sys.sleep(1)
    x
  }


  #a = snowFor(a_list, go_fun , cores = 2 ,er=er, do_clean = T)
  a = chunkSnowFor(a_list, go_fun , chunk_size = 10, cores = 2 ,er=er)

  expect_equal(unlist(a),1:20)

  expect_true(!any(startsWith(er$keys(),"SnowFor_")))
})



