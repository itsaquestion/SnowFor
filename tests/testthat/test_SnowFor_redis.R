context("SnowFor_Redis")


test_that("redis_base", {
  er = ErInit()

  go_fun = function(x){
    Sys.sleep(0.5)
    print(x)
    x
  }
  a = snowFor(1:10, go_fun,cores = 2,er=er)

  expect_equal(unlist(a), 1:10)

})


