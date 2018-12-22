context("SnowFor_Redis")


test_that("redis_base", {
  er = ErInit()
  the_key = "tmp_key"
  if(the_key %in% er$keys()){
    er$del(the_key,F)
  }

  go_fun = function(x){
    Sys.sleep(0.5)
    print(x)
    x
  }
  a = snowFor(1:100, go_fun,cores = 2,er=er,er_key = the_key)

  expect_equal(unlist(a), 1:100)
  expect_true(the_key %in% er$keys())
  #er$del(the_key,F)
})


