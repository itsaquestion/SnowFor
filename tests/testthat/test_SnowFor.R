context("Basic usage")
library(SnowFor)

go_fun = function(x){
  Sys.sleep(1)
  x
}
a = snowFor(1:6, go_fun,cores = 2)

test_that("Basic usage", {
  expect_equal(unlist(a), c(1,2,3,4,5,6))
})

