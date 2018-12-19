#' ThreadError 多线程出错所返回的对象
#'
#' @param e error
#' @param i 出错时snowFor的第一个参数
#'
#' @return
#' @export
#' @RCurl
#'
#' @examples
ThreadError = function(e,i) {
  ret = list(pid = Sys.getpid(), error = e,i = i)
  class(ret) = "ThreadError"
  ret
}

#' @export
print.ThreadError = function(x, ...) {
  cat("Thread Error: Pid:", x$pid,"\n\ti =",x$i,"; Error Message:", x$error$message)
}

#' getErrors 从snwoFor的结果中选出ThreadError的结果
#'
#' @param x snwoFor的返回
#'
#' @return
#' @export
#'
#' @examples
getErrors = function(x) {
  is_error = (sapply(x, class) == "ThreadError")
  x[is_error]
}
