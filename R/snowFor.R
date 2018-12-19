#' snowFor 利用snow和foreach的多线程map函数
#'
#' 用法等同于lapply。但出错不会中断，会返回一个ThreadError对象。
#'
#' @param x 一个list，传给foreach的第一个参数
#' @param FUN 要运行的函数
#' @param pre_fun 预备函数，用于初始化每个节点
#' @param cores 现成数量，默认为全部逻辑核心数
#' @param env 存放cl对象的环境，默认为globalenv()
#' @return
#' @export
#' @import snow
#' @import tcltk
#' @import doSNOW
#' @import foreach
#'
#' @examples
#' go_fun = function(x){
#'   Sys.sleep(1)
#'   x
#'  }
#' a = snowFor(1:10,go_fun,cores = 2)
#'
#'
snowFor = function(x,
                   FUN,
                   pre_fun = NULL,
                   cores = parallel::detectCores(),
                   env = globalenv()) {


  tryCatch(
    stopCluster(env$.snowfor_cl),
    error = function(e) e
  )

  cores = min(cores,length(x))

  cat("\nCores =",cores,"\n")
  cat("Making clusters ... ")
  assign(".snowfor_cl", makeSOCKcluster(cores, outfile = ""), envir = env)
  registerDoSNOW(env$.snowfor_cl)

  cat("done.\n")

  if (!is.null(pre_fun)) {
    cat("Perparing nodes ... ")
    clusterCall(env$.snowfor_cl, pre_fun)
    cat("done.\n")
  }

  pb <- txtProgressBarETA(max = length(x))
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)


  ret = vector(mode = "list", length = length(x))

  tt = system.time({
    ret <- foreach(i = x, .options.snow = opts) %dopar% {
      tryCatch(
        FUN(i),
        error = function(e) {
          ThreadError(e,i)
        }
      )
    }
  })
  cat("\n")
  print(tt)

  stopCluster(env$.snowfor_cl)

  if (any(lapply(ret, class) == "ThreadError")) { warning("ThreadErrors! Check return.") }

  ret
}
