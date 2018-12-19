#' snowFor multithreading map-like function based on snow and foreach
#'
#' Errors won't break the loop.
#' Instead the loop will return an ThreadError object and keep going.
#'
#' @param x list to loop
#' @param FUN the function for mapping
#' @param pre_fun prepare function to init all nodes
#' @param varlist name string vector of objects to be exported to nodes: "a_variable"
#' @param cores number of threads
#' @param env env to store the cl object. defalut: globalenv()
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
                   varlist = NULL,
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

  if (!is.null(pre_fun) | !is.null(varlist)) {
    cat("Perparing nodes ... ")
    if(!is.null(pre_fun)){
      clusterCall(env$.snowfor_cl, pre_fun)
    }
    if(!is.null(varlist)){
      clusterExport(env$.snowfor_cl, varlist)
    }
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
