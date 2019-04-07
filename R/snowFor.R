#' snowFor multithreading map-like function based on snow and foreach
#'
#' Errors won't break the loop.
#' Instead the loop will return an ThreadError object and keep going.
#'
#' @param x list to loop. Accept vector, list and data.frame(by row)
#' @param FUN the function for mapping
#' @param pre_fun prepare function to init all nodes
#' @param varlist name string vector of objects to be exported to nodes: "a_variable"
#' @param cores number of threads
#' @param env env to store the cl object. defalut: globalenv()
#' @param er an EasyRedis object. (eg: er = EasyRedis::ErInit())
#' @param do_clean if er != NULL and do_clean = T, then clean SnowFor information in Redis.
#' @return
#' @export
#' @import snow
#' @import tcltk
#' @import doSNOW
#' @import foreach
#' @import EasyRedis
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
                   env = globalenv(),
                   er = NULL,
                   do_clean = T) {

  
  if (is.data.frame(x)) {
    x = split(x, seq(nrow(x)))
  }

  tryCatch(
    stopCluster(env$.snowfor_cl),
    error = function(e) e
  )


  if(!is.null(er) & do_clean){
    cleanSnow(er)
  }

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

  makeProgress = function(total_size){
    progress <- function(n) {
      if(!is.null(er)){
        updateChunkInfo(er,list(time = Sys.time(),n=n,p=n/total_size))
      }
      setTxtProgressBar(pb, n)
    }
    progress
  }

  opts <- list(progress = makeProgress(length(x)))


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

  if(!is.null(er)){
    delChunkInfo(er)
  }

  cat("\n")
  print(tt)

  stopCluster(env$.snowfor_cl)

  if (any(lapply(ret, class) == "ThreadError")) { warning("ThreadErrors! Check return.") }

  ret
}


updateChunkInfo = function(er,info,er_key = "SnowFor_chunk"){
  er$set(er_key,info)
}

delChunkInfo = function(er,er_key = "SnowFor_chunk"){
  er$del("SnowFor_chunk",ask = F)
}

cleanSnow = function(er){
  all_keys = er$keys()
  all_keys = all_keys[startsWith(all_keys,"SnowFor_")]
  if(length(all_keys)>0){
    er$del(all_keys,F)
  }
}



