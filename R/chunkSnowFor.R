#' chunkSnowFor a wrapper for snowFor(), but split by chunk_size first.
#'
#' @param x list to loop
#' @param FUN the function for mapping
#' @param chunk_size size of *every* chunk.
#' @param deley_sec sleep for some time between chunks (in seconds)
#' @param ... params here will pass to snowFor()
#'
#' @return result of snowFor()
#' @export
#'
#' @examples
chunkSnowFor = function(x, FUN, chunk_size, deley_sec = 0, ...) {

  if (is.data.frame(x)) {
    x = split(x, seq(nrow(x)))
  }


  er = EasyRedis::ErInit()

  cleanSnow(er)

  er$set("SnowFor_total",length(x))
  er$set("SnowFor_acc",0)

  x_split = split(x, ceiling(seq_along(x) / chunk_size))

  n = length(x_split)
  c = 0
  ret = unlist(lapply(x_split, function(a_chunk) {
    c <<- c + 1

    if(c>1 & deley_sec>0){
      cat("\nSleep for",deley_sec,"seconds.\n")
      Sys.sleep(deley_sec)
    }

    tt = strftime(Sys.time(),"%Y-%m-%d %H:%M:%S")

    cat("\n-------------------------------")
    cat("\n*",tt)
    cat("\n* Chunks", c, "/", n, "=", round(c * 100 / n, 2), "%")
    cat("\n-------------------------------\n")
    ret = snowFor(a_chunk,FUN,do_clean = F, ...)

    acc = er$get("SnowFor_acc")
    er$set("SnowFor_chunk",list(time = Sys.time(),n=0,p=0))
    er$set("SnowFor_acc",acc + length(ret))

    ret
  }), recursive = FALSE)
  names(ret) = NULL

  cleanSnow(er)
  ret
}
