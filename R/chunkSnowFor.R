#' chunkSnowFor a wrapper for snowFor(), but split by chunk_size first.
#'
#' @param x list to loop
#' @param FUN the function for mapping
#' @param chunk_size size of *every* chunk.
#' @param ... params here will pass to snowFor()
#'
#' @return result of snowFor()
#' @export
#'
#' @examples
chunkSnowFor = function(x,FUN,chunk_size, ...) {
  x_split = split(x, ceiling(seq_along(x) / chunk_size))

  n = length(x_split)
  c = 0
  ret = unlist(lapply(x_split, function(a_chunk) {
    c <<- c + 1
    cat("\n-------------------------------")
    cat("\n* Chunks", c, "/", n, "=", round(c * 100 / n, 2), "%")
    cat("\n-------------------------------\n")

    snowFor(a_chunk,FUN,...)
  }), recursive = FALSE)
  names(ret) = NULL
  ret
}
