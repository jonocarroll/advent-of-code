#' @export
ex <- function(n) {
  readLines(paste0("inst/example", n,".txt"))
}
