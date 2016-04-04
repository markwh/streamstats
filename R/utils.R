
#' @export
setTimeout <- function(seconds) {
  stopifnot(is.numeric(seconds))
  message(sprintf("Timeout set to %s seconds", seconds))
  options("timeout" = seconds)
}


fs_toDf <- function(fselem) {
  fselem %>%
    lapply(as.data.frame, stringsAsFactors = FALSE) %>%
    bind_rows()
}
