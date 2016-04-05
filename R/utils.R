
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

#' @export
extractLaLo <- function(watershed) {
  outdf <- watershed$featurecollection[[1]]$feature$features %>%
    lapply(function(x) setNames(x$geometry$coordinates, c("lon", "lat"))) %>%
    bind_rows()

  if (nrow(outdf) > 1) {
    ids <- watershed$featurecollection[[1]]$feature$features %>%
      vapply(function(x) x$properties$ID, FUN.VALUE = character(1))
    outdf$ID = ids
  }

  outdf
}
