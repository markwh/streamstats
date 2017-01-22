# basinChars.R
# Basin characteristics functions

#' Get Availale Basin Characteristics
#'
#' The basin characteristics resource represent a list of characteristics that
#' defines the study area, such as Drainage Area and mean annual precipitation.
#'
#' @param rcode StreamStats 2-3 character code that identifies the Study Area
#' @param group (optional) Key word parameter group filter.
#' @importFrom magrittr "%>%"
#' @export
availChars <- function(rcode, group = NULL) {
  args <- list(rcode = rcode, group = group)
  ret1 <- sstat_get("parameters.json", args)
  ret1$parameters <- ret1$parameters %>%
    lapply(as.data.frame, stringsAsFactors = FALSE) %>%
    dplyr::bind_rows()
  ret1
}

#' Compute basin characteristics
#'
#' Returns the computed basin characteristics based on the request configuration.
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#' @param workspaceID	Service workspace received from watershed
#'  service result
#' @param includeparameters Comma separated list of region
#'  parameters to compute. Default: true, will return all parameters for region
#' @export
#'
computeChars <- function(workspaceID, rcode, includeparameters = "true") {
  args <- list(rcode = rcode, workspaceID = workspaceID,
               includeparameters = includeparameters)
  stopifnot(is(workspaceID, "character") && length(workspaceID) == 1)
  ret1 <- sstat_get("parameters.json", args)
  ret1$parameters <- ret1$parameters %>%
    lapply(as.data.frame, stringsAsFactors = FALSE) %>%
    dplyr::bind_rows()
  ret1
}
