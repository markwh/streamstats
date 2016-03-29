# flowstats.R
# flow statistics computed for the study area.

#' Get available flow statistics for a region
#' returns a list of flowstatistics available to be computed in the selected
#' region.
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#'  @export
availFlowStats <- function(rcode) {
  args <- list(rcode = rcode)
  ret1 <- sstat_get("flowstatistics.json", args)
  ret1$flowstatistics <- ret1$flowstatistics %>%
    lapply(as.data.frame) %>%
    dplyr::bind_rows()
  ret1
}



#' Compute flow statistics
#' Returns the computed flow statistic values based on the request configuration.
#' @param rcode 2-3 character code that identifies the Study Area (either a
#'  State or a Regional Study)
#' @param workspaceID	Service workspace received from watershed
#'  service result
#' @param includeflowtypes Comma separated list of flow types
#'   to compute. Default: true, will return all flow types available for region
#' @export
computeFlowStats <- function(rcode, workspaceID, includeparameters = "true") {
  args <- list(rcode = rcode, workspaceID = workspaceID,
               includeparameters = includeparameters)
  ret1 <- sstat_get("flowstatistics.json", args)
  ret1$flowstatistics <- ret1$flowstatistics %>%
    lapply(as.data.frame) %>%
    dplyr::bind_rows()
  ret1
}
