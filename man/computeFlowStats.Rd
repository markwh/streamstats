% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flowStats.R
\name{computeFlowStats}
\alias{computeFlowStats}
\title{Compute flow statistics}
\usage{
computeFlowStats(workspaceID, rcode, simplify = FALSE)
}
\arguments{
\item{workspaceID}{Service workspace received from watershed
service result}

\item{rcode}{2-3 character code that identifies the Study Area (either a
State or a Regional Study)}

\item{simplify}{if TRUE, combine results across regression regions using an area-weighted average.
This will also remove the `error` column.}
}
\description{
Returns the computed flow statistic values based on the request configuration.
Currently does *not* return all of the information retrieved by the API call.
References cited include
}
