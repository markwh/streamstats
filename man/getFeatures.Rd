% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features.R
\name{getFeatures}
\alias{getFeatures}
\title{Get features for a given workspace}
\usage{
getFeatures(
  workspaceID,
  rcode,
  crs,
  features = c("globalwatershedpoint", "globalwatershed"),
  simplify = "true"
)
}
\arguments{
\item{workspaceID}{Service workspace received from watershed
service result}

\item{crs}{string ESPSG spatial reference code. The function rgdal::showEPSG
might come in handy.}

\item{simplify}{boolean    Whether to simplify returned result, defaut: true}

\item{includefeatures}{string    true    Comma separated list of features to
include in response. See Feature resource for more information.
Default: true, returns delineated basin and pourpoint}
}
\description{
returns a returns a collection of spatial feature names available for the
workspace.
}
