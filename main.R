# main.R

library(devtools)
library(testthat)


# testing
use_travis()
check()
test()

# documenting, building
document()
load_all()

# datasets
use_data(westfield)
use_data(pommoqusset)

# Using packages

use_package("magrittr")
use_package("dplyr")
use_package("rgdal")
use_package("leaflet")
use_package("jsonlite")
use_package("httr")
use_package("maps")
use_package("assertthat")
use_package("maptools")
