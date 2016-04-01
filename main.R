# main.R

library(devtools)
library(testthat)


# testing
test()

# documenting, building
document()
load_all()

# datasets
use_data(westfield)
use_data(pommoqusset)

# Using packages

use_package("magrittr")
use_package("leaflet", type = "Suggests")
use_package("jsonlite")
use_package("httr")
use_package("maps")
use_package("assertthat")
use_package("maptools")
