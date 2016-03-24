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

# Using packages

use_package("magrittr")
use_package("leaflet", type = "Suggests")
use_package("jsonlite")
