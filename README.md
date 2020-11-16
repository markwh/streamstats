## `streamstats`: An R package for using the USGS Streamstats API

See API documentation over [here](https://streamstats.usgs.gov/streamstatsservices/#)

Downloading and using (in R):

```
devtools::install_github("markwh/streamstats")
library(streamstats)
```

### Demo of Basic Usage

Assuming you already have the lat-lon of an outlet, computing the contributing watershed is as easy as:

```
ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326, 
                          includeparameters = "true", includeflowtypes = "true")
```

(if timeout is reached before a result is returned, you can specify it using `setTimeout([number of seconds])`)

Here, `crs` is the coordinate reference system number (ESPSG spatial reference code). 

To see what it returned, the `leafletWatershed` function gives a simple interactive map.

```
leafletWatershed(ws1)
```

This can be written to a shapefile via the following:

```
writeShapefile(watershed = ws1, layer = "layer_name", dir = "ws1_shp", what = "boundary")
```


Other statistics can be found using the `computeChars` (for watershed characteristics such as basin area and land-use) and `computeFlowStats` (for statistics such as flow percentiles)

```
chars1 <- computeChars(workspaceID = ws1$workspaceID, rcode = "MA")
chars1$parameters

stats1 <- computeFlowStats(workspaceID = ws1$workspaceID, rcode = "MA", simplify = TRUE)
```



