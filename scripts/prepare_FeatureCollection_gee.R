## I want to get a code like this

# var aoi = ee.FeatureCollection(
#   [ee.Feature( ee.Geometry.Point([-3.91396681778133, 36.8751111254096]), {label: 'APH'}),
#    ee.Feature( ee.Geometry.Point([-3.92656163312495, 36.8801695201546]), {label: 'ASH1'}),
#    ...
#    ee.Feature( ee.Geometry.Point([-2.85591591149569, 37.377179665491]), {label: 'BAH'})
#    ee.Feature( ee.Geometry.Point([-3.46026340729382, 37.1016959916015]), {label: 'NAD'})]);



library(glue)
library(tidyverse)

g <- terra::vect( "data/geoinfo/geoinfo_dendro_adaptamed_pinus.shp") |>
  as.data.frame(geom="XY") |>
  unite("sp_elev", c(sp_code, elev_code), remove = FALSE) |>
  dplyr::select(site_code, sp_elev, elev, lat = y, long = x) |>
  mutate(x = paste0("ee.Feature( ee.Geometry.Point([",
                    long, " ", lat, "]), {label: '", site_code, "'}),"))

glue::glue_collapse(g$x, sep = "\n")









