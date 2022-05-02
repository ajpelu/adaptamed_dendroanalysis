# pkgs
library(ncdf4)
library(raster)
library(sf)
library(tidyverse)


# library(devtools)
# install_github("ncwebmapper/ncwebmapper")

# Read the nc file
nc <- nc_open("/Users/ajpelu/Downloads/NDVI.nc")
print(nc)
# Get the crs
nc_crs <- ncatt_get(nc,0)
nc_close(nc)

# Read nc as raster
raw <- raster::brick("/Users/ajpelu/Downloads/NDVI.nc")
crs(raw) <- crs(paste0("+init=",nc_crs$crs)) # project

# Transpose and flip
t <- setNames(t(raw), names(raw))
ft <- setNames(flip(t, direction = 'x'), names(t))
r <- setNames(flip(ft, direction = 'y'), names(ft))

# extract
# Read the shapefile
centroids_raw <- st_read("/Volumes/GoogleDrive/My Drive/PROYECTOS/adaptamed_dendro/data/geoinfo_dendro_simplified.shp")

dicc_site <- centroids_raw %>%
  st_drop_geometry() %>%
  dplyr::select(site_code, Specie)

elev_cat <- read_csv(here::here("data/categories_elevation_site_code.csv"))

# Transform the points, select only site code
centroids <- st_transform(centroids_raw, nc_crs$crs) %>%
  dplyr::select(site_code)

# Extract
ext <- raster::extract(r, centroids, df=TRUE, sp=TRUE) %>%
  data.frame()

# Format dates
ndvi <- ext %>%
  dplyr::select(site_code, starts_with("X")) %>%
  pivot_longer(-site_code, names_to = "rawdate") %>%
  mutate(date = as.Date(
           gsub("\\.", "-", gsub("X","", rawdate)),
           format = "%Y-%m-%d")) %>%
  dplyr::select(site_code, date, ndvi=value) %>%
  inner_join(dicc_site) %>%
  inner_join(elev_cat)

write_csv(ndvi, here::here("data/dendroadaptamed_ndvicsic1km.csv"))
