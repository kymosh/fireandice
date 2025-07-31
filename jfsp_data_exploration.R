# Load packages
packages <- c( 'here', 'terra', 'sf', 'ggplot2', 'ggspatial', 'rnaturalearth', 'rnaturalearthdata', 'devtools')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Load the GeoPackage that contains lidar data
wesm <- st_read(here('data', 'raw', 'other', 'WESM.gpkg'))

# Filter to only rows where ql == 'QL 1'
wesm.ql1 <- wesm[wesm$ql == 'QL 1', ]


# bring in all files of aso data from all basins
tif.dir <- 'G:/Fire_Snow_Dynamics/data/raw/ASO/tif/other_basins'

tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = T)

aso.extents <- lapply(tif.files, function(f) {
  r <- rast(f)
  as.polygons(ext(r), crs = crs(r))  # convert extent to polygon
})
aso.extents <- do.call(rbind, aso.extents)
aso.extents.sf <- st_as_sf(aso.extents)

# use US base map
devtools::install_github('ropensci/rnaturalearthhires')
us <- ne_states(country = 'United States of America', returnclass = 'sf')

