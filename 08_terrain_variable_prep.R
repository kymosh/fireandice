# Load packages
packages <- c('here', 'terra', 'purrr', 'tools')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Study extents
castle.extent <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_castle_32611.shp'))
creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))

# Define input/output
out.dir <- here('data', 'processed', 'processed', 'tif')

dem.creek.32611 <- rast(here('data', 'processed', 'processed', 'tif', 'dem_creek_32611.tif'))
dem.castle.32611 <- rast(here('data', 'processed', 'processed', 'tif', 'dem_castle_32611.tif'))

# Your named list of rasters
rasters <- list(
  aspect.creek     = rast(here('data', 'processed', 'processed', 'tif', 'aspect_creek.tif')),
  aspect.castle    = rast(here('data', 'processed', 'processed', 'tif', 'aspect_castle.tif')),
  tpi.130.creek    = rast(here('data', 'processed', 'processed', 'tif', 'tpi130_creek.tif')),
  tpi.510.creek    = rast(here('data', 'processed', 'processed', 'tif', 'tpi510_creek.tif')),
  tpi.2010.creek   = rast(here('data', 'processed', 'processed', 'tif', 'tpi2010_creek.tif')),
  tpi.130.castle   = rast(here('data', 'processed', 'processed', 'tif', 'tpi130_castle.tif')),
  tpi.510.castle   = rast(here('data', 'processed', 'processed', 'tif', 'tpi510_castle.tif')),
  tpi.2010.castle  = rast(here('data', 'processed', 'processed', 'tif', 'tpi2010_castle.tif')),
  slope.creek      = rast(here('data', 'processed', 'processed', 'tif', 'slope_creek.tif')),
  slope.castle     = rast(here('data', 'processed', 'processed', 'tif', 'slope_castle.tif'))
)

# Target CRS from creek.extent
target.crs <- crs(dem.creek.32611)

# Helper function to generate output filename
make_outname <- function(name) {
  # replace dots with underscores, add _32611.tif
  paste0(gsub('\\.', '_', name), '_32611.tif')
}

# Loop to reproject if needed, assign names, and save
# Helper to extract clean layer name
clean_layer_name <- function(name) {
  # Strip .creek, .castle etc.
  name %>%
    str_replace('\\.creek$', '') %>%
    str_replace('\\.castle$', '')
}

# Revised loop
iwalk(rasters, function(r, name) {
  current_epsg <- crs(r, describe = TRUE)$code
  
  if (current_epsg != '32611') {
    message('Reprojecting ', name)
    r <- project(r, target.crs)
  } else {
    message('Skipping reprojection for ', name)
  }
  
  # Assign name inside the raster
  layer.name <- clean_layer_name(name)
  names(r) <- layer.name
  
  # Output path
  out_file <- file.path(out.dir, make_outname(name))
  writeRaster(r, out_file, overwrite = TRUE)
  message('Saved ', out_file, ' with layer name ', layer.name)
})



######### Check to make sure all saved properly
# List all reprojected files
reproj.files <- list.files(
  here('data', 'processed', 'processed', 'tif'),
  pattern = '_32611\\.tif$',
  full.names = TRUE
)

# Loop through and check layer names
iwalk(reproj.files, function(path, i) {
  r <- rast(path)
  cat(basename(path), '→', names(r), '\n')
})
