# Load packages
packages <- c( 'here', 'exactextractr', 'raster', 'sf', 'terra', 'geodata',
               'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr', 'stringr', 'purrr', 'tools')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# study extents
castle.extent <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_castle_32611.shp'))
creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))

# check to make sure all look right and are clipped/cropped
# plot(aspect.creek)
# plot(hli.creek)
# plot(tpi.130.creek)
# plot(tpi.510.creek)
# plot(tpi.2010.creek)
# plot(dem.creek)

rasters <- list(
  aspect.creek     = rast(here('data', 'processed', 'processed', 'tif', 'aspect_creek.tif')),
  aspect.castle    = rast(here('data', 'processed', 'processed', 'tif', 'aspect_castle.tif')),
  hli.creek        = rast(here('data', 'processed', 'processed', 'tif', 'hli_creek.tif')),
  hli.castle       = rast(here('data', 'processed', 'processed', 'tif', 'hli_castle.tif')),
  tpi.130.creek    = rast(here('data', 'processed', 'processed', 'tif', 'tpi130_creek.tif')),
  tpi.510.creek    = rast(here('data', 'processed', 'processed', 'tif', 'tpi510_creek.tif')),
  tpi.2010.creek   = rast(here('data', 'processed', 'processed', 'tif', 'tpi2010_creek.tif')),
  tpi.130.castle   = rast(here('data', 'processed', 'processed', 'tif', 'tpi130_castle.tif')),
  tpi.510.castle   = rast(here('data', 'processed', 'processed', 'tif', 'tpi510_castle.tif')),
  tpi.2010.castle  = rast(here('data', 'processed', 'processed', 'tif', 'tpi2010_castle.tif')),
  dem.creek        = rast(here('data', 'processed', 'processed', 'tif', 'dem_creek_32611.tif')),
  dem.castle       = rast(here('data', 'processed', 'processed', 'tif', 'dem_castle_32611.tif')),
  slope.creek     = rast(here('data', 'processed', 'processed', 'tif', 'slope_creek.tif')),
  slope.castle    = rast(here('data', 'processed', 'processed', 'tif', 'slope_castle.tif'))
)

epsg.codes <- map(rasters, ~ crs(.x, describe = TRUE)$code)

out.dir <- here('data', 'processed', 'processed', 'tif')
target.crs <- crs(rasters$dem.creek)

iwalk(rasters, function(r, name) {
  epsg <- crs(r, describe = TRUE)$code
  if (epsg == '4269') {
    message('Reprojecting and saving: ', name)
    
    # Reproject
    r.reproj <- project(r, target.crs)
    
    # Build output filename
    out.name <- paste0(gsub('\\.', '_', name), '_32611.tif')
    out.path <- file.path(out.dir, out.name)
    
    # Write to file
    writeRaster(r.reproj, out.path, overwrite = TRUE)
  } else {
    message('Skipping (already 32611): ', name)
  }
})

# add slope
slope_rasters <- list(
  slope.creek     = rast(here('data', 'processed', 'processed', 'tif', 'slope_creek.tif')),
  slope.castle    = rast(here('data', 'processed', 'processed', 'tif', 'slope_castle.tif')))

iwalk(slope_rasters, function(r, name) {
  epsg <- crs(r, describe = TRUE)$code
  if (epsg == '4269') {
    message('Reprojecting and saving: ', name)
    
    # Reproject
    r.reproj <- project(r, target.crs)
    
    # Build output filename
    out.name <- paste0(gsub('\\.', '_', name), '_32611.tif')
    out.path <- file.path(out.dir, out.name)
    
    # Write to file
    writeRaster(r.reproj, out.path, overwrite = TRUE)
  } else {
    message('Skipping (already 32611): ', name)
  }
})
