packages <- c( 'here', 'terra', 
               'tidyverse')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# load in DEMs
creek.dem <- rast(here('data', 'raw', 'background_variables', 'tif', 'DEM_creek.tif'))
castle.dem <- rast(here('data', 'raw', 'background_variables', 'tif', 'DEM_castle.tif'))

# need to clip and mask using study extents
# Read study extents
castle.extent <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_castle_32611.shp'))
creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))

# reproject dem to match study extent
crs(creek.extent, describe = T)$code 
crs(creek.dem, describe = T)$code

creek.dem.32611 <- project(creek.dem, crs(creek.extent))
castle.dem.32611 <- project(castle.dem, crs(castle.extent))

# crop and mask
creek.dem.32611.crop <- crop(creek.dem.32611, creek.extent)
creek.dem.32611 <- mask(creek.dem.32611.crop, creek.extent)
castle.dem.32611.crop <- crop(castle.dem.32611, castle.extent)
castle.dem.32611 <- mask(castle.dem.32611.crop, castle.extent)

out.dir <- here('data', 'processed', 'processed', 'tif')

# Write new DEM files
writeRaster(creek.dem.32611, filename = file.path(out.dir, 'dem_creek_32611.tif'), overwrite = TRUE)
writeRaster(castle.dem.32611, filename = file.path(out.dir, 'dem_castle_32611.tif'), overwrite = TRUE)
