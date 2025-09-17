packages <- c( 'here', 'terra', 
               'tidyverse', 'sf')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# load in DEMs
creek.dem.strm <- rast(here('data', 'raw', 'background_variables', 'tif', 'DEM_creek.tif'))
castle.dem.strm <- rast(here('data', 'raw', 'background_variables', 'tif', 'DEM_castle.tif'))

creek.dem.nasadem <- rast(here('data', 'raw', 'background_variables', 'tif', 'dem_NASADEM_creek.tif'))
creek.dem.glo30 <- rast(here('data', 'raw', 'background_variables', 'tif', 'dem_GLO30_creek.tif'))

# need to clip and mask using study extents
# Read study extents
castle.extent <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_castle_32611.shp'))
creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))

# check CRS
crs(creek.extent, describe = T)$code # 32611
crs(creek.dem.strm, describe = T)$code 
crs(creek.dem.glo30, describe = T)$code # 32611
crs(creek.dem.nasadem, describe = T)$code # 32611

creek.dem.32611 <- project(creek.dem.strm, crs(creek.extent))
castle.dem.32611 <- project(castle.dem.strm, crs(castle.extent))

# crop and mask
creek.dem.32611.crop <- crop(creek.dem.32611, creek.extent)
creek.dem.32611 <- mask(creek.dem.32611.crop, creek.extent)
castle.dem.32611.crop <- crop(castle.dem.32611, castle.extent)
castle.dem.32611 <- mask(castle.dem.32611.crop, castle.extent)

creek.dem.32611.crop <- crop(creek.dem.32611, creek.extent)
creek.dem.32611 <- mask(creek.dem.32611.crop, creek.extent)

out.dir <- here('data', 'processed', 'processed', 'tif')

# Write new DEM files
writeRaster(creek.dem.32611, filename = file.path(out.dir, 'dem_creek_32611.tif'), overwrite = TRUE)
writeRaster(castle.dem.32611, filename = file.path(out.dir, 'dem_castle_32611.tif'), overwrite = TRUE)
