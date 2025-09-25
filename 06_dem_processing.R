packages <- c( 'here', 'terra', 
               'tidyverse', 'sf')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)


# load in DEM
creek.dem.nasadem <- rast(here('data', 'raw', 'background_variables', 'tif', 'dem_NASADEM_creek.tif'))

# need to clip and mask using study extents
# Read study extents
castle.extent <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_castle_32611.shp'))
creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))

# check CRS
crs(creek.extent, describe = T)$code # 32611
crs(creek.dem.nasadem, describe = T)$code # 32611


# crop and mask
creek.dem.nasadem.crop <- crop(creek.dem.nasadem, creek.extent)
creek.nasadem <- mask(creek.dem.nasadem.crop, creek.extent)

out.dir <- here('data', 'processed', 'processed', 'tif')

# Write new DEM file
writeRaster(creek.nasadem, filename = file.path(out.dir, 'nasadem_creek.tif'), overwrite = TRUE)






# create DEM that is only >5000ft for (most) of analysis
dem <- rast(here('data', 'raw', 'background_variables', 'tif', 'nasadem_creek.tif'))

# create elevation mask 
mask.5000ft <- dem > 1524 #1524m = 5000ft

# mask dem
creek.nasadem.5000 <- mask(dem, mask.5000ft, maskvalue = 0)

# write new file
writeRaster(creek.nasadem.5000, filename = file.path(out.dir, 'nasadem_creek_5000.tif'), overwrite = TRUE)
