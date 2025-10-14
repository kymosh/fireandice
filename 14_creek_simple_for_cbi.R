packages <- c(
  'here', 'exactextractr', 'raster', 'sf', 'terra', 'geodata',
  'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr', 'lubridate'
)
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# create shapefile with appropriate attribute names for Parks et al. (2019) GEE script for CBI
creek.fire.0 <- st_read(here('data', 'raw', 'fire_info', 'shp', 'creek_fire_perimeter.shp'))

creek.fire <- creek.fire.0 %>%
  mutate(
    ALARM_DATE = as.Date(ALARM_DATE),
    CONT_DATE = as.Date(CONT_DATE),
    Fire_ID = FIRE_NAME,
    Fire_Year = YEAR_,
    Start_Day = yday(ALARM_DATE),
    End_Day = yday(CONT_DATE)) %>%
  select(Fire_ID, Fire_Year, Start_Day, End_Day, geometry)

crs(creek.fire, describe = T)$code
creek.fire.32611 <- st_transform(creek.fire, 32611)

# shapefile to be used as asset in GEE  
st_write(creek.fire.32611, here('data', 'raw', 'fire_info', 'shp', 'creek_simple.shp'))



# read in resulting cbi from GEE (using bias-corrected version)
cbi.bc <- rast(here('data', 'raw', 'fire_info', 'tif', 'creek_cbibc.tif'))



# crop and mask
cbibc.creek.cropped <- crop(cbi.bc, creek.fire.32611)
cbibc.final <- mask(cbibc.creek.cropped, creek.fire.32611)

# write file
out.dir <- here('data', 'processed', 'processed', 'tif', '30m')
writeRaster(cbibc.final, filename = file.path(out.dir, 'creek_cbi_bc_firescaronly.tif'), overwrite = TRUE)




############ extend CBI to whole study area (make unburned areas = 0)
cbibc.final <- rast(here('data', 'processed', 'processed', 'tif', 'extra', 'creek_cbi_bc_firescaronly_30m.tif'))

# read in study extent
study.extent.shp <- st_read(here('data', 'processed', 'processed','shp', 'study_extent_creek_32611.shp'))

# convert to raster
study.extent <- raster(extent(study.extent.shp), 
                       res = res(cbibc.final),
                       crs = crs(cbibc.final))

# expand cbi to raster extent
cbibc.full <- extend(cbibc.final, study.extent)

# fill in NAs with 0 (unburned)
cbibc.full[is.na(cbibc.full)] <- 0
plot(cbibc.full)

# now recrop to study area and mask
cbibc.crop <- crop(cbibc.full, study.extent.shp)
cbibc.study.area <- mask(cbibc.crop, study.extent.shp)

# mask to elevation
# read in dem to mask cbi by elevation
dem.elev <- rast(here('data', 'processed', 'processed', 'tif', '30m', 'nasadem_creek_30m_1524.tif'))

cbibc.elev <- mask(cbibc.study.area, dem.elev)
par(mfrow = c(1, 2))
plot(cbibc.elev)
plot(dem.elev)
writeRaster(cbibc.elev, filename = file.path(out.dir, 'creek_cbibc_30m_1524.tif'), overwrite = TRUE)
par(mfrow = c(1, 1))
