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

# shapefile to be assed as asset in GEE  
st_write(creek.fire.32611, here('data', 'raw', 'fire_info', 'shp', 'creek_simple.shp'))



# read in resulting cbi from GEE (using bias-corrected version)
cbi.bc <- rast(here('data', 'raw', 'fire_info', 'tif', 'creek_cbibc.tif'))



# crop and mask
cbibc.creek.cropped <- crop(cbi.bc, creek.fire.32611)
cbibc.final <- mask(cbibc.creek.cropped, creek.fire.32611)

out.dir <- here('data', 'processed', 'processed', 'tif')

writeRaster(cbibc.final, filename = file.path(out.dir, 'creek_cbi_bc.tif'), overwrite = TRUE)




