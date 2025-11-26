# Load packages
packages <- c( 'here', 'dplyr', 'stringr', 'terra', 'tibble', 'ggplot2', 'sf')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

tif <- rast(here('data', 'raw', 'ASO', 'tif', 'ASO_SanJoaquin_Mosaic_2021May03_swe_50m.tif'))
res(tif)

#test change
# test again


### Check to see if KNP fire has SWE data from ASO
knp <- st_read(here('data', 'raw', 'fire_info', 'shp', 'knp_fire_perimeter.shp'))
kaweah <- rast(here('data', 'raw', 'ASO', 'tif', 'ASO_Kaweah_2024Apr03_swe_50m.tif'))

crs(knp, describe = T)$code
crs(kaweah, describe = T)$code

knp.32611 <- st_transform(knp, crs(kaweah))

plot(kaweah)
plot(st_geometry(knp.32611), add = TRUE, border = 'red')

creek.study.area <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))
creek.4326 <- st_transform(creek.study.area, 4326)
st_bbox(creek.4326)

### figure out highest elevation the creek fire burned
cbi <- rast(here('data', 'processed', 'processed', 'tif', 'extra', 'creek_cbi_bc_firescaronly_30m.tif'))
dem <- rast(here('data', 'processed', 'processed', 'tif', '30m', 'nasadem_creek_30m.tif'))

res(cbi)
res(dem)

plot(cbi)
plot(dem)

study.extent.shp <- st_read(here('data', 'processed', 'processed','shp', 'study_extent_creek_32611.shp'))
dem.crop <- crop(dem, cbi)
dem.burned <- mask(dem.crop, cbi)

plot(dem.burned)

stack <- c(cbi, dem.burned)
df <- as.data.frame(stack, xy = T, na.rm = T)
head(df)

# find max elevation where cbi > 0
max(df$elevation[df$CBI_bc > 0], na.rm = T)



#### look at land cover classes
lc <- rast(here('data', 'raw', 'background_variables', 'tif', 'creek_NALCMS_2020.tif'))
plot(lc)

# CA boundary
library(tigris)
library(here)
library(sf)

# Download California state boundary
ca_boundary <- states(cb = TRUE, class = "sf")  # specify sf class
ca_boundary <- ca_boundary[ca_boundary$STUSPS == "CA", ]
plot(ca_boundary)

# Save the shapefile
st_write(ca_boundary, here('data', 'raw', 'background_variables', 'shp', 'ca_boundary.shp'))

