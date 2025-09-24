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
