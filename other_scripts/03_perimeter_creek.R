
packages <- c(
  'here', 'exactextractr', 'raster', 'sf', 'terra', 'geodata',
  'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr'
)
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Read in creek fire shapefile
creek.fire.0 <- st_read(here('data', 'raw', 'fire_info', 'shp', 'creek_fire_perimeter.shp'))

# Filter for desired fire perimeter by date
creek.fire.perimeter <- creek.fire.0[creek.fire.0$ALARM_DATE == as.Date('2020-09-04'), ]
creek.fire.perimeter <- creek.fire.perimeter %>% select(-Shape_Area)

# Plot result
plot(
  st_geometry(creek.fire.perimeter),
  col = adjustcolor('green', alpha.f = 0.5),
  main = 'Creek Study Extent'
)
# Write cleaned shapefile
st_write(
  creek.fire.perimeter,
  here('data', 'raw', 'fire_info', 'shp', 'creek_fire_perimeter.shp'),
  delete_layer = TRUE
)


