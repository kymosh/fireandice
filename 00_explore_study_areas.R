packages <- c('tidyverse', 'sf', 'terra')
lapply(packages, library, character.only = TRUE)

fires <- st_read('data/raw/fire_info/shp/possible_fires.shp')
fires.poly <- st_cast(fires, 'POLYGON')

fires.single <- fires.poly %>%
  group_by(Incid_Name) %>%
  summarise()

dem1 <- rast('data/raw/background_variables/tif/nasadem_ca_1.tif')
dem2 <- rast('data/raw/background_variables/tif/nasadem_ca_2.tif')
dem <- mosaic(dem1, dem2)
writeRaster(dem, 'data/raw/background_variables/tif/nasadem_ca_10m.tif', overwrite = TRUE)

# transform fires to match the DEM CRS first
crs(dem, describe = T)$code
fires.4269 <- st_transform(fires.single, crs(dem))

# turn into vector
fires.v.4269 <- vect(fires.4269)

# empty list to store fires in 
fire.dems <- vector('list', nrow(fires.v.4269))
fire.names <- make.unique(gsub(' ', '_', as.character(fires.4269$Incid_Name)))

# loop through fires, crop and mask the DEM, and reproject to match the fire CRS
for (i in 1:nrow(fires.v.4269)) {
  
  fire.poly <- fires.v.4269[i, ]
  
  dem.crop <- crop(dem, fire.poly)
  dem.fire <- mask(dem.crop, fire.poly)
  
  dem.fire.32611 <- project(dem.fire, 'EPSG:32611')
  
  fire.dems[[i]] <- dem.fire.32611
}
names(fire.dems) <- fire.names
plot(fire.dems[[1]])
plot(fire.dems[[2]])
plot(fire.dems[[6]])


elev.summary <- data.frame(
  fire = names(fire.dems),
  n.cells = sapply(fire.dems, function(x) {
    global(!is.na(x), 'sum', na.rm = T)[1, 1]
  }), 
  n.above.2000 = sapply(fire.dems, function(x) {
    global(x > 1999, 'sum', na.rm = T)[1, 1]
  })
)

elev.summary <- elev.summary %>%
  mutate(
    cell.area.m2 = sapply(fire.dems, function(x) prod(res(x))),
    area.above.2000.ha = n.above.2000 * cell.area.m2 / 10000,
    total.area.ha = n.cells * cell.area.m2 / 10000)

ggplot(elev.summary, aes(x = reorder(fire, area.above.2000.ha), y = area.above.2000.ha)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = 'Area (ha) of fire area above 2000 m'
  ) +
  theme_bw()


