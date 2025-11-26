packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)


# load in DEM
creek.dem.nasadem <- rast(here('data', 'raw', 'background_variables', 'tif', 'nasadem_creek.tif'))

# need to clip and mask using study extents
# Read study extents
# castle.extent <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_castle_32611.shp'))
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
# mask.elev <- dem > 1524 & dem < 2674 #1524m = 5000ft, 2674 is 98% percentile of burned elevations
mask.elev <- dem > 1524  #1524m = 5000ft

# mask dem
creek.nasadem.elev <- mask(dem, mask.elev, maskvalue = 0)
plot(creek.nasadem.elev)

# write new file
writeRaster(creek.nasadem.elev, filename = file.path(out.dir, '30m', 'nasadem_creek_30m_1524.tif'), overwrite = TRUE)


# also create dem that has been resampled to 500m to crop sdd to 
dem.for.sdd <- rast(here(out.dir, '30m', 'nasadem_creek_30m_1524.tif'))
dem.for.sdd <- dem.for.sdd * 1.0  
plot(dem.for.sdd)

# sdd to resample to
sdd <- rast(here(out.dir, '500m', 'creek_terraclimate_wy2018_500m_1524_2674.tif'))
plot(sdd)

dem.500m <- resample(dem.for.sdd, sdd, method = 'average')
plot(dem.500m)
writeRaster(dem.500m, filename = file.path(out.dir, '500m', 'creek_dem_500m_1524.tif'), overwrite = TRUE)







# explore elevation distributions
study.area <- rast(here('data', 'processed', 'processed', 'tif', 'creek_dem_500m_1524_2674.tif'))
dem <- rast(here('data', 'processed', 'processed', 'tif', '30m', 'nasadem_creek_30m.tif'))


creek.perim <- st_read(here('data', 'raw', 'fire_info', 'shp', 'creek_simple.shp'))

# classify dem as burned or unburned
burned <- mask(crop(dem, creek.perim), creek.perim)
unburned <- mask(dem, creek.perim, inverse = T)

# convert rasters to df
burned.df <- as.data.frame(burned, xy = F, na.rm = T)
unburned.df <- as.data.frame(unburned, xy = F, na.rm = T)

burned.df$area <- 'burned'
unburned.df$area <- 'unburned'
names(unburned.df)[names(unburned.df) == 'layer'] <- 'elevation'

# combine
elev.df <- rbind(burned.df, unburned.df)

ggplot(elev.df, aes(x = elevation, fill = area)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = 'Elevation Distributions',
       x = 'Elevation (m)',
       y = 'Density')

ggplot(elev.df, aes(x = elevation, fill = area)) +
  geom_histogram(alpha = 0.5, position = 'identity', bins = 50) +
  theme_minimal() +
  labs(title = 'Elevation Histograms',
       x = 'Elevation (m)',
       y = 'Count')

# calculate burned elevation percentiles
burned.q <- quantile(burned.df$elevation,
                     probs = c(0.95, 0.98, 0.99),
                     na.rm = T)
burned.q

# density plot showing different percentiles
ggplot(subset(elev.df, area == 'burned'),
       aes(x = elevation)) +
  geom_density(fill = 'firebrick', alpha = 0.4) +
  geom_vline(xintercept = burned.q,
             linetype = 'dashed',
             color = 'black') +
  theme_minimal() +
  labs(title = 'Burned Area Elevation Distribution',
       x = 'Elevation (m)',
       y = 'Density')

# same graph but with unburned density plot also
ggplot(elev.df, aes(x = elevation, fill = area)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = burned.q,
             linetype = 'dashed',
             color = 'black') +
  theme_minimal() +
  labs(title = 'Elevation Distributions: Burned vs Unburned',
       x = 'Elevation (m)',
       y = 'Density')

# use 98% percentile to clip upper elevations (2674m)

# create dem that is 50m resolution
swe <-  rast(here('data', 'processed', 'processed', 'tif', '50m', 'ASO_SanJoaquin_2023_0317_swe_50m_1524.tif')) 
dem.30m <- rast(here('data', 'processed', 'processed', 'tif', '30m', 'nasadem_creek_30m_1524.tif'))
dem.30m <- dem.30m * 1.0  
dem50m <- resample(dem.30m, swe, method = 'average')

writeRaster(dem50m, here('data', 'processed', 'processed', 'tif', '50m', 'nasadem_creek_50m_1524.tif'), overwrite = T)
