packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'spatialEco')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# just checking to see status of SWE data
swe <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_SanJoaquin_2020_0414_SUPERswe_50p0m_agg_clipped.tif'))

dem <- rast(here('data', 'processed', 'processed', 'tif', 'nasadem_creek_5000.tif'))

# 3 methods of resampling from 50m to 30m
swe.30.bl <- resample(swe, dem, method = 'bilinear')
swe.30.nn <- resample(swe, dem, method = 'near')
swe.30.rms <- resample(swe, dem, method = 'rms')

zoom.ext <- c(310000, 311500, 4120000, 4121500)

par(mfrow = c(1,1)) 
plot(swe.30.bl)
plot(swe.30.nn)
plot(swe.30.rms)
plot(swe.30.nn, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4])
plot(swe.30.rms, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4])
plot(swe.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4])

# method of going to 10 first then 30m
template.10m <- rast(ext(swe.50m), res=10, crs=crs(swe.50m))
swe.10.bl <- resample(swe, template.10m, method = 'bilinear')
swe.10.30.bl <- resample(swe.10.bl, dem, method = 'bilinear')
plot(swe.10.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4])
plot(swe.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4])

#### SDD ####

sdd <- rast(here('data', 'raw', 'SDD', 'creek_sdd_wy2021.tif'))
plot(sdd)
crs(sdd, describe = T)$code
sdd.32611 <- project(sdd, 'EPSG:32611')
res(sdd.32611)

creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))

dem.5000 <- rast(here('data', 'processed', 'processed', 'tif', 'nasadem_creek_5000.tif'))

sdd.creek <- sdd %>%
  crop(creek.extent) %>%
  mask(creek.extent) 

plot(sdd.creek)
sdd.creek.5000 <- mask(sdd.creek, dem.5000)




