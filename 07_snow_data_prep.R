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

par(mfrow = c(1,3)) 
plot(swe.30.nn, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "nearest neighbor")
plot(swe.30.rms, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "rms")
plot(swe.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "bilinear")

# method of going to 10 first then 30m
template.10m <- rast(ext(swe.50m), res=10, crs=crs(swe.50m))
swe.10.bl <- resample(swe, template.10m, method = 'bilinear')
swe.10.30.bl <- resample(swe.10.bl, dem, method = 'bilinear')
plot(swe.10.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "50m to 10m to 30m, bilinear")
plot(swe.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "50m to 30m, bilinear")


# Don't need this code anymore because we're not resampling SWE
# #### picking bl for now just as a placeholder to build rest of code
# in.dir <- here('data', 'processed', 'processed', 'tif')
# out.dir <- here('data', 'processed', 'processed', 'tif')
# 
# # create a list of all swe files to be resampled
# swe.files <- list.files(in.dir,
#                         pattern = '^ASO_SanJoaquin_2020.*\\.tif$', # get rid of 2020 when ready to run for all
#                         full.names = T)
# 
# # loop though swe files and resample each, resaving the new result
# for (f in swe.files) {
#   r = rast(f)
#   r.30m <- resample(r, dem, method = 'near') # change method of necessary
#   
#   new.name <- basename(f)
#   new.name <- gsub('50m', '30m', new.name) # replace 50m with 30m 
#   new.name <- gsub('_clipped', '', new.name) # get rid of "clipped"
#   
#   out.name <- file.path(out.dir, new.name)
#   writeRaster(r.30m, out.name, overwrite = T)
# }

#### SDD ####
par(mfrow = c(1,1))
sdd <- rast(here('data', 'raw', 'SDD', 'creek_sdd_wy2021.tif'))
plot(sdd)
crs(sdd, describe = T)$code
sdd.32611 <- project(sdd, 'EPSG:32611')
res(sdd.32611)
    
    
     
      
    
# reproject SDD to 32611

in.dir <- here('data', 'raw', 'SDD')
out.dir <- here('data', 'processed', 'processed', 'tif')

sdd.files <- list.files(in.dir, pattern = '^creek_sdd.*\\.tif$', full.names = T)

for (f in sdd.files) {
  r = rast(f)
  r.32611 <- project(r, 'EPSG:32611') # still trying to figure out if I need a method here
  
  new.name <- sub('\\.tif$', '_32611.tif', basename(f))
  out.name <- file.path(out.dir, new.name)
  
  writeRaster(r.32611, out.name, overwrite = T)
}








creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))

dem.5000 <- rast(here('data', 'processed', 'processed', 'tif', 'nasadem_creek_5000.tif'))

sdd.creek <- sdd %>%
  crop(creek.extent) %>%
  mask(creek.extent) 

plot(sdd.creek)
sdd.creek.5000 <- mask(sdd.creek, dem.5000)




