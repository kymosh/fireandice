packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'spatialEco')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# just checking to see status of SWE data
swe <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_SanJoaquin_2020_0414_SUPERswe_50m_clipped.tif'))

dem <- rast(here('data', 'processed', 'processed', 'tif', 'nasadem_creek_elev.tif'))
res(dem)

# # 3 methods of resampling from 50m to 30m
# swe.30.bl <- resample(swe, dem, method = 'bilinear')
# swe.30.nn <- resample(swe, dem, method = 'near')
# swe.30.rms <- resample(swe, dem, method = 'rms')
# 
# zoom.ext <- c(310000, 311500, 4120000, 4121500)
# 
# par(mfrow = c(1,3)) 
# plot(swe.30.nn, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "nearest neighbor")
# plot(swe.30.rms, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "rms")
# plot(swe.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "bilinear")
# 
# # method of going to 10 first then 30m
# template.10m <- rast(ext(swe.50m), res=10, crs=crs(swe.50m))
# swe.10.bl <- resample(swe, template.10m, method = 'bilinear')
# swe.10.30.bl <- resample(swe.10.bl, dem, method = 'bilinear')
# plot(swe.10.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "50m to 10m to 30m, bilinear")
# plot(swe.30.bl, xlim=zoom.ext[1:2], ylim=zoom.ext[3:4], main = "50m to 30m, bilinear")


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
sdd.32611 <- project(sdd, 'EPSG:32611', method = 'near')
res(sdd.32611)
    
    
# reproject SDD to 32611 and mask to study area

in.dir <- here('data', 'raw', 'SDD')
out.dir <- here('data', 'processed', 'processed', 'tif', '500m')
dem.500 <- rast(here(out.dir, 'creek_dem_500m_1524.tif'))

sdd.files <- list.files(in.dir, pattern = '^creek_sdd.*\\.tif$', full.names = T)

for (f in sdd.files) {
  r <- rast(f)
  r.32611 <- project(r, 'EPSG:32611', method = 'near')
  r.32611.masked <- mask(r.32611, dem.500)
  
  new.name <- sub('\\.tif$', '_32611_1524.tif', basename(f))
  out.name <- file.path(out.dir, new.name)
  
  writeRaster(r.32611.masked, out.name, overwrite = T)
}

test <- rast(here(out.dir, 'creek_sdd_wy2023_32611_1524.tif'))
plot(test)


sdd.creek.5000 <- mask(sdd.creek, dem.5000)




##### rename SWE files that are clipped to 5000 to reflect that so I can change them to the correct elevations withough losing them
# List all files in out.dir that start with "ASO_"
aso.files <- list.files(out.dir, pattern = '^ASO_', full.names = TRUE)

# Create new names with "_5000" before the extension
new.names <- sub('(\\.tif)$', '_5000\\1', basename(aso.files))
new.paths <- file.path(out.dir, new.names)

# Rename the files
file.rename(aso.files, new.paths)


####### SWE ####### 
# reclip to match new elevation

# open one file just to test
swe <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_SanJoaquin_2025_0509_swe_50m_clipped_5000.tif'))
plot(swe)
res(swe) #confirm resolution is exactly 50x50
# this is the dem that has already been clipped to the correct elevations
dem.elev <- rast(here('data', 'processed', 'processed', 'tif', 'nasadem_Creek_elev.tif'))


swe.clipped <- mask(swe, elev.mask.50m)
plot(swe.clipped)



# create elevation mask
elev.mask <- dem.elev
elev.mask[!is.na(elev.mask)] <- 1
# resample mask to 50m
elev.mask.50m <- resample(elev.mask, swe, method = 'average')

# list ASO_SanJoaquin files to be changed
aso <- list.files(out.dir, pattern = '^ASO_SanJoaquin', full.names = T)

for (f in aso) {
  r <- rast(f)
  r.50m <- resample(r, swe, method = 'near') # necessary because some files have res of 50.0001x50.0001
  r.elev <- mask(r.50m, elev.mask.50m)
  
  new.name <- sub('clipped_5000', '1524_2674', basename(f))
  out.name <- file.path(out.dir, new.name)
  writeRaster(r.elev, out.name, overwrite = T)
}

test <- rast(here(out.dir, 'ASO_SanJoaquin_2023_0121_swe_50m_1524_2674.tif'))
plot(test)

# some SWE data have mismatched extents
# crop all to the ref file




