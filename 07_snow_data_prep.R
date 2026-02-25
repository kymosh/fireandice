packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'spatialEco')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# just checking to see status of SWE data
swe <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_SanJoaquin_2020_0414_SUPERswe_50m_clipped.tif'))

dem <- rast(here('data', 'processed', 'processed', 'tif', 'nasadem_creek_elev.tif'))
res(dem)



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
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
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


# ----- check all origins/res/crs -----

out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
aso.files <- list.files(out.dir, pattern = '^ASO_', full.names = TRUE)

# check CRS
for (f in aso.files) {
  r <- rast(f)
  print(crs(r, describe = T)$code)
}

# check res
for (f in aso.files) {
  r <- rast(f)
  print(res(r))
}

# check origin
for (f in aso.files) {
  r <- rast(f)
  print(origin(r))
}

# since some have different origins and (veryyyy slightly) different resolutions, must match to template grid

template <- rast(aso.files[12]) # has 50 res and 0,0 origin

# put aligned files here temporarily 
tmp.dir <- file.path(out.dir, 'aso_aligned_temp')
dir.create(tmp.dir, showWarnings = F)

for (f in aso.files[1:11]) {
  r <- rast(f)
  r.align <- project(r, template, method = 'near') # align to template grid
  writeRaster(r.align, file.path(tmp.dir, basename(f)), overwrite = TRUE)
}

aligned.aso <- list.files(tmp.dir, pattern = 'ASO', full.names = TRUE)

# verify to make sure they have the correct res, origin, and crs after reprojecting

for (f in aligned.aso) {
  r <- rast(f)
  print(res(r))
}

for (f in aligned.aso) {
  r <- rast(f)
  print(origin(r))
}


pre.align <- rast(file.path(out.dir, 'ASO_SanJoaquin_2020_0414_swe_50m_1524.tif'))
post.align <- rast(file.path(tmp.dir, 'ASO_SanJoaquin_2020_0414_swe_50m_1524.tif'))

plot(pre.align)
plot(post.align)

names(pre.align)
names(post.align)

summary(values(pre.align))
summary(values(post.align))

# write rasters to replace 
file.copy(list.files(tmp.dir, full.names=TRUE),
          dirname(aso.files[1]),
          overwrite=TRUE)

# final check
aso.files <- list.files(out.dir, pattern = '^ASO_', full.names = TRUE)

# check CRS
for (f in aso.files) {
  r <- rast(f)
  print(crs(r, describe = T)$code)
}

# check res
for (f in aso.files) {
  r <- rast(f)
  print(res(r))
}

# check origin
for (f in aso.files) {
  r <- rast(f)
  print(origin(r))
}


