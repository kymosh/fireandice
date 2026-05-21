packages <- c('tidyverse', 'sf', 'spatialEco', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# ----------------------------------------------------------------------------------
# SDD
# ----------------------------------------------------------------------------------

# this code takes the SDD raster of all of CA for the defined year and reprojects, clips to the defined study area, and saves the output

sdd.fires <- function(fire, year, overwrite = TRUE) {
  
  message('Processing ', fire, ' WY', year, '...')
  
  # template used to define target CRS and resolution
  temp.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek/other_metrics'
  template <- rast(file.path(temp.dir, 'nasadem_creek_500m_1524.tif'))
  
  # sdd for all of california
  ca.sdd.modis <- rast(paste0('data/raw/SDD/ca_sdd_wy', year, '_native.tif'))
  
  # shapefile
  extent <- st_read(paste0('data/processed/processed/shp/studyarea_extents/study_extent_', fire , '_simple.shp'))
  # remove extra variable
  extent <- extent %>%
    select(-area)
  extent <- st_transform(extent, crs(template))
  
  # redefine modis CRS 
  sin.crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m"
  crs(ca.sdd.modis) <- sin.crs
  
  # reproject, crop, and mask
  sdd.32611 <- project(ca.sdd.modis, crs(template), res = res(template), method = 'near')
  sdd.crop <- crop(sdd.32611, vect(extent))
  sdd.mask <- mask(sdd.crop, vect(extent))
  plot(sdd.mask)
  plot(extent, color = NA, border = 'red', add = T)
  
  out.dir <- paste0('data/processed/processed/tif/500m/', fire, '/snow_metrics')
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  writeRaster(sdd.mask, file.path(out.dir, paste0(fire, '_sdd_wy', year, '_500m.tif')), overwrite = TRUE)
  
}

sdd.fires('castle', 2023)
sdd.fires('castle', 2024)
sdd.fires('castle', 2025)

sdd.fires('caldor', 2023)
sdd.fires('caldor', 2024)
sdd.fires('caldor', 2025)

sdd.fires('dixie', 2022)
sdd.fires('dixie', 2023)
sdd.fires('dixie', 2024)
sdd.fires('dixie', 2025)

sdd.fires('creek', 2021)
sdd.fires('creek', 2022)
sdd.fires('creek', 2023)
sdd.fires('creek', 2024)
sdd.fires('creek', 2025)


##### rename SWE files that are clipped to 5000 to reflect that so I can change them to the correct elevations withough losing them
# List all files in out.dir that start with "ASO_"
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
aso.files <- list.files(out.dir, pattern = '^ASO_', full.names = TRUE)

# Create new names with "_5000" before the extension
new.names <- sub('(\\.tif)$', '_5000\\1', basename(aso.files))
new.paths <- file.path(out.dir, new.names)

# Rename the files
file.rename(aso.files, new.paths)


# ----------------------------------------------------------------------------------
# SWE
# ----------------------------------------------------------------------------------
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

out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/snow_metrics'
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

template.32610 <- file.path(j.dir, 'tif/50m/snow_metrics/ASO_American_20230131_swe_50m_clipped.tif')
template.32611 <- file.path(j.dir, 'tif/50m/snow_metrics/ASO_Kern_20240508_swe_50m_clipped.tif')
# has 50 res and 0,0 origin

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
 
# check extent
for (f in aso.files) {
  r <- rast(f)
  print(ext(r))
}

# ------- combine into single SWE rasterstack -------
swe.stack <- rast(aso.files)

# --- fix names

fn <- basename(aso.files) # list all file names
dates <- sub('ASO_SanJoaquin_(\\d{4}_\\d{4}).*', '\\1', fn) # extract dates
dates <- gsub('_', '', dates)   # 2020_0414 to 20200414
date.vec <- as.Date(dates, format = '%Y%m%d') # format as date

# set layer names with swe prefix
names(swe.stack) <- paste0('swe_', format(date.vec, '%Y%m%d'))

#store true time dimension
time(swe.stack) <- date.vec

names(swe.stack)

writeRaster(swe.stack, file.path(out.dir, 'creek_swe_50m.tif'))

# --------------------------------------------------------------------------------
# find peak swe for each year
# --------------------------------------------------------------------------------
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
swe <- rast(file.path(out.dir, 'creek_swe_50m.tif'))

# extract WY from dates
wy <- format(time(swe), '%Y')

years <- unique(wy)

swe.peak <- rast(lapply(years, function(y) {
  app(swe[[wy == y]], max, na.rm = TRUE)
}))

names(swe.peak) <- paste0('swe_peak_wy', years)

names(swe.peak)

writeRaster(swe.peak, file.path(out.dir, 'creek_swe_peak_50m_1524.tif'))

# ---- aggregate PEAK swe to 500m for sdd -------

target <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek/snow_metrics/creek_sdd_wy2021_32611_1524.tif')
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek'

out.list <- vector('list', length = nlyr(swe.peak))

for (i in seq_len(nlyr(swe.peak))) {
  
  nm <- names(swe.peak)[i]
  message('Exact resampling: ', i, '/', nlyr(swe.peak), '  ', nm)
  
  # single-layer SpatRaster
  x <- swe.peak[[i]]
  
  # exact area-weighted mean to target grid
  y <- exactextractr::exact_resample(x, target, fun = 'mean')
  
  names(y) <- paste0(nm, '_500m')
  
  # write immediately (safer)
  f <- file.path(out.dir, paste0(nm, '_500m.tif'))
  writeRaster(y, f, overwrite = TRUE)
  
  out.list[[i]] <- y
}

# stack back together
swe.500m <- rast(out.list)

# restore missing CRS
crs(swe.500m) <- crs(target)

# check
origin(target) == origin(swe.500m)
res(target) == res(swe.500m)
crs(target) == crs(swe.500m)

plot(swe.500m)
# save output
writeRaster(swe.500m, file.path(out.dir, 'creek_swe_peak_500m.tif'), overwrite = TRUE)



# ---- aggregate swe to 500m for sdd -------
swe <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/creek_swe_50m.tif')

target <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek/snow_metrics/creek_sdd_wy2021_32611_1524.tif')
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek'

out.list <- vector('list', length = nlyr(swe))

for (i in seq_len(nlyr(swe))) {
  
  nm <- names(swe)[i]
  message('Exact resampling: ', i, '/', nlyr(swe.peak), '  ', nm)
  
  # single-layer SpatRaster
  x <- swe[[i]]
  
  # exact area-weighted mean to target grid
  y <- exactextractr::exact_resample(x, target, fun = 'mean')
  
  names(y) <- paste0(nm, '_500m')
  
  # write immediately (safer)
  f <- file.path(out.dir, paste0(nm, '_500m.tif'))
  writeRaster(y, f, overwrite = TRUE)
  
  out.list[[i]] <- y
}

# stack back together
swe.500m <- rast(out.list)

# restore missing CRS
crs(swe.500m) <- crs(target)

# check
origin(target) == origin(swe.500m)
res(target) == res(swe.500m)
crs(target) == crs(swe.500m)

plot(swe.500m)
# save output
writeRaster(swe.500m, file.path(out.dir, 'creek_swe_500m.tif'), overwrite = TRUE)




# troubleshooting
og.23 <- rast('data/raw/SDD/sdd_2023_test_ogscript.tif')
r <- rast('data/raw/SDD/creek_sdd_wy2024_6974.tif')
r.correct <- rast('data/raw/SDD/creek_sdd_wy2023_5837.tif')

crs(r) <- '+proj=sinu +R=6371007.181 +nadgrids=@null +wktext'

r.32611 <- project(r, template, method = 'near')
r.32611.masked <- mask(r.32611, template)
plot(r.32611.masked)

r.32611.correct <- project(r.correct, template, method = 'near')
r.32611.correct.masked <- mask(r.32611.correct, template)
plot(r.32611.correct.masked)

r1 <- r.32611.masked
r2 <- r.32611.masked.x

d <- r1 - r2
global(d, c('min', 'max'), na.rm = TRUE)
freq(d)
plot(d)

# percent of cells with big differences
mean(abs(values(d)) > 30, na.rm = TRUE)

# percent with small differences
mean(abs(values(d)) <= 5, na.rm = TRUE)



# troubleshooting
sdd.dir <- 'data/raw/SDD'
sdd.files <- list.files(path = sdd.dir, pattern = '\\.tif$', full.names = T)

for (s in sdd.files) {
  r <- rast(s)
  print(res(r))
}




sdd.files
creek <- rast(file.path(sdd.dir, 'old_SDD_downloads/creek_sdd_wy2025.tif'))
res(creek)
creek.32611 <- project(creek, 'EPSG:32611', method = 'near')
res(creek)

c <- rast(creek.files[5])
res(c)
c.32611 <- project(c , 'EPSG:32611', method = 'near')
res(c.32611)

creek.files <- list.files('data/raw/SDD/old_SDD_downloads', pattern = '\\.tif$', full.names = T)
for (s in creek.files) {
  r <- rast(s)
  print(res(r))
}

for (c in creek.files[5:9]) {
  r <- rast(s)
  print(res(r))
  c.32611 <- project(r, 'EPSG:32611', method = 'near')
  print(res(c.32611))
}

r <- rast(creek.files[5])
r.32611 <- project(r, template, method = 'near')
res(r.32611)
plot(r.32611)

r <- rast(file.path(sdd.dir, 'old_SDD_downloads/creek_sdd_wy2025.tif'))
crs(r) <- '+proj=sinu +R=6371007.181 +nadgrids=@null +wktext' # force CRS to the native MODIS projection that it was exported in 
r.32611 <- project(r, template, method = 'near')
res(r.32611)
plot(r.32611)

r.32611

ext(r)
ext(r.32611)
ext(template)

global(r.32611, range, na.rm = TRUE)

freq(r.32611, useNA = 'always')

global(r, range, na.rm = TRUE)
plot(r)
crs(r)



for (s in sdd.files) {
  r <- rast(s)
  print(crs(r))
}








