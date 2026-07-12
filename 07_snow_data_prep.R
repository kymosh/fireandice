packages <- c('tidyverse', 'sf', 'spatialEco', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# ----------------------------------------------------------------------------------
# SDD
# ----------------------------------------------------------------------------------

# this code takes the SDD raster of all of CA for the defined year and reprojects, clips to the defined study area, and saves the output

sdd.fires <- function(fire, year, epsg, overwrite = TRUE) {
  
  message('Processing ', fire, ' WY', year, '...')
  
  # define target CRS
  target.crs <- paste0('EPSG:', epsg)
  
  # template used to define resolution
  temp.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek/other_metrics'
  template <- rast(file.path(temp.dir, 'nasadem_creek_500m_1524.tif'))
  
  # sdd for all of california
  ca.sdd.modis <- rast(paste0('data/raw/SDD/ca_sdd_wy', year, '_native.tif'))
  
  # shapefile
  extent <- st_read(paste0('data/processed/processed/shp/studyarea_extents/study_extent_', fire , '_simple.shp'))
  # remove extra variable
  extent <- extent %>%
    select(-area)
  extent <- st_transform(extent, target.crs)
  
  # redefine modis CRS 
  sin.crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m"
  crs(ca.sdd.modis) <- sin.crs
  
  # reproject, crop, and mask
  sdd.3261x <- project(ca.sdd.modis, target.crs, res = res(template), method = 'near')
  sdd.crop <- crop(sdd.3261x, vect(extent))
  sdd.mask <- mask(sdd.crop, vect(extent))
  plot(sdd.mask)
  plot(extent, color = NA, border = 'red', add = T)
  
  out.dir <- paste0('data/processed/processed/tif/500m/', fire, '/snow_metrics')
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  writeRaster(sdd.mask, file.path(out.dir, paste0(fire, '_sdd_wy', year, '_500m.tif')), overwrite = TRUE)
  
}

# ----- run function for each fire, each year -----
sdd.fires('castle', 2023, epsg = 32611)
sdd.fires('castle', 2024, epsg = 32611)
sdd.fires('castle', 2025, epsg = 32611)

sdd.fires('caldor', 2023, epsg = 32610)
sdd.fires('caldor', 2024, epsg = 32610)
sdd.fires('caldor', 2025, epsg = 32610)

sdd.fires('dixie', 2022, epsg = 32610)
sdd.fires('dixie', 2023, epsg = 32610)
sdd.fires('dixie', 2024, epsg = 32610)
sdd.fires('dixie', 2025, epsg = 32610)

sdd.fires('creek', 2021, epsg = 32611)
sdd.fires('creek', 2022, epsg = 32611)
sdd.fires('creek', 2023, epsg = 32611)
sdd.fires('creek', 2024, epsg = 32611)
sdd.fires('creek', 2025, epsg = 32611)


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





# ----- check all origins/res/crs -----

out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/snow_metrics'
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


# ----------------------------------------------------------------------------------
# Align SWE rasters so they all have the same origin, resolution, and respective CRS
# ----------------------------------------------------------------------------------

# since some have different origins and (veryyyy slightly) different resolutions, must match to template grid

snow.dir <- 'data/processed/processed/tif/50m/snow_metrics'
template.32610 <- rast(file.path(snow.dir, 'ASO_American_20230131_swe_50m_clipped.tif'))
template.32611 <- rast(file.path(snow.dir, 'ASO_Kern_20240508_swe_50m_clipped.tif'))

tmp.dir <- file.path(snow.dir, 'aso_aligned_temp')
dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)

aso.files <- list.files(snow.dir, pattern = '\\.tif', full.names = T)

for (f in aso.files) {
  
  r <- rast(f)
  epsg <- crs(r, describe = TRUE)$code
  
  # make template grid for reprojecting
  template.grid <- if (epsg == '32610') {
    template.32610
  } else if (epsg == '32611') {
    template.32611
  } else {
    stop('Unexpected EPSG for ', basename(f), ': ', epsg)
  }
  
  local.template <- rast(
    ext = ext(r),
    resolution = res(template.grid),
    crs = crs(template.grid)
  )
  
  origin(local.template) <- origin(template.grid)
  
  r.align <- resample(r, local.template, method = 'near')
  
  writeRaster(
    r.align,
    file.path(tmp.dir, basename(f)),
    overwrite = TRUE
  )
}

# --- sanity check ---

aligned.aso <- list.files(tmp.dir, pattern = '^ASO_.*\\.tif$', full.names = TRUE)

check <- data.frame(
  file = basename(aligned.aso),
  epsg = sapply(aligned.aso, function(f) crs(rast(f), describe = TRUE)$code),
  xres = sapply(aligned.aso, function(f) res(rast(f))[1]),
  yres = sapply(aligned.aso, function(f) res(rast(f))[2]),
  xorigin = sapply(aligned.aso, function(f) origin(rast(f))[1]),
  yorigin = sapply(aligned.aso, function(f) origin(rast(f))[2])
)

check
unique(check[, c('epsg', 'xres', 'yres', 'xorigin', 'yorigin')])

plot(rast(aligned.aso[55]))
plot(rast(aso.files[55]))
# should look exactly the same




# ----------------------------------------------------------------------------------
# ------- combine into single SWE rasterstack * if only one watershed * -------
# ----------------------------------------------------------------------------------
# --- creek ---
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/snow_metrics'
aso.files <- list.files(out.dir, pattern = '^ASO_', full.names = TRUE)

swe.stack <- rast(aso.files)

# - fix names -

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

# --- dixie ---
out.dir <- 'data/processed/processed/tif/50m/snow_metrics'
aso.files <- list.files(out.dir, pattern = '^ASO_Feather', full.names = TRUE)

swe.stack <- rast(aso.files)

# - fix names -

fn <- basename(aso.files) # list all file names
dates <- sub(
  paste0('ASO_', watershed, '_(\\d{8}).*'),
  '\\1',
  fn
)
date.vec <- as.Date(dates, format = '%Y%m%d') # format as date

# set layer names with swe prefix
names(swe.stack) <- paste0('swe_', format(date.vec, '%Y%m%d'))

#store true time dimension
time(swe.stack) <- date.vec

names(swe.stack)

writeRaster(swe.stack, 'data/processed/processed/tif/50m/dixie/snow_metrics/dixie_swe_50m.tif')

# ----- find peak swe for each year -----

fire <- 'dixie'
out.dir <- paste0('data/processed/processed/tif/50m/', fire, '/snow_metrics/')
swe <- rast(paste0(out.dir, fire, '_swe_50m.tif'))

# extract WY from dates
wy <- format(time(swe), '%Y')

years <- unique(wy)

swe.peak <- rast(lapply(years, function(y) {
  app(swe[[wy == y]], max, na.rm = TRUE)
}))

names(swe.peak) <- paste0('swe_peak_wy', years)

names(swe.peak)

writeRaster(swe.peak, paste0(out.dir, fire, '_swe_peak_50m.tif'))


# ----------------------------------------------------------------------------------
# ------- combine into single SWE rasterstack * if more than one watershed * -------
# ----------------------------------------------------------------------------------
fire <- 'caldor'
watersheds <- 'American|Truckee'

in.dir <- 'data/processed/processed/tif/50m/snow_metrics'

out.dir <- file.path(
  'data/processed/processed/tif/50m',
  fire,
  'snow_metrics'
)

dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

# identify SWE files for both watersheds
aso.files <- list.files(
  in.dir,
  pattern = paste0('^ASO_(', watersheds, ')_\\d{8}_swe_50m_clipped\\.tif$'),
  full.names = TRUE
)

# extract file information
file.info <- data.frame(
  file = aso.files,
  filename = basename(aso.files)
) %>%
  mutate(
    watershed = sub(
      '^ASO_([^_]+)_.*',
      '\\1',
      filename
    ),
    date = as.Date(
      sub(
        '^ASO_[^_]+_(\\d{8}).*',
        '\\1',
        filename
      ),
      format = '%Y%m%d'
    ),
    wy = format(date, '%Y')
  )

file.info


# split files by watershed and year
peak.groups <- file.info %>%
  group_by(watershed, wy) %>%
  group_split()

# calculate peak SWE for each watershed-year
watershed.peaks <- lapply(peak.groups, function(x) {
  
  swe <- rast(x$file)
  
  peak <- app(
    swe,
    max,
    na.rm = TRUE
  )
  
  names(peak) <- paste0(
    unique(x$watershed),
    '_swe_peak_wy',
    unique(x$wy)
  )
  
  list(
    watershed = unique(x$watershed),
    wy = unique(x$wy),
    raster = peak
  )
})

# inspect
sapply(watershed.peaks, function(x) {
  paste(x$watershed, x$wy)
})

# now combine watersheds by year:
years <- sort(unique(file.info$wy))

swe.peak.list <- lapply(years, function(y) {
  
  # select all watershed peak rasters for this year
  year.rasters <- lapply(
    watershed.peaks[
      sapply(watershed.peaks, function(x) x$wy == y)
    ],
    function(x) x$raster
  )
  
  if (length(year.rasters) == 0) {
    stop(paste('No peak SWE rasters found for', y))
  }
  
  if (length(year.rasters) == 1) {
    
    year.peak <- year.rasters[[1]]
    
  } else {
    
    # merge rasters with different, non-overlapping extents
    year.peak <- year.rasters[[1]]
    
    for (i in 2:length(year.rasters)) {
      year.peak <- merge(
        year.peak,
        year.rasters[[i]]
      )
    }
  }
  
  names(year.peak) <- paste0('swe_peak_wy', y)
  
  year.peak
})

swe.peak <- rast(swe.peak.list)

names(swe.peak)
plot(swe.peak)

writeRaster(
  swe.peak,
  file.path(
    out.dir,
    paste0(fire, '_swe_peak_50m.tif')
  ),
  overwrite = TRUE
)

# ----------------------------------------------------------------------------------
# aggregate PEAK swe to 500m for sdd 
# ----------------------------------------------------------------------------------
fire <- 'castle'
target <- rast(paste0('data/processed/processed/tif/500m/', fire, '/snow_metrics/', fire, '_sdd_wy2023_500m.tif'))
out.dir <- paste0('data/processed/processed/tif/500m/', fire, '/snow_metrics/')

swe.peak <- rast(paste0('data/processed/processed/tif/50m/', fire, '/snow_metrics/', fire, '_swe_peak_50m.tif'))

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
  f <- file.path(out.dir, paste0(fire, '_', nm, '_500m.tif'))
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
writeRaster(swe.500m, paste0(out.dir, fire, '_swe_peak_500m.tif'), overwrite = TRUE)



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








