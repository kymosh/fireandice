packages <- c('dplyr', 'tidyr', 'tools', 'exactextractr', 'terra')
lapply(packages, library, character.only = T)

# ===========================================================================================
# Create 9-tile test mosaic for testing r.sun
# ===========================================================================================

tiles <- c(
  '11SLB1150', '11SLB1250', '11SLB1350',
  '11SLB1151', '11SLB1251', '11SLB1351',
  '11SLB1152', '11SLB1252', '11SLB1352'
)

# file directory
dem.dir <- 'data/raw/DEM/creek'

# all files in directory
all.files <- list.files(dem.dir, pattern= '\\.tif$', full.names = TRUE)
# keep only the 9 test tiles
test.files <- all.files[grepl(paste(tiles, collapse = '|'), basename(all.files))]          

# read as rasters
dem.list <- lapply(test.files, rast)
# combine
dem.test <- do.call(mosaic, dem.list)
# check
plot(dem.test)

# check res
res(dem.test) #0.5 0.5

# get template grid from chms
temp.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
temp.files <- list.files(temp.dir, pattern = '\\.tif$', full.names = T)
temp.files <- temp.files[grepl(paste(tiles, collapse = '|'), basename(temp.files))]   
template <- lapply(temp.files, rast)
template <- do.call(mosaic, template)

# project dem onto grid
dem.32611 <- project(dem.test, template, method = 'bilinear')

# check
res(dem.32611)
crs(dem.32611, describe = T)$code
origin(dem.32611)

writeRaster(dem.32611, 'data/processed/processed/tif/1m/creek_dtm_9tile.tif')


# ===========================================================================================
# Reproject DEM to correct res and origin
# ===========================================================================================

# file directory
dem.dir <- 'data/raw/DEM/creek'
dem.files <- list.files(dem.dir, pattern = '\\.tif$', full.names = TRUE)


# read as raster
start <- Sys.time()
dem.rasters <- lapply(dem.files, rast)
dem.r <- do.call(mosaic, dem.rasters)
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')

# template
# get template grid from chms
temp.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
temp.files <- list.files(temp.dir, pattern = '\\.tif$', full.names = T)
template <- lapply(temp.files, rast)
template <- do.call(mosaic, template)

# project dem onto grid
start <- Sys.time()
dem.32611 <- project(dem.r, template, method = 'bilinear')
writeRaster(dem.32611, 'data/processed/processed/tif/1m/creek_dtm_1m.tif')
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
# took 5.3 hours


# ===========================================================================================
# Create DSM
# ===========================================================================================

# ----- test mosaic first -----

# create test mosaic first
tiles <- c(
  '11SLB1150', '11SLB1250', '11SLB1350',
  '11SLB1151', '11SLB1251', '11SLB1351',
  '11SLB1152', '11SLB1252', '11SLB1352'
)

# dem
dem <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dem_test_9.tif')

# chm files
chm.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = T)
chm.files <- chm.files[grepl(paste(tiles, collapse = '|'), basename(chm.files))]

# output directory
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dsm_test_9'
dir.create(out.dir, recursive = T, showWarnings = F)

# function to make dsm from dem and chm
dsm.from.dem.chm <- function(f) {
  
  # read in chm tile
  chm <- rast(f)
  
  # crop dem to that tile's extent
  dem.tile <- crop(dem, chm, snap = 'near')
  
  chm[is.na(chm)] <- 0
  
  dsm <- dem.tile + chm
  
  out.file <- file.path(out.dir, sub('creek_chm', 'creek_dsm', basename(f)))
  
  writeRaster(dsm, out.file, overwrite = T)

  }
  
dsm.files <- lapply(chm.files, dsm.from.dem.chm)

# check  
x <- rast(dsm.files[1])  
plot(x)

# combine into single mosaic
dsm <- do.call(mosaic, dsm.files)
plot(dsm)

writeRaster(dsm, 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dsm_test_9.tif')

# ----- full mosaic -----

# full 1m raster dem
dem <- rast('data/processed/processed/tif/1m/creek_dtm_1m.tif')

# chm files
# keep chm files by tile
chm.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = T)

# output directory
out.dir <- 'data/processed/processed/tif/1m/creek_dsm'
dir.create(out.dir, recursive = T, showWarnings = F)

# apply function
start <- Sys.time()
dsm.files <- lapply(chm.files, dsm.from.dem.chm)
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
# 134 minutes

# combine into single mosaic
start <- Sys.time()
dsm <- do.call(mosaic, dsm.files)
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
writeRaster(dsm, 'data/processed/processed/tif/1m/creek_dsm_1m.tif')
# 30 minutes





# ===========================================================================================
# Check data
# ===========================================================================================

dir <- 'data/processed/processed/tif/rsun_test_outputs'
beam <- rast(file.path(dir, 'rsun_beam_day15.tif'))
diff <- rast(file.path(dir, 'rsun_diff_day15.tif'))
global <- rast(file.path(dir, 'rsun_global_day15.tif'))
plot(beam)
plot(diff)
plot(global)

x <- rast('data/processed/processed/tif/5m/creek_rad/rad_global_dtm_day15_5m.tif')
plot(x)

x <- rast('data/processed/processed/tif/5m/creek_rad/rad_global_dsm_day15_5m.tif')
plot(x)

files <- list.files('data/processed/processed/tif/rsun_test_outputs', pattern = '\\.tif$', full.names = T)
dsm <- rast(files[9])
dtm <- rast(files[10])
plot(dsm)
plot(dtm)

# plot with fixed scale
rng <- range(c(values(dsm), values(dtm)), na.rm = TRUE)
plot(dsm, range = rng)
plot(dtm, range = rng)

# compare with chm
chm.list <- lapply(chm.files, rast)
chm <- do.call(mosaic, chm.list)
plot(chm)


# ------- Sensitivity analysis for albedo -------
dir <- 'data/processed/processed/tif/rsun_test_outputs'
a.2 <- rast(file.path(dir, 'rad_global_dtm_day75_02alb.tif'))
a.6 <- rast(file.path(dir, 'rad_global_dtm_day75_06alb.tif'))

diff.abs <- a.6 - a.2
diff.pct <- (a.6 - a.2) / a.2 * 100

global(diff.abs, c('mean', 'min', 'max'), na.rm = TRUE)
global(diff.pct, c('mean', 'min', 'max'), na.rm = TRUE)

# troubleshooting
x <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_chm_32611/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7732_norm.tif')
x <- dem
origin(x)
crs(x, describe = T)$code
res(x)

x <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dem_test_.tif')
plot(x)

test.chm <- rast(chm.files[1])
dem.test <- crop(dem, test.chm)

compareGeom(dem.test, test.chm)

files <- list.files('data/processed/processed/tif/rsun_test_outputs', pattern = '\\.tif$', full.names = T)
new <- sub('\\.tif$', '_02alb.tif', files)
file.rename(files, new)

summary(values(a.6))
global(a.6, c('min', 'max', 'mean'), na.rm = TRUE)

# ------- Sensitivity analysis for resolution  -------

res.1 <- rast(files[8])
res.5 <- rast(files[9])

res(res.1)
res(res.5)
crs(res.1, describe = T)$code

res.1to5 <- aggregate(res.1, fact = 5, fun = mean, na.rm = TRUE)

diff <- res.1to5 - res.5

global(diff, c('mean', 'min', 'max'), na.rm = TRUE)
global(abs(diff), c('mean', 'min', 'max'), na.rm = TRUE)

diff.pct <- (res.1to5 - res.5) / res.1to5 * 100

global(diff.pct, c('mean', 'min', 'max'), na.rm = TRUE)
global(diff.pct, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)

res.1to50 <- aggregate(res.1, fact = 50, fun = mean, na.rm = TRUE)
res.5to50 <- aggregate(res.5, fact = 10, fun = mean, na.rm = TRUE)

diff <- res.1to50 - res.5to50

global(diff, c('mean', 'min', 'max'), na.rm = TRUE)
global(abs(diff), c('mean', 'min', 'max'), na.rm = TRUE)

diff.pct <- (res.1to50 - res.5to50) / res.1to50 * 100

global(diff.pct, c('mean', 'min', 'max'), na.rm = TRUE)
global(diff.pct, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)

# ===========================================================================================
# Compute Radiation Metrics from Rsun Outputs
# ===========================================================================================

# ----- compute accumulation/melt metric -----

season_rad <- function(dir, surface) {
  
  files <- list.files(dir, pattern = paste0('^rad_global_', surface, '_day\\d+_5m\\.tif$'), full.names = TRUE)

  # order files sequentially by day
  days <- as.numeric(gsub('.*day|_5m\\.tif', '', basename(files)))
  ord <- order(days)
  files <- files[ord]
  days <- days[ord]
  
  # print files and days to check order
  print(data.frame(
    index = seq_along(files),
    day = days,
    file = basename(files)
  ))
  
  # read in rasters
  dec <- rast(files[7])
  jan <- rast(files[1])
  feb <- rast(files[2])
  mar <- rast(files[3])
  apr <- rast(files[4])
  may <- rast(files[5])
  jun <- rast(files[6])
  
  # calculate weighted mean for each season
  accum <- ((dec * 31) + (jan * 31) + (feb * 28) + (mar * 31)) / sum(31, 31, 28, 31)
  melt <- ((apr * 30) + (may * 31) + (jun * 30)) / sum(30, 31, 30)
  
  names(accum) <- paste0('rad_', surface, '_accum')
  names(melt) <- paste0('rad_', surface, '_melt')
  
  writeRaster(accum, file.path(dir, paste0('rad_', surface, '_accum_5m.tif')), overwrite = TRUE)
  writeRaster(melt, file.path(dir, paste0('rad_', surface, '_melt_5m.tif')), overwrite = TRUE)
  
  return(list(accum = accum, melt = melt))
}

# run function

dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/5m/creek_rad' # from my comp
#dir <- 'data/processed/processed/tif/5m/creek_rad'   # from processing comp

dtm.rad <- season_rad(dir, 'dtm')
dsm.rad <- season_rad(dir, 'dsm')


# ===========================================================================================
# resample to match SDD and SWE
# ===========================================================================================

resample_rad <- function(r, season, surface, template.path) {
  
  # read template
  template <- rast(template.path)
  out.dir <- dirname(template.path) # extract out.dir from template file
  res <- gsub('.*_(\\d+m)\\.tif$', '\\1', basename(template.path))  # extract res from template file name
  
  # resample 
  r.out <- exact_resample(r, template, fun = 'mean')
  
  # rename layer
  names(r.out) <- paste0('rad_', surface, '_', season)
  
  # output file name
  out.name <- paste0('creek_rad_', surface, '_', season, '_', res, '.tif')
  
  # write
  writeRaster(r.out, file.path(out.dir, out.name), overwrite = TRUE)
  
  return(r.out)
}

# setup
template.path.500 <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek/creek_sdd_500m.tif'
template.path.50 <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/creek_swe_50m.tif'

rad.list <- list(
  dtm = dtm.rad,
  dsm = dsm.rad
)

templates <- list(
  '500m' = template.path.500,
  '50m'  = template.path.50
)

# loop through surfaces, seasons, and resolutions to resample and save rasters
for (surface in c('dtm', 'dsm')) {
  for (season in c('accum', 'melt')) {
    for (res in c('500m', '50m')) {
      
      r <- rad.list[[surface]][[season]]
      template.path <- templates[[res]]
      
      resample_rad(
        r = r,
        season = season,
        surface = surface,
        template.path = template.path
      )
    }
  }
}

# check
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek' 
files <- list.files(dir, pattern = '^creek_rad_', full.names = TRUE)
dsm.a <- rast(files[1]) 
dsm.m <- rast(files[2]) 
dtm.a <- rast(files[3]) 
dtm.m <- rast(files[4]) 
plot(dsm.a) 
plot(dsm.m)
plot(dtm.a)
plot(dtm.m)
