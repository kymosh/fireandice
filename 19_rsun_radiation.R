packages <- c('dplyr', 'tidyr', 'tools', 'exactextractr', 'terra')
lapply(packages, library, character.only = T)

packages <- c('sf', 'mapview', 'lidR', 'dplyr', 'raster', 'future', 'future.apply', 'stringr', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
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

# set-up
fire = 'dixie'
target.crs <- '32610'


# directories
base.dir <-  'J:/Fire_Snow/fireandice/data/' # KM comp
base.dir <-  'data/' # processing comp
dir.1m <- paste0(base.dir, 'processed/processed/tif/1m/')

acq <- c(
  'CA_SierraNevada_4_2022',
  'CA_SierraNevada_6_2022',
  'CA_SierraNevada_4_2022_low',
  'CA_SierraNevada_6_2022_low',
  'CA_SierraNevada_7_2022_low',
  'CA_SierraNevada_7_2022'
)

# ----- create mosaic of DTM (if multiple acquisitions) -----

# function to mosaic each acquisition
library(future)
library(future.apply)
library(terra)

plan(multisession, workers = 3)  # maybe 2-3 for huge 1m rasters

mosaic_acq <- function(acq) {
  
  message('Mosaicking ', acq, '...')
  
  dem.dir <- paste0(base.dir, 'raw/DEM/', fire, '/', acq)
  
  files <- list.files(dem.dir, pattern = '\\.tif$', full.names = TRUE)
  
  if (length(files) == 0) {
    warning('No files found for ', acq)
    return(NULL)
  }
  
  r.col <- sprc(files)
  
  out.file <- paste0(
    dir.1m,
    fire,
    '_dtm_1m_',
    acq,
    '.tif'
  )
  
  m <- mosaic(r.col, filename = out.file, overwrite = TRUE)
  
  out.file
}

dem.acq.files <- future_lapply(acq, mosaic_acq)

dem.acq <- lapply(unlist(dem.acq.files), rast)

# start with 1st raster and add each subsequent acq
combine <- dem.acq[[1]]

for (i in 2:length(dem.acq)) {
  # create new extent that contains both raster areas
  e <- union(ext(combine), ext(dem.acq[[i]]))
  
  # extend extent of OG raster
  combine.ext <- extend(combine, e)
  # extent extent of adding raster
  next.ext <- extend(dem.acq[[i]], e)
  
  # ensure extents are now the same
  print(compareGeom(combine.ext, next.ext, stopOnError = FALSE))
  
  # merge rasters
  combine <- cover(combine.ext, next.ext)
}

# check
plot(combine)

epsg <- crs(combine, describe = T)$code

# save
out.file <- paste0(dir.1m, fire, '_dtm_1m_', epsg, '.tif')
writeRaster(combine, out.file, overwrite = TRUE)


# ----- create mosaic of DTM (if just single acquisition) -----
# set-up
fire = 'castle'
target.crs <- '32611'


# directories
# base.dir <-  'J:/Fire_Snow/fireandice/data/' # KM comp
base.dir <-  'data/' # processing comp
out.dir <- paste0(base.dir, 'processed/processed/tif/1m/', fire, '/')
dtm.dir <- paste0(out.dir, fire, '_dtm_32611')
out.file <- paste0(out.dir, fire, '_dtm_1m_', target.crs, '.tif')

files <- list.files(dtm.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)  

r.col <- sprc(files)
  
m <- mosaic(r.col, filename = out.file, overwrite = TRUE)

# sanity check
plot(m)


# ----- reproject chm and dtm to correct res -----
chm.dir <- paste0(dir.1m, fire)
chm.file <- list.files(chm.dir, pattern = 'chm.*\\.tif', full.names = T)
chm <- rast(chm.file)
dtm.file <- list.files(chm.dir, pattern = 'dtm.*\\.tif', full.names = T)
dtm <- rast(dtm.file)

# create template raster
template <- rast(
  ext(chm),
  resolution = 1,
  crs = paste0('EPSG:', target.crs)
)
origin(template) <- c(0, 0)

# reproject
chm.reproj <- project(
  chm,
  template,
  method = 'bilinear',
  align_only = TRUE
)

out.file <- paste0(dir.1m, fire, '_chm_1m_', target.crs, '.tif')
writeRaster(chm.reproj, out.file, overwrite = TRUE)


# reproject dtm to same template
start <- Sys.time()
dtm.reproj <- project(
  dtm,
  template,
  method = 'bilinear',
  align_only = TRUE
)
out.file <- paste0(dir.1m, fire, '_dtm_1m_', target.crs, '.tif')
writeRaster(dtm.reproj, out.file, overwrite = TRUE)

end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
# took 5.3 hours


# ===========================================================================================
# Create DSM
# ===========================================================================================
# ----- function to make dsm from dem and chm (*from tiled inputs*)-----
dsm.from.dtm.chm <- function(f) {
  
  # read in chm tile
  chm <- rast(f)
  
  # crop dem to that tile's extent
  dtm.tile <- crop(dtm, chm, snap = 'near')
  
  chm[is.na(chm)] <- 0
  
  if (!compareGeom(dtm.tile, chm, stopOnError = FALSE)) {
    stop('DTM and CHM do not align for: ', basename(f))
  }
  
  dsm <- dtm.tile + chm
  
  out.file <- file.path(out.dir, sub('castle_chm', 'castle_dsm', basename(f)))
  
  writeRaster(
    dsm,
    out.file,
    overwrite = TRUE,
    wopt = list(gdal = c('COMPRESS=LZW'))
  )
  
  return(out.file)
  
}

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


# run
dsm.files <- lapply(chm.files, dsm.from.dtm.chm)

# check  
x <- rast(dsm.files[1])  
plot(x)

# combine into single mosaic
dsm <- do.call(mosaic, dsm.files)
plot(dsm)

writeRaster(dsm, 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dsm_test_9.tif')

# ----- full mosaic -----

# full 1m raster dtm
dtm <- rast('data/processed/processed/tif/1m/castle/castle_dtm_1m_32611.tif')

# chm files
# keep chm files by tile
chm.dir <- 'data/processed/processed/tif/1m/castle/castle_chm_32611'
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = T)

# output directory
out.dir <- 'data/processed/processed/tif/1m/castle/castle_dsm_32611'
dir.create(out.dir, recursive = T, showWarnings = F)

# check this first!
chm <- rast(chm.files[1])
dtm.tile <- crop(dtm, chm, snap = 'near')
compareGeom(dtm.tile, chm)

# apply function
start <- Sys.time()
dsm.files <- lapply(chm.files, dsm.from.dtm.chm)
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
# 134 minutes

dsm.files <- unlist(dsm.files)

# combine into single mosaic
start <- Sys.time()

dsm <- mosaic(
  sprc(dsm.files),
  filename = 'data/processed/processed/tif/1m/castle/castle_dsm_1m_32611.tif',
  overwrite = TRUE,
  wopt = list(gdal = c('COMPRESS=LZW'))
)

end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
# 30 minutes








# ----- make DSM from full dtm and chm mosaic -----
fire <- 'dixie'
res <- '5m'
in.dir <- paste0('data/processed/processed/tif/', res, '/', fire, '/')
# read in files
target.crs <- 32610

dtm.file <- paste0(in.dir, fire, '_dtm_', res, '.tif')
chm.file <- paste0(in.dir, fire, '_chm_', res, '.tif')

dtm <- rast(dtm.file)
chm <- rast(chm.file)

# sanity check
plot(dtm)
plot(chm)

# check CRS
crs(dtm) == crs(chm)

# check if same
compareGeom(dtm, chm, stopOnError = FALSE)

# if chm is very slightly smaller, crop to chm
#dtm.crop <- crop(dtm, chm, snap = "near")

# now check if same
#compareGeom(dtm.crop, chm, stopOnError = FALSE)

# make DSM
dsm <- dtm + chm

out.file <- paste0(in.dir, fire, '_dsm_', res, '.tif')
writeRaster(dsm, out.file, overwrite = TRUE)


# ===========================================================================================
# Resample dsm and dtm to 5m
# ===========================================================================================
# ---castle and caldor ---
fire <- 'dixie'
in.dir <- paste0('data/processed/processed/tif/1m/', fire)
out.dir <- paste0('data/processed/processed/tif/5m/', fire)
dir.create(out.dir, showWarnings = F, recursive = T)
#surface <- '(dsm|dtm)' #chose one
surface <- 'chm'


files <- list.files(in.dir, pattern = paste0(fire, '_', surface, '.*\\.tif$'), full.names = T)

for (f in files) {
  r <- rast(f)
  r.5 <- aggregate(r, fact = 5, fun = mean, na.rm = TRUE)
  # determine whether file is DSM or DTM
  #surface <- if (grepl('_dsm', basename(f))) 'dsm' else 'dtm'
  out.file <- file.path(out.dir, paste0(fire, '_', surface, '_5m.tif'))
  writeRaster(r.5, out.file)
}

# check
files <- list.files(out.dir, full.names = T)
plot(rast(files[2]))



# --- just for dixie ---
fire <- 'dixie'
in.dir <- paste0('data/processed/processed/tif/1m/')
out.dir <- paste0('data/processed/processed/tif/5m/', fire)
dir.create(out.dir, showWarnings = F, recursive = T)

files <- list.files(in.dir, pattern = paste0(fire, '_dtm.*\\.tif$'), full.names = T)
x <- rast(files[1])
res(x)

plan(multisession, workers = 10)

process_file <- function(f) {
  
  library(terra)
  
  message('Processing ', basename(f))
  
  r <- rast(f)
  
  # aggregate native 0.5 m raster to 5 m
  fact <- round(5 / res(r)[1])
  r.5 <- aggregate(r, fact = fact, fun = mean, na.rm = TRUE)
  
  # make 5 m EPSG:32610 template
  e.32610 <- project(ext(r.5), from = crs(r.5), to = 'epsg:32610')
  
  template <- rast(
    ext = e.32610,
    res = 5,
    crs = 'epsg:32610'
  )
  
  origin(template) <- c(0, 0)
  
  r.32610 <- project(
    r.5,
    template,
    method = 'bilinear'
  )
  
  surface <- if (grepl('_dsm', basename(f))) 'dsm' else 'dtm'
  
  acq <- sub(
    paste0('^', fire, '_', surface, '_1m_'),
    '',
    tools::file_path_sans_ext(basename(f))
  )
  
  out.file <- file.path(
    out.dir,
    paste0(fire, '_', surface, '_5m_', acq, '_32610.tif')
  )
  
  writeRaster(r.32610, out.file, overwrite = TRUE)
  
  out.file
}

out.files <- future_lapply(files, process_file)

plan(sequential)


# once all 5m acqs have been mosaiced:
files <- list.files(out.dir, pattern = '\\.tif$', full.names = T)

for (f in files) {
  r <- rast(f)
  cat('\n', basename(f), '\n')
  print(dim(r))
  print(ext(r))
  print(res(r))
  print(origin(r))
  print(crs(r, describe = TRUE)$code)
}

dem.acq <- lapply(files, rast)

# start with 1st raster and add each subsequent acq
combine <- dem.acq[[1]]

for (i in 2:length(dem.acq)) {
  # create new extent that contains both raster areas
  e <- union(ext(combine), ext(dem.acq[[i]]))
  
  # extend extent of OG raster
  combine.ext <- extend(combine, e)
  # extent extent of adding raster
  next.ext <- extend(dem.acq[[i]], e)
  
  # ensure extents are now the same
  print(compareGeom(combine.ext, next.ext, stopOnError = FALSE))
  
  # merge rasters
  combine <- cover(combine.ext, next.ext)
}

# check
plot(combine)

epsg <- crs(combine, describe = T)$code

# save
out.file <- file.path(out.dir, paste0(fire, '_dtm_5m.tif'))
writeRaster(combine, out.file, overwrite = TRUE)




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
season_rad <- function(fire, surface) {
  
  dir <- paste0('data/processed/processed/tif/5m/', fire, '/rad')
  files <- list.files(dir, pattern = paste0('^', fire, '_rad_global_', surface, '_day\\d+_5m\\.tif$'), full.names = TRUE)

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
  
  writeRaster(accum, file.path(dir, paste0(fire, '_rad_', surface, '_accum_5m.tif')), overwrite = TRUE)
  writeRaster(melt, file.path(dir, paste0(fire, '_rad_', surface, '_melt_5m.tif')), overwrite = TRUE)
  
  return(list(accum = accum, melt = melt))
}

# run function
fire <- 'dixie'

dtm.rad <- season_rad(fire, 'dtm')
dsm.rad <- season_rad(fire, 'dsm')


# ===========================================================================================
# resample to match SDD and SWE
# ===========================================================================================

resample_rad <- function() {
  
  r.file <- paste0('data/processed/processed/tif/5m/', fire, '/rad/', fire, '_rad_', surface, '_', season, '_5m.tif')
  
  r <- rast(r.file)
  
  if (res == '500m') {
    template.path <- paste0('data/processed/processed/tif/500m/', fire, '/', fire, '_sdd_500m.tif')
  } else {
    template.path <- paste0('data/processed/processed/tif/50m/', fire, '/', fire, '_swe_peak_50m.tif')
  }
  
  # read template (just the first layer)
  template <- rast(template.path)[[1]]

  # resample
  r.out <- exact_resample(r, template, fun = 'mean')
  
  # rename layer
  names(r.out) <- paste0('rad_', surface, '_', season)
  
  out.dir <- paste0('data/processed/processed/tif/', res, '/', fire, '/rad_metrics/')
  
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  
  # output file name
  out.name <- paste0(out.dir, fire, '_rad_', surface, '_', season, '_', res, '.tif')
  
  # write
  writeRaster(r.out, out.name, overwrite = TRUE)
  
  return(r.out)
}

# setup
fire <- 'caldor'
season <- 'melt' # melt or accum
surface <- 'dsm' # dtm or dsm
res <- '50m'

resample_rad()

# check
test <- rast(paste0('data/processed/processed/tif/', res, '/', fire, '/', fire, '_rad_', surface, '_', season, '_', res, '.tif'))
plot(test)

# loop through all
fires <- c('caldor', 'castle', 'dixie')
surfaces <- c('dtm', 'dsm')
seasons <- c('accum', 'melt')
resolutions <- c('50m', '500m')
fires <- c('dixie')

for (fire in fires) {
  for (surface in surfaces) {
    for (season in seasons) {
      for (res in resolutions) {
        
        message(
          'Processing ',
          fire, ' | ',
          surface, ' | ',
          season, ' | ',
          res
        )
        
        resample_rad()
      }
    }
  }
}

# ----- save as raster stack -----
fire <- 'castle'
res <- '50m'


files.dir <- paste0('data/processed/processed/tif/', res, '/', fire, '/rad_metrics/')
files <- list.files(files.dir, full.names = T)

rad <- rast(files)

plot(rad)

out.path <- paste0('data/processed/processed/tif/', res, '/', fire, '/', fire, '_rad_', res, '.tif')

writeRaster(rad, out.path)

# loop through all
fires <- c('caldor', 'castle', 'dixie')
resolutions <- c('50m', '500m')

for (fire in fires) {
  for (res in resolutions) {
    files.dir <- paste0('data/processed/processed/tif/', res, '/', fire, '/rad_metrics/')
    files <- list.files(files.dir, full.names = T)
    
    files <- sort(files)
    
    # confirm expected files were found
    if (length(files) == 0) {
      warning('No radiation files found for ', fire, ' at ', res)
      next
    }
    
    rad <- rast(files)

    out.path <- paste0('data/processed/processed/tif/', res, '/', fire, '/', fire, '_rad_', res, '.tif')
    
    writeRaster(rad, out.path, overwrite = TRUE)
  }
}



# ----- Combine into raster stack and save -----
resolutions <- c('50m', '500m')
fires <- c('caldor', 'castle', 'dixie')

# this code needs to be fixed slightly but not worth it 
# copy to backup
for (res in resolutions) {
  for (fire in fires) {
  in.dir <- paste0('data/processed/processed/tif/', res, '/', fire, '/')
  out.dir1 <- paste0('J:/Fire_Snow/fireandice/data/processed/processed/tif/', res, '/', fire, '/') 
  out.dir2 <- paste0('G:/Fire_Snow_Dynamics_backup/data/processed/processed/tif/', res, '/', fire, '/' )
  rad.file <- paste0(in.dir, fire, '_rad_', res, '.tif')
  rad.all.files <- list.files(paste0(in.dir, 'rad_metrics'), full.names = T)
  file.copy(rad.file, out.dir1, overwrite = TRUE)
  file.copy(rad.all.files, out.dir1, overwrite = TRUE)
  file.copy(rad.file, out.dir2, overwrite = TRUE)
  file.copy(rad.all.files, out.dir2, overwrite = TRUE)
  }
}


# troubleshoot
e <- union(ext(dem.acq[[1]]), ext(dem.acq[[2]]))
template <- rast(
  ext = e,
  resolution = 1,
  crs = 'EPSG:32610'
)
origin(template) <- c(0, 0)

dem.acq.32610 <- lapply(dem.acq, function(r) {
  project(
    r,
    template,
    method = 'near',
    align_only = TRUE
  )
})
plot(dem.acq.32610[[1]])
plot(dem.acq.32610[[2]], add = T)

combine <- dem.acq.32610[[1]]

for (i in 2:length(dem.acq.32610)) {
  # create new extent that contains both raster areas
  e <- union(ext(combine), ext(dem.acq.32610[[i]]))
  
  # extend extent of OG raster
  combine.ext <- extend(combine, e)
  # extent extent of adding raster
  next.ext <- extend(dem.acq.32610[[i]], e)
  
  # ensure extents are now the same
  print(compareGeom(combine.ext, next.ext, stopOnError = FALSE))
  
  # merge rasters
  combine <- cover(combine.ext, next.ext)
}

# check
plot(combine)

out.file <- paste0(dir.1m, fire, '_dtm_1m_32610.tif')
writeRaster(combine, out.file, overwrite = TRUE)

# ---------- troubleshoot ----------------------

# reproject last acquisiton of dixie to 5m and mosaic
dem.dir <- 'data/raw/DEM/dixie/CA_SierraNevada_6_2022'
dem.files <- list.files(dem.dir, full.names = T)
tile.out.dir <- 'data/processed/processed/tif/5m/dixie/dixie_dtm_5m_32610_acq6'
dir.create(tile.out.dir, showWarnings = FALSE, recursive = TRUE)

plan(multisession, workers = 10)

process_tile <- function(f) {
  
  library(terra)
  
  message('Processing ', basename(f))
  
  r <- rast(f)
  
  # aggregate native 1 m tile to 5 m first
  r.5 <- aggregate(r, fact = 5, fun = mean, na.rm = TRUE)
  
  # get projected extent in EPSG:32610
  e.32610 <- project(ext(r.5), from = crs(r.5), to = 'epsg:32610')
  
  # create aligned 5 m template
  template <- rast(
    ext = e.32610,
    res = 5,
    crs = 'epsg:32610'
  )
  
  origin(template) <- c(0, 0)
  
  # project onto template
  r.32610 <- project(
    r.5,
    template,
    method = 'bilinear'
  )
  
  out.file <- file.path(
    tile.out.dir,
    paste0(tools::file_path_sans_ext(basename(f)), '_5m_32610.tif')
  )
  
  writeRaster(r.32610, out.file, overwrite = TRUE)
  
  out.file
}

out.files <- future_lapply(dem.files, process_tile)

tile.files <- list.files(
  tile.out.dir,
  pattern = '\\.tif$',
  full.names = TRUE
)

length(tile.files)

r.col <- sprc(tile.files)

dixie.dtm.5m.acq6 <- mosaic(r.col, fun = 'mean')

out.mosaic <- 'data/processed/processed/tif/5m/dixie/dixie_dtm_5m_32610_acq6.tif'

writeRaster(
  dixie.dtm.5m.acq6,
  out.mosaic,
  overwrite = TRUE
)




chm <- project(chm, dtm, method = 'near')
writeRaster(chm, 'data/processed/processed/tif/5m/dixie/dixie_chm_5m.tif')
res(dtm)
origin(dtm)
