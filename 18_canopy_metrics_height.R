packages <- c('lidR', 'dplyr', 'future', 'future.apply')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)




# ==============================================================================
#  Calculate Height Metrics
# ==============================================================================

# load in all catalogs
files.test.36 <- readRDS('J:/Structure_Data/Fire_Snow/fireandice/data/processed/processed/laz/test_laz_36.rds')
ctg.test.36 <- readLAScatalog(files.test.36)


files.test.5 <- files.test.36[1:5]
ctg.test.5 <- readLAScatalog(files.test.5)

# full normalized catalog
ctg.norm <- readLAScatalog('data/processed/processed/laz/normalized/creek') 

# ----- test on subset of 5 tiles first -----

# parallel
set_lidr_threads(1)
plan(multisession, workers = 2)

# catalog options
opt_progress(ctg.test.5) <- TRUE
opt_chunk_size(ctg.test.5) <- 0
opt_chunk_buffer(ctg.test.5) <- 0
opt_laz_compression(ctg.test.5) <- TRUE
opt_select(ctg.test.5) <- 'xyzc'


# outputs
out.dir.50m <- 'data/processed/processed/tif/50m/creek_height_test_5'
dir.create(out.dir.50m, recursive = TRUE, showWarnings = FALSE)
opt_output_files(ctg.test.5) <- file.path(out.dir.50m, 'height_{ORIGINALFILENAME}')

# run
height.stack.50m <- pixel_metrics(ctg.test.5,
                                 ~ height.metrics(Z, Classification),
                                 res = 50)
# 1m took 28 minutes on 5 tiles
# 50m took about 2 minutes
tifs.50m <- list.files(out.dir.50m, pattern = '\\.tif$', full.names = TRUE)
length(tifs.50m)

r.50 <- terra::rast(tifs.50m[1])
names(r.50)
plot(r.50$zmax)

# ----- test on block of 36 tiles -----

# baseline catalog settings (same for all benchmarks)
opt_progress(ctg.test.36) <- FALSE   # keep benchmarking clean
opt_chunk_size(ctg.test.36) <- 0     # tile-by-tile
opt_chunk_buffer(ctg.test.36) <- 0
opt_laz_compression(ctg.test.36) <- TRUE

opt_select(ctg.test.36) <- 'xyzc'

# function to figure out what parallel settings are most efficient
library(lidR)
library(future)

# consistent options for benchmarking
opt_progress(ctg.test.36) <- FALSE
opt_chunk_size(ctg.test.36) <- 0
opt_chunk_buffer(ctg.test.36) <- 0
opt_laz_compression(ctg.test.36) <- TRUE
opt_select(ctg.test.36) <- 'xyzc'  # only read what you need

bench.height50 <- function(workers, threads) {
  
  set_lidr_threads(threads)
  plan(multisession, workers = workers)
  
  out.dir <- file.path(
    'data/processed/processed/tif/50m/height_metrics/bench_36',
    paste0('w', workers, '_t', threads)
  )
  dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
  opt_output_files(ctg.test.36) <- file.path(out.dir, 'height50_{ORIGINALFILENAME}')
  
  t0 <- Sys.time()
  
  pixel_metrics(
    ctg.test.36,
    ~ height.metrics(Z, Classification),
    res = 50
  )
  
  round(as.numeric(difftime(Sys.time(), t0, units = 'mins')), 2)
}


bench.height50(workers = 12, threads = 1) # 9.73 min
bench.height50(workers = 6, threads = 2) # 14.24 min
bench.height50(workers = 2, threads = 4) # 39.64 min

# ----- final run -----

# parallel settings
set_lidr_threads(1)
plan(multisession, workers = 12)

# lidr options
opt_progress(ctg.norm) <- TRUE
opt_chunk_size(ctg.norm) <- 0
opt_chunk_buffer(ctg.norm) <- 0
opt_laz_compression(ctg.norm) <- TRUE
opt_select(ctg.norm) <- 'xyzc'  # only read what you need

# outputs
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/height_metrics'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
opt_output_files(ctg.norm) <- file.path(out.dir, 'height_{ORIGINALFILENAME}')

# RUN
start.time <- Sys.time()
height.stack.50m.full <- pixel_metrics(ctg.norm,
                                       ~ height.metrics(Z, Classification),
                                       res = 50)
end.time <- Sys.time()

message('Height metrics (50 m) finished at: ', format(end.time, '%Y-%m-%d %H:%M:%S'))
message('Elapsed minutes: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))

# height metrics took 1894 min (31.56 hours)

# ------- check ------
test <- rast("data/processed/processed/tif/50m/creek/canopy_metrics/6340/height_metrics_6340/height_USGS_LPC_CA_SierraNevada_B22_11SKB7840_norm.tif")

crs(test, describe = TRUE)$code
res(test)
plot(test)

# -------- reproject -------

dem50 <- rast('data/processed/processed/tif/50m/creek/other_metrics/nasadem_creek_50m_1524.tif')
crs(dem50, describe = T)$code
plot(dem50)
res(dem50)
origin(dem50)

height.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/6340/height_metrics_6340'
height.files <- list.files(height.dir, pattern = '\\.tif$', full.names = TRUE)
length(height.files)
test.height.files <- height.files[1:5]
length(test.height.files)

out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/height_metrics_32611'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

start.time <- Sys.time()
for (f in height.files) {
  
  r <- rast(f)
  
  r.32611 <- project(r, 'EPSG:32611', method = 'bilinear')
  
  # crop dem50 to this tile extent, snapping to dem50grid
  tmpl.tile <- crop(dem50, ext(r.32611), snap = 'out')
  
  r.align <- resample(r.32611, tmpl.tile)
  
  out.file <- file.path(out.dir, basename(f))
  writeRaster(r.align, out.file, overwrite = TRUE)
}
end.time <- Sys.time()
message('Elapsed minutes: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))

# ----- test -----
height.test <- rast('data/processed/processed/tif/50m/creek/canopy_metrics/height_metrics_32611/height_USGS_LPC_CA_SierraNevada_B22_11SKB8031_norm.tif')
plot(height.test)
origin(height.test)
crs(height.test, describe = T)$code
res(height.test)







# -----  this will replace the above script -----

# ==============================================================================
#  Height Metrics Function
# ==============================================================================

height.metrics <- function(z, cl) {
  
  z.canopy <- z[z > 2 & cl != 2]
  
  if (length(z.canopy) == 0) {
    example <- stdmetrics_z(1:10)
    out <- as.list(rep(NA_real_, length(example)))
    names(out) <- names(example)
    out$zmax_true <- NA_real_
  } else {
    out <- as.list(stdmetrics_z(z.canopy))
    out$zmax_true <- max(z.canopy)
  }
  
  # drop metric that is meaningless by construction
  out$pzabove2 <- NULL
  
  out
}

# ------ run height metrics function -----

# function to calculate 50m lidar-based canopy height metrics from normalized laz files
# save native EPSG:6340 outputs
# reproject/resample
# outputs to the appropriate UTM template grid (EPSG:32610 or EPSG:32611)

run.height.metrics <- function(fire, acq, run.test = TRUE) {
  
  # determine correct epsg code per fire
  epsg <- dplyr::case_when(
    fire %in% c('castle', 'creek') ~ 32611, 
    fire %in% c('caldor', 'dixie') ~ 32610
  )
  
  j.dir <- 'data/processed/processed' # base directory for PROCESSING COMP
  norm.dir <- file.path(j.dir, paste0('laz/normalized/', fire), acq) # where normalized files live
  ctg.norm <- readLAScatalog(norm.dir)
  
  # if run.test == TRUE, only run on 5 tiles. Select FALSE for full run
  if (run.test) {
    files <- list.files(norm.dir, pattern = '\\.la[sz]$', full.names = TRUE)
    test.files <- files[2:6]
    ctg.run <- readLAScatalog(test.files)
  } else {
    ctg.run <- ctg.norm
  }
  
  # filter out obviously bad values
  opt_filter(ctg.run) <- '-drop_z_below -0.25 -drop_z_above 75 -drop_class 7 18'
  
  # --- settings ---
  set_lidr_threads(1)
  plan(multisession, workers = ifelse(run.test, 2, 12))
  
  opt_progress(ctg.run) <- TRUE
  opt_chunk_size(ctg.run) <- 0
  opt_chunk_buffer(ctg.run) <- 0
  opt_laz_compression(ctg.run) <- TRUE
  opt_select(ctg.run) <- 'xyzc'
  
  # --- outputs ---
  # different directories depending on if test or not
  if (run.test) {
    out.dir <- paste0(j.dir, '/tif/50m/tests/', fire, '_height_test')
  } else {
    out.dir <- paste0(j.dir, '/tif/50m/', fire, '/canopy_metrics/height_metrics_6340')
  }
  
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  
  opt_output_files(ctg.run) <- file.path(out.dir, 'height_{ORIGINALFILENAME}')
  
  # --- run ---
  start.time <- Sys.time()
  
  # calculate metrics
  height.stack.50m <- pixel_metrics(
    ctg.run,
    ~ height.metrics(Z, Classification),
    res = 50
  )
  
  
  # --- reproject ---
  
  # templates reprojection
  
  template.32610 <- file.path(j.dir, 'tif/50m/snow_metrics/ASO_American_20230131_swe_50m_clipped.tif')
  template.32611 <- file.path(j.dir, 'tif/50m/snow_metrics/ASO_Kern_20240508_swe_50m_clipped.tif')
  
  # choose correct template
  if (epsg == 32610) {
    template <- rast(template.32610)
  } else if (epsg == 32611) {
    template <- rast(template.32611)
  }
  
  # reproject output to correct epsg
  height.stack.50m.proj <- terra::project(
    height.stack.50m,
    template,
    method = 'near',
    align_only = TRUE
  )
  
  # outdir for reprojected files
  out.dir.proj <- paste0(
    j.dir,
    '/tif/50m/',
    fire,
    '/canopy_metrics/height_metrics_',
    epsg
  )
  
  dir.create(out.dir.proj, showWarnings = FALSE, recursive = TRUE)
  
  # save
  writeRaster(
    height.stack.50m.proj,
    file.path(out.dir.proj, paste0(fire, '_height_metrics_', epsg, '.tif')),
    overwrite = TRUE
  )
  
  end.time <- Sys.time()
  message('Elapsed minutes: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))
  
  height.stack.50m.proj
}

# ==============================================================================
#  Mosaic into single raster
# ==============================================================================
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/height_metrics_32611'
files <- list.files(out.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)
raster.list <- lapply(files, rast)
raster.collection <- sprc(raster.list)

m <- mosaic(raster.collection)

# ----- mask out bodies of water -----
# read in shape file of study area
creek <- read_sf('data/processed/processed/shp/mosher_creek_studyarea/study_extent_creek_32611.shp')
# download nhd water data
water <- get_nhdphr(AOI = creek, type = 'nhdwaterbody')
m.masked <- mask(m, water, inverse = TRUE)

# save
writeRaster(m.masked, 'data/processed/processed/tif/50m/creek/canopy_metrics/creek_height_metrics_50m_32611_masked.tif', overwrite = TRUE)


