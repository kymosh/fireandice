packages <- c('lidR', 'dplyr', 'future', 'future.apply')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


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


# ==============================================================================
#  Mosaic into single raster
# ==============================================================================
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/height_metrics_32611'
files <- list.files(out.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)
raster.list <- lapply(files, rast)
raster.collection <- sprc(raster.list)

m <- mosaic(raster.collection)

write.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics'
out.m <- file.path(write.dir, 'creek_height_metrics_50m_32611.tif')
writeRaster(m, out.m, overwrite = T, 
            wopt = list(gdal = c('COMPRESS=LZW', 'TILED=YES', 'BIGTIFF=YES')))

plot(m$zmax)

