packages <- c('lidR', 'dplyr', 'future', 'future.apply')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
#  Cover Metrics
# ==============================================================================

cover.metrics <- function(z, cl) {
  
  ok <- !is.na(z) & !is.na(cl)
  z  <- z[ok]
  cl <- cl[ok]
  
  n <- length(z)
  if (n == 0) return(list(
    canopy_open_2m = NA_real_,
    cover_2m = NA_real_,
    pzabove5 = NA_real_,
    pzabove10 = NA_real_,
    ground_frac = NA_real_
  ))
  
  canopy_open_2m <- sum(z < 2) / n
  
  list(
    canopy_open_2m = canopy_open_2m,
    cover_2m = 1 - canopy_open_2m,
    pzabove5 = sum(z > 5) / n,
    pzabove10 = sum(z > 10) / n,
    ground_frac = sum(cl == 2) / n
  )
}




# ==============================================================================
#  Cover Metrics Run
# ==============================================================================

ctg.norm <- readLAScatalog('data/processed/processed/laz/normalized/creek') 

# ----- test on 5 tiles -----

dir.test.36 <- 'data/processed/ALS/normalized_laz_test_tiles'
files.test.36 <- list.files(dir.test.36, full.names = TRUE)
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
out.dir.50m <- 'data/processed/processed/tif/50m/creek/cover_test_5'
dir.create(out.dir.50m, recursive = TRUE, showWarnings = FALSE)
opt_output_files(ctg.test.5) <- file.path(out.dir.50m, 'cover_{ORIGINALFILENAME}')

# run
cover.stack.50m <- pixel_metrics(ctg.test.5,
                                  ~ cover.metrics(Z, Classification),
                                  res = 50)


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
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/cover_metrics_6340'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
opt_output_files(ctg.norm) <- file.path(out.dir, 'cover_{ORIGINALFILENAME}')

# RUN
start.time <- Sys.time()
cover.stack.50m.full <- pixel_metrics(ctg.norm,
                                       ~ cover.metrics(Z, Classification),
                                       res = 50)
end.time <- Sys.time()

message('Cover metrics (50 m) finished at: ', format(end.time, '%Y-%m-%d %H:%M:%S'))
message('Elapsed minutes: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))

# height metrics took 1894 min (31.56 hours)

# ------- check ------
test <- rast("data/processed/processed/tif/50m/creek/canopy_metrics/cover_metrics_6340/height_USGS_LPC_CA_SierraNevada_B22_11SKB7840_norm.tif")

crs(test, describe = TRUE)$code
res(test)
plot(test)

# -------- reproject -------

dem50 <- rast('data/processed/processed/tif/50m/creek/topo_climate_fire_metrics/nasadem_creek_50m_1524.tif')
crs(dem50, describe = T)$code
plot(dem50)
res(dem50)

cover.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/cover_metrics_6340'
cover.files <- list.files(covert.dir, pattern = '\\.tif$', full.names = TRUE)

out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/cover_metrics_32611'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

terraOptions(progress = 1)

for (f in cover.files) {
  r <- rast(f)
  
  r.32611 <- project(r, dem50, method = 'bilinear')
  
  out.file <- file.path(out.dir, basename(f))
  writeRaster(r.32611, out.file, overwrite = TRUE)
}