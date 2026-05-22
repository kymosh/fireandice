packages <- c('lidR', 'dplyr', 'future', 'future.apply', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
#  Cover Metrics
# ==============================================================================

# this is the code to calculate laz - based pixel metrics for canopy cover

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


# ------ run cover metrics function -----

# function to calculate 50m lidar-based canopy cover metrics from normalized laz files
# save native EPSG:6340 outputs
# reproject/resample
# outputs to the appropriate UTM template grid (EPSG:32610 or EPSG:32611)

run.cover.metrics <- function(fire, acq, run.test = TRUE) {
  
  # determine correct epsg code per fire
  epsg <- dplyr::case_when(
    fire == 'castle' ~ 32611, 
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
    out.dir <- paste0(j.dir, '/tif/50m/tests/', fire, '_cover_test')
  } else {
    out.dir <- paste0(j.dir, '/tif/50m/', fire, '/canopy_metrics/cover_metrics_6340/', acq)
  }
  
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  
  opt_output_files(ctg.run) <- file.path(out.dir, 'cover_{ORIGINALFILENAME}')
  
  # --- run ---
  start.time <- Sys.time()
  
  # calculate metrics
  cover.stack.50m <- pixel_metrics(
    ctg.run,
    ~ cover.metrics(Z, Classification),
    res = 50
  )
  
  
  # --- reproject per tile ---
  
  template.32610 <- file.path(j.dir, 'tif/50m/snow_metrics/ASO_American_20230131_swe_50m_clipped.tif')
  template.32611 <- file.path(j.dir, 'tif/50m/snow_metrics/ASO_Kern_20240508_swe_50m_clipped.tif')
  
  # determine epsg 
  if (epsg == 32610) {
    template <- rast(template.32610)
  } else if (epsg == 32611) {
    template <- rast(template.32611)
  } else {
    stop('No template defined for EPSG: ', epsg)
  }
  
  # define out.dir depending on if test or not
  if (run.test) {
    
    out.dir.proj <- paste0(
      j.dir, '/tif/50m/tests/',
      fire, '_cover_test_proj'
    )
    
  } else {
    
    out.dir.proj <- paste0(
      j.dir, '/tif/50m/', fire,
      '/canopy_metrics/cover_metrics_', epsg,
      '/', acq
    )
  }
  
  dir.create(out.dir.proj, showWarnings = FALSE, recursive = TRUE)
  
  native.files <- list.files(out.dir, pattern = '^cover_.*\\.tif$', full.names = TRUE)
  
  proj.files <- character(length(native.files))
  
  for (i in seq_along(native.files)) {
    
    f <- native.files[i]
    r <- rast(f)
    
    r.proj <- terra::project(
      r,
      template,
      method = 'near',
      align_only = TRUE
    )
    
    # preserve layer names
    names(r.proj) <- names(r)
    
    proj.files[i] <- file.path(out.dir.proj, basename(f))
    
    writeRaster(
      r.proj,
      proj.files[i],
      overwrite = TRUE
    )
  }
  
  end.time <- Sys.time()
  message('Elapsed minutes: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))
  
  proj.files
}


# ----- run function -----

# --- caldor ---
cover.stack.50m <- run.cover.metrics(
  fire = 'caldor',
  acq = 'CA_SierraNevada_8_2022',
  run.test = FALSE
)
# done


cover.stack.50m <- run.cover.metrics(
  fire = 'caldor',
  acq = 'CA_SierraNevada_5_2022',
  run.test = FALSE
)

# --- castle ---
cover.stack.50m <- run.cover.metrics(
  fire = 'castle',
  acq = 'CA_SierraNevada_9_14_2022',
  run.test = FALSE
)

# --- dixie ---
cover.stack.50m <- run.cover.metrics(
  fire = 'dixie',
  acq = 'CA_SierraNevada_6_2022',
  run.test = FALSE
)
# started, didn't finish because I never updated script :(

cover.stack.50m <- run.cover.metrics(
  fire = 'dixie',
  acq = 'CA_SierraNevada_4_2022',
  run.test = FALSE
)

cover.stack.50m <- run.cover.metrics(
  fire = 'dixie',
  acq = 'CA_SierraNevada_7_2022',
  run.test = FALSE
)

# height metrics for creek took 1894 min (31.56 hours)
# cover metrics for creek took 1795 min 

# ----- check -----

# check test
out.dir <- 'data/processed/processed/tif/50m/tests/caldor_cover_test'
out.dir <- 'data/processed/processed/tif/50m/caldor/canopy_metrics/cover_metrics_32610/CA_SierraNevada_5_2022'
test.files <- list.files(out.dir, pattern = '\\.tif', full.names = T)
test <- lapply(test.files, rast)
test.m <- do.call(mosaic, test)
plot(test.m)
plot(rast(test.files[1]))
crs(test.m, describe = T)$code
res(test.m)
origin(test.m)

# check final
out.dir <- 'data/processed/processed/tif/50m/caldor/canopy_metrics/cover_metrics_6340/CA_SierraNevada_8_2022'
test.files <- list.files(out.dir, pattern = '\\.tif', full.names = T)
test <- lapply(test.files, rast)
test.m <- do.call(mosaic, test)
plot(rast(test.files[1]))

plot(test.m)
plot(test3)
crs(test.m, describe = TRUE)$code
res(test3)
origin(test3)
plot(test4)
crs(test4, describe = TRUE)$code
res(test4)
origin(test4)




# ==============================================================================
#  Mosaic into single raster
# ==============================================================================

out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics'
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

cover.masked <- mask(m, water, inverse = TRUE)
writeRaster(cover.masked, 'data/processed/processed/tif/50m/creek/canopy_metrics/creek_cover_metrics_50m_32611_masked.tif', overwrite = TRUE)






# delete later

# fix names
cover.names <- c(
  'canopy_open_2m',
  'cover_2m',
  'pzabove5',
  'pzabove10',
  'ground_frac'
)

for (f in test.files) {
  r <- rast(f)
  names(r) <- cover.names
  writeRaster(r, f, overwrite = TRUE)
}