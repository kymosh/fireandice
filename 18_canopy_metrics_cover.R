packages <- c('lidR', 'dplyr', 'future', 'future.apply', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
#  Cover Metrics
# ==============================================================================

# this is the code to calculate laz - based pixel metrics for canopy cover and height

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


# ----- Catalog setup -----

fire <- 'caldor'
acq <- 'CA_SierraNevada_8_2022'

# pick depending on which computer
j.dir <- 'data/processed/processed' # processing comp
#j.dir <- 'J:/Structure_Data/Fire_Snow/fireandice/data/processed/processed'

# normalized tiles
norm.dir <- file.path(j.dir, paste0('laz/normalized/', fire), acq)
ctg.norm <- readLAScatalog(norm.dir) 


# to test or not to test
run.test <- TRUE # set TRUE for test, FALSE for full run

# if TRUE, just run on test tiles, if FALSE, keep full ctg
if (run.test) {
  
  files <- list.files(norm.dir, pattern = '\\.la[sz]$', full.names = TRUE)
  test.files <- files[2:6]
  ctg.run <- readLAScatalog(test.files)
  
} else {
  
  ctg.run <- ctg.norm
}

plot(ctg.run, chunk = TRUE)

# filter points to remove obvious bad high/low points
opt_filter(ctg.run) <- '-drop_z_below -0.25 -drop_z_above 75 -drop_class 7 18'


# ----- settings -----
set_lidr_threads(1)
plan(multisession, workers = ifelse(run.test, 2, 12))

# catalog options
opt_progress(ctg.run) <- TRUE
opt_chunk_size(ctg.run) <- 0
opt_chunk_buffer(ctg.run) <- 0
opt_laz_compression(ctg.run) <- TRUE
opt_select(ctg.run) <- 'xyzc'

# outputs
if (run.test) {
  out.dir <- paste0('data/processed/processed/tif/50m/tests/', fire, '_cover_test')
} else {
  out.dir <- paste0('data/processed/processed/tif/50m/', fire, '/canopy_metrics/cover_metrics_6340')
}

dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

opt_output_files(ctg.run) <- file.path(out.dir, 'cover_{ORIGINALFILENAME}')

# ----- run -----
start.time <- Sys.time()

cover.stack.50m <- pixel_metrics(
  ctg.run,
  ~ cover.metrics(Z, Classification),
  res = 50
)

end.time <- Sys.time()

message('Elapsed minutes: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))






# height metrics for creek took 1894 min (31.56 hours)
# cover metrics for creek took 1795 min 

# ------- check ------
test.files <- list.files(out.dir, full.names = T)
test1 <- rast(test.files[1])
test2 <- rast(test.files[2])

plot(test1)
crs(test1, describe = TRUE)$code
res(test1)
plot(test2)
plot(test)

# -------- reproject -------

dem50 <- rast('data/processed/processed/tif/50m/creek/other_metrics/nasadem_creek_50m_1524.tif')

cover.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/6340/cover_metrics_6340'
cover.files <- list.files(cover.dir, pattern = '\\.tif$', full.names = TRUE)
length(cover.files)
cover.files.test <- cover.files[1:5]

out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/cover_metrics_32611'

dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

start.time <- Sys.time()
for (f in cover.files) {
  
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
# creek data took 23.28 min

# ----- check -----
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/canopy_metrics/cover_metrics_32611'
test3 <- rast(file.path(out.dir, 'cover_USGS_LPC_CA_SierraNevada_B22_11SKB7940_norm.tif'))
test4 <- rast(file.path(out.dir, 'cover_USGS_LPC_CA_SierraNevada_B22_11SKB7840_norm.tif'))

plot(test3)
crs(test3, describe = TRUE)$code
res(test3)
origin(test3)
plot(test4)
crs(test4, describe = TRUE)$code
res(test4)
origin(test4)

# ------ run cover metrics function -----

# function to calculate 50m lidar-based canopy cover metrics from normalized laz files
# save native EPSG:6340 outputs
# reproject/resample
# outputs to the appropriate UTM template grid (EPSG:32610 or EPSG:32611)

run.cover.metrics <- function(fire, acq, template, run.test = TRUE) {
  
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
    out.dir <- paste0(j.dir, '/tif/50m/', fire, '/canopy_metrics/cover_metrics_6340')
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
  cover.stack.50m.proj <- terra::project(
    cover.stack.50m,
    template,
    method = 'near'
  )
  
  # outdir for reprojected files
  out.dir.proj <- paste0(
    j.dir,
    '/tif/50m/',
    fire,
    '/canopy_metrics/cover_metrics_',
    epsg
  )
  
  dir.create(out.dir.proj, showWarnings = FALSE, recursive = TRUE)
  
  # save
  writeRaster(
    cover.stack.50m.proj,
    file.path(out.dir.proj, paste0(fire, '_cover_metrics_', epsg, '.tif')),
    overwrite = TRUE
  )
  
  end.time <- Sys.time()
  message('Elapsed minutes: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))
  
  cover.stack.50m.proj
}


# ----- run function -----

cover.stack.50m <- run.cover.metrics(
  fire = 'caldor',
  acq = 'CA_SierraNevada_5_2022',
  run.test = TRUE
)






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

snow.files <- list.files(file.path(j.dir, 'tif/50m/snow_metrics'), pattern = '\\.tif', full.names = T)
snow <- lapply(snow.files, rast)

snow.check <- data.frame(
  file = basename(snow.files),
  crs = sapply(snow, crs),
  xres = sapply(snow, function(x) res(x)[1]),
  yres = sapply(snow, function(x) res(x)[2]),
  xorigin = sapply(snow, function(x) origin(x)[1]),
  yorigin = sapply(snow, function(x) origin(x)[2])
)

snow.check


