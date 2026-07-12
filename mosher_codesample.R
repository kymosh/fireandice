packages <- c('sf', 'mapview', 'lidR', 'dplyr', 'raster', 'future', 'future.apply', 'stringr', 'terra')
lapply(packages, library, character.only = T)

# generate 1m canopy height model from normalized lidar point clouds

# ------------------------------------------------------------------------------
# Setup 
# ------------------------------------------------------------------------------

# define fire and lidar acquisition to process
fire <- 'dixie' 
acq <- 'CA_SierraNevada_4_2022_low' 

# set base directory
j.dir <- 'data/processed' 

# path for normalized lidar tiles
norm.dir <- file.path(j.dir, paste0('laz/normalized/', fire), acq)

# read normalized lidar tiles as lascatalog
ctg.norm <- readLAScatalog(norm.dir) 

# to test or not to test
run.test <- FALSE # set TRUE for test, FALSE for full run


# ------------------------------------------------------------------------------
# Inspect normalized lidar data
# ------------------------------------------------------------------------------

# plot all tiles
plot(ctg.norm, mapview = TRUE, map.types = "Esri.WorldImagery")

# view 1 tile for exploratory checks
norm.file <- ctg.norm@data$filename[1]
las.norm <- readLAS(norm.file)

# check point classifications
table(las.norm$Classification)
# should only be classes 1 and 2 ("ground" and "unclassified")

# visualize point heights
plot(las.norm, color = 'Z', legend = TRUE)

# ground z distribution
summary(las.norm$Z[las.norm$Classification == 2])
quantile(las.norm$Z[las.norm$Classification == 2], c(0.05, 0.5, 0.95), na.rm = TRUE)
# should be centered near 0m

# unclassified z distribution 
summary(las.norm$Z[las.norm$Classification != 2])

# check lowest height across all points
min(las.norm$Z)


# --- check negative heights ---

# small negatives are normal and OK, but check for large or many negative values

# proportion of all points below 0m
sum(las.norm$Z < 0, na.rm = TRUE)
mean(las.norm$Z < 0, na.rm = TRUE)

# proportion of all points below -0.5m
sum(las.norm$Z < -0.5, na.rm = TRUE)
mean(las.norm$Z < -0.5, na.rm = TRUE)

# proportion of all points below -1m
sum(las.norm$Z < -1, na.rm = TRUE)
mean(las.norm$Z < -1, na.rm = TRUE)



# =================================================================================
# Canopy Height Model
# =================================================================================

# set number of workers for parallel processing
workers <- 10 # use 10 for processing comp, 6 for km comp

# tile IDs used for test runs
test.tiles <- c('11SKD4406', '11SKD4407', '11SKD4306', '11SKD4307')

# shouldn't need to change anything below!

# ----- Select tiles to process -----

# if run.test =  TRUE, just run on test tiles, if FALSE, run on full lascatalog

if (run.test) {
  
  test.files <- ctg.norm@data$filename[
    grepl(paste(test.tiles, collapse = '|'), basename(ctg.norm@data$filename))
  ]
  
  ctg.run <- readLAScatalog(test.files)
  
} else {
  
  ctg.run <- ctg.norm
}

# inspect
plot(ctg.run, chunk = TRUE)

# remove implausible normalized heights prior to creating chm
# keep only points between -0.25 m and 90 m 
opt_filter(ctg.run) <- '-drop_z_below -0.25 -drop_z_above 90'


# ----- settings -----

# output resolution (m)
res.m <- 1

# set output directory and create it if it doesn't already exist
out.dir <- file.path(j.dir, paste0('tif/1m/', fire, '/', fire, '_chm_6340/', acq))
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

# --- parallel processing and catalog options ---

# use future parallelization using catalog chunks (tiles)
plan(multisession, workers = workers) 
set_lidr_threads(1) # important to avoid nested parallelism

opt_progress(ctg.run) <- TRUE # show progress
opt_chunk_size(ctg.run) <- 0 # process one tile at a time
opt_chunk_buffer(ctg.run) <-  0 # buffer not needed for chm
opt_output_files(ctg.run) <- file.path(out.dir, paste0(fire, '_chm_{ORIGINALFILENAME}')) # output file names using the original tile name

# ----- generate chm -----

start.time <- Sys.time()

chm <- rasterize_canopy(ctg.run, res = res.m, algorithm = p2r(subcircle = 0.2), overwrite = TRUE)

print(Sys.time() - start.time)

# ----- QA -----

# read output chm tiles
tif.files <- list.files(out.dir, pattern = '\\.tif', full.names = T)
tifs <- lapply(tif.files, rast)

# plot one
plot(tifs[[1]])

# optional visual QA for test runs
if (run.test) {
  
  chm.vrt <- vrt(tifs)
  plot(chm.vrt)
  
}