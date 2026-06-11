packages <- c('sf', 'mapview', 'lidR', 'dplyr', 'raster', 'future', 'future.apply', 'stringr', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


# NOTE: the final normalization code was saved as a script in data/scripts and run through the command prompt

# ------------------------------------------------------------------------------
# Catalog setup 
# ------------------------------------------------------------------------------
fire <- 'dixie'
acq <- 'CA_SierraNevada_4_2022_low'

# pick depending on which computer
#j.dir <- 'data/processed/processed' # processing comp
j.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed' # km comp

# normalized tiles
norm.dir <- file.path(j.dir, paste0('laz/normalized/', fire), acq)
ctg.norm <- readLAScatalog(norm.dir) 

# to test or not to test
run.test <- FALSE # set TRUE for test, FALSE for full run

# ----- inspect -----
plot(ctg.norm)
plot(ctg.norm, mapview = T, map.types = "Esri.WorldImagery")

# look at one normalized tile
norm.file <- ctg.norm@data$filename[1]
las.norm <- readLAS(norm.file)

# table of classification points
table(las.norm$Classification)
# only classes 1 and 2, so just ground and "not ground"

# visualization
plot(las.norm, color = 'Z', legend = T)
plot(ctg.norm)

# ground Z distribution
summary(las.norm$Z[las.norm$Classification == 2])
quantile(las.norm$Z[las.norm$Classification == 2], c(0.05, 0.5, 0.95), na.rm = TRUE)
# we would expect lots of 0s (because it's ground!) and want to check for large outliers


# Non-ground (likely vegetation, buildings, etc.)
summary(las.norm$Z[las.norm$Classification != 2])

min(las.norm$Z)

hist(
  las.norm$Z[las.norm$Classification != 2 & las.norm$Z < 80],
  breaks = 100,
  main = 'Height above ground (non-ground points)',
  xlab  = 'Z (m)'
)

sum(las.norm$Z < 0, na.rm = TRUE)
mean(las.norm$Z < 0, na.rm = TRUE)

sum(las.norm$Z < -0.5, na.rm = TRUE)
mean(las.norm$Z < -0.5, na.rm = TRUE)

sum(las.norm$Z < -1, na.rm = TRUE)
mean(las.norm$Z < -1, na.rm = TRUE)

mean(las.norm$Z[las.norm$Classification != 2] < -0.5, na.rm = TRUE)
mean(las.norm$Z[las.norm$Classification != 2] < -1, na.rm = TRUE)


# test to determine which algorithm for CHM to use is in 00_canopy_height_model_alg_exploration

# =================================================================================
# Canopy Height Model
# =================================================================================

workers <- 8 # 10 for processing comp


# shouldn't need to change anything in this section!

test.tiles <- c('11SKD4406', '11SKD4407', '11SKD4306', '11SKD4307')

# if TRUE, just run on test tiles, if FALSE, keep full ctg
if (run.test) {
  
  test.files <- ctg.norm@data$filename[
    grepl(paste(test.tiles, collapse = '|'), basename(ctg.norm@data$filename))
  ]
  
  ctg.run <- readLAScatalog(test.files)
  
} else {
  
  ctg.run <- ctg.norm
}

plot(ctg.run, chunk = TRUE)

# filter points to remove obvious bad high/low points
opt_filter(ctg.run) <- '-drop_z_below -0.25 -drop_z_above 75 -drop_class 7 18'

# ----- CHM Settings -----

# 1m resolution
res.m <- 1

out.dir <- file.path(j.dir, paste0('tif/1m/', fire, '/', fire, '_chm_6340/', acq))
dir.create(out.dir, recursive = T, showWarnings = F)

# parallel processing settings
plan(multisession, workers = workers) 
set_lidr_threads(1) # important to avoid nested parallelism

opt_progress(ctg.run) <- TRUE
opt_chunk_size(ctg.run) <- 0 # process tile by tile
opt_chunk_buffer(ctg.run) <-  0 # buffer not needed for CHM
opt_laz_compression(ctg.run) <- TRUE   
opt_output_files(ctg.run) <- file.path(out.dir, paste0(fire, '_chm_{ORIGINALFILENAME}'))

# ----- run CHM -----

# started at 6:48pm on 5/17
# restart catalog processing at failed chunk
# opt_restart(ctg.run) <- 1

start.time <- Sys.time()
chm <- rasterize_canopy(ctg.run, res = res.m, algorithm = p2r(subcircle = 0.2), overwrite = TRUE)
print(Sys.time() - start.time)

# ----- check results -----
tif.files <- list.files(out.dir, pattern = '\\.tif', full.names = T)
tifs <- lapply(tif.files, rast)
plot(tifs[[1]])

test.tif <- do.call(mosaic, c(tifs, fun = 'max'))
plot(test.tif)

chm.vrt <- vrt(tifs)
plot(chm.vrt)


# START 10:13am 1/29/26
# END 16:07 1/30/26
# RUNTIME: 30 hours

##### NOTE: I ran this for the creek file (2889 tiles) and it took about 30 hours to run
# These were my parallel settings:
# plan(multisession, workers = 10)
# set_lidr_threads(1) # important to avoid nested parallelism

# ----- check results -----
chm.test <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_chm/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB8030_norm.tif')
plot(chm.test)
crs(chm.test, describe = T)$code
# CRS 6340
res(chm.test)
# 1 x 1



# ==============================================================================
#  Mosaic into single raster
# ==============================================================================
library(terra)
library(sf)
library(nhdplusTools)

# --- settings - change these ---
fire <- 'castle'
metric <- 'chm'

# out.dir.base <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/' # KM comp
out.dir.base <- 'data/processed/processed/tif/1m/' # processing comp

acqs <- c(
  'CA_SierraNevada_5_2022',
  'CA_SierraNevada_8_2022'
)

path <- paste0(out.dir.base, fire, '/', fire, '_chm_6340/', acqs[1])
files <- list.files(path, full.names = T)
r <- rast(files[1])

epsg <- 32610



# --- settings - don't touch ---
# study area + water
fire.shp <- read_sf(
  paste0('data/processed/processed/shp/studyarea_extents/study_extent_', fire, '_simple.shp')
)

water <- get_nhdphr(
  AOI = st_transform(fire.shp, 'EPSG:32610'),
  type = 'nhdwaterbody'
)

water <- st_transform(water, 'EPSG:6339')
water.v <- vect(water)

st_write(water, paste0('data/processed/processed/shp/nhd_water_', fire, '_6339.shp'))

# --- mosaic together ---
mosaic_acq <- function(acq) {
  
  message('Mosaicking ', acq, '...')
  
  out.dir <- paste0(
    out.dir.base,
    fire,
    '/',
    fire,
    '_chm_6340/',
    acq
  )
  
  files <- list.files(out.dir, pattern = '\\.tif$', full.names = TRUE)
  
  if (length(files) == 0) {
    warning('No files found for ', acq)
    return(NULL)
  }
  
  r.list <- lapply(files, rast)
  r.col <- sprc(r.list)
  
  m <- mosaic(r.col)
  names(m) <- names(r.list[[1]])
  
  m.masked <- mask(m, water.v, inverse = TRUE)
  names(m.masked) <- names(m)
  
  m.masked
}

# run function on each acq
masked.list <- lapply(acqs, mosaic_acq)
template <- masked.list[[1]]

masked.list.32610 <- lapply(masked.list, function(x) {
  
  r <- project(
    x,
    template,
    method = 'near',
    align_only = TRUE
  )
  
  names(r) <- names(x)
  r
})

masked.list <- masked.list.32610

# combine
combine <- masked.list[[1]]


for (i in 2:length(masked.list)) {
  
  e <- union(ext(combine), ext(masked.list[[i]]))
  
  combine.ext <- extend(combine, e)
  next.ext <- extend(masked.list[[i]], e)
  
  combine <- cover(combine.ext, next.ext)
  names(combine) <- names(masked.list[[1]])
}

plot(combine)

# save
out.file <- paste0(out.dir.base, fire, '/chm_1m_', epsg, '.tif')

writeRaster(combine, out.file, overwrite = TRUE)







# =================================================================================
# Reproject CHM to target CRS
# =================================================================================

# NOTE: this code has been used for dtm not just chm! Make sure in.dir is what you want
# in.dir  <- 'data/processed/processed/tif/1m/castle/castle_chm_6340'
# in.dir <- 'data/processed/processed/tif/1m/creek_chm_test_36'
in.dir <- 'data/raw/DEM/castle'
out.dir <- 'data/processed/processed/tif/1m/castle/castle_dtm_32611'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(in.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)

# check CRS
r <- rast(files[1])
crs(r, describe = T)$code

# ----- create template -----

# load in raster with correct CRS
template <- rast('data/processed/processed/tif/50m/castle/canopy_metrics/castle_cover_metrics_50m.tif')

crs(template, describe = T)$code # 32611
res(template)
origin(template)

target.crs <- crs(template)
dem.origin <- origin(template)

# ----- function to project + write one tile -----
project_one <- function(f, out.dir, target.crs, dem.origin) {
  
  library(terra)
  
  out.name <- file.path(out.dir, basename(f))
  if (file.exists(out.name)) return(out.name)  # skip if already done
  
  
  r <- rast(f)
  
  # convert tile extent -> polygon, project polygon -> target CRS, then get extent
  tile.poly <- as.polygons(ext(r))
  crs(tile.poly) <- crs(r)  # assign source CRS
  tile.poly.32611 <- project(tile.poly, target.crs)
  tile.ext.32611 <- ext(tile.poly.32611)
  
  # build 1m template cropped to this tile
  template <- rast(ext = tile.ext.32611,
                   res = 1,
                   crs = target.crs)
  
  # inherit DEM grid alignment
  origin(template) <- dem.origin
  
  rp <- project(r, template, method = 'bilinear')
  
  writeRaster(rp, out.name,
              overwrite = TRUE,
              wopt = list(gdal = c('COMPRESS=LZW'))
  )
  
  out.name
}


# ----- run in parallel -----
plan(multisession, workers = 8)
terraOptions(threads = 1)


#test on 5 tiles first
test.files <- files[1:5]

start.time <- Sys.time()
out.files <- future_lapply(files,
                           FUN = project_one,
                           out.dir = out.dir,
                           target.crs = target.crs,
                           dem.origin = dem.origin,
                           future.seed = TRUE)
end.time <- Sys.time()
message('Reproj finished at: ', format(end.time, '%Y-%m-%d %H:%M:%S'))
message('Elapsed minutes: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))
# elapsed time: 5 - 15.78 minutes

# ----- check -----
outs <- list.files(out.dir, pattern = '\\.tif$', full.names = TRUE)
samp <- outs[1:5]
x <- rast(samp[1])
plot(x)
crs(x, describe = TRUE)$code
res(x)


# alignment check
rlist <- lapply(samp, rast)
grid_aligned <- function(r, tol = 1e-6) {
  o <- origin(r)
  s <- res(r)
  e <- ext(r)
  
  ax <- abs(((xmin(e) - o[1]) / s[1]) - round((xmin(e) - o[1]) / s[1])) < tol
  ay <- abs(((ymin(e) - o[2]) / s[2]) - round((ymin(e) - o[2]) / s[2])) < tol
  
  ax && ay
}

aligned <- sapply(rlist, grid_aligned)
table(aligned)

# 
is_full_1km <- vapply(chm.files, function(f) {
  r <- rast(f)
  e <- ext(r)
  w <- xmax(e) - xmin(e)
  h <- ymax(e) - ymin(e)
  isTRUE(all.equal(w, 1000)) && isTRUE(all.equal(h, 1000))
}, logical(1))

table(is_full_1km)

chm.full <- chm.files[is_full_1km]
chm.partial <- chm.files[!is_full_1km]

length(chm.full)
length(chm.partial)


# =================================================================================
# Trim away partial tiles
# =================================================================================
# for some reason the NW corner of the Creek study area contains partial tiles that are messing up the metrics being calculated from the CHM.
# this code trims those away.

is_full_1km <- vapply(chm.files, function(f) {
  r <- rast(f)
  e <- ext(r)
  w <- xmax(e) - xmin(e)
  h <- ymax(e) - ymin(e)
  isTRUE(all.equal(w, 1000)) && isTRUE(all.equal(h, 1000))
}, logical(1))

table(is_full_1km)

chm.full <- chm.files[is_full_1km]
chm.partial <- chm.files[!is_full_1km]

length(chm.full)
length(chm.partial)

chm.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
trash.dir <- file.path(chm.dir, 'partial_tiles_removed')
dir.create(trash.dir, showWarnings = F)

file.rename(chm.partial, file.path(trash.dir, basename(chm.partial)))


# ----- build VRT for metric computation -----

chm.files <- list.files('data/processed/processed/tif/1m/creek_chm_32611',
                        pattern = '\\.tif$',
                        full.names = T)
length(chm.files)
vrt.path <- 'data/processed/processed/tif/1m/creek_chm_32611/creek_chm_1m_32611.vrt'
vrt(chm.files, filename = vrt.path, overwrite = TRUE)



# NOTE: Individual metrics were moved to their own separate script file










# ==============================================================================
#  Direction-Based Metrics
# ==============================================================================

# ------- set up parallelization
plan(multisession, workers = 4)

# --------- create buffer ----------

# # define buffer
# buffer.m <- 60 # should equal max_dist_m
# 
# # split into tiles
# 
# tiles <- makeTiles(chm, 
#                    y = c(1000, 1000), 
#                    buffer = ceiling(buffer.m / res(chm)[1]),
#                    value = 'collection')
# 
# tile.extents <- lapply(tiles, ext)
# rm(tiles)  # important: drop terra objects
# gc()

# ----- function for distance from North or South canopy -----
dir.sector.dist <- function(target_mask, from_mask,
                            sector = 'S',
                            max_dist_m = 60 # may need to increase this number when calculating on more tiles
                            ) {

  # --- this is how many "rings" we will search ---
  res.m <- res(target_mask)[1]
  max.k <- floor(max_dist_m / res.m) 
  
  # --- create blank raster with same geometry as target ---
  out <- rast(target_mask) 
  values(out) <- NA_real_
  
  # --- ensure masks have same spatial extent ---
  from_mask <- crop(from_mask, target_mask)
  
  
  for (k in 1:max.k) {
    
    # --- moves pixels north/south and form fan-shaped search region ---
    dy <- k * res.m
    horiz.range <- k * res.m
    
    # --- move target pixels in the decided direction ---
    if (sector == 'N') {
      shifted <- shift(target_mask, dy = -dy)
    }
    
    if (sector == 'S') {
      shifted <- shift(target_mask, dy = dy)
    }
    
    # --- allows lateral deviation ---
    for (dx in seq(-horiz.range, horiz.range, by = res.m)) {
      
      # shift by (dx, dy) and resample back onto original grid
      candidate <- resample(
        shift(shifted, dx = dx),
        target_mask,
        method = 'near'
      )
      
      # keep value only when gap pixel is found
      hit <- mask(candidate, from_mask)
      # euclidean distance 
      dist <- sqrt(dx^2 + dy^2)
      
      # --- if canopy is found, write distance
      out <- cover(out, ifel(!is.na(hit), dist, NA_real_))
    }
  }
  
  
  out
}




# # ----- scaling up ------
# dist.tiles <- lapply(seq_along(tile.extents), function(i) {
# 
#   message('Starting tile ', i)
# 
#   tile.ext <- tile.extents[[i]]
# 
#   chm.local <- rast('data/processed/processed/tif/1m/chm_test.tif')
# 
#   # recreate raster inside worker
#   chm.buf <- crop(chm.local, tile.ext)
# 
#   gap.mask.buf <- ifel(!is.na(chm.buf) & chm.buf < 2, 1, NA)
#   canopy.mask.buf <- ifel(!is.na(chm.buf) & chm.buf >= 2, 1, NA)
# 
#   dist.buf <- dir.sector.dist(
#     target_mask = gap.mask.buf,
#     from_mask   = canopy.mask.buf,
#     sector      = 'S',
#     max_dist_m  = 60
#   )
# 
#   # remove buffer safely
#   tile.core.ext <- ext(trim(chm.buf, pad = FALSE))
#   crop(dist.buf, tile.core.ext)
# })

# dist.canopy.south <- do.call(mosaic, dist.tiles)
# saveRDS(dist.canopy.south, 'data/processed/processed/rds/dist_canopy_south_test.rds')

# Phase 1 - test on 5 tiles, sequentially
chm <- rast('data/processed/processed/tif/1m/chm_test.tif')
gap.mask <- ifel(!is.na(chm) & chm < 2, 1, NA)
canopy.mask <- ifel(!is.na(chm) & chm >= 2, 1, NA)

# ----- run function and time it ----
start.time <- Sys.time()

dist.canopy.south <- dir.sector.dist(
  target_mask = gap.mask,
  from_mask = canopy.mask,
  sector = 'S',
  max_dist_m = 60
)

end.time <- Sys.time()
message(
  'Runtime: ',
  round(difftime(end.time, start.time, units = 'mins'), 2),
  ' minutes'
)
saveRDS(dist.canopy.south, 'data/processed/processed/rds/dist_canopy_south_test.rds')

terra::plot(
  terra::crop(dist.canopy.north, ext_small),
  col = viridisLite::viridis(100, direction = -1),
  main = 'Dist to Canopy - North: Zoom'
)


