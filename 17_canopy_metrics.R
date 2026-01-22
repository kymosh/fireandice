packages <- c('terra', 'sf', 'mapview', 'lidR', 'dplyr', 'ForestGapR', 'raster', 'future', 'future.apply')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


#----------------------
# Catalog setup 
#----------------------

ctg <- readLAScatalog('data/raw/ALS/laz/creek_fire') # change this to ALS/creek when ready to run on all tiles
ctg.full <- readLAScatalog('J:/Structure_Data/Fire_Snow/fireandice/data/raw/ALS/laz_creek')
plot(ctg.full)

# =================================================================================
# test on 36 tiles 
# =================================================================================
d <- ctg.full@data
nms <- names(d)

# --- find extent columns (names vary by lidR version) ---
xmn_name <- nms[grep('Min\\.X|^xmin$|Xleft',   nms, ignore.case = TRUE)][1]
xmx_name <- nms[grep('Max\\.X|^xmax$|Xright',  nms, ignore.case = TRUE)][1]
ymn_name <- nms[grep('Min\\.Y|^ymin$|Ybottom', nms, ignore.case = TRUE)][1]
ymx_name <- nms[grep('Max\\.Y|^ymax$|Ytop',    nms, ignore.case = TRUE)][1]

xmn <- d[[xmn_name]]; xmx <- d[[xmx_name]]
ymn <- d[[ymn_name]]; ymx <- d[[ymx_name]]

# --- your target point ---
x0 <- 310000
y0 <- 4130000

# --- block size ---
block_m <- 6000  # 6 km -> ~36 1km tiles

# define bbox centered on (x0, y0)
xmin_b <- x0 - block_m/2
xmax_b <- x0 + block_m/2
ymin_b <- y0 - block_m/2
ymax_b <- y0 + block_m/2

# tiles that intersect the bbox
keep <- (xmx > xmin_b) & (xmn < xmax_b) &
  (ymx > ymin_b) & (ymn < ymax_b)

files.sub <- d$filename[keep]
length(files.sub)

ctg <- readLAScatalog(files.sub)
plot(ctg)
plot(ctg, mapview = TRUE, map.types = "Esri.WorldImagery")  # Interactive map of catalog tiles with Esri imagery basemap
plot(ctg, chunk = TRUE)

#filter out unwanted points 
opt_filter(ctg) <- '-drop_class 7 18 -drop_withheld'

# =================================================================================
# Process laz files
# =================================================================================

# ------------------------ lidR parallelism setup ------------------------
plan(sequential)
set_lidr_threads(14)     
opt_laz_compression(ctg) <- TRUE
opt_progress(ctg) <- TRUE
opt_chunk_size(ctg) <- 0 # 0 = 1000
opt_chunk_buffer(ctg) <- 20 # check that this is enough

# set output file names
# NOTE if rerunning, make sure this folder is empty
opt_output_files(ctg) <- 'data/processed/ALS/normalized/tile_norm_{XLEFT}_{YBOTTOM}'

# using TIN because we have good ground point classification already
system.time(
  ctg.norm <- normalize_height(ctg, tin())
)

# --------------------- future parallelism setup ------------------------
set_lidr_threads(1)
plan(multisession, workers = 14)

files <- ctg@data$filename
filter_str <- opt_filter(ctg)

# ---  Normalize ---
normalize_one <- function(f, filter_str) {
  out <- file.path(
    'data/processed/ALS/normalized',
    paste0(tools::file_path_sans_ext(basename(f)), '_norm.laz')
  )
  
  # only runs if file does not exist
  if (file.exists(out) && file.info(out)$size > 50 * 1024^2) return(TRUE)
  
  las <- readLAS(f, filter = filter_str)
  if (is.empty(las)) return(FALSE)
  
  lasn <- normalize_height(las, tin())
  writeLAS(lasn, out)
  TRUE
}

system.time({
  ok <- future_sapply(files, normalize_one, filter_str = filter_str)
})

table(ok, useNA = 'ifany')

plan(sequential)

# normalize heights using point cloud
# NOTE: I am using the point cloud, not a DTM here to normalize. This is computationally heavier than using a DTM. May have to use DTM when processing entire study area dataset
#library(future)
#plan(multisession, workers = 4)
#set_lidr_threads(1)   

# using TIN because we have good ground point classification already
system.time(
ctg.norm <- normalize_height(ctg, tin())
)

saveRDS(ctg.norm, 'data/processed/processed/rds/ctg_norm_test_rds')
ctg.norm <- readRDS('data/processed/processed/rds/ctg_norm_test_rds')

# look at one normalized tile
norm.file <- ctg.norm@data$filename[1]
las.norm <- readLAS(norm.file)

# visualization
plot(las.norm, color = 'Z', legend = T)
plot(ctg.norm)

# ground Z distribution
summary(las.norm$Z[las.norm$Classification == 2])
quantile(las.norm$Z[las.norm$Classification == 2], c(0.05, 0.5, 0.95), na.rm = TRUE)
# all zero, which is what we would expect (because it's ground!)

# Non-ground (likely vegetation, buildings, etc.)
summary(las.norm$Z[las.norm$Classification != 2])

min(las.norm$Z)

hist(
  las.norm$Z[las.norm$Classification != 2 & las.norm$Z < 80],
  breaks = 100,
  main = 'Height above ground (non-ground points)',
  xlab  = 'Z (m)'
)

res.m <- 1

# clear output pattern
# NOTE: necessary step to avoid overwrite issues if rerunning
opt_output_files(ctg.norm) <- ""

# ------ canopy height model --------

chm <- rasterize_canopy(ctg.norm, res = 1, algorithm = pitfree())
saveRDS(chm, 'data/processed/processed/rds/chm_test.rds')
writeRaster(chm, 'data/processed/processed/tif/1m/chm_test.tif', overwrite = TRUE) 

#chm <- readRDS('data/processed/processed/rds/chm_test.rds')

# check CRS
crs(chm, describe = T)$code
# CRS 6340
res(chm)
# 1 x 1

# reproject to 32611
chm <- project(chm, 'EPSG:32611')
crs(chm, describe = T)$code
chm[chm < 0] <- 0

writeRaster(chm, 'data/processed/processed/tif/1m/chm_test.tif', overwrite = TRUE) 
saveRDS(chm, 'data/processed/processed/rds/chm_test.rds')
# ==============================================================================
# Calculate canopy metrics 
# ==============================================================================

# clear output pattern
# NOTE: necessary step to avoid overwrite issues if rerunning
opt_output_files(ctg.norm) <- ""

# ------- height metrics --------

height.metrics <- function(z, cl) {
  z.canopy <- z[z > 2 & cl != 2]
  
  if (length(z.canopy) == 0) {
    example <- stdmetrics_z(1:10)
    out <- as.list(rep(NA_real_, length(example)))
    names(out) <- names(example)
    out$zmax_true <- NA_real_
    return(out)
  }
  
  std <- stdmetrics_z(z.canopy)
  out <- as.list(std)
  
  out$zmax_true <- max(z.canopy)
  
  return(out)
}

height.stack <- pixel_metrics(ctg.norm, ~ height.metrics(Z, Classification), res = 1)
# need to get delete pzabove 2 since it's 100%

# reproject to 32611
height.stack <- project(height.stack, 'EPSG:32611')
saveRDS(height.stack, 'data/processed/processed/rds/height_test.rds')
# height.stack <- readRDS('data/processed/processed/rds/height_test.rds')

# ------- cover metrics --------

cover.metrics <- function(z, cl) {
  n_all = length(z)
  list(
    pzabove2 = sum(z > 2) / n_all,
    pzabove5 = sum(z > 5) / n_all,
    pzabove10 = sum(z > 10) / n_all,
    p_open = 1 - (sum(z > 2) / n_all),
    gap_frac_pc = sum(z < 2) / n_all,
    ground_frac_pc = sum(cl == 2) / n_all)
}

cover.stack <- pixel_metrics(ctg.norm, ~ cover.metrics(Z, Classification), res = 1)

# reproject to 32611
cover.stack <- project(cover.stack, 'EPSG:32611')
saveRDS(cover.stack, 'data/processed/processed/rds/cover_test.rds')
# cover.stack <- readRDS('data/processed/processed/rds/cover_test.rds')

# ------- PAD/PAI metrics --------
# Compute Plant Area Density (PAD) and Plant Area Index (PAI)
# from LiDAR point heights "z".
# LAD/PAD tells us the vertical distribution of plant material.

pad.metrics <- function(z) {
  # divide the canopy into height bins of size 'dz'
  pad.profile <- try(lad(z, dz = 1), silent = T) # use 1m vertical slices
  
  # If lad() failed OR returned nothing, we return all NAs.
  if (inherits(pad.profile, "try-error") ||
      is.null(pad.profile) ||
      nrow(pad.profile) == 0) {
    
    return(list(
      PAI      = NA_real_,   # total plant area
      PAD_mean = NA_real_,   # average density
      PAD_SD   = NA_real_,   # variability in density
      PAD_CV   = NA_real_,   # relative variability
      PAD_max  = NA_real_,   # densest canopy layer
      H_PADmax = NA_real_    # height of densest layer
    ))
  }
  
  # extract PAD values and corresponding height bins
  pad_vals <- pad.profile$lad # plant area density at each height
  heights <- pad.profile$z    # height (in m)
  
  keep <- heights >= 1
  pad_vals <- pad_vals[keep]
  heights  <- heights[keep]
  
  
  # If nothing left after filtering, return NAs.
  if (length(pad_vals) == 0) {
    return(list(
      PAI      = NA_real_,
      PAD_mean = NA_real_,
      PAD_SD   = NA_real_,
      PAD_CV   = NA_real_,
      PAD_max  = NA_real_,
      H_PADmax = NA_real_
    ))
  }
  
  # compute PAD/PAI metrics
  pai_val <- sum(pad_vals, na.rm = T)               # total plant area
  pad_mean_val <- mean(pad_vals, na.rm = T)         # average plant density
  pad_max_val <- max(pad_vals, na.rm = T)
  pad_sd_val <- sd(pad_vals, na.rm = T)             # variability
  pad_cv_val <- pad_sd_val / pad_mean_val           # coefficient of variablity
  H_padmax_val <- heights[which.max(pad_vals)]      # height where max density occurs
  
  # Return named list for rasterization
  return(list(
    PAI      = pai_val,
    PAD_mean = pad_mean_val,
    PAD_SD   = pad_sd_val,
    PAD_CV   = pad_cv_val,
    PAD_max  = pad_max_val,
    H_PADmax = H_padmax_val
  ))
  
}

pad.stack <- pixel_metrics(ctg.norm, ~ pad.metrics(Z), res = 1)

pad.stack <- project(pad.stack, 'EPSG:32611')
saveRDS(pad.stack, 'data/processed/processed/rds/pad_test.rds')
# pad.stack <- readRDS('data/processed/processed/rds/pad_test.rds')


# ==============================================================================
#  Fractal Dimension 
# ==============================================================================

boxcount.fractal.dim <- function(mat, box.sizes) {
  
  # mat: matrix with 1 = gap, NA = no gap
  # box sizes: vector of box sizes (in pixels)
  
  mat[is.na(mat)] <- 0
  
  n.box <- is.numeric(length(box.sizes))
  
  for (i in seq_along(box.sizes)) {
    
    bs <- box.sizes[i]
    
    # trim matrix so dimensions divisible by box size
    nr <- nrow(mat) - (nrow(mat) %% bs)
    nc <- ncol(mat) - (ncol(mat) %% bs)
    
    m <- mat[1:nr, 1:nc, drop = F]
    
    # reshape into blocks
    m <- array(
      m, dim = c(bs, nr / bs, bs, nc / bs)
    )
    
    # sum within each block
    block.sum <- apply(m, c(2, 4), sum)
    
    # count occupied boxes
    n.box[i] <- sum(block.sum > 0)
  }
  
  # remove zero-count scales
  keep <- n.box > 0 
  
  if(sum(keep) < 2) return(NA_real_)
  
  fit <- lm(log(n.box[keep]) ~ log(1 / box.sizes[keep]))
  
  coef(fit)[2]
  
}


box.sizes <- c(1, 2, 5, 10, 25)

fractal.dim.fun <- function(v, ...) {
  
  mat <- matrix(v, nrow = 50, ncol = 50, byrow = TRUE)
  
  boxcount.fractal.dim(
    mat = mat,
    box.sizes = box.sizes
  )
}

fractal.dim.50m <- aggregate(gap.mask.1m, fact = 50, fun = fractal.dim.fun)




# ==============================================================================
#  Gap Metrics
# ==============================================================================

terraOptions(memfrac = 0.7)


# define gap as height is less than 2m
gap.mask.1m <- ifel(!is.na(chm) & chm < 2, 1, NA)
names(gap.mask.1m) <- 'gap_mask'

# check
table(values(gap.mask.1m), useNA = "ifany")
plot(gap.mask.1m)

# create individual gaps with their own ID numbers
gap.id.1m <- patches(gap.mask.1m, directions = 8)
names(gap.id.1m) <- 'gap_id'

summary(gap.id.1m)

# add gap size
gaps <- values(gap.id.1m)
gaps <- gaps[!is.na(gaps)]

# calculate gap areas
gap.areas <- table(gaps) * prod(res(gap.id.1m))
# create df of indiv gaps and their areas
gap.df <- data.frame(gap_ID = as.integer(names(gap.areas)),
                     gap_area_m2 = as.numeric(gap.areas))
# inspect
head(gap.df)
summary(gap.df$gap_area_m2)

##### break into ecologically meaningful gap size bins
gap.df$gap_class <- cut(
  gap.df$gap_area_m2,
  breaks = c(0, 10, 100, 1000, Inf),
  labels = c('small', 'medium', 'large', 'xlarge')
)

# map gap class back to the raster
gap.lookup <- data.frame(gap_id = gap.df$gap_ID,
                         gap_class = as.integer(gap.df$gap_class))

gap.class.1m <- classify(gap.id.1m, rcl = as.matrix(gap.lookup))

# ------- aggregate to 50m -------
fact <-  50 / res(gap.class.1m)[1]

# core gap pct
gap.pct.50 <- aggregate(
  !is.na(gap.id.1m),
  fact = fact,
  fun = mean,
  na.rm = T
)
names(gap.pct.50) <- 'gap_pct'

# gap-specific percentages
gap_small <- aggregate(
  gap.class.1m == 1,
  fact = fact,
  fun = mean,
  na.rm = T
)
gap_medium <- aggregate(
  gap.class.1m == 2,
  fact = fact,
  fun = mean,
  na.rm = T
)
gap_large <- aggregate(
  gap.class.1m == 3,
  fact = fact,
  fun = mean,
  na.rm = T
)
gap_xlarge <- aggregate(
  gap.class.1m == 4,
  fact = fact,
  fun = mean,
  na.rm = T
)

names(gap_small)  <- 'gap_small_pct'
names(gap_medium) <- 'gap_medium_pct'
names(gap_large)  <- 'gap_large_pct'
names(gap_xlarge) <- 'gap_xlarge_pct'



# visualize
plot(gap_large)

ext_small <- ext(308350, 308550, 4135500, 4135700)

terra::plot(
  terra::crop(gap.class.1m, ext_small),
  col = viridisLite::viridis(100, direction = -1),
  main = 'Gap Mask: Zoom'
)

plot(gap.class.1m, col = viridisLite::viridis(100, direction = -1))

ext_chm <- ext(308000, 309000, 4135000, 4139000) 
terra::plot(
  terra::crop(swe.rast$cbibc, ext_chm),
  col = viridisLite::viridis(100),
  main = 'Gap Mask: Zoom'
)

# ------- distance to gap/canopy -----------------

# recalculate gap mask to keep NAs (necessary for dist calculation)
gap.mask.for.dist <- ifel(!is.na(chm) & chm < 2, 1, NA)
dist.to.gap.all <- distance(gap.mask.for.dist)
names(dist.to.gap.all) <- 'dist_to_gap'


canopy.mask.for.dist <- ifel(!is.na(chm) & chm >= 2, 2, NA)
dist.to.canopy.all <- distance(canopy.mask.for.dist)
names(dist.to.canopy.all) <- 'dist_to_canopy'

# mask out gap pixels
dist.to.gap <- mask(dist.to.gap.all, canopy.mask.for.dist)
# mask out canopy pixels
dist.to.canopy <- mask(dist.to.canopy.all, gap.mask.for.dist)

# ------- aggregate to 50m -----

dist.to.gap.mean <- aggregate(dist.to.gap, fact = fact, fun = mean, na.rm = T)


dist.to.canopy.mean <- aggregate(dist.to.canopy, fact = fact, fun = mean, na.rm = T)
dist.to.canopy.max <- aggregate(dist.to.canopy, fact = fact, fun = max, na.rm = T)

# rename
names(dist.to.gap.mean)    <- 'dist_to_gap_mean'

names(dist.to.canopy.mean) <- 'dist_to_canopy_mean'
names(dist.to.canopy.max)  <- 'dist_to_canopy_max'

# ------- direction-based metric -------

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

# function for distance from North or South canopy
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

# ------ combine ------

# combine into single 50m stack
gap.metrics.50m <- c(gap_small, gap_medium, gap_large, gap_xlarge, gap.pct.50, dist.to.gap.mean, dist.to.canopy.mean, dist.to.canopy.max)



# ==============================================================================
#                     Aggregate other metrics to 50m
# ==============================================================================
# first remove redundant metrics from height.stack (cover.stack metrics are better for theses)
height.stack <- height.stack[[ !names(height.stack) %in% c('pzabove2','pzabove5','pzabove10') ]]




# ---------- height, cover, and pad stacks ------------
canopy.metrics.minusgap <- c(height.stack, cover.stack, pad.stack)

mean.vars <- c('zmean', 'zq50', 'zq95', 'zentropy', 'zsd', 'pzabovezmean', 'pzabove2', 'pzabove5', 'pzabove10', 'p_open', 'ground_frac_pc', 'gap_frac_pc')
max.vars <- c('zmax')


cell.size <- res(canopy.metrics.minusgap)[1]
fact <- res(target.swe)[1] / cell.size

canopy.mean.50m <- aggregate(canopy.metrics.minusgap[[mean.vars]], fact = fact, fun = mean, na.rm = T)
canopy.max.50m <- aggregate(canopy.metrics.minusgap[[max.vars]], fact = fact, fun = max, na.rm = T)

# sanity check
plot(canopy.mean.50m$zq95, main = 'Canopy Height (zq95) 50m')

# combine all metrics
canopy.metrics.50m <- c(canopy.mean.50m, canopy.max.50m, gap.metrics.50m)
saveRDS(canopy.metrics.50m, 'data/processed/processed/rds/canopy_metrics_50m_test.rds')

plot(canopy.metrics.50m)


