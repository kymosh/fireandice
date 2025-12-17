packages <- c('terra', 'sf', 'mapview', 'lidR', 'aRchi', 'TreeLS', 'dplyr', 'ForestGapR', 'raster', 'future')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


#----------------------
# Catalog setup 
#----------------------
ctg <- readLAScatalog('data/raw/ALS/laz/random_tiles')
set_lidr_threads(8)     # lidR parallelization
opt_laz_compression(ctg) <- TRUE
opt_progress(ctg) <- TRUE
opt_chunk_size(ctg) <- 0
opt_chunk_buffer(ctg) <- 20


#plot(ctg, mapview = TRUE, map.types = "Esri.WorldImagery")  # Interactive map of catalog tiles with Esri imagery basemap

# tile.dtm = rasterize_terrain(ctg, res = 3.28, algorithm = tin())
# should run without warnings, try again if orange

# # just look at 1 las file
# las1 <- readLAS(ctg@data$filename[1])
# las1
# # see number of each classification categories
# table(las1$Classification)
# 
#filter out unwanted points 
opt_filter(ctg) <- '-drop_class 7 18 -drop_withheld'
# 
# # check to make sure it worked
# las1.clean <- readLAS(ctg@data$filename[1], filter = '-drop_class 7 18 -drop_withheld')
# table(las1.clean$Classification)




# ---  Normalize ---

# set output file names
# NOTE if rerunning, make sure this folder is empty
opt_output_files(ctg) <- 'data/processed/ALS/normalized/tile_norm_{XLEFT}_{YBOTTOM}'

# normalize heights using point cloud
# NOTE: I am using the point cloud, not a DTM here to normalize. This is computationally heavier than using a DTM. May have to use DTM when processing entire study area dataset

# using TIN because we have good ground point classification already
# ctg.norm <- normalize_height(ctg, tin())

# saveRDS(ctg.norm, 'data/processed/processed/rds/ctg_norm_test_rds')
ctg.norm <- readRDS('data/processed/processed/rds/ctg_norm_test_rds')

# look at one normalized tile
norm.file <- ctg.norm@data$filename[1]
las.norm <- readLAS(norm.file)

# visualization
plot(las.norm, color = 'Z', legend = T)

# ground Z distribution
summary(las.norm$Z[las.norm$Classification == 2])
quantile(las.norm$Z[las.norm$Classification == 2], c(0.05, 0.5, 0.95), na.rm = TRUE)
# all zero, which is what we would expect (because it's ground!)

# Non-ground (likely vegetation, buildings, etc.)
summary(las.norm$Z[las.norm$Classification != 2])



hist(
  las.norm$Z[las.norm$Classification != 2 & las.norm$Z < 80],
  breaks = 100,
  main = 'Height above ground (non-ground points)',
  xlab  = 'Z (m)'
)

res.m <- 1

# clear output pattern
# NOTE: necessary step to avoid overwrite issues if rerunning
# opt_output_files(ctg.norm) <- ""

# ------ canopy height model --------

# chm <- rasterize_canopy(ctg.norm, res = 1, algorithm = pitfree())
# saveRDS(chm, 'data/processed/processed/rds/chm_test.rds')
chm <- readRDS('data/processed/processed/rds/chm_test.rds')

# check CRS
crs(chm, describe = T)$code
# CRS 6340
res(chm)
# 1 x 1

# reproject to 32611
chm <- project(chm, 'EPSG:32611')
crs(chm, describe = T)$code

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

# cover.stack <- pixel_metrics(ctg.norm, ~ cover.metrics(Z, Classification), res = 1)
# saveRDS(cover.stack, 'data/processed/processed/rds/cover_test.rds')
cover.stack <- readRDS('data/processed/processed/rds/cover_test.rds')

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


# ==============================================================================
#  Gap Metrics
# ==============================================================================

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

#------ aggregate to 50m -------
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

# ------------- distance to gap/canopy -----------------

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

# ----- aggregate to 50m -----

dist.to.gap.mean <- aggregate(dist.to.gap, fact = fact, fun = mean, na.rm = T)


dist.to.canopy.mean <- aggregate(dist.to.canopy, fact = fact, fun = mean, na.rm = T)
dist.to.canopy.max <- aggregate(dist.to.canopy, fact = fact, fun = max, na.rm = T)

# rename
names(dist.to.gap.mean)    <- 'dist_to_gap_mean'

names(dist.to.canopy.mean) <- 'dist_to_canopy_mean'
names(dist.to.canopy.max)  <- 'dist_to_canopy_max'

# ---------------- direction-based metric ------------------
library(terra)

dir.sector.dist <- function(target_mask, from_mask,
                            sector = 'N',
                            max_dist_m = 200) {
  
  res.m <- res(target_mask)[1]
  max.k <- floor(max_dist_m / res.m)
  
  out <- rast(target_mask)
  values(out) <- NA_real_
  
  from_mask <- crop(from_mask, target_mask)
  
  for (k in 1:max.k) {
    
    dy <- k * res.m
    horiz.range <- k * res.m
    
    if (sector == 'N') {
      shifted <- shift(target_mask, dy = -dy)
    }
    
    if (sector == 'S') {
      shifted <- shift(target_mask, dy = dy)
    }
    
    for (dx in seq(-horiz.range, horiz.range, by = res.m)) {
      
      candidate <- resample(
        shift(shifted, dx = dx),
        target_mask,
        method = 'near'
      )
      
      hit <- mask(candidate, from_mask)
      dist <- sqrt(dx^2 + dy^2)
      
      out <- cover(out, ifel(!is.na(hit), dist, NA_real_))
    }
  }
  
  out
}

    
dist.canopy.north <- dir.sector.dist(
  target_mask = canopy.mask.for.dist,
  from_mask = gap.mask.for.dist,
  sector = 'N',
  max_dist_m = 200
)

# started at 4:10pm on 12/16
saveRDS(dist.canopy.north, 'data/processed/processed/rds/dist_canopy_north_test.rds')
plot(dist.canopy.north)

# ------ combine ------

# combine into single 50m stack
gap.metrics.50m <- c(gap_small, gap_medium, gap_large, gap_xlarge, gap.pct.50, dist.to.gap.mean, dist.to.canopy.mean, dist.to.canopy.max)

# ==============================================================================

# ==============================================================================
#                     Aggregate other metrics to 50m
# ==============================================================================
# first remove redundant metrics from height.stack (cover.stack metrics are better for theses)
height.stack <- height.stack[[ !names(height.stack) %in% c('pzabove2','pzabove5','pzabove10') ]]

# bad
crs(cover.stack) <- "EPSG:32611"
crs(height.stack) <- "EPSG:32611"
crs(pad.stack) <- "EPSG:32611"


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


