packages <- c('terra', 'sf', 'mapview', 'lidR', 'aRchi', 'TreeLS', 'dplyr', 'ForestGapR', 'raster')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


mean.csm <- rast(here('data', 'raw', 'ALS', 'tif', 'CreekFire_2021_MeanCSM_Meters.tif'))
rm(meancsm)

summary(mean.csm)

#test

# ---  Read catalog & set options ---
ctg <- readLAScatalog('data/raw/ALS/laz/random_tiles')   # Load a LAScatalog from the folder of tiles
set_lidr_threads(8)         # Use 8 threads for lidR operations (parallel where supported)
opt_laz_compression(ctg) <- TRUE      # Write LAZ (compressed) outputs instead of LAS
opt_progress(ctg)        <- TRUE      # Show a progress bar for catalog operations

plot(ctg, mapview = TRUE, map.types = "Esri.WorldImagery")  # Interactive map of catalog tiles with Esri imagery basemap

tile.dtm = rasterize_terrain(ctg, res = 3.28, algorithm = tin())

# data inspection
ctg
# just look at 1 las file
las1 <- readLAS(ctg@data$filename[1])
las1
# see number of each classification categories
table(las1$Classification)

# filter out unwanted points 
opt_filter(ctg) <- '-drop_class 7 18 -drop_withheld'

# check to make sure it worked
las1.clean <- readLAS(ctg@data$filename[1], filter = '-drop_class 7 18 -drop_withheld')
table(las1.clean$Classification)

# process one tile at a time
opt_chunk_size(ctg) <- 0  # would change this when scaling up with more data
# buffer of 20m to enable seamless normalization 
opt_chunk_buffer(ctg) <- 20

# ---  Normalize ---

# set output file names
opt_output_files(ctg) <- 'data/processed/ALS/normalized/tile_norm_{XLEFT}_{YBOTTOM}'

# normalize heights using point cloud
# NOTE: I am using the point cloud, not a DTM here to normalize. This is computationally heavier than using a DTM. May have to use DTM when processing entire study area dataset

# using TIN because we have good ground point classification already
ctg.norm <- normalize_height(ctg, tin())

ctg.norm

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

quantile(
  las.norm$Z[las.norm$Classification != 2],
  c(0.5, 0.9, 0.95, 0.99, 0.999),
  na.rm = TRUE
)

hist(
  las.norm$Z[las.norm$Classification != 2 & las.norm$Z < 80],
  breaks = 100,
  main = 'Height above ground (non-ground points)',
  xlab  = 'Z (m)'
)


# exploration to figure out what resolution to use
# Example at 1 m
res.m <- 1

# at a 1m pixel, get a number of how many points per pixel to see if dense enought
density.test <- pixel_metrics(
  ctg.norm,
  ~ list(n = length(Z)),
  res = res.m
)

# make a list of all the density values
vals <- values(density.test, mat = FALSE)
# see what propotion of values are <10 (too low)
sum(vals$n < 10, na.rm = T)
# only 4% are less than 10, so 1m should be fine

# ------ canopy height model --------

chm <- rasterize_canopy(ctg.norm, res = 1, algorithm = pitfree())

# ----- Compute canopy metrics ------

# z = height
# cl = classification code 

canopy_metrics <- function(z, cl)
{ # ----- canopy height metrics -----       # only above 2m
  n_all <- length(z)                        # how many points per pixel
  if (n_all == 0) return(NULL)              # if no points in a pixel, return NULL
  z_can <- z[z > 2 &                        # height must be > 2m
               cl != 2]                     # can't be a ground classified point (shouldn't be anyways)
  if (length(z_can) > 0) {                  # make sure there are canopy points to begin with
    std <- stdmetrics_z(z_can)
    out <- list(zmean = as.numeric(std['zmean']),
                zsd = as.numeric(std['zsd']),
                zq95 = as.numeric(std['zq95']),
                zentropy = as.numeric(std['zentropy']))
    } else { 
      out <- list(zmean = NA_real_,
                  zsd = NA_real_,
                  zq95 = NA_real_,
                  zentropy = NA_real_ )}
  # ----- canopy cover metrics -----        # on all pixels (incl <2m)
  out$pzabove2 = sum(z > 2) / n_all         # proportion of returns above 2m
  out$pzabove10 = sum(z > 10) / n_all       # proportion of returns above 10m (tall canopy)
  out$p_2_10 = sum(z > 2 & z <= 10) / n_all # proportion between 2 and 10m (midstory)
  out$p_open = 1 - out$pzabove2             # proportion open canopy
  return(out)
  }

  

res.m = 1

# remove output directory so that forces to use temporary files
opt_output_files(ctg.norm) <- ""

# apply function to ctg.norm to create raster
canopy.rasters <- pixel_metrics(ctg.norm, ~ canopy_metrics(Z, Classification), res = res.m)

plot(canopy.rasters$zq95, main = 'Canopy height (zq95)')
plot(canopy.rasters$pzabove2, main = 'Canopy cover (>2 m)')
plot(canopy.rasters$p_open, main = '             Gap fraction (1 - pzabove2)')
plot(canopy.rasters$p_2_10, main = 'Midstory (2–10 m)')
plot(canopy.rasters$zentropy, main = '           Canopy vertical complexity')

plot(canopy.rasters, col = height.colors(50))

writeRaster(canopy.rasters,'data/processed/ALS/tif/canopy_metrics_1m_test.tif', overwrite = T)

# ==============================================================================
# recalculate canopy metrics 
# ==============================================================================

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

cover.stack <- pixel_metrics(ctg.norm, ~ cover.metrics(Z, Classification), res = 1)

# ------- PAD/PAI metrics --------
# Compute Plant Area Density (PAD) and Plant Area Index (PAI)
# from LiDAR point heights "z".
#
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
  
  # extract PAD values and corresponding height bines
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

# ====------- gap metrics ----------- BAD
# =============================================


# # we first have to write and re-read in to force the correct data type
# writeRaster(cover.stack$pzabove2, 
#             'pzabove2_fixed.tif', 
#             overwrite = TRUE)
# 
# summary(cover.stack)
# 
# pzabove2_fixed <- rast('pzabove2_fixed.tif')
# datatype(pzabove2_fixed)     # should NOT be ""
# 
# 
# gap.mask <- pzabove2_fixed < 0.05
# table(values(gap.mask), useNA = 'ifany')
# 
# 
# 
# 
# 
# gap.metrics <- function(chm, pzabove2, gap_threshold = 0.05) {
#   
#   # 1 — GAP MASK (TRUE gaps where pzabove2 < threshold)
#   gap.mask <- pzabove2 < gap_threshold 
#   values(gap.mask) <- as.integer(values(gap.mask))   # force 0/1
#   
#   # 2 — convert to polygons
#   gap.poly.0 <- as.polygons(gap.mask, dissolve = TRUE, values = TRUE)
#   
#   # the attribute column name will match the raster name
#   att <- names(gap.poly.0)[1]
#   gap.poly <- gap.poly.0[ gap.poly.0[[att]] == 1 ]
#   
#   # 3 — NO GAPS case
#   if (nrow(gap.poly) == 0) {
#     gap_area_rast    <- rast(chm); values(gap_area_rast)    <- NA
#     gap_radius_rast  <- rast(chm); values(gap_radius_rast)  <- NA
#     gap_id_rast      <- rast(chm); values(gap_id_rast)      <- NA
#     dist_to_canopy   <- rast(chm); values(dist_to_canopy)   <- NA
#     dist_to_gap_edge <- rast(chm); values(dist_to_gap_edge) <- NA
#     
#     gap.stack <- c(gap_area_rast,
#                    gap_radius_rast,
#                    gap_id_rast,
#                    dist_to_canopy,
#                    dist_to_gap_edge)
#     
#     names(gap.stack) <- c(
#       'gap_area',
#       'gap_radius',
#       'gap_id',
#       'dist_to_canopy',
#       'dist_to_gap_edge'
#     )
#     
#     return(gap.stack)
#   }
#   
#   # 4 — gap attributes
#   gap.poly$gap_area   <- expanse(gap.poly, unit = 'm')
#   gap.poly$gap_radius <- sqrt(gap.poly$gap_area / pi)
#   gap.poly$gap_id     <- 1:nrow(gap.poly)
#   
#   # 5 — rasterize attributes back onto canopy grid
#   gap_area_rast   <- rasterize(gap.poly, chm, field = 'gap_area')
#   gap_radius_rast <- rasterize(gap.poly, chm, field = 'gap_radius')
#   gap_id_rast     <- rasterize(gap.poly, chm, field = 'gap_id')
#   
#   # 6 — distance metrics
#   dist_to_canopy <- distance(!gap.mask)
#   
#   edges <- boundaries(gap.mask, directions = 8, classes = FALSE)
#   edges <- classify(edges, cbind(NA, 0))
#   inner.edges <- mask(edges, gap.mask, maskvalue = 0)
#   dist_to_gap_edge <- distance(!inner.edges)
#   
#   # 7 — combine
#   gap.stack <- c(
#     gap_area_rast,
#     gap_radius_rast,
#     gap_id_rast,
#     dist_to_canopy,
#     dist_to_gap_edge
#   )
#   
#   names(gap.stack) <- c(
#     'gap_area',
#     'gap_radius',
#     'gap_id',
#     'dist_to_canopy',
#     'dist_to_gap_edge'
#   )
#   
#   return(gap.stack)
# }
# 
# 
# gap.stack <- gap.metrics(chm, cover.stack$pzabove2)
# names(gap.stack)
# summary(gap.stack)
# 
# gap.stack <- resample(gap.stack, height.stack, method = 'bilinear')




# ==============================================================================
#  GAP METRICS 
# ==============================================================================
# ForestGapR requires chm to be a raster::raster
chm.r <- raster::raster(chm)

# Plotting chm
plot(chm.r, col=viridis(10))

# Setting height thresholds (e.g. 10 meters)
threshold <- 2 # No points >2 m = gap
size<-c(1,10000) #all gaps between 1 and 1000m

# Detecting forest gaps
gaps.rast <- getForestGaps(chm_layer = chm.r, threshold, size)


#plot
plot(chm.r, col=viridis(10))
plot(gaps.rast, col="red", add=TRUE, main="Forest Canopy Gap", legend=FALSE)


# zoom
ext_small <- ext(308350, 308550, 4135500, 4135700)
terra::plot(
  terra::crop(chm, ext_small),
  col = viridisLite::plasma(100)
)
plot(gaps.rast, col="red", add=TRUE, main="Forest Canopy Gap", legend=F)

# calc stats
gaps.stats <- GapStats(gap_layer = gaps.rast, chm_layer = chm.r)

# ----------- aggregate gap metrics to 50m ---------------------

# turn back into spatraster
gaps <- rast(gaps.rast)

# force it to report correct CRS
crs(gaps) <- "EPSG:32611"
plot(chm)
plot(gaps, col = 'red', add = T)


names(gaps) <- 'gap_id'

# change to binary gap or no gap
gap.mask <- ifel(!is.na(gaps), 1, 0)
names(gap.mask) <- 'is_gap'
unique(gap.mask)
plot(gap.mask, colNA='red')


# plot binary gaps
terra::plot(
  terra::crop(gap.mask, ext_small),
  col = viridisLite::plasma(2)
)

cell.size <- res(gap.mask)[1]
fact <- res(target.swe)[1] / cell.size

gap.count.50 <- aggregate(gap.mask, fact = fact, fun = sum, na.rm = T)
names(gap.count.50) <- 'gap_area_m2'

pixel.area.50 <- fact*50

# gap.pct.50 is % of each 50 m pixel that is gap
gap.pct.50 <- gap.count.50*(cell.size^2) / pixel.area.50

ga <- gaps.stats$gap_area
id <- gaps.stats$gap_id

gap.bins <- ifelse(ga < 5, 1,
             ifelse(ga < 10, 2,
             ifelse(ga < 10000, 3, 0)))

look.up.class <- cbind(id, gap.bins)
look.up.class <- as.matrix(look.up.class)

gap.class <- classify(gaps, look.up.class)
hist(gap.class)
plot(gap.class)


# create binary for each size class
gap.small <- ifel(gap.class == 1, 1, 0)
gap.medium <- ifel(gap.class == 2, 1, 0)
gap.large <- ifel(gap.class == 3, 1, 0)

names(gap.small)  <- 'gap_small'
names(gap.medium) <- 'gap_medium'
names(gap.large)  <- 'gap_large'

gaps.small.area <- aggregate(gap.small, fact = fact, fun = sum, na.rm = T)
gaps.medium.area <- aggregate(gap.medium, fact = fact, fun = sum, na.rm = T)
gaps.large.area <- aggregate(gap.large, fact = fact, fun = sum, na.rm = T)

gaps.small.pct <- gaps.small.area * (cell.size^2) / pixel.area.50
gaps.medium.pct <- gaps.medium.area * (cell.size^2) / pixel.area.50
gaps.large.pct <- gaps.large.area * (cell.size^2) / pixel.area.50

names(gaps.small.pct)  <- 'gap_small_pct'
names(gaps.medium.pct) <- 'gap_medium_pct'
names(gaps.large.pct)  <- 'gap_large_pct'

gap.metrics.50m <- c(
  gaps.small.pct,
  gaps.medium.pct,
  gaps.large.pct,
  gap.pct.50
)

names(gap.metrics.50m)



# ==============================================================================

# ==============================================================================
#                     Aggregate other metrics to 50m
# ==============================================================================
# first remove redundant metrics from height.stack (cover.stack metrics are better for theses)
height.stack <- height.stack[[ !names(height.stack) %in% c('pzabove2','pzabove5','pzabove10') ]]

# force correct projection
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
plot(canopy.max.50m$zmax)

# combine all metrics
canopy.metrics.50m <- c(canopy.mean.50m, canopy.max.50m, gap.metrics.50m)

# remove the 2 metrics that didn't work
canopy.metrics.50m <- canopy.metrics.50m[[ !names(canopy.metrics.50m) %in% c('gap_medium_pct', 'gap_large_pct') ]]
plot(canopy.metrics.50m)

# ---------- combine 3 stacks into master stack ---------








writeRaster(
  canopy.metrics,
  'data/processed/ALS/tif/canopy_metrics_all_1m_test.tif',
  overwrite = TRUE
)

names(canopy.metrics)
# ==============================================================================
# reproject data
# ==============================================================================

target.swe <- rast('data/processed/processed/tif/50m/ASO_SanJoaquin_2020_0608_swe_50m_1524.tif')

canopy.metrics.32611 <- project(canopy.metrics, crs(target.swe), method = 'near')


# ==============================================================================
# Aggregate to 50m
# ==============================================================================

# variables where means are meaningful
continuous_vars <- c(
  'zmax','zmean','zsd','zskew','zkurt','zentropy',
  grep('^zq', names(canopy.metrics.32611), value=TRUE),
  grep('^zpcum', names(canopy.metrics.32611), value=TRUE),
  'zmax_true',
  'pzabove2','pzabove5','pzabove10','p_open',
  'gap_frac_pc','ground_frac_pc',
  'pzabovezmean',
  'PAI','PAD_mean','PAD_SD','PAD_CV','PAD_max','H_PADmax'
)

canopy.continuous <- canopy.metrics.32611[[continuous_vars]]


# ==============================================================================
# test for spatial autocorrelation
# ==============================================================================





# ==============================================================================
# create aspect- dependent edginess metric
# ==============================================================================

###### this is not right, getting east/west shading due to incorrect directions. Back to the drawing board. Need outside help most likely. 

# create canopy mask for presence of canopy
# canopy is present if canopy height > 2m 
canopy.logical <- canopy.rasters$zq95 > 2
canopy.mask <- ifel(canopy.logical, 1, 0)


# calculate mean canopy height H
H <- global(canopy.rasters$zq95, 'mean', na.rm = T)[1,1]
H

# extract canopy mask into matrix
cm <- as.matrix(canopy.mask, wide = T)
cm[is.na(cm)] <- 0    # convert NAs → 0 (open)
nr <- nrow(cm)
nc <- ncol(cm)

res.m <- res(canopy.mask)[1]

# define 8 directions to "search"a
dirs.deg <- c(330, 345, 0, 15, 30, 150, 165, 180, 195, 210)
dirs.rad <- dirs.deg * pi / 180

# function to search from the pixel
ray.distance <- function(r0, c0, theta, max_steps, cm, res.m) {
  for (step in 1:max_steps) {
    # compute location along ray
    r <- round(r0 - step * sin(theta))
    c <- round(c0 + step * cos(theta))
    
    # boundary check
    if (r < 1 || r > nrow(cm) || c < 1 || c > ncol(cm))
      return(NA_real_)
    
    # canopy hit?
    if (cm[r, c] == 1)
      return(step * res.m)
  }
  return(NA_real_) # no canopy found within max distance
}

max_dist <- 3 * H
max_steps <- ceiling(max_dist / res.m)

dist.array <- array(NA_real_, dim = c(nr, nc, length(dirs.rad)))

for (j in 1:nc) { # for each column
  for (i in 1:nr) { # for each row()
    if(cm[i, j] == 0) { # only compute for open pixels
      for (k in 1:length(dirs.rad)) {
        dist.array[i, j, k] <- ray.distance(i, j, dirs.rad[k], max_steps, cm, res.m) }}}}

dist.north <- apply(dist.array[, , 1:5], c(1, 2), min, na.rm = T)
dist.south <- apply(dist.array[, , 6:10], c(1, 2), min, na.rm = T)

dist.north[is.infinite(dist.north)] <- NA_real_
dist.south[is.infinite(dist.south)] <- NA_real_


# convert to rasters
dist.north.r <- rast(dist.north, ext = ext(canopy.mask), crs = crs(canopy.mask))
dist.south.r <- rast(dist.south, ext = ext(canopy.mask), crs = crs(canopy.mask))

# edginess
edginess.north <- dist.north.r / (3*H)
edginess.north <- ifel(edginess.north > 1, 1, edginess.north)
edginess.south <- dist.south.r / (3*H)
edginess.south <- ifel(edginess.south > 1, 1, edginess.south)
edginess.total <- edginess.north + edginess.south

# visualization ============

# quick glance
plot(edginess.north, main='Edginess to the North', col=viridis::viridis(200))
plot(edginess.south, main='Edginess to the South', col=viridis::viridis(200))

# choose breakpoints centered at zero
rng <- max(abs(global(contrast, "range", na.rm=TRUE)))

brks <- seq(-rng, rng, length.out = 255)

# diverging palette: blue → gray → red
pal <- colorRampPalette(c("blue", "gray90", "red"))

contrast <- edginess.south - edginess.north

plot(crop(contrast, ext_small),
     col = pal(254),
     breaks = brks,
     main = "Directional Edginess Contrast (South - North)")


plot(edginess.north, col=viridis::viridis(200), main='Edginess North')
plot(canopy.mask, col=c("transparent","black"), add=TRUE, legend=FALSE)

# zoom
ext_small <- ext(308200, 308400, 4135500, 4135700)  

plot(crop(edginess.north, ext_small), 
     main='Zoom: Edginess North', 
     col=viridis::magma(200))

plot(crop(canopy.mask, ext_small), 
     add=TRUE, col=c("transparent","white"), legend=FALSE)

#### getting east/west shading. NOT RIGHT