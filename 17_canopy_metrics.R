packages <- c('sf', 'mapview', 'lidR', 'dplyr', 'raster', 'future', 'future.apply', 'stringr', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


# NOTE: the final normalization code was saved as a script in data/scripts and run through the command prompt

#----------------------
# Catalog setup 
#----------------------

# normalized tiles
ctg.norm <- readLAScatalog('data/processed/ALS/normalized/creek') 

# ----- sanity check -----
plot(ctg.norm)
plot(ctg.norm, mapview = T, map.types = "Esri.WorldImagery")

# look at one normalized tile
norm.file <- ctg.norm@data$filename[1]
las.norm <- readLAS(norm.file)

# visualization
plot(las.norm, color = 'Z', legend = T)
plot(ctg.norm)

# ground Z distribution
summary(las.norm$Z[las.norm$Classification == 2])
quantile(las.norm$Z[las.norm$Classification == 2], c(0.05, 0.5, 0.95), na.rm = TRUE)
# mostly zero, which is what we would expect (because it's ground!)

# Non-ground (likely vegetation, buildings, etc.)
summary(las.norm$Z[las.norm$Classification != 2])

min(las.norm$Z)

hist(
  las.norm$Z[las.norm$Classification != 2 & las.norm$Z < 80],
  breaks = 100,
  main = 'Height above ground (non-ground points)',
  xlab  = 'Z (m)'
)

# test to determine which algorithm for CHM to use is in 00_canopy_height_model_alg_exploration



# =================================================================================
# Canopy Height Model
# =================================================================================

# ----- make block of 36 tiles for test -----
d <- ctg.norm@data
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


ctg.sub <- readLAScatalog(files.sub)
plot(ctg.sub)
plot(ctg.sub, mapview = TRUE, map.types = "Esri.WorldImagery")  # Interactive map of catalog tiles with Esri imagery basemap
plot(ctg.sub, chunk = TRUE)

# ----- check results -----
chm.vrt <- vrt(tifs)
plot(chm.vrt)

# build a point density raster at the same resolution
opt_output_files(ctg.sub) <- file.path(out.dir, 'density_{ORIGINALFILENAME}')
dens <- grid_density(ctg.sub, res = 1)

# mosaic for quick viewing if dens is tiled output
plot(dens, main = 'Point density (pts / m^2)')

# percent NA overall
v <- values(chm.vrt, mat = FALSE)
pct.na <- mean(is.na(v)) * 100
pct.na


# ----- set up parallel -----
plan(multisession, workers = 10)
set_lidr_threads(1) # important to avoid nested parallelism

# ----- CHM settings and output -----
res.m <- 1

out.dir <- 'data/processed/processed/tif/1m/creek_chm'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

opt_progress(ctg.norm) <- TRUE
opt_chunk_size(ctg.norm) <- 0 # process tile by tile
opt_chunk_buffer(ctg.norm) <-  0 # buffer not needed for CHM
opt_laz_compression(ctg.norm) <- TRUE   


opt_output_files(ctg.norm) <- file.path(out.dir, 'creek_chm_{ORIGINALFILENAME}')

# ----- run CHM -----

chm <- rasterize_canopy(ctg.norm, res = res.m, algorithm = p2r())
# START 10:13am 1/29/26
# END 16:07 1/30/26
# RUNTIME: 30 hours

##### NOTE: I ran this for the creek file (2889 tiles) and it took about 30 hours to run
# These were my parallel settings:
# plan(multisession, workers = 10)
# set_lidr_threads(1) # important to avoid nested parallelism
# I think in the future I would set plan to sequential and let my lidr threads to 10-12

# ----- check results -----
chm.test <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_chm/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB8030_norm.tif')

plot(chm.test)

# ----- reproject -----

# check CRS
crs(chm.test, describe = T)$code
# CRS 6340
res(chm.test)
# 1 x 1

in.dir  <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_chm_6340'
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_chm_32611'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(in.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)

# --- build one shared 32611 template (1 m) ---
# Read headers only (fast; does not load full raster values)
r0 <- rast(files[1])

# Union extent in SOURCE CRS across all tiles
# ext() is metadata-only; this is still pretty quick over thousands of files.
ext.list <- lapply(files, function(f) ext(rast(f)))

xmin.src <- min(sapply(ext.list, xmin))
xmax.src <- max(sapply(ext.list, xmax))
ymin.src <- min(sapply(ext.list, ymin))
ymax.src <- max(sapply(ext.list, ymax))

ext.src <- ext(xmin.src, xmax.src, ymin.src, ymax.src)

# Make a polygon of the union extent, assign source CRS, then project it to 32611
e.poly <- as.polygons(ext.src)
crs(e.poly) <- crs(r0)

e.poly.32611 <- project(e.poly, 'EPSG:32611')
ext.tgt <- ext(e.poly.32611)

# Snap extent outward to whole meters to avoid edge clipping
xmin.tgt <- floor(xmin(ext.tgt))
ymin.tgt <- floor(ymin(ext.tgt))
xmax.tgt <- ceiling(xmax(ext.tgt))
ymax.tgt <- ceiling(ymax(ext.tgt))

template <- rast(
  ext = ext(xmin.tgt, xmax.tgt, ymin.tgt, ymax.tgt),
  res = 1,
  crs = 'EPSG:32611'
)

# Force a consistent grid origin (UTM meters)
origin(template) <- c(0, 0)

crs(template, describe = T)$code
# CRS 
res(template)


# check: res 1 x 1, crs EPSG:32611

# --- function to project + write one tile ---
project_one <- function(f) {
  
  r <- rast(f)
  
  # project to shared template; bilinear is appropriate for continuous CHM heights
  rp <- project(r, template, method = 'bilinear')
  
  # output name: keep original base name, but change folder
  out.name <- file.path(out.dir, basename(f))
  
  # Optional: if you want the filename to explicitly say 32611:
  # out.name <- file.path(out.dir, sub('6340', '32611', basename(f)))
  
  writeRaster(
    rp,
    filename = out.name,
    overwrite = TRUE,
    wopt = list(gdal = c('COMPRESS=LZW'))
  )
  
  TRUE
}

# --- run (serial, safest) ---
# For 2889 rasters this will take a while, but it is robust.
ok <- vapply(files, project_one, logical(1))
sum(ok)

# ==============================================================================
# Calculate canopy metrics 
# ==============================================================================


# ----- Reload CHM -----
chm.files <- list.files('data/processed/processed/tif/1m/creek_chm',
                        pattern = '\\.tif$',
                        full.names = T)

# build virtual mosaic of rasters
# stores references to all the files without loading everything at once
vrt.file <- vrt(chm.files)



# NOTE: Individual metrics were moved to their own separate script file





# ==============================================================================
# PAD/PAI Metrics
# ==============================================================================
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


