packages <- c('dplyr', 'exactextractr', 'terra')
lapply(packages, library, character.only = T)

# ------------------------------------------------------------------
# Combine rasters into single stack
# ------------------------------------------------------------------

# --- read in metric rasters ---
fire <- 'creek'
dir <- paste0('data/processed/processed/tif/50m/', fire, '/canopy_metrics/')

cover <- rast(paste0(dir, fire, '_cover_metrics_50m.tif'))
gap <- rast(paste0(dir, fire, '_gap_dist_metrics_50m.tif'))
height <- rast(paste0(dir, fire, '_height_metrics_50m.tif'))

names(cover)  <- paste0('cover_', names(cover))
names(gap)    <- paste0('gap_', names(gap))
names(height) <- paste0('ht_', names(height))

rasters <- list(cover = cover,
                gap = gap,
                height = height)

# ----- check if all matching 

# CRS
lapply(rasters, function(r) crs(r, describe = TRUE)$code)

# resolution
lapply(rasters, res)

# extent
lapply(rasters, ext)
# cover has smallest extent

# origin
lapply(rasters, origin)

# --- if origins are off! ---

# usually it's the gap one that is off, if so, use this
gap.aligned <- resample(
  rasters$gap,
  rasters$cover,
  method = 'bilinear'
)

rasters.aligned <- list(
  cover = rasters$cover,
  gap = gap.aligned,
  height = rasters$height
)


# ----- combine into single stack 
canopy.stack <- rast(rasters.aligned)

# restore correct names
names(canopy.stack) <- unlist(lapply(rasters, names))

plot(canopy.stack)
out.name <- paste0(dir, fire, '_canopy_metrics_50m.tif')

# save
writeRaster(canopy.stack, out.name)

# ------------------------------------------------------------------
# Aggregate Canopy Metrics to 500m
# ------------------------------------------------------------------

fire <- 'dixie'

dir <- 'data/processed/processed/tif/'
in.dir <- paste0(dir, '50m/', fire)
out.dir <- paste0(dir, '500m/', fire, '/canopy_metrics')

# SDD data at ~500m res
target <- rast(paste0(dir, '500m/', fire, '/snow_metrics/', fire, '_sdd_wy2023_500m.tif'))
# canopy metric data at 50 m res
canopy <- rast(paste0(in.dir, '/', fire, '_canopy_metricS_50m.tif'))

# check CRS# check CRScanopy.stack
crs(target) == crs(canopy)

# 4) Loop through layers
out.list <- vector('list', length = nlyr(canopy))

for (i in seq_len(nlyr(canopy))) {
  
  nm <- names(canopy)[i]
  message('Exact resampling: ', i, '/', nlyr(canopy), '  ', nm)
  
  # single-layer SpatRaster
  x <- canopy[[i]]
  
  # exact area-weighted mean to target grid
  y <- exactextractr::exact_resample(x, target, fun = 'mean')
  
  names(y) <- paste0(nm, '_500m')
  
  # write immediately (safer)
  f <- file.path(out.dir, paste0(nm, '_500m.tif'))
  writeRaster(y, f, overwrite = TRUE)
  
  out.list[[i]] <- y
}

# stack back together
canopy.500m <- rast(out.list)

# explicitly preserve layer names
names(canopy.500m) <- paste0(
  names(canopy),
  '_500m'
)

# restore missing CRS
crs(canopy.500m) <- crs(target)

# check
origin(target) == origin(canopy.500m)
res(target) == res(canopy.500m)
crs(target) == crs(canopy.500m)

out.path <- paste0(dir, '500m/', fire, '/', fire, '_canopy_metrics_500m.tif')
# save output
writeRaster(canopy.500m, out.path, overwrite = TRUE)





