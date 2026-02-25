packages <- c('dplyr', 'exactextractr', 'terra')
lapply(packages, library, character.only = T)

# ------------------------------------------------------------------
# Combine rasters into single stack
# ------------------------------------------------------------------

# ----- read in metric rasters
dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics'

cover <- rast(file.path(dir, 'creek_cover_metrics_50m_32611_masked.tif'))
fd <- rast(file.path(dir, 'creek_fractal_dim_50m_32611_masked.tif'))
gap <- rast(file.path(dir, 'creek_gap_50m_32611_masked.tif'))
height <- rast(file.path(dir, 'creek_height_metrics_50m_32611_masked.tif'))

names(cover)  <- paste0('cover_', names(cover))
names(fd)     <- paste0('fd_', names(fd))
names(gap)    <- paste0('gap_', names(gap))
names(height) <- paste0('ht_', names(height))

rasters <- list(cover = cover,
                fd = fd, 
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

# ----- crop by smallest ext 

# find smallest raster by cell count
cells <- sapply(rasters, ncell)
template <- rasters[[ which.min(cells) ]]

# crop all rasters to that template
rasters.cropped <- lapply(rasters, function(r) {
  crop(r, template)
})

# ----- combine into single stack 
canopy.stack <- rast(rasters.cropped)

# restore correct names
names(canopy.stack) <- unlist(lapply(rasters.cropped, names))

plot(canopy.stack)

# save
writeRaster(canopy.stack, file.path(dir, 'canopy_metrics_50m.tif'))

# ------------------------------------------------------------------
# Aggregate Canopy Metrics to 500m
# ------------------------------------------------------------------

dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif'
in.dir <- file.path(dir, '50m/creek')
out.dir <- file.path(dir, '500m/creek/canopy_metrics')

# SDD data at ~500m res
target <- rast(file.path(out.dir, 'creek_sdd_wy2021_32611_1524.tif'))
# canopy metric data at 50 m res
canopy <- rast(file.path(in.dir, 'creek_canopy_metricS_50m.tif'))

# check CRS# check CRScanopy.stack
crs(target) == crs(canopy)
# TRUE


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

# restore missing CRS
crs(canopy.500m) <- crs(target)

# check
origin(target) == origin(canopy.500m)
res(target) == res(canopy.500m)
crs(target) == crs(canopy.500m)

out.dir <- file.path(dir, '500m/creek')

# save output
writeRaster(canopy.500m, file.path(out.dir, 'canopy_metrics_500m.tif'), overwrite = TRUE)
