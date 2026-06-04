# ==============================================================================
#  Mosaic into single raster
# ==============================================================================
library(terra)
library(sf)
library(nhdplusTools)

# ----- Acqusition 1 -----

fire <- 'castle'
acq <- 'CA_SierraNevada_9_14_2022'
epsg <- '32611'
metric <- 'height'


out.dir <- paste0('data/processed/processed/tif/50m/', fire, '/canopy_metrics/', metric, '_metrics_', epsg, '/', acq)
files <- list.files(out.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)
raster.list <- lapply(files, rast)
raster.collection <- sprc(raster.list)

m <- mosaic(raster.collection)
plot(m)
crs(m, describe = T)$code

# --- mask out bodies of water ---
# read in shape file of study area
fire.shp <- read_sf(paste0('data/processed/processed/shp/studyarea_extents/study_extent_', fire, '_simple.shp'))
# download nhd water data
water <- get_nhdphr(AOI = fire.shp, type = 'nhdwaterbody')

masked1 <- mask(m, water, inverse = TRUE)
plot(masked1)

# if not combining:
out.file <-  paste0('data/processed/processed/tif/50m/', fire, '/canopy_metrics/', fire, '_', metric, '_metrics_50m_', epsg, '.tif')

writeRaster(masked1, out.file, overwrite = TRUE)



# ----- Acqusition 2 -----
acq <- 'CA_SierraNevada_5_2022'

out.dir <- paste0('data/processed/processed/tif/50m/', fire, '/canopy_metrics/', metric, '_metrics_', epsg, '/', acq)
files <- list.files(out.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)
raster.list <- lapply(files, rast)
raster.collection <- sprc(raster.list)

m <- mosaic(raster.collection)
plot(m)
crs(m, describe = T)$code

# --- mask out bodies of water ---

masked2 <- mask(m, water, inverse = TRUE)
plot(masked2)

# ----- combine into 1 -----

# make common extent
e <- union(ext(masked1), ext(masked2))

masked1.ext <- extend(masked1, e)
masked2.ext <- extend(masked2, e)

# masked1 takes priority, masked2 fills NA
combine <- cover(masked1.ext, masked2.ext)
names(combine) <- names(masked2)
plot(combine)


out.file <-  paste0('data/processed/processed/tif/50m/', fire, '/canopy_metrics/', fire, '_', metric, '_metrics_50m_', epsg, '.tif')

writeRaster(combine, out.file, overwrite = TRUE)
