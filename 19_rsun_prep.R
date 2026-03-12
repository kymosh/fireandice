packages <- c('dplyr', 'tidyr', 'tools', 'terra')
lapply(packages, library, character.only = T)

# ===========================================================================================
# Create 9-tile test mosaic for testing r.sun
# ===========================================================================================

tiles <- c(
  '11SLB1150', '11SLB1250', '11SLB1350',
  '11SLB1151', '11SLB1251', '11SLB1351',
  '11SLB1152', '11SLB1252', '11SLB1352'
)

# file directory
dem.dir <- 'data/raw/DEM/creek'

# all files in directory
all.files <- list.files(dem.dir, pattern= '\\.tif$', full.names = TRUE)
# keep only the 9 test tiles
test.files <- all.files[grepl(paste(tiles, collapse = '|'), basename(all.files))]          

# read as rasters
dem.list <- lapply(test.files, rast)
# combine
dem.test <- do.call(mosaic, dem.list)
# check
plot(dem.test)

# check res
res(dem.test) #0.5 0.5

# get template grid from chms
temp.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
temp.files <- list.files(temp.dir, pattern = '\\.tif$', full.names = T)
temp.files <- temp.files[grepl(paste(tiles, collapse = '|'), basename(temp.files))]   
template <- lapply(temp.files, rast)
template <- do.call(mosaic, template)

# project dem onto grid
dem.32611 <- project(dem.test, template, method = 'bilinear')

# check
res(dem.32611)
crs(dem.32611, describe = T)$code
origin(dem.32611)

writeRaster(dem.32611, 'data/processed/processed/tif/1m/creek_dem_9tile.tif')


# ===========================================================================================
# Reproject DEM to correct res and origin
# ===========================================================================================

# file directory
dem.dir <- 'data/raw/DEM/creek'
dem.files <- list.files(dem.dir, pattern = '\\.tif$', full.names = TRUE)


# read as raster
start <- Sys.time()
dem.rasters <- lapply(dem.files, rast)
dem.r <- do.call(mosaic, dem.rasters)
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')

# template
# get template grid from chms
temp.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
temp.files <- list.files(temp.dir, pattern = '\\.tif$', full.names = T)
template <- lapply(temp.files, rast)
template <- do.call(mosaic, template)

# project dem onto grid
start <- Sys.time()
dem.32611 <- project(dem.r, template, method = 'bilinear')
writeRaster(dem.32611, 'data/processed/processed/tif/1m/creek_dem_1m.tif')
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
# took 5.3 hours


# ===========================================================================================
# Create DSM
# ===========================================================================================

# ----- test mosaic first -----

# create test mosaic first
tiles <- c(
  '11SLB1150', '11SLB1250', '11SLB1350',
  '11SLB1151', '11SLB1251', '11SLB1351',
  '11SLB1152', '11SLB1252', '11SLB1352'
)

# dem
dem <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dem_test_9.tif')

# chm files
chm.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_chm_32611'
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = T)
chm.files <- chm.files[grepl(paste(tiles, collapse = '|'), basename(chm.files))]

# output directory
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dsm_test_9'
dir.create(out.dir, recursive = T, showWarnings = F)

# function to make dsm from dem and chm
dsm.from.dem.chm <- function(f) {
  
  # read in chm tile
  chm <- rast(f)
  
  # crop dem to that tile's extent
  dem.tile <- crop(dem, chm, snap = 'near')
  
  chm[is.na(chm)] <- 0
  
  dsm <- dem.tile + chm
  
  out.file <- file.path(out.dir, sub('creek_chm', 'creek_dsm', basename(f)))
  
  writeRaster(dsm, out.file, overwrite = T)

  }
  
dsm.files <- lapply(chm.files, dsm.from.dem.chm)

# check  
x <- rast(dsm.files[1])  
plot(x)

# combine into single mosaic
dsm <- do.call(mosaic, dsm.files)
plot(dsm)

writeRaster(dsm, 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dsm_test_9.tif')

# ----- full mosaic -----

# full 1m raster dem
dem <- rast('data/processed/processed/tif/1m/creek_dem_1m.tif')

# chm files
# keep chm files by tile
chm.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = T)

# output directory
out.dir <- 'data/processed/processed/tif/1m/creek_dsm'
dir.create(out.dir, recursive = T, showWarnings = F)

# apply function
start <- Sys.time()
dsm.files <- lapply(chm.files, dsm.from.dem.chm)
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
# 134 minutes

# combine into single mosaic
start <- Sys.time()
dsm <- do.call(mosaic, dsm.files)
end <- Sys.time()
message('Finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')

writeRaster(dsm, 'data/processed/processed/tif/1m/creek_dsm_1m.tif')



# troubleshooting
x <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_chm_32611/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7732_norm.tif')
x <- dem
origin(x)
crs(x, describe = T)$code
res(x)

x <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dem_test_.tif')
plot(x)

test.chm <- rast(chm.files[1])
dem.test <- crop(dem, test.chm)

compareGeom(dem.test, test.chm)
































