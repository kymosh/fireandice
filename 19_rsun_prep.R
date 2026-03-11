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
dem.dir <- dem.dir <- 'data/raw/DEM/creek'
dem.files <- list.files(dem.dir, pattern = '\\.tif$', full.names = TRUE)

# read as raster
dem.rasters <- lapply(dem.files, rast)
dem.r <- do.call(mosaic, dem.rasters) #9:06

# template
# get template grid from chms
temp.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
temp.files <- list.files(temp.dir, pattern = '\\.tif$', full.names = T)
template <- lapply(temp.files, rast)
template <- do.call(mosaic, template)

# project dem onto grid
dem.32611 <- project(dem.test, template, method = 'bilinear')

writeRaster(dem.32611, 'data/processed/processed/tif/1m/creek_dem_mosaic.tif')



# troubleshooting
x <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_chm_32611/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7732_norm.tif')
origin(x)
crs(x, describe = T)$code
res(x)

x <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek_dem_9tile.tif')
plot(x)


































