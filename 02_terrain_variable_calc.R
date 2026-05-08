packages <- c('tidyverse', 'sf', 'spatialEco', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# create buffered shapefile for DEM retrival
creek.study.area <- vect('data/processed/processed/shp/studyarea_extents/study_extent_creek_simple.shp')
castle.study.area <- vect('data/processed/processed/shp/studyarea_extents/study_extent_castle_simple.shp')
caldor.study.area <- vect('data/processed/processed/shp/studyarea_extents/study_extent_caldor_simple.shp')
dixie.study.area <- vect('data/processed/processed/shp/studyarea_extents/study_extent_dixie_simple.shp')
buffer.dist <- 2500

fires.study.area <- list(
  creek = creek.study.area,
  castle = castle.study.area,
  caldor = caldor.study.area,
  dixie = dixie.study.area
)

study.area.buff <- lapply(names(fires.study.area), function(x) {
  buff <- buffer(fires.study.area[[x]], buffer.dist)
  out.file <- paste0('data/processed/processed/shp/', x, '_buffered.shp')
  writeVector(buff, out.file, overwrite = T)
  return(buff)
})

# preserve names
names(study.area.buff) <- names(fires.study.area)

# ---- download NASADEM from GEE -----

# ---- mask buffered DEMs and save -----

fire.dems <- list(
  creek = rast('data/raw/background_variables/tif/NASADEM_creek_buffered.tif'),
  castle = rast('data/raw/background_variables/tif/NASADEM_castle_buffered.tif'),
  caldor = rast('data/raw/background_variables/tif/NASADEM_caldor_buffered.tif'),
  dixie = rast('data/raw/background_variables/tif/NASADEM_dixie_buffered.tif')
)

out.dir <- 'data/processed/processed/tif/30m'

dem.masked <- lapply(names(fire.dems), function(x) {
  
  # mask DEM to buffered study area
  dem.mask <- mask(
    fire.dems[[x]],
    study.area.buff[[x]]
  )
  
  # output path
  out.file <- file.path(out.dir, paste0('nasadem_', x, '_buffered.tif')
  )
  
  # write raster
  writeRaster(
    dem.mask,
    out.file,
    overwrite = TRUE
  )
  
  # return raster to list
  return(dem.mask)
})

# preserve names
names(dem.masked) <- names(fire.dems)

plot(dem.masked$creek)
plot(dem.masked$castle)
plot(dem.masked$caldor)
plot(dem.masked$dixie)

# ----- Calculate Terrain Variables -----

topo.vars <- lapply(names(dem.masked), function(x) {
  
  dem <- dem.masked[[x]]
  
  # calculate variables
  slope.rad <- terrain(dem, v = 'slope', unit = 'radians')
  aspect.rad <- terrain(dem, v = 'aspect', unit = 'radians')
  aspect.sin <- sin(aspect.rad)
  aspect.cos <- cos(aspect.rad)
  tpi150 <- tpi(dem, win = "circle", s = 150)
  tpi510 <- tpi(dem, win = "circle", s = 510)
  tpi1200 <- tpi(dem, win = "circle", s = 1200)
  tpi2010 <- tpi(dem, win = "circle", s = 2010)
  
  topo.stack <- c(
    slope.rad,
    aspect.sin,
    aspect.cos,
    tpi150,
    tpi510,
    tpi1200,
    tpi2010
  )
  
  names(topo.stack) <- c(
    'slope',
    'aspect_sin',
    'aspect_cos',
    'tpi150',
    'tpi510',
    'tpi1200',
    'tpi2010'
  )
  
  writeRaster(
    topo.stack,
    file.path(out.dir, paste0('topo_', x, '_30m.tif')),
    overwrite = TRUE
  )
  
  return(topo.stack)
  
})

# preserve names
names(topo.vars) <- names(dem.masked)



