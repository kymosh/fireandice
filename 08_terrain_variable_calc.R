packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'spatialEco')
install.packages(setdiff(packages, rownames(installed.packages())))

lapply(packages, library, character.only = TRUE)

# create buffered shapefile for DEM retrival
study.area <- vect('data/processed/processed/shp/study_extent_creek_32611.shp')
buffer <- 2500

study.area.buff <- buffer(study.area, 2500)
writeVector(study.area.buff, 'data/processed/processed/shp/creek_buffered.shp')

# ---- download NASADEM from GEE -----

# mask DEM
dem <- rast('data/raw/background_variables/shp/NASADEM_creek_buffered.tif')
dem.mask <- mask(dem, study.area.buff)
plot(dem.mask)

writeRaster(dem.mask, 'data/processed/processed/tif/30m/creek/creek_dem_buffered_30m.tif')

# ----- Calculate Terrain Variables -----

dem <- rast('data/processed/processed/tif/30m/creek/creek_dem_buffered_30m.tif')
dem.elev <- rast(here('data', 'processed', 'processed', 'tif', '30m', 'creek', 'nasadem_creek_30m_1524.tif'))

# inspect dems
print(dem)
plot(dem)
hist(dem)
plot(is.na(dem), main = 'NA cells in DEM')

# calculate variables
slope.rad <- terrain(dem, v = 'slope', unit = 'radians')
aspect.rad <- terrain(dem, v = 'aspect', unit = 'radians')
aspect.sin <- sin(aspect.rad)
aspect.cos <- cos(aspect.rad)
tpi150 <- tpi(dem, win = "circle", s = 150)
tpi510 <- tpi(dem, win = "circle", s = 510)
tpi1200 <- tpi(dem, win = "circle", s = 1200)
tpi2010 <- tpi(dem, win = "circle", s = 2010)

plot(slope.rad)
plot(aspect.rad)
plot(tpi150)
plot(tpi510)
plot(tpi1200)
plot(tpi2010)

variables <- list(
  slope  = slope.rad,
  aspect_sin = aspect.sin,
  aspect_cos = aspect.cos,
  tpi150 = tpi150,
  tpi510 = tpi510,
  tpi1200 = tpi1200,
  tpi2010 = tpi2010
)

# write files
out.dir <- 'data/processed/processed/tif/30m/creek'

# loop to mask variables to 1524m and write raster
for (name in names(variables)) {
  
  r <- variables[[name]]
  
  # crop and mask mask to elevation
  r.crop <- crop(r, dem.elev)
  r.mask <- mask(r.crop, dem.elev)
  
  # output filename
  out.file <- file.path(out.dir, paste0('creek_topo_', name, '_30m_1524.tif'))
  
  writeRaster(r.mask, out.file, overwrite = TRUE)
  
}

