packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'spatialEco')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

dem <- rast(here('data', 'processed', 'processed', 'tif', '30m', 'creek', 'nasadem_creek_30m.tif'))
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
hli <- hli(dem) #uses McCune(2007) calculation for hli
tpi150 <- tpi(dem, win = "circle", s = 150)
tpi510 <- tpi(dem, win = "circle", s = 510)
tpi1200 <- tpi(dem, win = "circle", s = 1200)
tpi2010 <- tpi(dem, win = "circle", s = 2010)

plot(slope.rad)
plot(aspect.rad)
plot(hli)
plot(tpi150)
plot(tpi510)
plot(tpi1200)
plot(tpi2010)

variables <- list(
  slope  = slope.rad,
  aspect = aspect.rad,
  aspect.sin = aspect.sin,
  aspect.cos = aspect.cos,
  hli    = hli,
  tpi150 = tpi150,
  tpi510 = tpi510,
  tpi1200 = tpi1200,
  tpi2010 = tpi2010
)

# write files
out.dir <- 'data/processed/processed/tif/30m'

# loop to mask variables to 1524m and write raster
for (name in names(variables)) {
  
  r <- variables[[name]]
  
  # mask to elevation
  r.mask <- mask(r, dem.elev)
  
  # output filename
  out.file <- file.path(out.dir, paste0('creek_topo_', name, '_30m_1524.tif'))
  
  writeRaster(r.mask, out.file, overwrite = TRUE)
  
}

