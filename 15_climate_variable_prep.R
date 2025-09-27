packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'spatialEco')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

clim <- rast(here('data', 'raw', 'background_variables', 'tif', 'creek_terraclimate_wy2018.tif'))
plot(clim)

swe <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_SanJoaquin_2020_0608_swe_50m_clipped.tif'))
sdd <- rast(here('data', 'processed', 'processed', 'tif', 'creek_sdd_wy2020_32611.tif'))

crs(swe, describe = T)$code
crs(sdd, describe = T)$code
crs(clim, describe = T)$code

#### comparison of different sampling methods

clim.30.bl <- resample(clim, dem.elev, method = 'bilinear')
clim.30.nn <- resample(clim, dem.elev, method = 'near')
clim.30.cub <- resample(clim, dem.elev, method = 'cubic')

par(mfrow = c(1,3)) 
plot(clim.30.bl$pr, main = "bilinear")
plot(clim.30.cub$pr, main = "cubic")
plot(clim.30.nn$pr, main = "nearest neighbor")

par(mfrow = c(1,2)) 
plot(clim$pr)
plot(clim.30.nn$pr)



###### resample all clim data

in.dir <- here('data', 'raw', 'background_variables', 'tif')
out.dir.50 <- here('data', 'processed', 'processed', 'tif', '50m') 
out.dir.500 <- here('data', 'processed', 'processed', 'tif', '500m') 

clim.files <- list.files(in.dir, pattern = '^creek_terraclimate.*\\.tif$', full.names = T)



### probably have to redo this after I mask out the upper and lower elevations

# resample to 50m
for (f in clim.files) {
  r = rast(f)
  r.50m = resample(r, swe, method = 'near') # change cubic to best method

  new.name = sub('\\.tif$', '_50m.tif', basename(f))
  out.name = file.path(out.dir.50, new.name)

  writeRaster(r.50m, out.name, overwrite = T)
}

check <- rast(file.path(out.dir.50, 'creek_terraclimate_wy2018_50m.tif'))
plot(check)

# resample to 500m
for (f in clim.files) {
  r = rast(f)
  r.500m = resample(r, sdd, method = 'near') # change cubic to best method
  
  new.name = sub('\\.tif$', '_500m.tif', basename(f))
  out.name = file.path(out.dir.500, new.name)
  
  writeRaster(r.500m, out.name, overwrite = T)
}

check <- rast(file.path(out.dir.500, 'creek_terraclimate_wy2018_500m.tif'))
plot(check)



