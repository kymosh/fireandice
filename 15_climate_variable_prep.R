packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'spatialEco')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

clim <- rast(here('data', 'raw', 'background_variables', 'tif', 'creek_terraclimate_wy2018.tif'))
plot(clim)

dem.elev <- rast(here('data', 'processed', 'processed', 'tif', 'nasadem_creek_elev.tif'))
plot(dem.elev)

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

clim.elev <- mask(clim.30.bl, dem.elev)



###### resample all clim data

## wait to run until figure out resample method
# 
# in.dir <- here('data', 'raw', 'background_variables', 'tif')
# out.dir <- here('data', 'processed', 'processed', 'tif')
# 
# clim.files <- list.files(in.dir, pattern = '^creek_terraclimate.*\\.tif$', full.names = T)
# 
# for (f in clim.files) {
#   r = rast(f)
#   r.30m = resample(r, dem.elev, method = 'cubic') # change cubic to best method
#   
#   new.name = sub('\\.tif$', '_30m.tif', basename(f))
#   out.name = file.path(out.dir, new.name)
#   
#   writeRaster(r.30m, out.name, overwrite = T)
# }
