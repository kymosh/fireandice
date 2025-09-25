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
clim.30.rms <- resample(clim, dem.elev, method = 'rms')

par(mfrow = c(1,3)) 
plot(clim.30.bl$pr)
plot(clim.30.rms$pr)
plot(clim.30.nn$pr)

par(mfrow = c(1,2)) 
plot(clim$pr)
plot(clim.30.nn$pr)

clim.elev <- mask(clim.30.bl, dem.elev)


