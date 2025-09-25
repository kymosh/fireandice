packages <- c( 'here', 'terra', 
               'tidyverse', 'sf', 'spatialEco')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

dem <- rast(here('data', 'raw', 'background_variables', 'tif', 'nasadem_creek.tif'))

# inspect dems
print(dem)
plot(dem)
hist(dem)
plot(is.na(dem), main = 'NA cells in DEM')

dem.5000 <- rast(here('data', 'processed', 'processed', 'tif', 'nasadem_creek_5000.tif'))
print(dem.5000)
plot(dem.5000)
hist(dem.5000)

# calculate variables
slope.rad <- terrain(dem.5000, v = 'slope', unit = 'radians')
aspect.rad <- terrain(dem.5000, v = 'aspect', unit = 'radians')
hli <- hli(dem.5000) #uses McCune(2007) calculation for hli

plot(slope.rad)
plot(aspect.rad)
plot(hli)

# TPI at 150, 510, and 2010

# calculate scales from meters to pixels to calculate TPI
tpi150 <- tpi(dem, win = "circle", s = 150)
tpi510 <- tpi(dem, win = "circle", s = 510)
tpi1200 <- tpi(dem, win = "circle", s = 1200)
tpi2010 <- tpi(dem, win = "circle", s = 2010)

plot(tpi150)
plot(tpi510)
plot(tpi1200)
plot(tpi2010)

# mask to only include above 5000ft
tpi150.5000 <- mask(tpi150, dem.5000)
tpi510.5000 <- mask(tpi510, dem.5000)
tpi1200.5000 <- mask(tpi1200, dem.5000)
tpi2010.5000 <- mask(tpi2010, dem.5000)

# write files
out.dir <- here('data', 'processed', 'processed', 'tif')

# tpi
writeRaster(tpi150.5000, filename = file.path(out.dir, 'creek_tpi150.tif'), overwrite = TRUE)
writeRaster(tpi510.5000, filename = file.path(out.dir, 'creek_tpi510.tif'), overwrite = TRUE)
writeRaster(tpi1200.5000, filename = file.path(out.dir, 'creek_tpi1200.tif'), overwrite = TRUE)
writeRaster(tpi2010.5000, filename = file.path(out.dir, 'creek_tpi2010.tif'), overwrite = TRUE)

clim <- rast(here('data', 'raw', 'background_variables', 'tif', 'creek_terraclimate_2018_jan_jul.tif'))
plot(clim)
