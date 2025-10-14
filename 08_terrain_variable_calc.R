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

dem.elev <- rast(here('data', 'processed', 'processed', 'tif', '30m', 'nasadem_creek_30m_1524.tif'))


# calculate variables
slope.rad <- terrain(dem.elev, v = 'slope', unit = 'radians')
aspect.rad <- terrain(dem.elev, v = 'aspect', unit = 'radians')
hli <- hli(dem.elev) #uses McCune(2007) calculation for hli

plot(slope.rad)
plot(aspect.rad)
plot(hli)

# write files
out.dir <- here('data', 'processed', 'processed', 'tif', '30m')

writeRaster(slope.rad, filename = file.path(out.dir, 'creek_topo_slope_30m_1524.tif'), overwrite = TRUE)
writeRaster(hli, filename = file.path(out.dir, 'creek_topo_hli_30m_1524.tif'), overwrite = TRUE)
writeRaster(aspect.rad, filename = file.path(out.dir, 'creek_topo_aspect_30m_1524.tif'), overwrite = TRUE)

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
tpi150.elev <- mask(tpi150, dem.elev)
tpi510.elev <- mask(tpi510, dem.elev)
tpi1200.elev <- mask(tpi1200, dem.elev)
tpi2010.elev <- mask(tpi2010, dem.elev)

# write files
writeRaster(tpi150.elev, filename = file.path(out.dir, 'creek_topo_tpi150_30m_1524.tif'), overwrite = TRUE)
writeRaster(tpi510.elev, filename = file.path(out.dir, 'creek_topo_tpi510_30m_1524.tif'), overwrite = TRUE)
writeRaster(tpi1200.elev, filename = file.path(out.dir, 'creek_topo_tpi1200_30m_1524.tif'), overwrite = TRUE)
writeRaster(tpi2010.elev, filename = file.path(out.dir, 'creek_topo_tpi2010_30m_1524.tif'), overwrite = TRUE)


