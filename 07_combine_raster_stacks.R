
# combine like files into single raster stack

# ------ terraclimate ------

# 50m or 500m just change in our dir
out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek'

clim.files <- list.files(out.dir, 'creek_terraclimate', full.names = T)

clim.stack <- rast(clim.files)

# get WY tag per file
fn <- basename(clim.files)
wy <- sub('.*(wy\\d{4}).*', '\\1', fn)   # wy2018, wy2019, ...

# get variable names inside each file (assume consistent across files)
vars <- names(rast(clim.files[1]))       # pr, swe, tmmx, tmmn

# make full names in the same order terra stacks them:
# (terra stacks file1 layers, then file2 layers, etc.)
new.names <- as.vector(sapply(wy, function(w) paste0('clim_', vars, '_', w)))

names(clim.stack) <- new.names
names(clim.stack)

writeRaster(clim.stack, file.path(out.dir, 'creek_terraclimate_500m_1524.tif'))

# ------ topo --------
# change to either 50m or 500m
out.dir <- 'data/processed/processed/tif/500m/creek'
#out.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/other_metrics'
topo.files <- list.files(file.path(out.dir, 'other_metrics'), 'creek_topo|nasadem', full.names = T)
topo.stack  <- rast(topo.files)

fn <- basename(topo.files)

# extract everything after "creek_topo_" and before "_500m"
nm <- sub('creek_topo_(.*)_500m.*', '\\1', fn)

# handle DEM separately (nasadem file)
nm[grepl('nasadem', fn)] <- 'elev'

# add topo prefix
names(topo.stack) <- paste0('topo_', nm)

names(topo.stack)

writeRaster(topo.stack, file.path(out.dir, 'creek_topo_500m.tif'), overwrite = TRUE)

# ------ sdd (500m only) ------

out.dir <- 'data/processed/processed/tif/500m/creek'
in.dir <- file.path(out.dir, 'snow_metrics')
sdd.files <- list.files(in.dir, pattern = '^creek_sdd', full.names = T)
sdd.stack <- rast(sdd.files)

fn <- basename(sdd.files)
# extract wyYYYY
wy <- sub('.*(wy\\d{4}).*', '\\1', fn)
# assign names
names(sdd.stack) <- paste0('sdd_', wy)
# check names
names(sdd.stack)

writeRaster(sdd.stack, file.path(out.dir, 'creek_sdd_500m.tif'), overwrite = TRUE)

# copy to backups
dest1 <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek/creek_sdd_500m.tif'
dest2 <- 'G:/Fire_Snow_Dynamics_backup/data/processed/processed/tif/500m/creek/creek_sdd_500m.tif'
writeRaster(sdd.stack, dest1, overwrite = TRUE)
writeRaster(sdd.stack, dest2, overwrite = TRUE)
