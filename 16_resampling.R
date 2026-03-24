packages <- c('tidyverse', 'dplyr', 'terra', 'exactextractr')
lapply(packages, library, character.only = T)

# decided on 2/25/26 to go back and re-resample these. I had previously used terra's resample but later realized that exactextractr would do a better job at resampling because of the odd number (452m2) that the target resolution for the SDD data is. 

# resample all data to SWE resolution

in.dir <- 'data/processed/processed/tif'
out.dir.50 <- file.path(in.dir, '50m/creek/other_metrics')
out.dir.500 <- file.path(in.dir, '500m/creek/other_metrics')

files <- list.files(file.path(in.dir, '30m/creek'), pattern = paste0('topo|cbi|nasadem', '_1524\\.tif$'), full.names = T)

# target resolution
swe <- rast(file.path(in.dir, '50m/creek/snow_metrics/ASO_SanJoaquin_2020_0414_swe_50m_1524.tif'))
sdd <- rast(file.path(in.dir, '500m/creek/snow_metrics/creek_sdd_wy2020_32611_1524.tif'))

# check to make sure all are in crs 32611 and res is 30m
for (f in files) {
  r <- rast(f)
  print(crs(r, describe = T)$code)
  }

for (f in files) {
  r <- rast(f)
  print(res(r))
}

# resample to 50m to match swe
for (f in files) {
  r <- rast(f)
  r.50m <- exact_resample(r, swe, fun = 'mean') 
  
  new.name <- sub('30m', '50m', basename(f))
  new.path <- file.path(out.dir.50, new.name)
  
  writeRaster(r.50m, new.path, overwrite = T)
}

test <- rast( file.path(out.dir.50, 'creek_topo_aspect_50m_1524.tif'))
plot(test)
res(test)
origin(test)


# resample to 500m to match sdd
for (f in files) {
  r <- rast(f)
  r.500m <- exact_resample(r, sdd, fun = 'mean')
  
  new.name <- sub('30m', '500m', basename(f))
  new.path <- file.path(out.dir.500, new.name)
  
  writeRaster(r.500m, new.path, overwrite = T)
}

test <- rast(file.path(out.dir.500, 'creek_topo_hli_500m_1524.tif'))
plot(test)
res(test)
origin(test)



# rename files and move
old.files <- list.files(file.path(out.dir.500, 'other_metrics'), pattern = '^creek_topo', full.names = T)
dest.dir <- file.path(out.dir.500, 'old_versions')
new.names <- paste0(sub('\\.tif$', '', basename(old.files)), '_unbufferedDEM.tif')
new.paths <- file.path(dest.dir, new.names)
file.rename(old.files, new.paths)
