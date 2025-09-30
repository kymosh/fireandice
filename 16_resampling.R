packages <- c('tidyverse', 'terra')
install.packages(set.diff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# resample all data to SDD resolution

in.dir <- here('data', 'processed', 'processed', 'tif')
out.dir <- here(in.dir, '50m')

topo.cbi <- list.files(here(in.dir, '30m'), full.names = T)

# target resolution
swe <- rast(here(in.dir, 'ASO_SanJoaquin_2024_0127_swe_50m_1524_2674.tif'))

# check to make sure all are in crs 32611 and res is 30m
for (f in topo.cbi) {
  r <- rast(f)
  print(crs(r, describe = T)$code)
  }

for (f in topo.cbi) {
  r <- rast(f)
  print(res(r))
}

# test
test.topo <- rast(here(in.dir, '30m', 'creek_topo_slope.tif'))
plot(test.topo)
test.topo.50m <- resample(test.topo, swe, method = 'average')
plot(test.topo.50m)

# resample to 50m to match swe
for (f in topo.cbi) {
  r <- rast(f)
  r.50m <- resample(r, swe, method = 'average')
  
  new.name <- sub('\\.tif$', '_50m_1524_2674.tif', basename(f))
  new.path <- file.path(out.dir, new.name)
  
  writeRaster(r.50m, new.path, overwrite = T)
}

test <- rast(here(out.dir, 'creek_topo_aspect_50m.tif'))
plot(test)
res(test)



