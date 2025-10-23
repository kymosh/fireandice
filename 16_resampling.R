packages <- c('tidyverse', 'terra')
install.packages(set.diff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# resample all data to SWE resolution

in.dir <- here('data', 'processed', 'processed', 'tif')
out.dir.50 <- here(in.dir, '50m')
out.dir.500 <- here(in.dir, '500m')

topo.cbi <- list.files(here(in.dir, '30m'), pattern = 'topo|cbi', full.names = T)

# target resolution
swe <- rast(here(in.dir, '50m', 'ASO_SanJoaquin_2024_0127_swe_50m_1524.tif'))
sdd <- rast(here(in.dir, '500m', 'creek_sdd_wy2020_32611_1524.tif'))

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
# test.topo <- rast(here(in.dir, '30m', 'creek_topo_slope_30m_1524.tif'))
# plot(test.topo)
# test.topo.500m <- resample(test.topo, sdd, method = 'average')
# plot(test.topo.500m)

# resample to 50m to match swe
for (f in topo.cbi) {
  r <- rast(f)
  r.50m <- resample(r, swe, method = 'average')
  
  new.name <- sub('30m', '50m', basename(f))
  new.path <- file.path(out.dir.50, new.name)
  
  writeRaster(r.50m, new.path, overwrite = T)
}

test <- rast(here(out.dir, 'creek_cbibc_30m_1524.tif'))
plot(test)
res(test)



# resample to 500m to match sdd
for (f in topo.cbi) {
  r <- rast(f)
  r.500m <- resample(r, sdd, method = 'average')
  
  new.name <- sub('30m', '500m', basename(f))
  new.path <- file.path(out.dir.500, new.name)
  
  writeRaster(r.500m, new.path, overwrite = T)
}

test <- rast(here(out.dir.500, 'creek_topo_hli_500m_1524.tif'))
plot(test)
res(test)





