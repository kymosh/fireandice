dir <- 'data/processed/processed/tif/50m/creek'
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'

fd <- rast(file.path(dir, 'canopy_metrics/fractal_dim_32611/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7732_norm_fractal_dim_50m.tif'))
gap <- rast(file.path(dir, 'canopy_metrics/gap_dist_32611/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7732_norm_gap_dist_metrics_50m.tif'))
height <- rast(file.path(dir, 'canopy_metrics/height_metrics_32611/height_USGS_LPC_CA_SierraNevada_B22_11SKB7732_norm.tif'))
dem50 <- rast(file.path(dir, 'other_metrics/nasadem_creek_50m_1524.tif'))

# CRS is all correct
crs(fd) == crs(gap) # TRUE
crs(gap) == crs(height) # TRUE

# fd and gap (calc from CHM) have origin of 0, 0
origin(fd) == origin(gap) # TRUE
origin(gap) == origin(height) # FALSE
# height has incorrect origin

origin(dem50)
origin(fd)
origin(height)


snow.metrics <- list.files(
  file.path(dir, 'snow_metrics'), 
  pattern = '\\.tif$',
  full.names = T)

height.metrics <- list.files(
  file.path(dir, 'canopy_metrics/height_metrics_32611'),
  pattern = '\\.tif$',
  full.names = T)

for (f in height.metrics) {
  r <- rast(f)
  cat(basename(f), 'origin:', origin(r), '\n')
}

for (f in snow.metrics) {
  r <- rast(f)
  cat(basename(f), 'origin:', origin(r), '\n')
}
# origin of 2020 - 2022 is different, the rest is 0,0

for (f in snow.metrics) {
  r <- rast(f)
  cat(basename(f), 'CRS:', crs(r, describe = T)$code, '\n')
}
# CRS all 32611

for (f in snow.metrics) {
  r <- rast(f)
  cat(basename(f), 'res:', res(r), '\n')
}
# 2020 - 2022 have resolution that is slightly off (50.0001)