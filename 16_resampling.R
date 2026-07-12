packages <- c('tidyverse', 'dplyr', 'terra', 'exactextractr')
lapply(packages, library, character.only = T)

# decided on 2/25/26 to go back and re-resample these. I had previously used terra's resample but later realized that exactextractr would do a better job at resampling because of the odd number (452m2) that the target resolution for the SDD data is. 

# resample all data to SWE resolution

# inputs
fire <- 'dixie'
res <- '500m' # 50 or 500

# settings (don't touch)
in.dir <- 'data/processed/processed/tif/'
out.dir <- paste0(in.dir, res, '/', fire)

# target resolution
swe <- rast(paste0(in.dir, '50m/', fire, '/canopy_metrics/', fire, '_cover_metrics_50m.tif'))
swe <- swe[[1]]
sdd <- rast(paste0(in.dir, '500m/', fire, '/snow_metrics/', fire, '_sdd_wy2023_500m.tif'))

template <- if (res == '50m') {
  swe
} else {
  sdd
}


files <- list.files(paste0(in.dir, '30m/', fire), pattern = '\\.tif$', full.names = TRUE)
files <- files[!grepl('nalcms', basename(files))] # remove landcover because it's categorical and will need to be resampled differently!


# check to make sure all have same crs, res, and origin

for (f in files) {
  r <- rast(f)
  plot(r)
}

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
  
  # resample each layer separately
  r.resamp <- lapply(1:nlyr(r), function(i) {
    exactextractr::exact_resample(
      r[[i]],
      template[[1]],
      fun = 'mean'
    )
  })
  
  # combine layers back into one SpatRaster
  r.resamp <- rast(r.resamp)
  
  # preserve original layer names
  names(r.resamp) <- names(r)
  
  # mask to the spatial footprint of the template
  r.resamp <- mask(r.resamp, template)
  
  new.name <- sub('30m', res, basename(f))
  new.path <- file.path(out.dir, new.name)
  
  writeRaster(
    r.resamp,
    new.path,
    overwrite = TRUE
  )
}

# ----- resample just landcover -----
f <- paste0('data/processed/processed/tif/30m/', fire, '/', fire, '_nalcms_30m.tif')

r <- rast(f)

# resample each layer separately
r.resamp <- lapply(1:nlyr(r), function(i) {
  exactextractr::exact_resample(
    r[[i]],
    template[[1]],
    fun = 'mode'
  )
})

# combine layers back into one SpatRaster
r.resamp <- rast(r.resamp)

# preserve original layer names
names(r.resamp) <- names(r)

# mask to the spatial footprint of the template
r.resamp <- mask(r.resamp, template)

new.name <- sub('30m', res, basename(f))
new.path <- file.path(out.dir, new.name)

writeRaster(
  r.resamp,
  new.path,
  overwrite = TRUE)

plot(rast(paste0('data/processed/processed/tif/', res, '/', fire, '/', fire, '_nalcms_', res, '.tif')))

# ----- fix CBI so it is 0 outside study area -----

cbi <- rast(paste0(out.dir, '/', fire, '_cbibc_', res, '.tif'))
cbi[is.na(cbi) & !is.na(template[[1]])] <- 0
cbi <- mask(cbi, template[[1]])
plot(cbi)
writeRaster(cbi, paste0(out.dir, '/', fire, '_cbibc_', res, '.tif'), overwrite = T)

test <- list.files(out.dir, full.names = T, pattern = '\\.tif$')
test <- test[3]
for (x in test) {
  r <- rast(x)
  plot(r)
}
