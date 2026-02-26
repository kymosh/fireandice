# read in all raster stacks and combine into single one for modeling

# combine all 500m rasters into single stack
# this is just a test so far to see if everthing matches and is able to be stacked. 
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek'
files <- list.files(dir, 'creek', full.names = TRUE)

sdd.stack <- rast(files)
# it worked!

names(sdd.stack)


dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
files <- list.files(dir, 'creek', full.names = TRUE)

swe.stack <- rast(files)
# extents don't match, unable to be stacked :(

# check extent
for (f in files) {
  r <- rast(f)
  print(ext(r))
}

files[1]
