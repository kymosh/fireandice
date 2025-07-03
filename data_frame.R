# Load packages
packages <- c( 'here', 'exactextractr', 'raster', 'sf', 'terra', 'geodata',
               'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr', 'stringr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# ASO
# Directory with input TIFs
tif.dir <- here('data', 'processed', 'processed', 'tif')

# List files with matching keywords
tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = TRUE)

# Filter files by basin
creek.tifs   <- tif.files[str_detect(tif.files, 'SanJoaquin.*clipped')]

file_info <- tibble(fname = basename(creek.tifs)) %>%
  mutate(
    fire       = 'creek',
    year       = str_extract(fname, '\\d{4}'),
    month      = str_extract(fname, '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)'),
    day        = str_extract(fname, '(?<=\\d{4}[A-Za-z]{3})\\d{1,2}'),  # 1- or 2-digit day
    date       = as.Date(str_c(year, month, str_pad(day, 2, pad = '0'), sep = '-'), format = '%Y-%b-%d'),
    variable   = str_extract(fname, '(?<=_)swe|depth|melt(?=_)'),  # tweak if needed
    resolution = str_extract(fname, '\\d{2,}m')
  )

# remove report tifs/superswe tifs
file_info <- file_info[-c(1, 2, 18), ]



##################### Creek Fire terrain variable stack
# create list of all creek variable files
# variable base names
var.names <- c('tpi_130', 'tpi_510', 'tpi_2010', 'slope', 'hli', 'dem')
# build full paths
creek.variables <- here('data', 'processed', 'processed', 'tif', paste0(var.names, '_creek_32611.tif'))


###### check CRS of all variables
unique(sapply(creek.variables, function(p) crs(rast(p), describe = TRUE)$code))
# all 32611, g2g


###### check resolution of all variables
unique(sapply(creek.variables, function(p) paste(res(rast(p)), collapse = 'x')))

# choose smallest res (10m) raster as reference raster
ref <- rast(creek.variables[which.min(sapply(creek.variables, function(p) prod(res(rast(p)))))])
# resample rasters to match
creek.variables.resampled <- lapply(creek.variables, function(p) {
  r <- rast(p)
  resample(r, ref, method = 'bilinear') 
})

unique(sapply(creek.variables.resampled, function(p) paste(res(rast(p)), collapse = 'x')))
# all 10m, g2g


###### check extents for all variables
names(creek.variables.resampled) <- var.names
for (name in names(creek.variables.resampled)) {
  r <- creek.variables.resampled[[name]]
  cat(name, '\n')
  print(ext(r))
  cat('\n')
}
# g2g

creek.stack <- rast(creek.variables.resampled)
creek.terrain.df <- as.data.frame(creek.stack, xy = TRUE, na.rm = TRUE)




######### re-scale variables to 0-1
# copy for safety
creek.terrain.scaled.df <- creek.terrain.df

# Get variable names to rescale (excluding x, y, and hli)- hli is already rescaled
vars.to.scale <- setdiff(names(creek.scaled.df), c('x', 'y', 'hli'))

# re-scale selected variables
creek.scaled.df[vars.to.scale] <- lapply(creek.scaled.df[vars.to.scale], function(x) {
  (x - min(x)) / (max(x) - min(x))
})
