####### this was all test code to make sure clipping the tifs would work before clipping them all together in a for loop

# Load packages
# packages <- c( 'here', 'exactextractr', 'raster', 'sf', 'terra', 'geodata',
#   'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr')
# install.packages(setdiff(packages, rownames(installed.packages())))
# lapply(packages, library, character.only = TRUE)
# 
# # Study extents
# castle.extent <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_castle_32611.shp'))
# creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))
# 
# # Visualize extents
# plot(st_geometry(creek.extent), col = adjustcolor('green', alpha.f = 0.5), main = 'Creek Study Extent')
# plot(st_geometry(castle.extent), col = adjustcolor('purple', alpha.f = 0.5), main = 'Castle Study Extent')
# 
# # Load ASO SWE raster
# tif <- rast(here('data', 'raw', 'ASO', 'tif', 'ASO_SanJoaquin_2023Jan21-24_swe_50m.tif'))
# 
# # Check coordinate reference systems
# st_crs(creek.extent) == st_crs(tif)
# 
# # Clip tif to study extent
# tif.crop <- crop(tif, vect(creek.extent))    # clip to bounding box
# tif.mask <- mask(tif.crop, vect(creek.extent))  # mask to actual shape

######################
# Load packages
packages <- c( 'here', 'exactextractr', 'raster', 'sf', 'terra', 'geodata',
  'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr', 'stringr', 'tools')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)


# Read study extents
castle.extent <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_castle_32611.shp'))
creek.extent  <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))





######################
# ASO tifs

# Directory with input ASO tifs
tif.dir <- here('data', 'raw', 'ASO', 'tif')

# Output directory
out.dir.tif <- here('data', 'processed', 'processed', 'tif')
dir.create(out.dir.tif, recursive = TRUE, showWarnings = FALSE)

# List files with matching keywords
tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = TRUE)

# Filter files by basin
creek.tifs   <- tif.files[str_detect(tif.files, 'SanJoaquin')]
kern.tifs   <- tif.files[str_detect(tif.files, 'Kern')]
kaweah.tifs <- tif.files[str_detect(tif.files, 'Kaweah')]

# Combine Kern and Kaweah
castle.tifs <- c(kern.tifs, kaweah.tifs)

# Helper function to clip and save rasters
clip.and.save <- function(file.path, extent.sf, out.dir.tif) {
  # Load raster
  tif <- rast(file.path)
  
  # Reproject extent to match raster
  extent.sf <- st_transform(extent.sf, crs(tif))
  
  # Clip to bounding box and mask to shape
  tif.crop <- crop(tif, vect(extent.sf))
  tif.mask <- mask(tif.crop, vect(extent.sf))
  
  # Create output name
  base.name <- tools::file_path_sans_ext(basename(file.path))
  out.name <- paste0(base.name, '_clipped.tif')
  out.path <- file.path(out.dir.tif, out.name)
  
  # Save
  writeRaster(tif.mask, out.path, overwrite = TRUE)
}

# Clip SanJoaquin files to creek.extent
for (f in creek.tifs) {
  clip.and.save(f, creek.extent, out.dir.tif)
}

# Clip Kern and Kaweah files to castle.extent
for (f in castle.tifs) {
  clip.and.save(f, castle.extent, out.dir.tif)
}






####################
# img topography files

# Input directory containing .img files
img.dir <- here('data', 'raw', 'background_variables', 'img')

# Output directory for clipped .img files
out.dir.tif <- here('data', 'processed', 'processed', 'tif')
dir.create(out.dir.tif, recursive = TRUE, showWarnings = FALSE)

# List all .img files (ignore .ige, .aux, etc.)
img.files <- list.files(img.dir, pattern = '\\.img$', full.names = TRUE)

# Helper function to clip and save rasters
clip.and.save <- function(file.path, extent.sf, fire, out.dir.tif) {
  message('Processing: ', basename(file.path))
  
  # Load raster
  r <- rast(file.path)
  
  # Reproject extent to match raster CRS
  extent.sf <- st_transform(extent.sf, crs(r))
  
  # Clip and mask
  r.crop <- crop(r, vect(extent.sf))
  r.mask <- mask(r.crop, vect(extent.sf))
  
  # Create output filename
  base.name <- file_path_sans_ext(basename(file.path))
  out.name <- paste0(base.name, '_', fire, '.tif')
  out.path <- file.path(out.dir.tif, out.name)
  
  # Save
  writeRaster(r.mask, out.path, overwrite = TRUE)
}

# Loop over all .img files and clip to creek extent
lapply(img.files, clip.and.save,
       extent.sf = creek.extent,
       fire = 'creek',
       out.dir.tif = out.dir.tif)

# same for castle fire
lapply(img.files, clip.and.save,
       extent.sf = castle.extent,
       fire = 'castle',
       out.dir.tif = out.dir.tif)
