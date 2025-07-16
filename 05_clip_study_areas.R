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
# Load ASO SWE raster
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

# Read DEMs
dem.castle <- rast(here('data', 'processed', 'processed', 'tif', 'dem_castle_32611.tif'))
dem.creek <- rast(here('data', 'processed', 'processed', 'tif', 'dem_creek_32611.tif'))

# Load sample ASO SWE raster
tif <- rast(here('data', 'raw', 'ASO', 'tif', 'ASO_SanJoaquin_2023Jan21-24_swe_50m.tif'))

# make sure tifs match extents and dem CRS
crs(dem.castle, describe = T)$code == crs(tif, describe = T)$code
crs(tif, describe = T)$code == crs(castle.extent, describe = T)$code

######################
# create mask to limit elevations to >5000ft (1524m)
creek.mask <- dem.creek > 1524
castle.mask <- dem.castle > 1524

# use mask to filter DEM
dem.creek.5000 <- mask(dem.creek, creek.mask, maskvalue = 0)
dem.castle.5000 <- mask(dem.castle, castle.mask, maskvalue = 0)
plot(dem.castle.5000)
plot(dem.creek.5000)

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
clip.and.save <- function(file.path, extent.sf, out.dir.tif, dem.masked) {
  # Load SWE raster
  tif <- rast(file.path)
  
  # Reproject extent to match SWE raster
  extent.sf <- st_transform(extent.sf, crs(tif))
  
  # Clip to bounding box and mask to shape
  tif.crop <- crop(tif, vect(extent.sf))
  tif.mask <- mask(tif.crop, vect(extent.sf))
  
  # Resample DEM to match SWE raster
  if (!compareGeom(tif.mask, dem.masked, stopOnError = FALSE)) {
    dem.masked <- resample(dem.masked, tif.mask, method = 'near')
  }
  
  # Apply elevation mask for >5000ft (1524m)
  tif.elev.masked <- mask(tif.mask, dem.masked, maskvalue = NA)
  
  # Create output name
  base.name <- tools::file_path_sans_ext(basename(file.path))
  out.name <- paste0(base.name, '_clipped.tif')
  out.path <- file.path(out.dir.tif, out.name)
  
  # Save
  writeRaster(tif.elev.masked, out.path, overwrite = TRUE)
}

# Clip SanJoaquin files to creek.extent
for (f in creek.tifs) {
  clip.and.save(f, creek.extent, out.dir.tif, dem.creek.5000)
}

# Clip Kern and Kaweah files to castle.extent
for (f in castle.tifs) {
  clip.and.save(f, castle.extent, out.dir.tif, dem.castle.5000)
}


# check example
creek.tif <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_SanJoaquin_Mosaic_2021Mar31-Apr1_swe_50m_clipped.tif'))
castle.tif <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_Kaweah_2024Feb11_swe_50m_clipped.tif'))
# 
# plot(creek.tif)
# plot(castle.tif)
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
clip.and.save <- function(file.path, extent.sf, fire, out.dir.tif, dem.masked, template.raster) {
  message('Processing: ', basename(file.path))
  
  # Load raster
  r <- rast(file.path)
  
  # Reproject extent to match raster CRS
  extent.sf <- st_transform(extent.sf, crs(r))
  
  # Clip and mask
  r.crop <- crop(r, vect(extent.sf))
  r.mask <- mask(r.crop, vect(extent.sf))
  
  # Reproject elevation mask to match CRS
  if (!crs(dem.masked) == crs(r.mask)) {
    dem.masked <- project(dem.masked, r.mask)
  }
  
  # Then check geometry and resample if needed
  if (!compareGeom(r.mask, dem.masked, stopOnError = FALSE)) {
    dem.masked <- resample(dem.masked, r.mask, method = 'near')
  }
  
  # Mask to >5000ft
  r.elev.masked <- mask(r.mask, dem.masked, maskvalue = NA)
  
  # Reproject and resample to SWE template
  if (crs(r.elev.masked) != crs(template.raster)) {
    r.elev.masked <- project(r.elev.masked, template.raster)
  }
  
  r.aligned <- resample(r.elev.masked, template.raster, method = 'bilinear')
  
  # Create output filename
  base.name <- file_path_sans_ext(basename(file.path))
  out.name <- paste0(base.name, '_', fire, '.tif')
  out.path <- file.path(out.dir.tif, out.name)
  
  # Save
  writeRaster(r.aligned, out.path, overwrite = TRUE)
}

# Loop over all .img files and clip to creek extent
lapply(img.files, clip.and.save,
       extent.sf = creek.extent,
       fire = 'creek',
       out.dir.tif = out.dir.tif,
       dem.masked = dem.creek.5000,
       template.raster = creek.tif)


# same for castle fire
lapply(img.files, clip.and.save,
       extent.sf = castle.extent,
       fire = 'castle',
       out.dir.tif = out.dir.tif,
       dem.masked = dem.castle.5000,
       template.raster = castle.tif)

# check
slope.creek <- rast(here('data', 'processed', 'processed', 'tif', 'slope_creek.tif'))
plot(slope.creek)
slope.castle <- rast(here('data', 'processed', 'processed', 'tif', 'slope_castle.tif'))
plot(slope.castle)
aspect.castle <- rast(here('data', 'processed', 'processed', 'tif', 'aspect_castle.tif'))
plot(aspect.castle)
