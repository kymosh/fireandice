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
tif <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_Kaweah_2021_0504_swe_50m_clipped.tif'))

# make sure tifs match extents and dem CRS
crs(dem.castle, describe = T)$code == crs(tif, describe = T)$code
crs(tif, describe = T)$code == crs(castle.extent, describe = T)$code
crs(dem.creek, describe = T)$code == crs(tif, describe = T)$code
crs(tif, describe = T)$code == crs(creek.extent, describe = T)$code


######################

# create mask to limit elevations to >5000ft (1524m)
creek.mask <- dem.creek > 1524
castle.mask <- dem.castle > 1524

# use mask to filter DEM
dem.creek.5000 <- mask(dem.creek, creek.mask, maskvalue = 0)
dem.castle.5000 <- mask(dem.castle, castle.mask, maskvalue = 0)
# plot(dem.castle.5000)
# plot(dem.creek.5000)








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

# one more time to file that had been missing
clip.and.save(
  file.path = 'G:/Fire_Snow_Dynamics/data/raw/ASO/tif/ASO_SanJoaquin_2024Apr29-May01_swe_50m.tif',
  extent.sf = creek.extent,
  out.dir.tif = out.dir.tif,
  dem.masked = dem.creek.5000
)

# Clip Kern and Kaweah files to castle.extent
for (f in castle.tifs) {
  clip.and.save(f, castle.extent, out.dir.tif, dem.castle.5000)
}







####################
# img topography files


# 
# # let's just do aspect for now
# # create elevation mask >5000ft (1524m)
# castle.elev.mask <- dem.castle > 1524
# castle.elev.mask <- resample(castle.elev.mask, aspect.castle.32611, method = 'near')
# 
# # reproject to 32611 and save
# aspect.32611 <- project(aspect, 'EPSG:32611', method = 'bilinear')
# writeRaster(aspect.32611, here('data', 'processed', 'processed', 'tif', 'aspect_32611.tif'), overwrite = TRUE)
# 
# # crop and mask to castle extent
# aspect.castle.32611 <- crop(aspect.32611, vect(castle.extent)) %>%
#   mask(vect(castle.extent))
# 
# # apply elevation mask
# aspect.castle.5000 <- mask(aspect.castle.32611, castle.elev.mask, maskvalue = FALSE)
# 
# # save
# writeRaster(aspect.castle.5000, here('data', 'processed', 'processed', 'tif', 'aspect_castle_5000.tif'), overwrite = TRUE)







# create elevation mask >5000ft (1524m)
castle.elev.mask <- dem.castle > 1524
creek.elev.mask <- dem.creek > 1524


# now loop through all img files
# Input and output paths
img.dir <- here('data', 'raw', 'background_variables', 'img')
out.dir.tif <- here('data', 'processed', 'processed', 'tif')

# Input directory containing .img files
img.files <- list.files(img.dir, pattern = '\\.img$', full.names = TRUE)

# CREEK
for (img.path in img.files) {
  
  # Construct base name and intermediate reprojection path
  base.name <- tools::file_path_sans_ext(basename(img.path))
  r32611.path <- file.path(out.dir.tif, paste0(base.name, '_32611.tif'))
  
  # Check if reprojected raster already exists
  if (file.exists(r32611.path)) {
    r.32611 <- rast(r32611.path)
  } else {
    # Load .img raster
    r <- rast(img.path)
    
    # Reproject to EPSG:32611
    r.32611 <- project(r, 'EPSG:32611', method = 'bilinear')
    
    # Save reprojected raster
    writeRaster(r.32611, r32611.path, overwrite = TRUE)
  }
  
  # Crop and mask to creek extent
  r.creek <- crop(r.32611, vect(creek.extent)) %>%
    mask(vect(creek.extent))
  
  # Resample elevation mask to match resolution and extent
  elev.mask.resampled <- resample(creek.elev.mask, r.creek, method = 'near')
  
  # Apply elevation mask
  r.final <- mask(r.creek, elev.mask.resampled, maskvalue = FALSE)
  
  # Construct output filename for final clipped raster
  out.path <- file.path(out.dir.tif, paste0(base.name, '_creek_5000.tif'))
  
  # Save final clipped raster
  writeRaster(r.final, out.path, overwrite = TRUE)
}

# CASTLE
for (img.path in img.files) {
  
  # Construct base name and intermediate reprojection path
  base.name <- tools::file_path_sans_ext(basename(img.path))
  r32611.path <- file.path(out.dir.tif, paste0(base.name, '_32611.tif'))
  
  # Check if reprojected raster already exists
  if (file.exists(r32611.path)) {
    r.32611 <- rast(r32611.path)
  } else {
    # Load .img raster
    r <- rast(img.path)
    
    # Reproject to EPSG:32611
    r.32611 <- project(r, 'EPSG:32611', method = 'bilinear')
    
    # Save reprojected raster
    writeRaster(r.32611, r32611.path, overwrite = TRUE)
  }
  
  # Crop and mask to creek extent
  r.castle <- crop(r.32611, vect(castle.extent)) %>%
    mask(vect(castle.extent))
  
  # Resample elevation mask to match resolution and extent
  elev.mask.resampled <- resample(castle.elev.mask, r.castle, method = 'near')
  
  # Apply elevation mask
  r.final <- mask(r.castle, elev.mask.resampled, maskvalue = FALSE)
  
  # Construct output filename for final clipped raster
  out.path <- file.path(out.dir.tif, paste0(base.name, '_castle_5000.tif'))
  
  # Save final clipped raster
  writeRaster(r.final, out.path, overwrite = TRUE)
}


# reproject to match SWE data resolution

# template tifs
creek.tif <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_SanJoaquin_2021_0331_swe_50m_clipped.tif'))
castle.kaweah.tif <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_Kaweah_2024_0211_swe_50m_clipped.tif'))
castle.kern.tif <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_Kern_2024_0508_swe_50m_clipped.tif'))
res(creek.tif)
res(castle.kaweah.tif)
topo.files <- list.files(out.dir.tif, pattern = '_5000\\.tif$', full.names = TRUE)

# Loop through each topo raster and process for creek and castle
for (topo.path in topo.files) {
  
  base.name <- tools::file_path_sans_ext(basename(topo.path))
  topo.rast <- rast(topo.path)
  
  # ----- Creek -----
  topo.creek <- crop(topo.rast, creek.tif) %>%
    resample(creek.tif, method = 'bilinear') %>%
    mask(vect(creek.extent)) %>%
    mask(creek.elev.mask, maskvalue = FALSE)
  
  writeRaster(topo.creek,
              filename = file.path(out.dir.tif, paste0(base.name, '_creek_5000_res.tif')),
              overwrite = TRUE)
  
  # ----- Castle -----
  topo.castle <- crop(topo.rast, castle.kern.tif) %>%
    resample(castle.kern.tif, method = 'bilinear') %>%
    mask(vect(castle.extent)) %>%
    mask(castle.elev.mask, maskvalue = FALSE)
  
  writeRaster(topo.castle,
              filename = file.path(out.dir.tif, paste0(base.name, '_castle_5000_res.tif')),
              overwrite = TRUE)
}



