######################
# ASO tifs


# Helper function to clip and save rasters
clip.and.save <- function(r.file.path, extent.sf, out.dir.tif) {
  
  # Load SWE raster
  tif <- rast(r.file.path)
  
  # Reproject extent to match SWE raster
  extent.sf <- st_transform(extent.sf, crs(tif))
  
  # Clip to bounding box and mask to shape
  tif.crop <- crop(tif, vect(extent.sf))
  tif.mask <- mask(tif.crop, vect(extent.sf))
  
  # Create output name
  base.name <- tools::file_path_sans_ext(basename(r.file.path))
  out.name <- paste0(base.name, '_clipped.tif')
  out.path <- file.path(out.dir.tif, out.name)
  
  # Save
  writeRaster(tif.mask, out.path, overwrite = TRUE)
  
  # return raster to environment
  return(tif.mask)
}



# ---------- setup ----------

# --- tif files ---

# Directory with input ASO tifs
tif.dir <- 'data/raw/ASO/tif'

# Output directory
out.dir.tif <- 'data/processed/processed/tif/50m'

# List files with matching keywords
tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = TRUE)

# Filter files by basin
creek.tifs   <- tif.files[str_detect(tif.files, 'SanJoaquin')]
castle.tifs   <- tif.files[str_detect(tif.files, 'Kern|Kaweah')]
caldor.tifs <- tif.files[str_detect(tif.files, 'American|Truckee')]
dixie.tifs <- tif.files[str_detect(tif.files, 'Feather')]

# double-check correct CRS
fires = list(creek.tifs[1], castle.tifs[1], caldor.tifs[1], dixie.tifs[1])
for (each in fires) {
  r <- rast(each)
  print(crs(r, describe = T)$code)
}

# --- study extent shp ---
creek.extent <- st_read('data/processed/processed/shp/studyarea_extents/study_extent_creek_simple.shp')
castle.extent <- st_read('data/processed/processed/shp/studyarea_extents/study_extent_castle_simple.shp')
caldor.extent <- st_read('data/processed/processed/shp/studyarea_extents/study_extent_caldor_simple.shp')
dixie.extent <- st_read('data/processed/processed/shp/studyarea_extents/study_extent_dixie_simple.shp')

# double-check correct CRS
fires = list(creek.extent, castle.extent, caldor.extent, dixie.extent)
for (each in fires) {
  print(crs(each, describe = T)$code)
}


# ----- run function -----

# creek
creek.swe.crop <- lapply(creek.tifs, function(f) {
  clip.and.save(
    r.file.path = f, 
    extent.sf = creek.extent,
    out.dir.tif = out.dir.tif
  )
})

names(creek.swe.crop) <- tools::file_path_sans_ext(
  basename(creek.tifs)
)

plot(creek.swe.crop[[1]])

# castle
castle.swe.crop <- lapply(castle.tifs, function(f) {
  clip.and.save(
    r.file.path = f, 
    extent.sf = castle.extent,
    out.dir.tif = out.dir.tif
  )
})

names(castle.swe.crop) <- tools::file_path_sans_ext(
  basename(castle.tifs)
)

plot(castle.swe.crop[[1]])

# caldor
caldor.swe.crop <- lapply(caldor.tifs, function(f) {
  clip.and.save(
    r.file.path = f, 
    extent.sf = caldor.extent,
    out.dir.tif = out.dir.tif
  )
})

names(caldor.swe.crop) <- tools::file_path_sans_ext(
  basename(caldor.tifs)
)

plot(caldor.swe.crop[[1]])

# dixie
dixie.swe.crop <- lapply(dixie.tifs, function(f) {
  clip.and.save(
    r.file.path = f, 
    extent.sf = dixie.extent,
    out.dir.tif = out.dir.tif
  )
})

names(dixie.swe.crop) <- tools::file_path_sans_ext(
  basename(dixie.tifs)
)

plot(dixie.swe.crop[[1]])

# ----- standardize names -----

# renames ASO files so that dates are standardized and any "Mosaic" text is removed

files <- list.files(out.dir.tif, pattern = '^ASO.*\\.tif$', full.names = TRUE)

standardize.aso.name <- function(x) {
  
  nm <- basename(x)
  
  # remove Mosaic/mosaic wherever it appears
  nm <- str_replace_all(nm, regex('_?mosaic_?', ignore_case = TRUE), '_')
  nm <- str_replace_all(nm, '__+', '_')
  
  # extract date string like 2023Apr13-14 or 2021Mar31-Apr1
  date.raw <- str_extract(nm, '\\d{4}[A-Za-z]{3}\\d{1,2}(-[A-Za-z]{3}?\\d{1,2}|-\\d{1,2})?')
  
  # get first date only
  year <- str_extract(date.raw, '^\\d{4}')
  mon <- str_extract(date.raw, '(?<=\\d{4})[A-Za-z]{3}')
  day <- str_extract(date.raw, '(?<=\\d{4}[A-Za-z]{3})\\d{1,2}')
  
  date.num <- format(
    as.Date(paste0(year, mon, day), format = '%Y%b%d'),
    '%Y%m%d'
  )
  
  # replace full original date/range with numeric first date
  nm <- str_replace(nm, fixed(date.raw), date.num)
  
  # clean any repeated underscores
  nm <- str_replace_all(nm, '__+', '_')
  
  nm
}

rename.df <- data.frame(
  old.path = files,
  old.name = basename(files),
  new.name = sapply(files, standardize.aso.name),
  stringsAsFactors = FALSE
)

rename.df$new.path <- file.path(out.dir.tif, rename.df$new.name)

rename.df[, c('old.name', 'new.name')]

# once above table looks good, go ahead and rename
file.rename(rename.df$old.path, rename.df$new.path)








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



# troubleshooting
kern <- rast(file.path(out.dir.tif, 'ASO_Kern_20240411_swe_50m_clipped.tif'))
kaw <- rast(file.path(out.dir.tif, 'ASO_Kaweah_20240211_swe_50m_clipped.tif'))
plot(kern)
plot(kaw)
plot(kern, add = T)
