
# this code clips study areas to the overlapping extents of the SWE data, and then apply an elevation filter to limit to areas above 5000ft (1524m) where snow is more likely to be present. This will be the final extent for the analysis, and will be used to create a mask for clipping all rasters to the same extent and resolution.

# Load packages
packages <- c('tidyverse', 'sf', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# --- HUC study extent ---
# read in shape files of study areas
shp.dir <- 'data/processed/processed/shp/studyarea_extents'
shp.files <- list.files(shp.dir, pattern = '^study_extent_.*huc\\.shp$', full.names = TRUE)
hucs.areas <- lapply(shp.files, vect)

# apply names to keep straight which is which
names(hucs.areas) <- gsub(
  'study_extent_|_3261.*_huc\\.shp',
  '',
  basename(shp.files)
)

# check
names(hucs.areas)
# check crs
crs(hucs.areas[[1]], describe = TRUE)$code

# --- NASADEMS ---
# read in NASADEMs (downloaded from GEE)
dem.files <- list.files('data/raw/background_variables/tif', pattern = '^nasadem.*\\.tif$', full.names = TRUE)

# Read DEMs
dems <- lapply(dem.files, rast)
# rename
names(dems) <- gsub(
  'nasadem_|\\.tif',
  '',
  basename(dem.files)
)
# check
names(dems)
crs(dems[[1]], describe = TRUE)$code

# --- SWE data extents ---
# combine CASTLE and CALDOR extents to single mosaic because they have two separate watershed extents
# castle 
castle.kern <- rast('data/raw/ASO/tif/ASO_Kern_2023Apr29-30_swe_50m.tif')
castle.kaweah <- rast('data/raw/ASO/tif/ASO_Kaweah_2025Feb21_swe_50m.tif')
castle.swe <- mosaic(castle.kern, castle.kaweah)

# caldor
caldor.amer <- rast('data/raw/ASO/tif/ASO_American_2023Jun02_swe_50m.tif')
caldor.truck <- rast('data/raw/ASO/tif/ASO_Truckee_2024May17-18_swe_50m.tif')
caldor.swe <- mosaic(caldor.amer, caldor.truck)

dixie.swe <- rast('data/raw/ASO/tif/ASO_Feather_2024May13-15_swe_50m.tif')
creek.swe <- rast('data/raw/ASO/tif/ASO_SanJoaquin_2024Feb22-25_swe_50m.tif')

swe.list <- list(
  caldor = caldor.swe,
  castle = castle.swe,
  creek = creek.swe,
  dixie = dixie.swe
)

# ----------------------------------------------------------------------------------
# Process Study Area function 
# ----------------------------------------------------------------------------------

calc.study.area <- function(fire, swe.area, hucs.areas, dem, out.dir.shp, out.dir.tif, elev.cutoff = 1524) {
  
  message('Processing ', fire, '...')
  
  # --- create polygon from swe data footprint ---
  r.data <- !is.na(swe.area)
  
  r.poly <- r.data %>%
    as.polygons(aggregate = TRUE, dissolve = TRUE) %>%
    subset(.[[1]] == 1) %>%
    fillHoles() %>%
    terra::aggregate()
  
  # and if statement to make sure CRS code matches
  
  # force CRS to match study area (they already should match)
  crs(r.poly) <- crs(hucs.areas[[fire]])
  
  # create polygon only where hucs.area and swe.area overlap
  overlap <- terra::intersect(hucs.areas[[fire]], r.poly)
  overlap.parts <- disagg(overlap)
  overlap.parts$area <- expanse(overlap.parts, unit = 'm')
  overlap.simple <- overlap.parts[which.max(overlap.parts$area), ]
  
  # save
  # this is the shp file for the study area extent, before being clipped to elevation
  writeVector(overlap.simple,
              file.path(out.dir.shp, paste0('study_extent_', fire, '_simple.shp')), 
              overwrite = TRUE)
  
  # --- crop DEM to new study area ---
  # crop to polygon that we just made
  dem.crop <- dem %>%
    crop(overlap.simple) %>%
    mask(overlap.simple)

  writeRaster(dem.crop,  file.path(out.dir.tif, paste0('dem_', fire, '.tif')), 
              overwrite = TRUE)
  
  # --- crop to only high elevations (>5000ft/1524m)
  # create mask to limit elevations to >5000ft (1524m)
  dem.high <- terra::mask(
    dem.crop,
    dem.crop > elev.cutoff,
    maskvalues = FALSE
  )
  
  writeRaster(dem.high,  file.path(out.dir.tif, paste0('dem_', fire, '_', elev.cutoff, '.tif')), 
              overwrite = TRUE)
  
  r.high <- !is.na(dem.high)
  
  high.poly <- r.high %>%
    terra::as.polygons(aggregate = TRUE, dissolve = TRUE) %>%
    subset(.[[1]] == 1) %>%
    terra::fillHoles() %>%
    terra::aggregate()
  
  writeVector(high.poly, file.path(out.dir.shp, paste0('study_extent_', fire, '_', elev.cutoff, '.shp')), 
              overwrite = TRUE)
  
  return(list(
    overlap = overlap.simple,
    dem.crop = dem.crop,
    dem.high = dem.high,
    high.poly = high.poly
  ))
}

fires <- names(hucs.areas)

out.dir.shp <- 'data/processed/processed/shp/studyarea_extents'
out.dir.tif <- 'data/processed/processed/tif/30m'


results <- lapply(fires, function(fire) {
  
  calc.study.area(
    fire = fire,
    swe.area = swe.list[[fire]],
    hucs.areas = hucs.areas,
    dem = dems[[fire]],
    out.dir.shp = out.dir.shp,
    out.dir.tif = out.dir.tif,
    elev.cutoff = 1524
  )
})

# ----- check results -----
par(mfrow = c(2, 2))

# simple overlap polygons
lapply(results, function(x) {
  plot(x$overlap)
})

# high-elevation polygons
lapply(results, function(x) {
  plot(x$high.poly)
})

# cropped DEM
lapply(results, function(x) {
  plot(x$dem.crop)
})

# high-elev DEM
lapply(results, function(x) {
  plot(x$dem.high)
})

# check CRS
crs(castle.swe, describe = T)$code

# --- create shp file of only overlapping extents ---

# visualize
plot(castle.swe)
plot(study.areas[[2]], add = TRUE, border = 'red', col = NA)

r.data <- !is.na(castle.swe)

r.poly <- r.data %>%
  terra::as.polygons(aggregate = TRUE, dissolve = TRUE) %>%
  subset(.[[1]] == 1) %>%
  terra::fillHoles() %>%
  terra::aggregate()

plot(castle.swe)
plot(r.poly, add = T, border = 'red', col = NA)
plot(study.areas[[2]], add = TRUE, border = 'white', col = NA)

# they are functionally the same already
terra::crs(r.poly) <- terra::crs(study.areas$castle)

overlap <- terra::intersect(study.areas$castle, r.poly)
plot(overlap)
overlap.parts <- disagg(overlap)
overlap.parts$area <- terra::expanse(overlap.parts, unit = 'm')
overlap.simple <- overlap.parts[which.max(overlap.parts$area), ]

# visualize
plot(castle.swe)
plot(r.poly, add = T, border = 'red', col = NA)
plot(study.areas[[2]], add = TRUE, border = 'white', col = NA)
plot(overlap.simple, , add = TRUE, col = NA, border = 'red', lwd = 2)

# save
writeVector(overlap.simple, 'data/processed/processed/shp/study_extent_castle_32611_simple.shp', overwrite = TRUE)

# --- Elevation filtering ---
# crop to polygon that we just made
dem.crop <- dems$castle %>%
  terra::crop(overlap.simple) %>%
  terra::mask(overlap.simple)

plot(dem.crop)
writeRaster(dem.crop, 'data/processed/processed/tif/30m/dem_castle.tif', overwrite = TRUE)

# create mask to limit elevations to >5000ft (1524m)
dem.high <- terra::mask(
  dem..crop,
  dem.crop > 1524,
  maskvalues = FALSE
)

plot(dem.high)
writeRaster(dem.high, 'data/processed/processed/tif/30m/dem_castle_1524.tif', overwrite = TRUE)

r.data <- !is.na(dem.high)

r.poly <- r.data %>%
  terra::as.polygons(aggregate = TRUE, dissolve = TRUE) %>%
  subset(.[[1]] == 1) %>%
  terra::fillHoles() %>%
  terra::aggregate()

writeVector(r.poly, 'data/processed/processed/shp/study_extent_castle_32611_1524.shp', overwrite = TRUE)

# ----------------------------------------------------------------------------------
# CALDOR Fire
# ----------------------------------------------------------------------------------


# check CRS
crs(caldor.swe, describe = T)$code

# ----- create shp file of only overlapping extents 
# visualize
plot(caldor.swe)
plot(study.areas$caldor, add = TRUE, border = 'red', col = NA)

r.data <- !is.na(caldor.swe)

r.poly <- r.data %>%
  terra::as.polygons(aggregate = TRUE, dissolve = TRUE) %>%
  subset(.[[1]] == 1) %>%
  terra::fillHoles() %>%
  terra::aggregate()

plot(caldor.swe)
plot(r.poly, add = T, border = 'red', col = NA)
plot(study.areas$caldor, add = TRUE, border = 'white', col = NA)

crs(r.poly, describe = T)$code 
crs(study.areas$caldor, describe = T)$code 

# they are functionally the same already
terra::crs(r.poly) <- terra::crs(study.areas$caldor)

overlap <- terra::intersect(study.areas$caldor, r.poly)
plot(overlap)
overlap.parts <- disagg(overlap)
overlap.parts$area <- terra::expanse(overlap.parts, unit = 'm')
overlap.simple <- overlap.parts[which.max(overlap.parts$area), ]

# visualize
plot(caldor.swe)
plot(r.poly, add = T, border = 'red', col = NA)
plot(study.areas$caldor, add = TRUE, border = 'white', col = NA)
plot(overlap.simple, , add = TRUE, col = NA, border = 'red', lwd = 2)

# save
writeVector(overlap.simple, 'data/processed/processed/shp/study_extent_caldor_32610_simple.shp', overwrite = TRUE)

# ----- Elevation filtering 

# crop to polygon that we just made
dem.crop <- dems$caldor %>%
  terra::crop(overlap.simple) %>%
  terra::mask(overlap.simple)

plot(dem.crop)
writeRaster(dem.crop, 'data/processed/processed/tif/30m/dem_caldor.tif', overwrite = TRUE)

# create mask to limit elevations to >5000ft (1524m)
dem.high <- terra::mask(
  dem.crop,
  dem.crop > 1524,
  maskvalues = FALSE
)

plot(dem.high)
writeRaster(dem.high, 'data/processed/processed/tif/30m/dem_caldor_1524.tif', overwrite = TRUE)

r.data <- !is.na(dem.high)

r.poly <- r.data %>%
  terra::as.polygons(aggregate = TRUE, dissolve = TRUE) %>%
  subset(.[[1]] == 1) %>%
  terra::fillHoles() %>%
  terra::aggregate()

plot(r.poly)

writeVector(r.poly, 'data/processed/processed/shp/study_extent_caldor_32610_1524.shp', overwrite = TRUE)







# ----------------------------------------------------------------------------------
# DIXIE Fire
# ----------------------------------------------------------------------------------

# combine
dixie.swe <- rast('data/raw/ASO/tif/ASO_Feather_2025Mar24-25_swe_50m.tif')

# check CRS
crs(caldor.swe, describe = T)$code

# ----- create shp file of only overlapping extents 
# visualize
plot(caldor.swe)
plot(study.areas$caldor, add = TRUE, border = 'red', col = NA)

r.data <- !is.na(caldor.swe)

r.poly <- r.data %>%
  terra::as.polygons(aggregate = TRUE, dissolve = TRUE) %>%
  subset(.[[1]] == 1) %>%
  terra::fillHoles() %>%
  terra::aggregate()

plot(caldor.swe)
plot(r.poly, add = T, border = 'red', col = NA)
plot(study.areas$caldor, add = TRUE, border = 'white', col = NA)

crs(r.poly, describe = T)$code 
crs(study.areas$caldor, describe = T)$code 

# they are functionally the same already
terra::crs(r.poly) <- terra::crs(study.areas$caldor)

overlap <- terra::intersect(study.areas$caldor, r.poly)
plot(overlap)
overlap.parts <- disagg(overlap)
overlap.parts$area <- terra::expanse(overlap.parts, unit = 'm')
overlap.simple <- overlap.parts[which.max(overlap.parts$area), ]

# visualize
plot(caldor.swe)
plot(r.poly, add = T, border = 'red', col = NA)
plot(study.areas$caldor, add = TRUE, border = 'white', col = NA)
plot(overlap.simple, , add = TRUE, col = NA, border = 'red', lwd = 2)

# save
writeVector(overlap.simple, 'data/processed/processed/shp/study_extent_caldor_32610_simple.shp', overwrite = TRUE)

# ----- Elevation filtering 

# crop to polygon that we just made
dem.crop <- dems$caldor %>%
  terra::crop(overlap.simple) %>%
  terra::mask(overlap.simple)

plot(dem.crop)
writeRaster(dem.crop, 'data/processed/processed/tif/30m/dem_caldor.tif', overwrite = TRUE)

# create mask to limit elevations to >5000ft (1524m)
dem.high <- terra::mask(
  dem.crop,
  dem.crop > 1524,
  maskvalues = FALSE
)

plot(dem.high)
writeRaster(dem.high, 'data/processed/processed/tif/30m/dem_caldor_1524.tif', overwrite = TRUE)

r.data <- !is.na(dem.high)

r.poly <- r.data %>%
  terra::as.polygons(aggregate = TRUE, dissolve = TRUE) %>%
  subset(.[[1]] == 1) %>%
  terra::fillHoles() %>%
  terra::aggregate()

plot(r.poly)

writeVector(r.poly, 'data/processed/processed/shp/study_extent_caldor_32610_1524.shp', overwrite = TRUE)




######################
# ASO tifs

# Directory with input ASO tifs
tif.dir <- 'data/raw/ASO/tif'

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



