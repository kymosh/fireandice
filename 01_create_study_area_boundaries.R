packages <- c('tidyverse', 'sf')
lapply(packages, library, character.only = TRUE)

# --------------------------------------------------------------------------------------
# Find intersecting HUCs for each fire
# --------------------------------------------------------------------------------------

# Read in shapefiles
castle.0 <- st_read('data/raw/fire_info/shp/castle_fire_perimeter.shp')
dixie.0  <- st_read('data/raw/fire_info/shp/dixie_fire_perimeter.shp')
caldor.0 <- st_read('data/raw/fire_info/shp/caldor_fire_perimeter.shp')
creek.0  <- st_read('data/raw/fire_info/shp/creek_fire_perimeter.shp')

huc.0 <- st_read('data/raw/background_variables/shp/HUC12_ca.shp')

fires <- list(
  castle = castle.0,
  dixie  = dixie.0,
  caldor = caldor.0,
  creek  = creek.0
)

# EPSG code for each fire
fire.epsg <- c(
  castle = 32611,
  dixie  = 32610,
  caldor = 32610,
  creek  = 32611
)

# Intersect each fire with HUCs in the correct CRS
fires.by.hucs <- lapply(names(fires), function(fire) {
  
  epsg <- fire.epsg[[fire]]
  
  fire.proj <- st_transform(fires[[fire]], epsg)
  huc.proj  <- st_transform(huc.0, epsg)
  
  huc.proj %>%
    st_filter(fire.proj, .predicate = st_intersects) %>%
    summarize()
})

names(fires.by.hucs) <- names(fires)

# write
out.dirs <- c(
  'data/processed/processed/shp/studyarea_extents',
  'J:/Fire_Snow/fireandice/data/processed/processed/shp/studyarea_extents',
  'G:/Fire_Snow_Dynamics_backup/data/processed/processed/shp/studyarea_extents'
)

lapply(out.dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

lapply(out.dirs, function(out.dir) {
  lapply(names(fires.by.hucs), function(fire) {
    
    epsg <- fire.epsg[[fire]]
    
    st_write(
      fires.by.hucs[[fire]],
      file.path(out.dir, paste0('study_extent_', fire, '_', epsg, '_huc.shp')),
      delete_layer = TRUE
    )
  })
})

# this code clips study areas to the overlapping extents of the SWE data, and then apply an elevation filter to limit to areas above 5000ft (1524m) where snow is more likely to be present. This will be the final extent for the analysis, and will be used to create a mask for clipping all rasters to the same extent and resolution.

# Load packages
library(terra)
rm(list = ls())

# --- HUC study extent ---
# read in shape files of study areas using the intersecting HUCs
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
# Process Study Area 
# ----------------------------------------------------------------------------------

# create function
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

# setup
fires <- names(hucs.areas)

out.dir.shp <- 'data/processed/processed/shp/studyarea_extents'
out.dir.tif <- 'data/processed/processed/tif/30m'

# run 
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

# check results 
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
