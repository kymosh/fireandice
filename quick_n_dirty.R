# Load packages
packages <- c( 'here', 'dplyr', 'stringr', 'terra', 'tibble', 'ggplot2', 'sf')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

#-------- 2/11/2026 -------- 
# trouble shooting weird strip in fractal_dim and gap code

library(terra)

chm.vrt <- rast('data/processed/processed/tif/1m/creek_chm_32611/creek_chm_1m_32611.vrt')

# extent you gave (UTM meters)
strip.ext <- ext(280000, 310000, 4160000, 4180000)

chm.strip <- crop(chm.vrt, strip.ext)

# quick diagnostics
na_frac <- global(is.na(chm.strip), 'mean', na.rm = TRUE)[1,1]
rng <- global(chm.strip, range, na.rm = TRUE)

na_frac
rng

# plot fast (downsample to 10 m so it renders quickly)
plot(aggregate(chm.strip, fact = 50, fun = 'mean', na.rm = TRUE))








#test change
# test again


### Check to see if KNP fire has SWE data from ASO
knp <- st_read(here('data', 'raw', 'fire_info', 'shp', 'knp_fire_perimeter.shp'))
kaweah <- rast(here('data', 'raw', 'ASO', 'tif', 'ASO_Kaweah_2024Apr03_swe_50m.tif'))

crs(knp, describe = T)$code
crs(kaweah, describe = T)$code

knp.32611 <- st_transform(knp, crs(kaweah))

plot(kaweah)
plot(st_geometry(knp.32611), add = TRUE, border = 'red')

creek.study.area <- st_read(here('data', 'processed', 'processed', 'shp', 'study_extent_creek_32611.shp'))
creek.4326 <- st_transform(creek.study.area, 4326)
st_bbox(creek.4326)

### figure out highest elevation the creek fire burned
cbi <- rast(here('data', 'processed', 'processed', 'tif', 'extra', 'creek_cbi_bc_firescaronly_30m.tif'))
dem <- rast(here('data', 'processed', 'processed', 'tif', '30m', 'nasadem_creek_30m.tif'))

res(cbi)
res(dem)

plot(cbi)
plot(dem)

study.extent.shp <- st_read(here('data', 'processed', 'processed','shp', 'study_extent_creek_32611.shp'))
dem.crop <- crop(dem, cbi)
dem.burned <- mask(dem.crop, cbi)

plot(dem.burned)

stack <- c(cbi, dem.burned)
df <- as.data.frame(stack, xy = T, na.rm = T)
head(df)

# find max elevation where cbi > 0
max(df$elevation[df$CBI_bc > 0], na.rm = T)



#### look at land cover classes
lc <- rast(here('data', 'raw', 'background_variables', 'tif', 'creek_NALCMS_2020.tif'))
plot(lc)

# CA boundary
library(tigris)
library(here)
library(sf)

# Download California state boundary
ca_boundary <- states(cb = TRUE, class = "sf")  # specify sf class
ca_boundary <- ca_boundary[ca_boundary$STUSPS == "CA", ]
plot(ca_boundary)

# Save the shapefile
st_write(ca_boundary, here('data', 'raw', 'background_variables', 'shp', 'ca_boundary.shp'))



# 2/5/26

frac <- rast("J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/canopy_metrics/fractal_dim_32611/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7733_norm_fractal_dim_50m.tif")
names(frac)
gap <- rast("J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/canopy_metrics/gap_dist_32611/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7838_norm_gap_dist_metrics_50m.tif")
names(gap)

# ------------------------ raster alignment --------------------------------------
# I want to check and make sure the 50m rasters I have (for the creek study area) are aligned correctly, specifically with the same origin
library(terra)
base.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'

dem <- file.path(base.dir, 'other_metrics', 'nasadem_creek_50m_1524.tif')
swe <- file.path(base.dir, 'snow_metrics', 'ASO_SanJoaquin_2021_0226_swe_50m_1524.tif')
cbi <- file.path(base.dir, 'other_metrics', 'creek_cbibc_50m_1524.tif')
clim <- file.path(base.dir, 'other_metrics', 'creek_terraclimate_wy2021_50m_1524.tif')
aspect <- file.path(base.dir, 'other_metrics', 'creek_topo_slope_50m_1524.tif')

frac <-  file.path(base.dir, 'canopy_metrics', 'fractal_dim_32611', 'creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7840_norm_fractal_dim_50m.tif')
gap <- file.path(base.dir, 'canopy_metrics', 'gap_dist_32611', 'creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7840_norm_gap_dist_metrics_50m.tif')
height <- file.path(base.dir, 'canopy_metrics', 'height_metrics_32611', 'height_USGS_LPC_CA_SierraNevada_B22_11SKB7840_norm.tif')
                 
full.files <- c(dem, swe, cbi, clim, aspect)
tile.files <- c(frac, gap, height)

full <- lapply(full.files, rast)
tile <- lapply(tile.files, rast)
ref <- full[[1]]

sapply(tile, function(r) compareGeom(ref, r, stopOnError = F))

origin(ref) # dem
origin(full[[2]]) # swe <---- this is a different origin!  
origin(full[[3]])
origin(full[[4]])  
origin(full[[5]])  

origin(tile[[1]])  
origin(tile[[2]])  
origin(tile[[3]])  

crs(ref) == crs(tile[[1]])
crs(ref) == crs(tile[[2]])

