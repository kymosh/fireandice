# Load packages
packages <- c( 'here', 'terra', 'sf', 'ggplot2', 'ggspatial', 'rnaturalearth', 'rnaturalearthdata', 'devtools', 'dplyr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# use US base map
# us <- ne_states(country = 'United States of America', returnclass = 'sf')
# us <- us[!us$postal %in% c('AK', 'HI', 'PR'), ]
# # reproject
# us <- st_transform(us, crs = 5070)
# 
# # western states
# western.states <- c('WA', 'OR', 'CA', 'NV', 'ID', 'MT', 'WY', 'UT', 'CO', 'AZ', 'NM')
# us.west <- us[us$postal %in% western.states, ]
# 
# 
# 
# # Load the GeoPackage that contains lidar data
# wesm <- st_read(here('data', 'raw', 'other', 'WESM.gpkg'))
# 
# # Filter to only rows where ql == 'QL 1'
# wesm.ql1 <- wesm[wesm$ql == 'QL 1', ]
# # reproject
# wesm.ql1 <- st_transform(wesm.ql1, crs = 5070)
# # Crop QL1 to western states
# wesm.ql1.west <- st_intersection(wesm.ql1, us.west)
# 
# # save as RDS for easier future analysis
# saveRDS(wesm.ql1.west, here('data', 'processed', 'processed', 'rds', 'wesm_ql1_west.rds'))


# # bring in all files of aso data from all basins
# tif.dir <- 'G:/Fire_Snow_Dynamics/data/raw/ASO/tif/other_basins'
# 
# tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = T)
# 
# aso.extents <- lapply(tif.files, function(f) {
#   r <- rast(f)
#   r <- project(r, 'EPSG:5070')  # Reproject raster to match target CRS
#   as.polygons(r, na.rm = TRUE)
# })
# 
# aso.extents <- do.call(rbind, aso.extents)
# aso.extents.sf <- st_as_sf(aso.extents)
# 
# # calculate overlap
# aso.ql1 <- sf::st_intersection(aso.extents.sf, wesm.ql1.west)

aso.ql1 <- aso.ql1 %>% 
  select(ql, collect_start, collect_end, workunit)


st_write(aso.ql1, here('data', 'processed', 'processed', 'shp', 'aso_ql1_overlap.shp'), delete_layer = T)
# # st_write(aso.ql1, here('data', 'processed', 'processed', 'gpkg', 'aso_ql1_overlap.gpkg'), delete_layer = T)


ggplot() +
  geom_sf(data = us.west, fill = 'grey95', color = 'grey70') +
  geom_sf(data = wesm.ql1.west, fill = 'palegreen3', color = NA) +
  geom_sf(data = aso.extents.sf, fill = '#2c7fb8', color = NA, alpha = 0.5) +
  geom_sf(data = aso.ql1, fill = 'purple', color = NA, alpha = 0.6) +
  coord_sf(expand = FALSE) +
  theme_minimal() + 
  labs(title = 'ASO Basin Coverage & QL1 LiDAR Extents',
       subtitle = 'Blue = ASO data, Green = QL1 Lidar, \nPurple = ASO & QL1 overlap')

wesm.ql1.west  <- readRDS(here('data', 'processed', 'processed', 'rds', 'wesm_ql1_west.rds'))
aso.ql1 <- st_read(here('data', 'processed', 'processed', 'gpkg', 'aso_ql1_overlap.gpkg'))