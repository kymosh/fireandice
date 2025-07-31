# Load packages
packages <- c( 'here', 'terra', 'sf', 'ggplot2', 'ggspatial', 'rnaturalearth', 'rnaturalearthdata', 'devtools')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Load the GeoPackage that contains lidar data
wesm <- st_read(here('data', 'raw', 'other', 'WESM.gpkg'))

# Filter to only rows where ql == 'QL 1'
wesm.ql1 <- wesm[wesm$ql == 'QL 1', ]
# reproject
wesm.ql1 <- st_transform(wesm.ql1, crs = 5070)



# bring in all files of aso data from all basins
tif.dir <- 'G:/Fire_Snow_Dynamics/data/raw/ASO/tif/other_basins'

tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = T)

aso.extents <- lapply(tif.files, function(f) {
  r <- rast(f)
  r <- project(r, 'EPSG:5070')  # Reproject raster to match target CRS
  as.polygons(r, na.rm = TRUE)
})

aso.extents <- do.call(rbind, aso.extents)
aso.extents.sf <- st_as_sf(aso.extents)

  

# use US base map
us <- ne_states(country = 'United States of America', returnclass = 'sf')
us <- us[!us$postal %in% c('AK', 'HI', 'PR'), ]
# reproject
us <- st_transform(us, crs = 5070)

# western states
western.states <- c('WA', 'OR', 'CA', 'NV', 'ID', 'MT', 'WY', 'UT', 'CO', 'AZ', 'NM')
us.west <- us[us$postal %in% western.states, ]

# Crop QL1 to western states
wesm.ql1.west <- st_intersection(wesm.ql1, us.west)



ggplot() +
  geom_sf(data = us.west, fill = 'grey95', color = 'grey70') +
  geom_sf(data = wesm.ql1.west, fill = 'palegreen3', color = NA) +
  geom_sf(data = aso.extents.sf, fill = '#2c7fb8', color = NA, alpha = 0.5) +
  theme_minimal() + 
  labs(title = 'ASO Basin Coverage & QL1 LiDAR Extents',
       subtitle = 'Blue = ASO footprints, Green = QL1 LiDAR polygons')

  
