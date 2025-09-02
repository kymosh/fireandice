# Load packages
packages <- c( 'here', 'terra', 'sf', 'ggplot2', 'ggspatial', 'rnaturalearth', 'rnaturalearthdata', 'devtools', 'dplyr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# use US base map
us <- ne_states(country = 'United States of America', returnclass = 'sf')
us <- us[!us$postal %in% c('AK', 'HI', 'PR'), ]
# reproject
us <- st_transform(us, crs = 5070)

# western states
western.states <- c('WA', 'OR', 'CA', 'NV', 'ID', 'MT', 'WY', 'UT', 'CO', 'AZ', 'NM')
us.west <- us[us$postal %in% western.states, ]
# get rid of unnecessary columns
us.west <- us.west %>%
  select(name)




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


#aso.ql1 <- st_read(here('data', 'processed', 'processed', 'gpkg', 'aso_ql1_overlap.gpkg'))
#aso.ql1.simple <- aso.ql1 %>% 
  select(ql, collect_start, collect_end, workunit) %>%
  st_simplify(dTolerance = 50) %>%
  st_make_valid() %>%
  .[!st_is_empty(.), ] %>%                             # remove empty geometries
  st_cast("MULTIPOLYGON") 


# st_write(aso.ql1.simple, here('data', 'processed', 'processed', 'shp', 'aso_ql1'), delete_layer = T)
aso.ql1 <- st_read(here('data', 'processed', 'processed', 'shp', 'aso_ql1.shp'))

ggplot() +
  geom_sf(data = us.west, fill = 'grey95', color = 'grey70') +
  #geom_sf(data = wesm.ql1.west, fill = 'palegreen3', color = NA) +
  #geom_sf(data = aso.extents.sf, fill = '#2c7fb8', color = NA, alpha = 0.5) +
  geom_sf(data = aso.ql1.simple, fill = 'purple', color = NA, alpha = 0.6) +
  coord_sf(expand = FALSE) +
  theme_minimal() + 
  labs(title = 'ASO Basin Coverage & QL1 LiDAR Extents',
       subtitle = 'Blue = ASO data, Green = QL1 Lidar, \nPurple = ASO & QL1 overlap')


###############################
# Used GEE to get MTBS fire data for the aso.ql1.simple shapefile

fires <- st_read(here('data', 'raw', 'fire_info', 'geojson', 'MTBS_within_ASO_and_QL1.geojson'))

fires.after2017 <- fires %>%
  mutate(
    ig_date = as.Date(as.POSIXct(Ig_Date/1000, origin = '1970-01-01', tz = 'UTC')), # change date to normal format
    ig_year = as.integer(format(ig_date, '%Y'))
  ) %>%
  filter(ig_year > 2017) %>% # only years 2017 and on
  select(Incid_Name, Incid_Type, ig_date) %>% # cut unnecessary columns

  library(sf)
library(dplyr)

fires.after2017.clean <- fires.after2017 %>%
  # Expand collections into their pieces
  st_cast("GEOMETRYCOLLECTION", warn = FALSE) %>%
  # Extract only polygons from those
  st_collection_extract("POLYGON") %>%
  # Drop any empties
  filter(!st_is_empty(geometry)) %>%
  # Standardize to multipolygon
  st_cast("MULTIPOLYGON") %>%
  # Repair any invalid geometry
  st_make_valid()


st_write(fires.after2017.clean,
         here("data", "processed", "processed", "shp", "fires_after2017_overlap.shp"),
         delete_layer = TRUE)


st_write(fires.after2017, here('data', 'processed', 'processed', 'shp', 'fires_after2017_overlap.shp'))

crs(fires.after2017, describe = T)$code
crs(aso.ql1, describe = T)$code

ggplot() +
  geom_sf(data = us.west, fill = 'grey95', color = 'grey70') +
  #geom_sf(data = wesm.ql1.west, fill = 'palegreen3', color = NA) +
  #geom_sf(data = aso.extents.sf, fill = '#2c7fb8', color = NA, alpha = 0.5) +
  geom_sf(data = aso.ql1, fill = 'purple', color = NA, alpha = 0.6) +
  geom_sf(data = fires.after2017, fill = 'red', color = NA, alpha = 0.6) +
  coord_sf(expand = FALSE) +
  theme_minimal() + 
  labs(title = 'ASO & Lidar with Fires Overlap',
       subtitle = '\nPurple = ASO & QL1 overlap, Red = Fires')

