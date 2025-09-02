# Load packages
packages <- c( 'here', 'terra', 'sf', 'ggplot2', 'ggspatial', 'rnaturalearth', 'rnaturalearthdata', 'devtools', 'dplyr', 'lubridate')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# use US base map
us <- ne_states(country = 'United States of America', returnclass = 'sf')
us <- us[!us$postal %in% c('AK', 'HI', 'PR'), ]
# reproject
us <- st_transform(us, crs = 5070)

# western states
western.states <- c('OR', 'CA', 'NV', 'UT', 'AZ', 'CO')
us.west <- us[us$postal %in% western.states, ]
# get rid of unnecessary columns
us.west <- us.west %>%
  select(name)
us.west <- st_transform(us.west, crs = 4326)
crs(us.west, describe = T)$code
crs(aso.ql1, describe = T)$code


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
# select(ql, collect_start, collect_end, workunit) %>%
#   st_simplify(dTolerance = 50) %>%
#   st_make_valid() %>%
#   .[!st_is_empty(.), ] %>%                             # remove empty geometries
#   st_cast("MULTIPOLYGON") 


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

fires.after2012 <- fires %>%
  mutate(
    ig_date = as.Date(as.POSIXct(Ig_Date/1000, origin = '1970-01-01', tz = 'UTC')), # change date to normal format
    ig_year = as.integer(format(ig_date, '%Y'))
  ) %>%
  filter(ig_year > 2012) %>% # only years 2012 and on
  select(Incid_Name, Incid_Type, ig_date) %>% # cut unnecessary columns
  # Expand collections to create a line per polygon
  st_cast("GEOMETRYCOLLECTION", warn = FALSE) %>%
  # Extract only polygons from those
  st_collection_extract("POLYGON") %>%
  # Drop any empties
  filter(!st_is_empty(geometry)) %>%
  # Standardize to multipolygon
  st_cast("MULTIPOLYGON") %>%
  # Repair any invalid geometry
  st_make_valid()


st_write(fires.after2012,
         here("data", "processed", "processed", "shp", "fires_after2012_overlap.shp"),
         delete_layer = TRUE)

# make sure CRS 
crs(fires.after2012, describe = T)$code == crs(aso.ql1, describe = T)$code

ggplot() +
  geom_sf(data = us.west, fill = 'grey95', color = 'grey70') +
  #geom_sf(data = wesm.ql1.west, fill = 'palegreen3', color = NA) +
  #geom_sf(data = aso.extents.sf, fill = '#2c7fb8', color = NA, alpha = 0.5) +
  geom_sf(data = aso.ql1, fill = 'purple', color = NA, alpha = 0.6) +
  geom_sf(data = fires.after2012, fill = 'red', color = NA, alpha = 0.6) +
  coord_sf(expand = FALSE) +
  theme_minimal() + 
  labs(title = 'ASO & Lidar with Fires Overlap',
       subtitle = '\nPurple = ASO & QL1 overlap, Red = Fires')

# attach fire info to each lidar polygon, keeping only polyons that actually intersects with fire.
aso.ql1.fire <- st_join(aso.ql1, fires.after2012, join = st_intersects, left = F)

# filter to keep only where fire date is before lidar collection start
aso.fire.within.5.start <- aso.ql1.fire %>% 
  filter(ig_date < cllct_s, # fire happened before lidar
         ig_date >= (cllct_s %m-% years(5)))                # within 5 years

# filter to keep only where fire date is before lidar collection end
aso.fire.within.5.end <- aso.ql1.fire %>% 
  filter(ig_date < cllct_n, # fire happened before lidar
         ig_date >= (cllct_n %m-% years(5)))     


# create map
ggplot() +
  geom_sf(data = us.west, fill = 'grey95', color = 'grey70') + # basemap
  # lidar collection polygons (purple)
  geom_sf(data = aso.ql1, fill = 'purple', color = NA, alpha = 0.6) +
  # fire scars that happened before lidar collection (red)
  geom_sf(data = aso.fire.within.5.end, fill = 'red', color = NA, alpha = 0.5) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = 'ASO & QL1 with Fires',
    subtitle = '\nPurple = QL1 coverage, Red = Fires that occured within 5 years before lidar collection'
  )

# zoom in by setting bounding box manually
ggplot() +
  geom_sf(data = us.west, fill = 'grey95', color = 'grey70') +
  geom_sf(data = aso.ql1, fill = 'purple', color = NA, alpha = 0.7) +
  geom_sf(data = aso.fire.within.5.end, fill = 'red', color = NA, alpha = 0.3) +
  coord_sf(
    xlim = c(-125, -110),  # longitude range
    ylim = c(32, 44),      # latitude range
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = 'ASO & QL1 with Fires',
    subtitle = '\nPurple = QL1 coverage, Red = Fires within 5 years before lidar collection'
  )



# test
# test test