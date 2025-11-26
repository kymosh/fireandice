# Load packages
packages <- c( 'here', 'terra', 'sf', 'ggplot2', 'ggspatial', 'rnaturalearth', 'rnaturalearthdata', 'devtools', 'dplyr', 'lubridate', 'purrr', 'tidyr', 'ggmap', 'tigris')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# use US base map
us <- ne_states(country = 'United States of America', returnclass = 'sf')
# us <- us[!us$postal %in% c('AK', 'HI', 'PR'), ]
# reproject
us <- st_transform(us, crs = 5070)

# western states
western.states <- c('OR', 'CA', 'NV', 'UT', 'AZ', 'CO', 'MT', 'NM', 'WA', 'ID', 'WY')
# get rid of unnecessary columns
us.west <- us %>%
  filter(postal %in% western.states) %>% # keep only western states
  select(name) %>% # keep only state name column
  st_transform(crs = 4326) #reproject




######### QL1 Lidar

# Load the GeoPackage that contains lidar data (where lidar is available)
#wesm <- st_read(here('data', 'raw', 'ALS', 'gpkg', 'WESM.gpkg'))
 
# pair down to only ql1 data in the west
# ***this step took a LONG time to process (~24 hours). Consider a different approach if for some reason you need to reprocess it. Outcome is saved as an RDS to avoid reprocessing.
# ql1.west <- wesm %>%
#   filter(ql == 'QL 1') %>% # filter to only rows where ql == 'QL 1'
#   st_transform(crs = 4326) %>% # reproject
#   select(ql, workunit,  collect_start, collect_end, geometry) %>% # keep only necessary columns
#   st_intersection(us.west) # crop to only western states
  
# save as RDS for easier future analysis
#saveRDS(ql1.west, here('data', 'processed', 'processed', 'rds', 'ql1_west.rds'))
#st_write(ql1.west, here('data', 'processed', 'processed', 'shp', 'ql1_west.shp'))

ql1.west <- readRDS(here('data', 'processed', 'processed', 'rds', 'ql1_west.rds'))





######### ASO 

tif.dir <- 'G:/Fire_Snow_Dynamics/data/raw/ASO/tif/other_basins'
tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = TRUE)

get_extent_sf <- function(file_path) {
  message("Processing: ", basename(file_path))
  
  r <- rast(file_path)
  r <- project(r, 'EPSG:4326')
  
  # skip if raster is empty
  if (all(is.na(values(r)))) {
    warning("Raster ", basename(file_path), " is all NA. Skipping.")
    return(NULL)
  }
  
  # convert to polygon
  ext_poly <- as.polygons(r, dissolve = TRUE)
  
  # only keep geometry, drop all raster value columns
  sf_poly <- st_as_sf(ext_poly)[, 0]
  
  # add file name column
  sf_poly$file <- basename(file_path)
}

aso.extents <- map(tif.files, get_extent_sf) %>%
  compact() %>%        # remove NULLs
  do.call(rbind, .)

# save as RDS for easier future analysis
saveRDS(aso.extents, here('data', 'processed', 'processed', 'rds', 'JFSP_aso_extents.rds'))
st_write(aso.extents, here('data', 'processed', 'processed', 'shp', 'aso_extents.shp'), delete_layer = T)


## check to make sure crs all match
crs(aso.extents, describe = T)$code
crs(ql1.west, describe = T)$code
crs(us.west, describe = T)$code




#aso.ql1 <- st_read(here('data', 'processed', 'processed', 'gpkg', 'aso_ql1_overlap.gpkg'))
#aso.ql1.simple <- aso.ql1 %>% 
# select(ql, collect_start, collect_end, workunit) %>%
#   st_simplify(dTolerance = 50) %>%
#   st_make_valid() %>%
#   .[!st_is_empty(.), ] %>%                             # remove empty geometries
#   st_cast("MULTIPOLYGON") 


# st_write(aso.ql1.simple, here('data', 'processed', 'processed', 'shp', 'aso_ql1'), delete_layer = T)
aso.ql1 <- st_read(here('data', 'processed', 'processed', 'shp', 'aso_ql1.shp'))

# crs(us.west, describe = T)$code
# crs(aso.ql1, describe = T)$code

ggplot() +
  geom_sf(data = us.west, fill = 'grey95', color = 'grey70') +
  #geom_sf(data = wesm.ql1.west, fill = 'palegreen3', color = NA) +
  #geom_sf(data = aso.extents.sf, fill = '#2c7fb8', color = NA, alpha = 0.5) +
  geom_sf(data = aso.ql1.simple, fill = 'purple', color = NA, alpha = 0.6) +
  coord_sf(expand = FALSE) +
  theme_minimal() + 
  labs(title = 'ASO Basin Coverage & QL1 LiDAR Extents',
       subtitle = 'Blue = ASO data, Green = QL1 Lidar, \nPurple = ASO & QL1 overlap')


######### Fires
# Used GEE to get MTBS fire data for the aso.ql1.simple shapefile

# fires <- st_read(here('data', 'raw', 'fire_info', 'geojson', 'MTBS_within_ASO_and_QL1.geojson'))

# fires.after2012 <- fires %>%
#   mutate(
#     ig_date = as.Date(as.POSIXct(Ig_Date/1000, origin = '1970-01-01', tz = 'UTC')), # change date to normal format
#     ig_year = as.integer(format(ig_date, '%Y'))
#   ) %>%
#   filter(ig_year > 2012) %>% # only years 2012 and on
#   select(Incid_Name, Incid_Type, ig_date, dNBR_offst, geometry) %>% # cut unnecessary columns
#   # Expand collections to create a line per polygon
#   st_cast("GEOMETRYCOLLECTION", warn = FALSE) %>%
#   # Extract only polygons from those
#   st_collection_extract("POLYGON") %>%
#   # Drop any empties
#   filter(!st_is_empty(geometry)) %>%
#   # Standardize to multipolygon
#   st_cast("MULTIPOLYGON") %>%
#   # Repair any invalid geometry
#   st_make_valid()

# st_write(fires.after2012,
#          here("data", "processed", "processed", "shp", "jfsp_fires_after2012_overlap.shp"),
#          delete_layer = TRUE)

fires.after2012 <- st_read(here("data", "processed", "processed", "shp", "jfsp_fires_after2012_overlap.shp"))





##########

# keep only QL1 polygons that intersect with ASO extents
# ql1.overlaps.aso <- st_filter(ql1.west, aso.extents)
# saveRDS(ql1.overlaps.aso, here('data', 'processed', 'processed', 'rds', 'ql1_overlaps_aso.rds'))
# st_write(ql1.overlaps.aso, here('data', 'processed', 'processed', 'shp', 'ql1_overlaps_aso.shp'))

# # and vice versa
# aso.overlaps.ql1 <- st_filter(aso.extents, ql1.west)
# saveRDS(aso.overlaps.ql1, here('data', 'processed', 'processed', 'rds', 'aso_overlaps_ql1.rds'))
# st_write(aso.overlaps.ql1,  here('data', 'processed', 'processed', 'shp', 'aso_overlaps_ql1.shp'))

# # keep only fires that happened within 5 years prior to lidar collection START
# ql1.fire <- st_join(ql1.west, fires.after2012, join = st_intersects, left = F)
# fires.within5.ql1collect <- ql1.fire %>%
#   filter(ig_date >= (collect_start %m-% years(5)), ig_date < collect_start)
# saveRDS(fires.within5.ql1collect, here('data', 'processed', 'processed', 'rds', 'fires_within5_ql1collect.rds'))
# st_write(fires.within5.ql1collect, here('data', 'processed', 'processed', 'shp', 'fires_within5_ql1collect.shp'))

# read back in (if needed)
ql1.overlaps.aso <- readRDS(here('data', 'processed', 'processed', 'rds', 'ql1_overlaps_aso.rds'))
aso.overlaps.ql1 <- readRDS(here('data', 'processed', 'processed', 'rds', 'aso_overlaps_ql1.rds'))
fires.within5.ql1collect <- readRDS(here('data', 'processed', 'processed', 'rds', 'fires_within5_ql1collect.rds'))


######### plot

# CA boundary
ca_bbox <- c(left = -125, bottom = 33, right = -115, top = 45)

register_stadiamaps("8f6524a2-2d27-429a-aeb1-287b33588230", write = FALSE)

ca_map <- get_stadiamap(
  bbox = ca_bbox,
  zoom = 7,  # Adjust zoom level as needed (higher = more detail)
  maptype = "stamen_toner_lite",
  crop = T
)

library(tigris)

ca_boundary <- states(cb = TRUE) %>% filter(NAME == "California")

ggmap(ca_map) +
  
  geom_sf(data = ca_boundary_sf, 
          fill = NA, 
          color = "black", 
          size = 1.2,  # Thick outline
          inherit.aes = FALSE) +
  
  # Layer 1: ql1.west - pretty transparent purple
  geom_sf(data = ql1.west, 
          fill = "purple", 
          color = NA , 
          alpha = 0.3,  # Pretty transparent
          inherit.aes = FALSE) +
  
  # Layer 2: aso.extents - somewhat transparent light blue
  geom_sf(data = aso.extents, 
          fill = "lightblue", 
          color = NA , 
          alpha = 0.7,  # Somewhat transparent
          inherit.aes = FALSE) +
  
  # Layer 3: fires.within5.ql1collect - very transparent red with bright red outline
  geom_sf(data = fires.after2012, 
          fill = "red", 
          color = "red",  # Bright red outline
          alpha = 0.15,  # Very transparent fill
          size = 0.8,    # Outline thickness
          inherit.aes = FALSE) +
  
  labs(
    title = "Potential California and Oregon Study Areas",
    subtitle = "Purple: QL1 Lidar | Light Blue: ASO Data | Red: Wildfires",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )



  


# get US states
us.states <- ne_states(country = "united states of america", returnclass = "sf")

ca_bbox <- c(xmin = -125, ymin = 35, xmax = -116, ymax = 44)

# keep only CA & OR & NV
ca.or <- us.states %>%
  dplyr::filter(name %in% c("California", "Oregon", "Nevada"))

ql1.west$layer <- "QL1 Lidar Availability"
aso.extents$layer <- "ASO Extents"
fires.after2012$layer <- "Fires within 5 Years prior of Lidar Collection"

# now plot with all
ggplot() +
  geom_sf(data = ca.or, fill = "grey95", color = "black") +
  geom_sf(data = ql1.west, aes(fill = layer), color = NA, alpha = 0.3) +
  geom_sf(data = aso.extents, aes(fill = layer), color = NA, alpha = 0.5) +
  geom_sf(data = fires.after2012, aes(fill = layer), color = 'red', alpha = 0.15) +
  coord_sf(xlim = c(ca_bbox["xmin"], ca_bbox["xmax"]),
           ylim = c(ca_bbox["ymin"], ca_bbox["ymax"])) +
  scale_fill_manual(name = "Layers", 
                    values = c("ASO Extents" = "deepskyblue3", 
                               "QL1 Lidar Availability" = "purple3", 
                               "Fires within 5 Years prior of Lidar Collection" = "red"),
                    labels = c("ASO Extents",
                               "QL1 Lidar Availability",
                               "Fires within 5 Years\nprior of Lidar Collection")) +
  labs(title = "California and Oregon Potential Study Areas") +
  theme_minimal() +
  theme(legend.position = "bottom")




ut_bbox <- c(xmin = -114, ymin = 37, xmax = -109, ymax = 42)

# keep only UT
ut <- us.states %>%
  dplyr::filter(name %in% c("Utah", 'Colorado', "Arizona", 'New Mexico', 'Wyoming', "Idaho"))

ql1.west$layer <- "QL1 Lidar Availability"
aso.extents$layer <- "ASO Extents"
fires.after2012$layer <- "Fires within 5 Years prior of Lidar Collection"

# now plot with all
ggplot() +
  geom_sf(data = ut, fill = "grey95", color = "black") +
  geom_sf(data = ql1.west, aes(fill = layer), color = NA, alpha = 0.3) +
  geom_sf(data = aso.extents, aes(fill = layer), color = NA, alpha = 0.5) +
  geom_sf(data = fires.after2012, aes(fill = layer), color = 'red', alpha = 0.15) +
  coord_sf(xlim = c(ut_bbox["xmin"], ut_bbox["xmax"]),
           ylim = c(ut_bbox["ymin"], ut_bbox["ymax"])) +
  scale_fill_manual(name = "Layers", 
                    values = c("ASO Extents" = "deepskyblue3", 
                               "QL1 Lidar Availability" = "purple3", 
                               "Fires within 5 Years prior of Lidar Collection" = "red"),
                    labels = c("ASO extents",
                               "QL1 lidar availability",
                               "Fires within 5 years\nprior of lidar collection")) +
  labs(title = "Utah Potential Study Area") +
  theme_minimal() +
  theme(legend.position = "bottom")          


dem <- rast(here('data', 'raw', 'background_variables', 'tif', 'DEM_creek.tif'))
res(dem)

