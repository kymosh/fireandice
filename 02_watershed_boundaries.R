packages <- c('dplyr', 'tidyr', 'sf', 'terra')
lapply(packages, library, character.only = T)

# Read in shapefiles
castle.0 <- st_read('data/raw/fire_info/shp/castle_fire_perimeter.shp')
dixie.0 <- st_read('data/raw/fire_info/shp/dixie_fire_perimeter.shp')
caldor.0 <- st_read('data/raw/fire_info/shp/caldor_fire_perimeter.shp')

huc.0 <- st_read('data/raw/background_variables/shp/HUC12_ca.shp'
) 

fires <- list(castle = castle.0, dixie = dixie.0, caldor = caldor.0)

# Reproject to 32611
huc <- st_transform(huc.0, 'EPSG:32611')
fires.32611 <- lapply(fires, st_transform, 'EPSG:32611')

# Intersect fire with HUCs
fire.by.hucs <- st_intersection(fires.32611, huc)

# Calculate area burned in each watershed (in hectares)
fire_by_watershed <- fire_by_watershed %>%
  mutate(burn_area_ha = st_area(.) / 10000)  # hectares

# Summarize by watershed
summary <- fire_by_watershed %>%
  group_by(Name) %>%
  summarize(total_burned_ha = sum(burn_area_ha))

print(summary)
