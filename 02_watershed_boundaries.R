# Load libraries
library(sf)
library(dplyr)
library(ggplot2)

# set working directory (PC)
setwd("C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice")
# mac
setwd("/Users/kyliemosher/OneDrive/thesis/fireandice")

# Read in shapefiles
castle_fire <- st_read("data/raw/fire_info/shp/castle_fire_perimeter.shp")
huc <- st_read("data/raw/background_variables/shp/ACE_HUC12s_WebMerc_1mXY.shp"
) 

# Reproject to match CRS (if needed)
castle_fire <- st_transform(castle_fire, st_crs(huc))

# Optional: filter watersheds by name
huc_filtered <- huc %>%
  filter(grepl("Kaweah|Kern", Name, ignore.case = TRUE))

# Intersect fire with HUCs
fire_by_watershed <- st_intersection(castle_fire, huc_filtered)

# Calculate area burned in each watershed (in hectares)
fire_by_watershed <- fire_by_watershed %>%
  mutate(burn_area_ha = st_area(.) / 10000)  # hectares

# Summarize by watershed
summary <- fire_by_watershed %>%
  group_by(Name) %>%
  summarize(total_burned_ha = sum(burn_area_ha))

print(summary)
