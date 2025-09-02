packages <- c( 'here', 'ggplot2', 
               'tidyverse', 'terra', 'sf', 'rnaturalearth', 'rnaturalearthdata', 'rgeos')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

creek.fire <- st_read(here('data', 'raw', 'fire_info', 'shp', 'creek_fire_perimeter.shp'))
hucs <- st_read(here('data', 'raw', 'background_variables', 'shp', 'ACE_HUC12s_WebMerc_1mXY.shp'))

crs(creek.fire, describe = T)$code
crs(hucs, describe = T)$code

# change crs to WGS.84
creek.fire.32611 <- st_transform(creek.fire, crs = 32611)
hucs.32611 <- st_transform(hucs, crs = 32611)

# hucs that intersect creek fire boundary
creek.hucs <- hucs.32611 %>%
  st_filter(creek.fire.32611, .predicate = st_intersects)

# View result
plot(st_geometry(creek.hucs), col = adjustcolor('blue', alpha.f = 0.5), main = 'Creek Fire Intersecting HUCs')
plot(st_geometry(creek.fire.32611), add = TRUE, border = 'red')

# get California basemap
states <- ne_states(country = 'United States of America', returnclass = 'sf')
california <- states %>% dplyr::filter(name == 'California')
california.32611 <- st_transform(california, 32611)



# Get the bounding box of the Creek Fire
bbox <- st_bbox(creek.fire.32611)

# Calculate center and expand bbox
x_center <- (bbox['xmin'] + bbox['xmax']) / 2
y_center <- (bbox['ymin'] + bbox['ymax']) / 2
x_range <- (bbox['xmax'] - bbox['xmin']) / 2
y_range <- (bbox['ymax'] - bbox['ymin']) / 2

# New expanded bounding box
expanded_bbox <- c(
  xmin = x_center - x_range,
  xmax = x_center + x_range,
  ymin = y_center - y_range,
  ymax = y_center + y_range
)

# Plot
ggplot() +
  geom_sf(data = california.32611, fill = 'gray95', color = 'gray70') +
  geom_sf(data = creek.hucs, fill = adjustcolor('deepskyblue3', alpha.f = 0.3), color = 'deepskyblue4', size = 0.3) +
  geom_sf(data = creek.fire.32611, fill = adjustcolor('red3', alpha.f = 0.4), color = 'red3', size = 0.4) +
  coord_sf(
    xlim = c(expanded_bbox['xmin'], expanded_bbox['xmax']),
    ylim = c(expanded_bbox['ymin'], expanded_bbox['ymax']),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = 'Creek Fire Perimeter and Intersecting HUCs',
    caption = 'Data sources: USGS, CalFire, HUC12 boundaries'
  ) +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    panel.background = element_rect(fill = 'white'),
    panel.grid = element_line(color = 'gray90')
  )

