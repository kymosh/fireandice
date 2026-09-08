packages <- c('dplyr', 'ggplot2',  'sf', 'rnaturalearth', 'rnaturalearthdata', 'ggspatial', 'prettymapr', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# --- HUCs ---
HUC.caldor <- st_read('data/processed/processed/shp/mapping/HUC_caldor.shp')
HUC.creek <- st_read('data/processed/processed/shp/mapping/HUC_creek.shp')
HUC.castle <- st_read('data/processed/processed/shp/mapping/HUC_castle.shp')

HUC.caldor <- HUC.caldor %>%
  st_transform(3857)

HUC.creek <- HUC.creek %>%
  st_transform(3857)

HUC.castle <- HUC.castle %>%
  st_transform(3857)

HUCs <- rbind(
  HUC.caldor,
  HUC.creek,
  HUC.castle
)


ql1.0 <- st_read('data/processed/processed/shp/mapping/ql1_2.shp')

# map base layers
world <- ne_countries(scale = 'medium', returnclass = 'sf')

states <- ne_states(
  country = 'United States of America',
  returnclass = 'sf'
)

# California boundary
ca <- states %>%
  filter(name_en == 'California') %>%
  st_transform(st_crs(3310))


# --- simplify ql1 shpfile ---
# crop lidar polygons to California bounding box
ql1 <- ql1.0 %>%
  st_crop(st_bbox(ca))

# combine into single geometry
ql1 <- ql1 %>%
  st_union() %>%
  st_as_sf()


ql1.simple <- ql1 %>%
  st_transform(3310) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 250)

st_write(ql1.simple, 'data/processed/processed/shp/mapping/ql1_simple.shp')

# --- simplify aso ---
aso <- st_read('data/processed/processed/shp/mapping/aso.shp')

aso.simple <- aso %>%
  st_transform(3310) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 250)

aso.simple <- aso.simple %>%
  st_crop(st_bbox(ca))

st_write(aso.simple, 'data/processed/processed/shp/mapping/aso_simple.shp', append = F)

# --- plot ---

crs(ql1.simple, describe = T)$code
crs(aso.simple, describe = T)$code
crs(ca, describe = T)$code


ca.map <- st_transform(ca, 3857)
aso.map <- st_transform(aso.simple, 3857)
ql1.map <- st_transform(ql1.simple, 3857)

bb <- st_bbox(ca.map)

# plot with just QL1 lidar
p.1 <- ggplot() +
  annotation_map_tile(
    type = 'osm',
    zoom = 6
  ) +
  geom_sf(
    data = ql1.map,
    aes(fill = 'QL1 lidar extent'),
    color = NA,
    alpha = 0.6
  ) +
  geom_sf(
    data = ca.map,
    fill = NA,
    color = 'black',
    linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c(
      'QL1 lidar extent' = 'olivedrab'
    ),
    breaks = c(
      'QL1 lidar extent'
    ),
    name = NULL
  ) +
  coord_sf(
    xlim = c(bb['xmin'] - 75000, bb['xmax'] + 75000),
    ylim = c(bb['ymin'] - 75000, bb['ymax'] + 75000),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = 'right',
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.2, 'cm')
  )

# plot adding ASO data
p.2 <- ggplot() +
  annotation_map_tile(
    type = 'osm',
    zoom = 6
  ) +
  geom_sf(
    data = ql1.map,
    aes(fill = 'QL1 lidar extent'),
    color = NA,
    alpha = 0.6
  ) +
  geom_sf(
    data = aso.map,
    aes(fill = 'ASO extent'),
    color = NA,
    alpha = 0.6
  ) +
  geom_sf(
    data = ca.map,
    fill = NA,
    color = 'black',
    linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c(
      'QL1 lidar extent' = 'olivedrab',
      'ASO extent' = 'lightskyblue'
    ),
    breaks = c(
      'QL1 lidar extent',
      'ASO extent'
    ),
    name = NULL
  ) +
  coord_sf(
    xlim = c(bb['xmin'] - 75000, bb['xmax'] + 75000),
    ylim = c(bb['ymin'] - 75000, bb['ymax'] + 75000),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = 'right',
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.2, 'cm')
  )


# plot adding fires
p.3 <- ggplot() +
  annotation_map_tile(
    type = 'osm',
    zoom = 6
  ) +
  geom_sf(
    data = ql1.map,
    aes(fill = 'QL1 lidar extent'),
    color = NA,
    alpha = 0.6
  ) +
  geom_sf(
    data = aso.map,
    aes(fill = 'ASO extent'),
    color = NA,
    alpha = 0.6
  ) +
  geom_sf(
    data = HUCs,
    aes(fill = 'Fires'),
    color = 'red',
    alpha = 0.2,
    linewidth = 0.6
  ) +
  geom_sf(
    data = ca.map,
    fill = NA,
    color = 'black',
    linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c(
      'QL1 lidar extent' = 'olivedrab',
      'ASO extent' = 'lightskyblue',
      'Fires' = 'red'
    ),
    breaks = c(
      'QL1 lidar extent',
      'ASO extent',
      'Fires'
    ),
    name = NULL
  ) +
  coord_sf(
    xlim = c(bb['xmin'] - 75000, bb['xmax'] + 75000),
    ylim = c(bb['ymin'] - 75000, bb['ymax'] + 75000),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = 'right',
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.2, 'cm')
  )

ggsave(
  'outputs/studyareas_map_1.png',
  plot = p.1,
  width = 8,
  height = 6,
  units = 'in',
  dpi = 300
)

ggsave(
  'outputs/studyareas_map_2.png',
  plot = p.2,
  width = 8,
  height = 6,
  units = 'in',
  dpi = 300
)

ggsave(
  'outputs/studyareas_map_3.png',
  plot = p.3,
  width = 8,
  height = 6,
  units = 'in',
  dpi = 300
)






