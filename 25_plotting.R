packages <- c('dplyr', 'ggplot2',  'sf', 'rnaturalearth', 'rnaturalearthdata', 'ggspatial', 'prettymapr', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ----- figure of something? -----
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







# ----- HUC study area boundary -----

# Read data
castle <- st_read('data/raw/fire_info/shp/castle_fire_perimeter.shp', quiet = TRUE)
caldor <- st_read('data/raw/fire_info/shp/caldor_fire_perimeter.shp', quiet = TRUE)
creek  <- st_read('data/raw/fire_info/shp/creek_fire_perimeter.shp', quiet = TRUE)

huc12 <- st_read('data/raw/background_variables/shp/HUC12_ca.shp', quiet = TRUE)

fires <- list(
  Caldor = caldor,
  Creek  = creek,
  Castle = castle
)

# output directory
out.dir <- 'figures/intersecting_huc_maps'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)


# Function to build map for one fire
plot_fire_hucs <- function(fire.name, fire.sf, huc.sf, out.dir) {
  
  # make sure CRS matches before spatial filtering
  huc.match <- st_transform(huc.sf, st_crs(fire.sf))
  
  # select HUCs that intersect fire perimeter
  huc.int <- st_filter(huc.match, fire.sf, .predicate = st_intersects)
  
  # transform both to lon/lat for plotting
  fire.plot <- st_transform(fire.sf, 4326)
  huc.plot  <- st_transform(huc.int, 4326)
  
  # bbox from HUCs so map includes full surrounding area
  bb <- st_bbox(huc.plot)
  xpad <- 0.03 * (bb$xmax - bb$xmin)
  ypad <- 0.03 * (bb$ymax - bb$ymin)
  
  p <- ggplot() +
    geom_sf(
      data = huc.plot,
      fill = 'lightblue',
      color = '#1f78b4',
      linewidth = 0.4
    ) +
    geom_sf(
      data = fire.plot,
      fill = '#c08a97',
      color = 'red',
      linewidth = 0.5,
      alpha = 0.65
    ) +
    coord_sf(
      xlim = c(bb$xmin - xpad, bb$xmax + xpad),
      ylim = c(bb$ymin - ypad, bb$ymax + ypad),
      expand = FALSE
    ) +
    labs(
      title = paste(fire.name, 'Fire Perimeter and Intersecting HUCs'),
      x = NULL,
      y = NULL,
      caption = 'Data sources: USGS, CalFire, HUC12 boundaries'
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = 'bold', hjust = 0.5),
      panel.grid.major = element_line(color = 'grey80', linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = 'black'),
      plot.caption = element_text(hjust = 0.5)
    )
  
  ggsave(
    filename = file.path(
      out.dir,
      paste0(tolower(fire.name), '_fire_intersecting_hucs.png')
    ),
    plot = p,
    width = 5,
    height = 5,
    dpi = 300
  )
  
  p
}


# Make maps
plots <- imap(
  fires,
  ~plot_fire_hucs(
    fire.name = .y,
    fire.sf = .x,
    huc.sf = huc12,
    out.dir = out.dir
  )
)

# view one
plots[['Creek']]

library(patchwork)

# remove repeated titles/captions and keep only fire names as panel titles
p.caldor <- plots[['Caldor']] +
  labs(
    title = 'Caldor',
    caption = NULL
  ) +
  theme(
    plot.title = element_text(face = 'bold', hjust = 0.5)
  )

p.creek <- plots[['Creek']] +
  labs(
    title = 'Creek',
    caption = NULL
  ) +
  theme(
    plot.title = element_text(face = 'bold', hjust = 0.5)
  )

p.castle <- plots[['Castle']] +
  labs(
    title = 'Castle',
    caption = NULL
  ) +
  theme(
    plot.title = element_text(face = 'bold', hjust = 0.5)
  )

# combine with one overall title and one caption
p.all <- (p.caldor + p.creek + p.castle) +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = 'Fire Perimeters and Intersecting HUC-12 Watersheds',
    caption = 'Data sources: USGS, CalFire, HUC12 boundaries'
  ) &
  theme(
    plot.title = element_text(face = 'bold', hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  )

p.all

# save
ggsave(
  'figures/intersecting_huc_maps/all_fires_intersecting_hucs.png',
  plot = p.all,
  width = 15,
  height = 5,
  dpi = 300
)

# ----- now same but cropping to elev -----

# Read fire perimeters
castle <- st_read('data/raw/fire_info/shp/castle_fire_perimeter.shp', quiet = TRUE)
caldor <- st_read('data/raw/fire_info/shp/caldor_fire_perimeter.shp', quiet = TRUE)
creek  <- st_read('data/raw/fire_info/shp/creek_fire_perimeter.shp', quiet = TRUE)

fires <- list(
  Caldor = caldor,
  Creek  = creek,
  Castle = castle
)

# huc12 layer
huc12 <- st_read('data/raw/background_variables/shp/HUC12_ca.shp', quiet = TRUE)


# final study-area extents (>1524 m)
final.dir <- 'data/processed/processed/shp/studyarea_extents'

final.files <- list.files(
  final.dir,
  pattern = '^study_extent_.*_1524\\.shp$',
  full.names = TRUE
)

final.extents <- lapply(final.files, st_read, quiet = TRUE)

names(final.extents) <- gsub(
  'study_extent_|_1524\\.shp',
  '',
  basename(final.files)
)

names(final.extents) <- c(
  caldor = 'Caldor',
  creek  = 'Creek',
  castle = 'Castle'
)[names(final.extents)]


# Function to make one panel
plot_final_study_area <- function(fire.name, fire.sf, huc.sf, final.sf) {
  
  # match CRS for spatial filtering
  huc.match <- st_transform(huc.sf, st_crs(fire.sf))
  final.match <- st_transform(final.sf, st_crs(fire.sf))
  
  # intersecting HUCs
  huc.int <- st_filter(huc.match, fire.sf, .predicate = st_intersects)
  
  # transform to lon/lat for plotting
  fire.plot  <- st_transform(fire.sf, 4326)
  huc.plot   <- st_transform(huc.int, 4326)
  final.plot <- st_transform(final.match, 4326)
  
  # use HUC extent for consistent framing of each panel
  bb <- st_bbox(huc.plot)
  xpad <- 0.03 * (bb$xmax - bb$xmin)
  ypad <- 0.03 * (bb$ymax - bb$ymin)
  
  ggplot() +
    geom_sf(
      data = huc.plot,
      fill = 'grey90',
      color = 'grey45',
      linewidth = 0.3
    ) +
    geom_sf(
      data = final.plot,
      fill = 'lightblue',
      color = '#1f78b4',
      linewidth = 0.5,
      alpha = 0.7
    ) +
    geom_sf(
      data = fire.plot,
      fill = NA,
      color = 'red',
      linewidth = 0.5
    ) +
    coord_sf(
      xlim = c(bb$xmin - xpad, bb$xmax + xpad),
      ylim = c(bb$ymin - ypad, bb$ymax + ypad),
      expand = FALSE
    ) +
    labs(
      title = fire.name,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = 'bold', hjust = 0.5),
      panel.grid.major = element_line(color = 'grey80', linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = 'black')
    )
}


# Build plots
p.caldor <- plot_final_study_area(
  fire.name = 'Caldor',
  fire.sf = fires[['Caldor']],
  huc.sf = huc12,
  final.sf = final.extents[['Caldor']]
)

p.creek <- plot_final_study_area(
  fire.name = 'Creek',
  fire.sf = fires[['Creek']],
  huc.sf = huc12,
  final.sf = final.extents[['Creek']]
)

p.castle <- plot_final_study_area(
  fire.name = 'Castle',
  fire.sf = fires[['Castle']],
  huc.sf = huc12,
  final.sf = final.extents[['Castle']]
)


# Combine side by side
p.all <- (p.caldor + p.creek + p.castle) +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = 'Final Study Areas and Intersecting HUC-12 Watersheds'
  ) &
  theme(
    plot.title = element_text(face = 'bold', hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  )

p.all

# save
dir.create('figures/intersecting_huc_maps', recursive = TRUE, showWarnings = FALSE)

ggsave(
  'figures/intersecting_huc_maps/all_fires_final_study_areas.png',
  plot = p.all,
  width = 15,
  height = 5,
  dpi = 300
)
