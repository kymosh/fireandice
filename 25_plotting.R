packages <- c('dplyr', 'ggplot2',  'sf', 'rnaturalearth', 'rnaturalearthdata', 'ggspatial', 'prettymapr', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ------------------------- figure of something? -----
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







# ------------------------- HUC study area boundary -----

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

# ------------------------- now same but cropping to elev -----

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

# register Times New Roman with Windows graphics
windowsFonts(
  'Times New Roman' = windowsFont('TT Times New Roman')
)

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
      linewidth = 0.25,
      alpha = 0.7
    ) +
    geom_sf(
      data = fire.plot,
      fill = NA,
      color = 'red',
      linewidth = 0.25
    ) +
    coord_sf(
      xlim = c(bb$xmin - xpad, bb$xmax + xpad),
      ylim = c(bb$ymin - ypad, bb$ymax + ypad),
      expand = FALSE,
      label_graticule = 'SW'
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 3)
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 4)
    ) +
    labs(
      title = fire.name,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(
      base_size = 10,
      base_family = 'Times New Roman'
    ) +
    theme(
      plot.title = element_text(
        family = 'Times New Roman',
        size = 12,
        face = 'bold',
        hjust = 0.5,
        margin = margin(b = 5)
      ),
      axis.text.x = element_text(
        family = 'Times New Roman',
        size = 10,
        color = 'black'
      ),
      axis.text.y = element_text(
        family = 'Times New Roman',
        size = 10,
        color = 'black'
      ),
      panel.grid.major = element_line(
        color = 'grey80',
        linewidth = 0.3
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 8, 5, 8)
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
    title = 'Final Study Areas and Intersecting HUC-12 Watersheds',
    theme = theme(
      plot.title = element_text(
        family = 'Times New Roman',
        size = 12,
        face = 'bold',
        hjust = 0.5,
        margin = margin(b = 5)
      )
    )
  )

p.all

# save
#dir.create('figures/intersecting_huc_maps', recursive = TRUE, showWarnings = FALSE)

ggsave(
  'figures/intersecting_huc_maps/all_fires_final_study_areas_2.png',
  plot = p.all,
  width = 180,
  height = 85,
  units = 'mm',
  dpi = 300
)


# ------------------------- Gap Percent Visualization -----------------------------
fire <- 'caldor'

r <- rast(paste0('data/processed/processed/tif/50m/', fire, '/', fire, '_canopy_metrics_50m.tif'))
gap <- r[[6]]
names(gap) <- 'gap_percent'
  
dem <- rast(paste0('data/processed/processed/tif/50m/', fire, '/', fire, '_nasadem_50m.tif'))

# chm
if (fire == 'caldor') {
  chm <- rast(paste0('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/', fire, '/', fire, '_chm_1m_32610.tif'))
  
} else if (fire == 'castle') {
  chm_files <- list.files('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/castle/75m/castle_chm_32611', pattern = '\\.tif$', full.names = TRUE)
  vrt(chm_files, 'J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/castle/75m/castle_chm_32611.vrt', overwrite = TRUE)
  chm <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/castle/75m/castle_chm_32611.vrt')
  
} else if (fire == 'creek') {
  chm <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/1m/creek/creek_chm_32611/creek_chm_1m_32611.vrt')}



# check alignment
compareGeom(gap, dem)

# build dataframe of candidate 50m pixels 
cand <- tibble(
  cell = 1:ncell(gap),
  gap_percent = values(gap)[,1],
  elevation   = values(dem)[,1]
) %>%
  filter(!is.na(gap_percent), !is.na(elevation)) %>%
  mutate(
    elev_class = case_when(
      elevation < 1750 ~ 'Low elevation',
      elevation > 2500 ~ 'High elevation',
      TRUE ~ NA_character_
    ),
    gap_class = case_when(
      between(gap_percent, .22, .28) ~ '25% gap',
      between(gap_percent, .47, .53) ~ '50% gap',
      between(gap_percent, .72, .78) ~ '75% gap',
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(elev_class), !is.na(gap_class))

set.seed(61)

cand.sample <- cand %>%
  mutate(
    gap_target = case_when(
      gap_class == "25% gap" ~ 0.25,
      gap_class == "50% gap" ~ 0.50,
      gap_class == "75% gap" ~ 0.75
    ),
    dist_to_target = abs(gap_percent - gap_target)
  ) %>%
  group_by(elev_class, gap_class) %>%
  slice_min(dist_to_target, n = 30, with_ties = FALSE) %>%
  ungroup()

library(terra)
library(dplyr)
library(purrr)

# Make sure the CHM is loaded using your existing fire-specific code

get_chm_gap <- function(cell_id) {
  
  # Create polygon for this exact 50m raster cell
  xy <- xyFromCell(gap, cell_id)
  
  half_res <- res(gap) / 2
  
  p <- as.polygons(
    ext(
      xy[1, 1] - half_res[1],
      xy[1, 1] + half_res[1],
      xy[1, 2] - half_res[2],
      xy[1, 2] + half_res[2]
    ),
    crs = crs(gap)
  )
  
  if (!same.crs(p, chm)) {
    p <- project(p, crs(chm))
  }
  
  chm.crop <- crop(chm, p, mask = TRUE)
  
  vals <- values(chm.crop, na.rm = TRUE)
  
  if (length(vals) == 0) {
    return(NA_real_)
  }
  
  mean(vals < 2) * 100
}

cand.sample <- cand.sample %>%
  mutate(
    chm_gap_pct = map_dbl(cell, get_chm_gap),
    raster_gap_pct = gap_percent * 100,
    gap_difference = abs(chm_gap_pct - raster_gap_pct)
  )

# pick candidates where calc gap pct closest matches gap_pct
cand.best <- cand.sample %>%
  filter(!is.na(chm_gap_pct)) %>%
  group_by(elev_class, gap_class) %>%
  slice_min(gap_difference, n = 1, with_ties = FALSE) %>%
  ungroup()

cand.best %>%
  select(
    elev_class,
    gap_class,
    elevation,
    raster_gap_pct,
    chm_gap_pct,
    gap_difference
  )


# ----- plot -----
plot.df <- map_dfr(seq_len(nrow(cand.best)), function(i) {
  
  cell_id <- cand.best$cell[i]
  xy <- xyFromCell(gap, cell_id)
  
  half_res <- res(gap) / 2
  
  p <- as.polygons(
    ext(
      xy[1, 1] - half_res[1],
      xy[1, 1] + half_res[1],
      xy[1, 2] - half_res[2],
      xy[1, 2] + half_res[2]
    ),
    crs = crs(gap)
  )
  
  if (!same.crs(p, chm)) {
    p <- project(p, crs(chm))
  }
  
  chm.crop <- crop(chm, p, mask = TRUE)
  
  binary <- ifel(chm.crop < 2, 0, 1)
  
  df <- as.data.frame(binary, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- 'canopy_class'
  
  df %>%
    mutate(
      x = x - xy[1, 1],
      y = y - xy[1, 2],
      canopy_class = factor(
        canopy_class,
        levels = c(0, 1),
        labels = c('Gap (<2 m)', 'Canopy (≥2 m)')
      ),
      elev_class = cand.best$elev_class[i],
      gap_class = cand.best$gap_class[i]
    )
})

plot.df <- plot.df %>%
  mutate(
    elev_class = factor(
      elev_class,
      levels = c('Low elevation', 'High elevation')
    ),
    gap_class = factor(
      gap_class,
      levels = c('25% gap', '50% gap', '75% gap')
    )
  )

p <- ggplot(plot.df, aes(x, y, fill = canopy_class)) +
  geom_raster() +
  facet_grid(elev_class ~ gap_class) +
  coord_equal(
    xlim = c(-25, 25),
    ylim = c(-25, 25),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = c(
      'Gap (<2 m)' = '#efff87',
      'Canopy (≥2 m)' = '#7aac2a'
    ),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = 'bold'),
    legend.position = 'bottom'
  )

# save plot
ggsave(
  filename = paste0('figures/', fire, '_gap_percent_examples.png'),
  plot = p,
  width = 10,
  height = 7,
  units = 'in',
  dpi = 300,
  bg = 'white'
)

# ----- now add CHM -----

# prepare CHM dataframe 
chm.df <- map_dfr(seq_len(nrow(cand.best)), function(i) {
  
  cell_id <- cand.best$cell[i]
  xy <- xyFromCell(gap, cell_id)
  
  half_res <- res(gap) / 2
  
  p <- as.polygons(
    ext(
      xy[1, 1] - half_res[1],
      xy[1, 1] + half_res[1],
      xy[1, 2] - half_res[2],
      xy[1, 2] + half_res[2]
    ),
    crs = crs(gap)
  )
  
  if (!same.crs(p, chm)) {
    p <- project(p, crs(chm))
  }
  
  chm.crop <- crop(chm, p, mask = TRUE)
  
  df <- as.data.frame(chm.crop, xy = TRUE, na.rm = TRUE)
  
  names(df)[3] <- 'height'
  
  df %>%
    mutate(
      x = x - xy[1, 1],
      y = y - xy[1, 2],
      elev_class = cand.best$elev_class[i],
      gap_class = cand.best$gap_class[i]
    )
})

chm.df <- chm.df %>%
  mutate(
    elev_class = factor(
      elev_class,
      levels = c('Low elevation', 'High elevation')
    ),
    gap_class = factor(
      gap_class,
      levels = c('25% gap', '50% gap', '75% gap')
    )
  )

# plot CHM 

p.chm <- ggplot(chm.df, aes(x, y, fill = height)) +
  geom_raster() +
  facet_grid(elev_class ~ gap_class) +
  coord_equal(
    xlim = c(-25, 25),
    ylim = c(-25, 25),
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    name = 'Canopy height (m)',
    option = 'viridis',
    limits = c(0, max(chm.df$height, na.rm = TRUE))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = 'bold'),
    legend.position = 'bottom'
  )

p.chm

# combine
library(terra)
library(dplyr)
library(purrr)
library(ggplot2)
library(maptiles)
library(viridisLite)

# figure setup

chm.max <- 50
chm.colors <- viridisLite::viridis(256)

# helper for continuous CHM colors
height_to_color <- function(h) {
  idx <- round(pmin(pmax(h, 0), chm.max) /
                 chm.max * 255) + 1
  
  chm.colors[idx]
}

# extract imagery and CHM for selected pixels

plot.list <- map(seq_len(nrow(cand.best)), function(i) {
  
  cell_id <- cand.best$cell[i]
  xy <- xyFromCell(gap, cell_id)
  half_res <- res(gap) / 2
  
  # create exact 50m pixel polygon
  p <- as.polygons(
    ext(
      xy[1, 1] - half_res[1],
      xy[1, 1] + half_res[1],
      xy[1, 2] - half_res[2],
      xy[1, 2] + half_res[2]
    ),
    crs = crs(gap)
  )
  
  if (!same.crs(p, chm)) {
    p <- project(p, crs(chm))
  }
  
  center.x <- (xmin(p) + xmax(p)) / 2
  center.y <- (ymin(p) + ymax(p)) / 2
  
  # CHM 
  chm.crop <- crop(chm, p, mask = TRUE)
  
  chm.df <- as.data.frame(
    chm.crop,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(chm.df)[3] <- 'height'
  
  chm.df <- chm.df %>%
    mutate(
      color = height_to_color(height),
      layer = 'CHM'
    )
  
  # binary gap classification 
  
  binary.df <- chm.df %>%
    mutate(
      color = if_else(
        height < 2,
        '#efff87',
        '#7aac2a'
      ),
      layer = 'Binary gap'
    )
  
  # satellite imagery
  
  # - 2022 NAIP imagery -
  
  sat <- get_naip(p, year = 2022)
  
  sat.df <- as.data.frame(
    sat,
    xy = TRUE,
    na.rm = TRUE
  )
  
  # first three bands are RGB
  rgb.vals <- as.matrix(sat.df[, 3:5])
  
  # handle either 0–1 or 0–255 image values
  if (max(rgb.vals, na.rm = TRUE) > 1) {
    rgb.vals <- rgb.vals / 255
  }
  
  rgb.vals <- pmin(pmax(rgb.vals, 0), 1)
  
  sat.df$color <- grDevices::rgb(
    rgb.vals[, 1],
    rgb.vals[, 2],
    rgb.vals[, 3]
  )
  
  sat.df$layer <- 'Satellite'
  
  sat <- crop(sat, p, mask = TRUE)
  
  sat.df <- as.data.frame(
    sat,
    xy = TRUE,
    na.rm = TRUE
  )
  
  # first three bands are RGB
  rgb.vals <- as.matrix(sat.df[, 3:5])
  
  rgb.vals <- pmin(pmax(rgb.vals, 0), 255)
  
  sat.df$color <- grDevices::rgb(
    rgb.vals[, 1],
    rgb.vals[, 2],
    rgb.vals[, 3],
    maxColorValue = 255
  )
  
  sat.df$layer <- 'Satellite'
  
  #  combine 
  
  bind_rows(
    sat.df %>% select(x, y, color, layer),
    chm.df %>% select(x, y, color, layer),
    binary.df %>% select(x, y, color, layer)
  ) %>%
    mutate(
      x = x - center.x,
      y = y - center.y,
      elev_class = cand.best$elev_class[i],
      gap_class = cand.best$gap_class[i]
    )
})

plot.df.combined <- bind_rows(plot.list)

# set facet ordering 

plot.df.combined <- plot.df.combined %>%
  mutate(
    elev_class = factor(
      elev_class,
      levels = c('Low elevation', 'High elevation')
    ),
    gap_class = factor(
      gap_class,
      levels = c('25% gap', '50% gap', '75% gap')
    ),
    layer = factor(
      layer,
      levels = c('Satellite', 'CHM', 'Binary gap')
    ),
    panel_row = factor(
      paste(elev_class, layer, sep = ' - '),
      levels = c(
        'Low elevation - Satellite',
        'Low elevation - CHM',
        'Low elevation - Binary gap',
        'High elevation - Satellite',
        'High elevation - CHM',
        'High elevation - Binary gap'
      )
    )
  )

# plot
p.combined <- ggplot(
  plot.df.combined,
  aes(x = x, y = y, fill = color)
) +
  geom_raster() +
  facet_grid(
    panel_row ~ gap_class
  ) +
  coord_equal(
    xlim = c(-25, 25),
    ylim = c(-25, 25),
    expand = FALSE
  ) +
  scale_fill_identity() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(
      size = 10,
      face = 'bold'
    ),
    panel.spacing = grid::unit(0.12, 'in'),
    plot.margin = margin(10, 10, 10, 10)
  )

p.combined

library(patchwork)

#  set factor ordering 

plot.df.combined <- plot.df.combined %>%
  mutate(
    gap_class = factor(
      gap_class,
      levels = c('25% gap', '50% gap', '75% gap')
    ),
    layer = factor(
      layer,
      levels = c('Satellite', 'CHM', 'Binary gap')
    )
  )

#  plotting function 

plot_gap_examples <- function(elev) {
  
  df <- plot.df.combined %>%
    filter(elev_class == elev)
  
  ggplot(df, aes(x = x, y = y, fill = color)) +
    geom_raster() +
    facet_grid(layer ~ gap_class) +
    coord_fixed(
      ratio = 1,
      xlim = c(-25, 25),
      ylim = c(-25, 25),
      expand = FALSE,
      clip = 'off'
    ) +
    scale_fill_identity() +
    labs(
      title = elev,
      x = NULL,
      y = NULL
    ) +
    theme_void() +
    theme(
      text = element_text(
        family = 'Times New Roman',
        size = 10
      ),
      plot.title = element_text(
        family = 'Times New Roman',
        size = 12,
        face = 'bold',
        hjust = 0.5,
        margin = margin(t = 5, b = 5)
      ),
      strip.text.x = element_text(
        family = 'Times New Roman',
        size = 10,
        face = 'bold',
        margin = margin(t = 5, b = 10)
      ),
      strip.clip = 'off',
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
      strip.text.y = element_blank(),
      strip.background = element_blank(),
      panel.spacing = grid::unit(0.12, 'in'),
      legend.position = 'none'
    )
}

# - create plots -

p.low <- plot_gap_examples('Low elevation')
p.high <- plot_gap_examples('High elevation')

ggsave(
  filename = paste0('figures/', fire, '_gap_chm_naip_examples_low_elev.png'),
  plot = p.low,
  width = 100,
  height = 100,
  units = 'mm',
  dpi = 300,
  bg = 'white'
)

ggsave(
  filename = paste0('figures/', fire, '_gap_chm_naip_examples_high_elev.png'),
  plot = p.high,
  width = 100,
  height = 100,
  units = 'mm',
  dpi = 300,
  bg = 'white'
)


