packages <- c( 'here', 'ggplot2', 
               'tidyverse', 'terra', 'sf', 'rnaturalearth', 'rnaturalearthdata', 'rgeos', 'viridis')
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

# zoomed-out plot of all of California
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

# zoomed-in version around Creek Fire
# Bounding box around HUCs, with padding
huc.bbox <- st_bbox(creek.hucs)
zoom.bbox <- c(
  xmin = huc.bbox['xmin'] - 5000,
  xmax = huc.bbox['xmax'] + 5000,
  ymin = huc.bbox['ymin'] - 5000,
  ymax = huc.bbox['ymax'] + 5000
)

# Zoomed-in map without CA background
ggplot() +
  geom_sf(data = creek.hucs,
          fill = adjustcolor('deepskyblue3', alpha.f = 0.3),
          color = 'deepskyblue4', size = 0.3) +
  geom_sf(data = creek.fire.32611,
          fill = adjustcolor('red3', alpha.f = 0.4),
          color = 'red3', size = 0.4) +
  coord_sf(
    xlim = c(zoom.bbox['xmin'], zoom.bbox['xmax']),
    ylim = c(zoom.bbox['ymin'], zoom.bbox['ymax']),
    expand = T
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


# Get bbox of creek.hucs and buffer it
huc.bbox <- st_bbox(creek.hucs)

zoom.bbox <- st_bbox(c(
  xmin = huc.bbox['xmin'] - 5000,
  xmax = huc.bbox['xmax'] + 5000,
  ymin = huc.bbox['ymin'] - 5000,
  ymax = huc.bbox['ymax'] + 5000
), crs = st_crs(creek.hucs))

# Now make polygon from bbox
zoom.window <- st_as_sfc(zoom.bbox)

# Clip hucs to zoom window
hucs.zoom <- st_intersection(hucs, zoom.window)


# Layer for legend
hucs$layer <- 'All HUCs'
creek.hucs$layer <- 'Intersecting HUCs'
creek.fire.32611$layer <- 'Creek Fire'

ggplot() +
  # background HUCs (all of CA)
  geom_sf(data = hucs, aes(fill = layer),
          color = 'gray70', size = 0.2, alpha = 0.2) +
  # highlighted intersecting HUCs
  geom_sf(data = creek.hucs, aes(fill = layer),
          color = 'deepskyblue4', size = 0.3, alpha = 0.4) +
  # fire perimeter
  geom_sf(data = creek.fire.32611, aes(fill = layer),
          color = 'red3', size = 0.4, alpha = 0.4) +
  # colors for legend
  scale_fill_manual(
    name = 'Layer',
    values = c(
      'All HUCs' = 'gray80',
      'Intersecting HUCs' = 'deepskyblue3',
      'Creek Fire' = 'red3'
    )
  ) +
  # zoom to creek.hucs extent
  coord_sf(
    xlim = c(zoom.bbox['xmin'], zoom.bbox['xmax']),
    ylim = c(zoom.bbox['ymin'], zoom.bbox['ymax']),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = 'Creek Fire and Surrounding HUCs',
    caption = 'Data sources: USGS, CalFire, HUC12 boundaries'
  ) +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    panel.background = element_rect(fill = 'white'),
    panel.grid = element_line(color = 'gray90'),
    legend.position = 'right'
  )

# ==============================================================================

swe <- rast('data/processed/processed/tif/50m/ASO_SanJoaquin_2023_0414_swe_50m_1524.tif')
swe <- rast('data/processed/processed/tif/50m/ASO_SanJoaquin_2022_0206_swe_50m_1524.tif')

sdd <- rast('data/processed/processed/tif/500m/creek_sdd_wy2022_32611_1524.tif')

plot(swe)
plot(sdd)

# ----------- SWE ------------
swe[swe > 0.8] <- NA
swe.df <- as.data.frame(swe, xy = TRUE, na.rm = TRUE)
names(swe.df)[3] <- 'swe'

max.swe <- max(swe.df$swe, na.rm = TRUE)
ggplot() +
  geom_raster(data = swe.df, aes(x = x, y = y, fill = swe)) +
  coord_equal() +
  scale_fill_gradientn(
    name = 'SWE (m)',
    colors = c('lightgrey', 'white', '#88bde6', '#08306b'),
    values = rescale(c(0, max.swe * 0.02, max.swe * 0.5, max.swe)),
    limits = c(0, max.swe)
  ) +
  labs(
    title = 'Snow Water Equivalent (SWE) in February, 2022',
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = 'bold'),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

summary(swe.df$swe)
hist(swe.df$swe)


# ---------------- SDD ------------------

sdd.df <- as.data.frame(sdd, xy = TRUE, na.rm = TRUE)

names(sdd.df)[3] <- 'sdd'
sdd.df$sdd[sdd.df$sdd == 0] <- NA


ggplot() +
  geom_tile(data = sdd.df, aes(x = x, y = y, fill = sdd)) +
  coord_equal() +
  scale_fill_viridis(
    name = 'SDD (Julian days)',
    option = 'mako',
    na.value = 'grey85'
  ) +
  labs(
    title = 'WY2022 Snow Disappearance Date',
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = 'bold'),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
