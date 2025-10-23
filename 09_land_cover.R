packages <- c( 'here', 'terra', 'tidyverse', 'ggplot2', 'RColorBrewer')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# create directory 
tif.dir <- here('data', 'processed', 'processed', 'tif')

# read in necessary data
lc <- rast(here('data', 'raw', 'background_variables', 'tif', 'creek_NALCMS_2020.tif'))
dem.30 <- rast(here(tif.dir, '30m', 'nasadem_creek_30m_1524.tif'))

# mask to correct elevations and mask out surrounding pixels

ext(lc)
ext(dem.30)
lc.crop <- crop(lc, dem.30)
lc.mask <- mask(lc.crop, dem.30)



writeRaster(lc.mask, here(tif.dir, '30m', 'creek_landcover_1524.tif'))

# explore landcover
lc.df <- as.data.frame(lc.mask)
plot(lc.mask)

# Define landcover class names
landcover_labels <- c(
  '1' = 'Temperate/sub-polar needleleaf forest',
  '5' = 'Temperate/sub-polar broadleaf deciduous forest',
  '6' = 'Mixed forest',
  '8' = 'Temperate/sub-polar shrubland',
  '10' = 'Temperate/sub-polar grassland',
  '14' = 'Wetland',
  '16' = 'Barren land',
  '17' = 'Urban/built-up',
  '18' = 'Water',
  '19' = 'Snow and ice'
)


# Create a distinct color palette with enough contrast
# Here we use 19 qualitative colors from RColorBrewer
palette_colors <- brewer.pal(12, 'Paired')  # 12 distinct colors
# repeat/extend to 19
palette_colors <- c(palette_colors, brewer.pal(8, 'Set3')[1:7])

# Make landcover a factor with descriptive labels
lc.df$landcover_factor <- factor(lc.df$landcover,
                                 levels = as.numeric(names(landcover_labels)),
                                 labels = landcover_labels)

# Horizontal bar chart
ggplot(lc.df, aes(x = landcover_factor, fill = landcover_factor)) +
  geom_bar(color = 'black') +
  scale_fill_manual(values = palette_colors) +
  labs(
    title = 'Distribution of Landcover Classes',
    x = NULL,
    y = 'Count'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # hide x-axis labels
    axis.ticks.x = element_blank(),
    legend.title = element_blank()
  )


###### and again showing true map colors
# Define palette colors (hex)
landcover_colors <- c(
  '#033e00','#5b725c','#6b7d2c','#b48833','#e0cd88','#6ca289','#a9abae',
  '#db2126','#4c73a1','#fff7fe'
)

# Make landcover a factor with labels
lc.df$landcover_factor <- factor(lc.df$landcover, 
                                 levels = as.numeric(names(landcover_labels)), 
                                 labels = landcover_labels)

# Plot histogram
ggplot(lc.df, aes(x = landcover_factor, fill = landcover_factor)) +
  geom_bar(color = 'black') +
  scale_fill_manual(values = landcover_colors) +
  labs(
    title = 'Distribution of Landcover Classes',
    x = 'Landcover Class',
    y = 'Count'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # hide x-axis labels
    legend.title = element_blank()
  )

plot(lc.mask)
plot(creek.scar, add = T, border = 'red', lwd = 1, col = NA)
creek.scar <- st_read(here('data', 'raw', 'fire_info', 'shp', 'creek_simple.shp'))
