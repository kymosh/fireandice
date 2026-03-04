packages <- c( 'here', 'terra', 'tidyverse', 'ggplot2', 'RColorBrewer')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# create directory 

tif.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif'

# read in necessary data
lc <- rast('J:/Fire_Snow/fireandice/data/raw/background_variables/tif/creek_NALCMS_2020.tif')

dem.30 <- rast(file.path(tif.dir, '30m/creek/nasadem_creek_30m_1524.tif'))

# mask to correct elevations and mask out surrounding pixels

ext(lc)
ext(dem.30)
lc.crop <- crop(lc, dem.30)
lc.mask <- mask(lc.crop, dem.30)



writeRaster(lc.mask, here(tif.dir, '30m', 'creek_landcover_30m_1524.tif'))

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






#######################  resample  #############################
tif.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif'

landcover <- rast(file.path(tif.dir, '30m/creek/creek_landcover_30m_1524.tif'))

# Define landcover groupings
lc.groups <- list(
  Undesirable = c(16, 17, 18, 19),
  Temperate_subpolar_needleleaf_forest = c(1),
  Temperate_subpolar_broadleaf_deciduous_forest = c(5),
  Mixed_forest = c(6),
  Temperate_subpolar_shrubland = c(8),
  Temperate_subpolar_grassland = c(10),
  Wetland = c(14)
)

# target resolution
swe <- rast(file.path(tif.dir, '50m/creek/snow_metrics/ASO_SanJoaquin_2024_0127_swe_50m_1524.tif'))
sdd <- rast(file.path(tif.dir, '500m/creek/snow_metrics/creek_sdd_wy2020_32611_1524.tif'))

#### SDD ####

# Initialize list for fractional layers
frac.list <- list()

# Loop over each landcover group
for (grp in names(lc.groups)) {
  classes <- lc.groups[[grp]]
  
  # Binary mask for this group (1 if pixel belongs to any of these classes)
  lc.bin <- landcover %in% classes
  
  # Fractional cover at sdd resolution (mean of 1's = proportion)
  lc.frac <- resample(lc.bin, sdd, method = 'average')
  
  # Mask to study area
  lc.frac <- mask(lc.frac, sdd)
  
  # Rename layer
  names(lc.frac) <- grp
  
  frac.list[[grp]] <- lc.frac
}

# Combine all fractional cover layers
landcover.frac.500m <- rast(frac.list)

# Check result
plot(landcover.frac.500m)
plot(landcover.frac.500m$Undesirable)

# Save raster stack 
writeRaster(landcover.frac.500m, file.path(tif.dir, '500m/creek/creek_landcover_fractional_groups_500m.tif'), overwrite = TRUE)

#### SWE ####

# Initialize list for fractional layers
frac.list <- list()

# Loop over each landcover group
for (grp in names(lc.groups)) {
  classes <- lc.groups[[grp]]
  
  # Binary mask for this group (1 if pixel belongs to any of these classes)
  lc.bin <- landcover %in% classes
  
  # Fractional cover at sdd resolution (mean of 1's = proportion)
  lc.frac <- resample(lc.bin, swe, method = 'average')
  
  # Mask to study area
  lc.frac <- mask(lc.frac, swe)
  
  # Rename layer
  names(lc.frac) <- grp
  
  frac.list[[grp]] <- lc.frac
}

# Combine all fractional cover layers
landcover.frac.50m <- rast(frac.list)

# Check result
plot(landcover.frac.50m)
plot(landcover.frac.50m$Temperate_subpolar_needleleaf_forest)


# Save raster stack 
writeRaster(landcover.frac.50m, file.path(tif.dir, '50m/creek/creek_landcover_fractional_groups_50m.tif'), overwrite = TRUE)



# troubleshooting
snow <- rast(file.path(tif.dir, '500m/creek/creek_landcover_fractional_groups_500m_nosnow_temp.tif'))
nosnow <- rast(file.path(tif.dir, '500m/creek/creek_landcover_fractional_groups_500m.tif'))

# 3) basic global summaries (min/max/mean per layer)
global(snow, fun = c('min','max','mean'), na.rm = TRUE)
global(nosnow, fun = c('min','max','mean'), na.rm = TRUE)

# 4) difference raster + per-layer stats
d <- snow - nosnow

# How different, per layer?
diff.stats <- global(d, fun = c('min','max','mean'), na.rm = TRUE)
print(diff.stats)

# 5) how many cells changed (tolerance helps avoid tiny float noise)
tol <- 1e-8
changed <- global(abs(d) > tol, fun = 'sum', na.rm = TRUE)
print(changed)

# 6) overall max absolute difference (quick sanity)
maxabs <- global(abs(d), fun = 'max', na.rm = TRUE)
print(maxabs)

# 7) optional: look at where differences occur for a specific layer
# e.g., if you suspect 'Undesirable' should differ
# plot(d[['Undesirable']])
# hist(values(d[['Undesirable']]), breaks = 50)