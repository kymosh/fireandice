packages <- c( 'here', 'exactextractr', 'terra', 'tidyverse', 'ggplot2', 'RColorBrewer')
# install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# none of this first part matters?? 
fire <- 'creek'

# create directory 
tif.dir <- 'data/processed/processed/tif'

# read in necessary data
lc.file <- paste0('data/raw/background_variables/tif/nalcms_30m_', fire, '.tif')
lc <- rast(lc.file)

dem.file <- paste0('data/processed/processed/tif/30m/', fire, '/', fire, '_nasadem_30m.tif')
dem.30 <- rast(dem.file)

# mask to correct elevations and mask out surrounding pixels

ext(lc)
ext(dem.30)
lc.crop <- crop(lc, dem.30)
landcover <- mask(lc.crop, dem.30)

# ----- if that doesn't work (like for creek), do this first: -----
# extract extents
rasters <- list(lc = lc, dem = dem.30)

exts <- lapply(rasters, ext)

# common extent
common.ext <- ext(
  max(sapply(exts, xmin)),
  min(sapply(exts, xmax)),
  max(sapply(exts, ymin)),
  min(sapply(exts, ymax))
)

# crop to common extent
rasters.crop <- lapply(
  rasters,
  crop,
  y = common.ext
)

landcover <- mask(
  rasters.crop$lc,
  rasters.crop$dem
)

# ----- continue ----- 
# convert NALCMS class 0 (background) to NA
landcover[landcover == 0] <- NA

# Define landcover class names
landcover_labels <- c(
  '1' = 'Temperate/sub-polar needleleaf forest',
  '5' = 'Temperate/sub-polar broadleaf deciduous forest',
  '6' = 'Mixed forest',
  '8' = 'Temperate/sub-polar shrubland',
  '10' = 'Temperate/sub-polar grassland',
  '14' = 'Wetland',
  '15' = 'Cropland',
  '16' = 'Barren land',
  '17' = 'Urban/built-up',
  '18' = 'Water',
  '19' = 'Snow and ice'
)

# ----- data exploration -----
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







#------------------  resample / aggregation  ------------------
# inputs
fire <- 'creek'

# read in necessary data
lc.file <- paste0('data/raw/background_variables/tif/nalcms_30m_', fire, '.tif')
landcover <- rast(lc.file)

# convert NALCMS class 0 (background) to NA
landcover[landcover == 0] <- NA

sort(unique(values(landcover)))

res <- '500m' # 50 or 500

# settings 
in.dir <- 'data/processed/processed/tif/'
out.dir <- paste0(in.dir, res, '/', fire, '/')

# target resolution
swe <- rast(paste0(in.dir, '50m/', fire, '/canopy_metrics/', fire, '_cover_metrics_50m.tif'))
swe <- swe[[1]]
sdd <- rast(paste0(in.dir, '500m/', fire, '/snow_metrics/', fire, '_sdd_wy2023_500m.tif'))
# creek
sdd <- rast(paste0(in.dir, '500m/', fire, '/snow_metrics/', 'swe_peak_wy2021_500m.tif'))

if (res == '50m') {
  target <- swe
} else if (res == '500m') {
  target <- sdd
} else {
  stop('res must be either \'50m\' or \'500m\'')
}

# Define landcover groupings
lc.groups <- list(
  Undesirable = c(15, 16, 17, 18, 19),
  Temperate_subpolar_needleleaf_forest = c(1),
  Temperate_subpolar_broadleaf_deciduous_forest = c(5),
  Mixed_forest = c(6),
  Temperate_subpolar_shrubland = c(8),
  Temperate_subpolar_grassland = c(10),
  Wetland = c(14)
)

# Initialize list for fractional layers
frac.list <- list()

# Loop over each landcover group
for (grp in names(lc.groups)) {
  classes <- lc.groups[[grp]]
  
  # Binary mask for this group (1 if pixel belongs to any of these classes)
  lc.bin <- landcover %in% classes
  lc.bin <- mask(lc.bin, landcover)
  
  # Fractional cover at sdd resolution (mean of 1's = proportion)
  lc.frac <- resample(lc.bin, target, method = 'average')
  
  # Mask to study area
  lc.frac <- mask(lc.frac, target)
  
  # Rename layer
  names(lc.frac) <- grp
  
  frac.list[[grp]] <- lc.frac
}

# Combine all fractional cover layers
landcover.frac <- rast(frac.list)

# Check result
plot(landcover.frac)
plot(landcover.frac$Temperate_subpolar_needleleaf_forest)


# Save raster stack 
writeRaster(landcover.frac, paste0(out.dir, fire, '_landcover_', res, '.tif'), overwrite = TRUE)

