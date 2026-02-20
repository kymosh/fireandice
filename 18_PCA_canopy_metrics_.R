packages <- c('dplyr', 'vegan', 'terra')
lapply(packages, library, character.only = T)

# ------------------------------------------------------------------
# Combine rasters into single stack
# ------------------------------------------------------------------

# ----- read in metric rasters
dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics'

cover <- rast(file.path(dir, 'creek_cover_metrics_50m_32611_masked.tif'))
fd <- rast(file.path(dir, 'creek_fractal_dim_50m_32611_masked.tif'))
gap <- rast(file.path(dir, 'creek_gap_50m_32611_masked.tif'))
height <- rast(file.path(dir, 'creek_height_metrics_50m_32611_masked.tif'))

names(cover)  <- paste0('cover_', names(cover))
names(fd)     <- paste0('fd_', names(fd))
names(gap)    <- paste0('gap_', names(gap))
names(height) <- paste0('ht_', names(height))

rasters <- list(cover = cover,
                fd = fd, 
                gap = gap,
                height = height)

# ----- check if all matching 

# CRS
lapply(rasters, function(r) crs(r, describe = TRUE)$code)

# resolution
lapply(rasters, res)

# extent
lapply(rasters, ext)
# cover has smallest extent

# origin
lapply(rasters, origin)

# ----- crop by smallest ext 

# find smallest raster by cell count
cells <- sapply(rasters, ncell)
template <- rasters[[ which.min(cells) ]]

# crop all rasters to that template
rasters.cropped <- lapply(rasters, function(r) {
  crop(r, template)
})

# ----- combine into single stack 
canopy.stack <- rast(rasters.cropped)

# restore correct names
names(canopy.stack) <- unlist(lapply(rasters.cropped, names))

plot(canopy.stack)

# save
writeRaster(canopy.stack, file.path(dir, 'canopy_metrics_50m.tif'))

# ------------------------------------------------------------------
# Principal Component Analysis
# ------------------------------------------------------------------

df <- as.data.frame(canopy.stack, cells = TRUE, na.rm = TRUE)

pca <- rda(df[ , -1], scale = TRUE)
summary(pca)

scores.df <- scores(pca, display = 'sites')

# create empty raster and fill by cell index
pc1.r <- canopy.stack[[1]]
values(pc1.r) <- NA_real_
values(pc1.r)[df$cell] <- scores.df[ ,1]
plot(pc1.r, main = 'PC1: Structural Development')

pc2.r <- canopy.stack[[1]]
values(pc2.r) <- NA_real_
values(pc2.r)[df$cell] <- scores.df[, 2]

plot(pc2.r, main = 'PC2: Gap / Fragmentation')

# broken stick plot
screeplot(pca, bstick = TRUE)

# PC1 vs PC2 Scatter
set.seed(112)
idx <- sample(nrow(scores.df), 50000)

plot(scores.df[idx, 1],
     scores.df[idx, 2],
     pch = 16,
     cex = 0.3,
     col = rgb(0,0,0,0.2),
     xlab = "PC1: Structural Development",
     ylab = "PC2: Gap / Fragmentation")

biplot(pca, scaling = 2)

ordiplot(pca, type = "none", scaling = 2)
text(pca, display = "species", scaling = 2, cex = 0.7)
