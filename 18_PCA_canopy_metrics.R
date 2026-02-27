packages <- c('dplyr', 'vegan', 'e1071', 'corrplot', 'terra')
lapply(packages, library, character.only = T)

# ------------------------------------------------------------------
# Principal Component Analysis
# ------------------------------------------------------------------

dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'

canopy.stack <- rast(file.path(dir, 'canopy_metrics_50m.tif'))
df <- as.data.frame(canopy.stack, cells = TRUE, na.rm = TRUE)
#write.csv(df, file.path(dir, 'canopy_metrics_50m_df.csv'))

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

# plot zq and zpcum
zq.vars     <- grep("zq", names(df), value = TRUE)
zpcum.vars  <- grep("zpcum", names(df), value = TRUE)

df.vert <- df[, c(zq.vars, zpcum.vars)]
pca.vert <- rda(df.vert, scale = TRUE)

scores.vert <- scores(pca.vert, display = "sites")

set.seed(1)
idx <- sample(nrow(scores.vert), 50000)

plot(scores.vert[idx,1],
     scores.vert[idx,2],
     pch = 16,
     cex = 0.3,
     col = rgb(0,0,0,0.2),
     xlab = "PC1 (Vertical)",
     ylab = "PC2 (Vertical)")


# ------------------------------------------------------------------
# Fuzzy C-Clustering
# ------------------------------------------------------------------

# ----- prep data -----

dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
canopy <- rast(file.path(dir, 'creek_canopy_metrics_50m.tif'))

# convert to df
df <- as.data.frame(canopy, cells = TRUE, na.rm = FALSE)

# remove NAs
vars <- setdiff(names(df), 'cell')
# keep only rows where *all* canopy metrics are present
ok <- complete.cases(df[, vars])
df.ok <- df[ok, c('cell', vars)]

# scale predictors
x <- scale(df.ok[, vars])

# ----- run fuzzy c-means ------

set.seed(14)
k <- 4

fc <- cmeans(
  x,
  centers = k,
  m = 2,
  iter.max = 200,
  method = 'cmeans'
)

# ----- map membership back to rasters -----
# template raster for geometry
template <- canopy[[1]]

# initialize a full-length membership matrix (all cells)
mem.full <- matrix(NA_real_, nrow = ncell(template), ncol = k)

# fill only the valid cells
mem.full[df.ok$cell, ] <- fc$membership

# membership rasters
mem.rasters <- lapply(1:k, function(i) {
  r <- template
  values(r) <- mem.full[, i]
  names(r) <- paste0('memb_c', i)
  r
})

membership.stack <- rast(mem.rasters)

# hard cluster (max membership)
hard.full <- rep(NA_integer_, ncell(template))
hard.full[df.ok$cell] <- fc$cluster

hard.raster <- template
values(hard.raster) <- hard.full
names(hard.raster) <- 'cluster_hard'

plot(membership.stack)
plot(hard.raster)
# ----- map clusters in PCA space ------

pca <- prcomp(x, center = FALSE, scale. = FALSE)

# build plotting df
plot.df <- data.frame(
  PC1 = pca$x[ , 1],
  PC2 = pca$x[ , 2],
  cluster = factor(fc$cluster),
  max_membership = apply(fc$membership, 1, max)
)

library(ggplot2)

ggplot(plot.df, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.4, size = 0.6) +
  theme_minimal() +
  labs(title = 'Fuzzy C-Means Clusters in PCA Space')

# add membership strength
ggplot(plot.df, aes(PC1, PC2, color = cluster, alpha = max_membership)) +
  geom_point(size = 0.6) +
  scale_alpha(range = c(0.2, 1)) +
  theme_minimal() +
  labs(title = 'Cluster Separation with Membership Strength')


# ------------------------------------------------------------------
# Variable Clustering
# ------------------------------------------------------------------

# build correlelogram
vars <- setdiff(names(df.ok), 'cell')
cor.mat <- cor(df.ok[, vars], use = 'pairwise.complete.obs')

# convert correlation to distance 
dist.mat <- as.dist(1 - abs(cor.mat))

# hierarchal clustering
hc <- hclust(dist.mat, method = 'average')
plot(hc, main = 'Variable Clustering Dendrogram')

# cut into clusters
threshold <- 0.75
cut.height <- 1 - threshold

var.clusters <- cutree(hc, h = cut.height)

split(names(var.clusters), var.clusters)
