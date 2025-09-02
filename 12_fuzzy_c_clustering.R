packages <- c( 'here', 'raster', 'sf', 'terra', 'geodata',
               'tidyverse', 'e1071')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Read in full scaled Creek terrain dataframe
creek.scaled.df <- readRDS(here('data', 'processed', 'dataframes', 'creek_terrain_scaled.rds'))

# rename dem to elev (elevation)
#names(creek.scaled.df)[names(creek.scaled.df) == 'dem'] <- 'elev'

# create matrix of just terrain variables, excluding x and y
#terrain.matrix <- creek.scaled.df %>%
  # dplyr::select(tpi_130, tpi_510, tpi_2010, slope, hli, elev) %>%
  # as.matrix()
#saveRDS(terrain.matrix, here('data', 'processed', 'dataframes', 'creek_terrain_matrix.rds'))

terrain.matrix <- readRDS(here('data', 'processed', 'dataframes', 'creek_terrain_matrix.rds'))


# create subset of matrix to run fuzzy c clustering on

set.seed(42)  # for reproducibility

# sample size 0.0005%
sample.size <- round(nrow(terrain.matrix) * 0.0005)
sample.rows <- sample(nrow(terrain.matrix), sample.size)
terrain.sample <- terrain.matrix[sample.rows, ]

# fuzzy c clustering on subset - 6 clusters
fcm.result.6 <- cmeans(
  terrain.sample,
  centers = 6,         # should be between 4 and 8, we'll start with 6
  m = 2,               # fuzziness parameter
  iter.max = 100,      # max iterations
  verbose = TRUE,
  method = 'cmeans'    # default method
)

# run PCA and plot just first 2 components
pca.result <- prcomp(terrain.sample, center = TRUE, scale. = TRUE)
pca.scores <- pca.result$x[, 1:2]  # just the first two components

# assign cluster results
cluster.hard <- fcm.result.6$cluster

# max membership value (how confident the algorithm was)
max.membership <- apply(fcm.result.6$membership, 1, max)

# combine for plotting
cluster.df <- data.frame(
  PC1 = pca.scores[, 1],
  PC2 = pca.scores[, 2],
  cluster = factor(cluster.hard),
  membership = max.membership
)

# plot results
ggplot(cluster.df, aes(x = PC1, y = PC2, color = cluster, alpha = membership)) +
  geom_point(size = 1.5) +
  scale_alpha(range = c(0.4, 1)) +
  labs(title = 'Fuzzy C-Means Clustering in PCA Space',
       color = 'Cluster', alpha = 'Max Membership') +
  theme_minimal()








# 4 clusters
fcm.result.4 <- cmeans(
  terrain.sample,
  centers = 4,         # should be between 4 and 8, we'll start with 6
  m = 2,               # fuzziness parameter
  iter.max = 100,      # max iterations
  verbose = TRUE,
  method = 'cmeans'    # default method
)

# assign cluster results
cluster.hard <- fcm.result.4$cluster

# max membership value (how confident the algorithm was)
max.membership <- apply(fcm.result.4$membership, 1, max)

# combine for plotting
cluster.df <- data.frame(
  PC1 = pca.scores[, 1],
  PC2 = pca.scores[, 2],
  cluster = factor(cluster.hard),
  membership = max.membership
)

# plot results
ggplot(cluster.df, aes(x = PC1, y = PC2, color = cluster, alpha = membership)) +
  geom_point(size = 1.5) +
  scale_alpha(range = c(0.4, 1)) +
  labs(title = 'Fuzzy C-Means Clustering in PCA Space',
       color = 'Cluster', alpha = 'Max Membership') +
  theme_minimal()

# calculate centroids
centroids <- fcm.result$centers 

# distance calculation (Euclidean)
compute_euclidean <- function(mat, centers) {
  n <- nrow(mat)
  k <- nrow(centers)
  dist.mat <- matrix(NA, n, k)
  
  for (j in 1:k) {
    # For each cluster center, compute distances for all rows
    center <- centers[j, ]
    dist.mat[, j] <- sqrt(rowSums((mat - matrix(center, n, ncol(mat), byrow = TRUE))^2))
  }
  
  return(dist.mat)
}

# Run it
dist.mat.4 <- compute_euclidean(terrain.matrix, centroids)

# compute fuzzy memberships from distances
m <- 2  # fuzziness parameter

inv.dist <- 1 / (dist.mat.4 + 1e-10)  # prevent divide-by-zero
power <- 2 / (m - 1)

membership.full <- inv.dist^power
membership.full <- membership.full / rowSums(membership.full)

# assign hard clusters
hard.cluster.full <- apply(membership.full, 1, which.max)

#### map results back to raster
# add cluster assignment to original dataframe
creek.scaled.df$cluster <- hard.cluster.full

# Create raster from data frame
creek.fuzzy.4 <- rast(
  creek.scaled.df[, c('x', 'y', 'cluster')],
  type = 'xyz',
  crs = 'EPSG:32611'  
)

# Save to file

writeRaster(creek.fuzzy.4, filename = here('data', 'processed', 'processed', 'tif', 'creek_fuzzy_4.tif'), overwrite = TRUE)

plot(creek.fuzzy.4, col = terrain.colors(4), main = 'Topographic Clusters')
