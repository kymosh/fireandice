packages <- c( 'here', 'dplyr', 'raster', 'sf', 'terra', 'geodata',
               'tidyverse', 'e1071')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Read in full dataframe
swe.df <- readRDS(here('data', 'processed', 'dataframes', 'swe_dataframe_1524_2674.rds'))
swe.df <- swe.df[, !duplicated(colnames(swe.df))]


# select only topographic variables and scale them to z scores
terrain.scaled <- swe.df %>%
  dplyr::select(tpi510, slope, aspect, hli, elev) %>%
  scale()
# save
saveRDS(terrain.scaled, here('data', 'processed', 'dataframes', 'creek_terrain_matrix_scaled_1524_2674.rds'))






# create subset of matrix to run fuzzy c clustering on

set.seed(42)  # for reproducibility

# sample size 0.0005%
sample.size <- round(nrow(terrain.scaled) * 0.0005)
sample.rows <- sample(nrow(terrain.scaled), sample.size)
terrain.sample <- terrain.scaled[sample.rows, ]

# fuzzy c clustering on subset - 5 clusters
fcm.result <- cmeans(
  terrain.sample,
  centers = 5,         # should be between 4 and 8, we'll start with 6
  m = 2,               # fuzziness parameter
  iter.max = 100,      # max iterations
  verbose = TRUE,
  method = 'cmeans'    # default method
)

fcm.result$centers
head(fcm.result$membership)
head(fcm.result$cluster)

terrain.sample.df <- as.data.frame(terrain.sample)
terrain.sample.df$cluster <- as.factor(fcm.result$cluster)

ggplot(terrain.sample.df, aes(x = tpi510, y = elev, color = cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal()

# fuzzy c clustering on subset - 6 clusters
fcm.result <- cmeans(
  terrain.sample,
  centers = 6,         # should be between 4 and 8, we'll start with 6
  m = 2,               # fuzziness parameter
  iter.max = 100,      # max iterations
  verbose = TRUE,
  method = 'cmeans'    # default method
)

fcm.result$centers
head(fcm.result$membership)
head(fcm.result$cluster)

terrain.sample.df <- as.data.frame(terrain.sample)
terrain.sample.df$cluster <- as.factor(fcm.result$cluster)

ggplot(terrain.sample.df, aes(x = tpi510, y = elev, color = cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal()


# fuzzy c clustering on subset - 8 clusters
fcm.result <- cmeans(
  terrain.sample,
  centers = 8,         # should be between 4 and 8, we'll start with 6
  m = 2,               # fuzziness parameter
  iter.max = 100,      # max iterations
  verbose = TRUE,
  method = 'cmeans'    # default method
)

fcm.result$centers
head(fcm.result$membership)
head(fcm.result$cluster)

terrain.sample.df <- as.data.frame(terrain.sample)
terrain.sample.df$cluster <- as.factor(fcm.result$cluster)

ggplot(terrain.sample.df, aes(x = tpi510, y = elev, color = cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal()








### use PCA results to cluster
pca <- prcomp(terrain.scaled, center = FALSE, scale. = FALSE) 
pc.mat <- pca$x[, 1:3] # uce PC1-PC3

set.seed(14)
terrain.clust <- cmeans(pc.mat, centers = 6, m = 2, iter.max = 200, verbose = 2)

# create new df for plotting
pca.df <- as.data.frame(pc.mat) %>%
  mutate(cluster = as.factor(terrain.clust$cluster))

# plot of clusters plotted against PC1 and PC2
ggplot(pca.df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_color_brewer(palette = 'Dark2') +
  theme_minimal(base_size = 14) +
  labs(title = 'Fuzzy C-Means Clusters (PC1 vs PC2)',
       x = 'Principal Component 1',
       y = 'Principal Component 2')



# make new column with cluster assignment
cluster.assignments <- terrain.clust$cluster

# make new df with x and y from swe.df
clusters.df <- data.frame(x = swe.df$x,
                          y = swe.df$y, 
                          cluster = terrain.clust$cluster)

# conver to raster(just for plotting)
cluster.rast <- rast(clusters.df, type = 'xyz')
plot(cluster.rast)

# add cluster assignment column to swe.df
swe.df.clustered <- swe.df %>%
  left_join(clusters.df %>% dplyr::select(x, y, cluster), by = c('x', 'y'))


# quick analysis to compare number of burned and unburned pixels in each cluster
swe.df.clustered <- swe.df.clustered %>%
  mutate(burned_status = ifelse(cbibc.30m > 0, "Burned", "Unburned"))

ggplot(swe.df.clustered, aes(x = factor(cluster), fill = burned_status)) +
  geom_bar(position = "dodge") +   # "dodge" puts bars side by side
  scale_fill_manual(values = c("Unburned" = "grey70", "Burned" = "firebrick")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Topographic Cluster",
    y = "Number of pixels",
    fill = "Status",
    title = "Burned vs Unburned Pixels by Topographic Cluster"
  )
