packages <- c( 'here', 'dplyr', 'raster', 'sf', 'terra', 'geodata',
               'tidyverse', 'e1071')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Read in full dataframe
swe.df <- readRDS(here('data', 'processed', 'dataframes', 'swe_dataframe_1524_2674.rds'))
swe.df <- swe.df[, !duplicated(colnames(swe.df))]


# select only topographic variables and scale them to z scores
terrain.df <- swe.df %>% 
  dplyr::select(x, y, tpi510, slope, aspect, hli, elev)

terrain.scaled <- swe.df %>%
  dplyr::select(tpi510, slope, aspect, hli, elev) %>%
  scale()
# save
#saveRDS(terrain.scaled, here('data', 'processed', 'dataframes', 'creek_terrain_matrix_scaled_1524_2674.rds'))
terrain.scaled <- readRDS(here('data', 'processed', 'dataframes', 'creek_terrain_matrix_scaled_1524_2674.rds'))





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








######### use PCA results to cluster (all variables)
pca <- prcomp(terrain.scaled, center = FALSE, scale. = FALSE) 
pc.mat <- pca$x[, 1:3] # use PC1-PC3
plot(pc.mat[,1], pc.mat[,2], col = terrain.df.clustered$cluster_6, pch=19)
summary(pca)$importance[2,]


# ----------------------------------------
# 1. Compute WSS for k = 1:max_k (subsample only)
# ----------------------------------------
compute_wss <- function(df_pca, max_k = 9, nstart = 10) {
  sapply(1:max_k, function(k) kmeans(df_pca, centers = k, nstart = nstart, iter.max = 1000)$tot.withinss)
}

wss <- compute_wss(pc.mat, max_k = 9, nstart = 10)
  
# ----------------------------------------
# 2. Find the elbow point numerically
# ----------------------------------------
find_elbow <- function(wss) {
  k <- 1:length(wss)
  line_start <- c(k[1], wss[1])
  line_end <- c(k[length(k)], wss[length(k)])
  distances <- abs((line_end[2]-line_start[2])*k - (line_end[1]-line_start[1])*wss + 
                     line_end[1]*line_start[2] - line_end[2]*line_start[1]) /
    sqrt((line_end[2]-line_start[2])^2 + (line_end[1]-line_start[1])^2)
  which.max(distances)
}

best.k <- find_elbow(wss)
best.k




set.seed(14) # for reproducibility 
##### run through loop for clustering 

# use 6:16 clusters
k.values <- 2:8

# make empty lists
clustering.list <- list()
cluster.assignments.list <- list()


for (k in k.values) {
  
  # run fuzzy means clustering
  clust <- cmeans(pc.mat, centers = k, m = 2, iter.max = 200, verbose = FALSE)
  
  # store results in list
  clustering.list[[as.character(k)]] <- clust
  cluster.assignments.list[[as.character(k)]] <- clust$cluster
  
}

# bind all cluster assignments into one data frame
cluster.assignments.df <- as.data.frame(cluster.assignments.list)

# name columns according to cluster number
names(cluster.assignments.df) <- paste0('cluster_', k.values)

# combine with your x and y from swe.df
clustered.xy <- cbind(swe.df[, c('x', 'y')], cluster.assignments.df)

# join back to terrain.df
terrain.df.clustered <- terrain.df %>%
  left_join(clustered.xy, by = c('x', 'y'))


###### analyze results to see what number of clusters is best

# compute Partition Coefficient (PC) and Partition Entropy (PE)
pc.values <- sapply(clustering.list, function(clust) {
  U <- clust$membership
  sum(U^2) / nrow(U)
})

pe.values <- sapply(clustering.list, function(clust) {
  U <- clust$membership
  -sum(U * log(U)) / nrow(U)
})

# plot metrics
metrics.df <- data.frame(
  k = k.values,
  PC = pc.values,
  PE = pe.values
)

ggplot(metrics.df) +
  geom_line(aes(x=k, y=PC), color='blue', size=1.2) +
  geom_point(aes(x=k, y=PC), color='blue', size=2) +
  geom_line(aes(x=k, y=PE), color='red', size=1.2) +
  geom_point(aes(x=k, y=PE), color='red', size=2) +
  labs(x='Number of clusters (k)',
       y='Metric Value',
       title='Fuzzy C-Means Clustering Quality',
       subtitle='Blue = Partition Coefficient (higher better), Red = Partition Entropy (lower better)') +
  theme_minimal()

selected.k <- 2:8

plot.df <- terrain.df.clustered %>%
  dplyr::select(x, y, paste0("cluster_", selected.k)) %>%
  pivot_longer(cols = starts_with("cluster_"),
               names_to = "k_cluster",
               values_to = "cluster") %>%
  mutate(k = as.integer(sub("cluster_", "", k_cluster)))

ggplot(plot.df, aes(x = x, y = y, fill = factor(cluster))) +
  geom_raster() +
  scale_fill_viridis_d(option = "plasma") +
  facet_wrap(~ k, ncol = 3) +
  labs(fill = "Cluster",
       x = "X coordinate",
       y = "Y coordinate",
       title = "Spatial distribution of clusters",
       subtitle = "Selected k-values: 6, 8, 9, 10, 16") +
  theme_minimal()

x.min <- 295000
x.max <- 300000
y.min <- 4150000
y.max <- 4155000

plot.df <- terrain.df.clustered %>%
  filter(x >= x.min, x <= x.max, y >= y.min, y <= y.max) %>%
  dplyr::select(x, y, paste0("cluster_", selected.k)) %>%
  pivot_longer(cols = starts_with("cluster_"),
               names_to = "k_cluster",
               values_to = "cluster") %>%
  mutate(k = as.integer(sub("cluster_", "", k_cluster)))

ggplot(plot.df, aes(x = x, y = y, color = factor(cluster))) +
  geom_point(size = 1, alpha = 0.7) +
  facet_wrap(~ k, ncol = 3) +
  scale_color_viridis_d(option = "plasma") +
  labs(color = "Cluster",
       x = "X coordinate",
       y = "Y coordinate",
       title = "Zoomed-in spatial distribution of clusters",
       subtitle = "Selected k-values: 6, 8, 9, 10, 16") +
  theme_minimal()

pc.df <- as.data.frame(pc.mat) %>%
  dplyr::select(PC1, PC2) %>%
  bind_cols(terrain.df.clustered %>% dplyr::select(paste0("cluster_", selected.k))) %>%
  pivot_longer(cols = starts_with("cluster_"),
               names_to = "k_cluster",
               values_to = "cluster") %>%
  mutate(k = as.integer(sub("cluster_", "", k_cluster)))

ggplot(pc.df, aes(x = PC1, y = PC2, color = factor(cluster))) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~ k, ncol = 3) +
  scale_color_viridis_d(option = "plasma") +
  labs(color = "Cluster",
       x = "PC1",
       y = "PC2",
       title = "Clusters in PCA space",
       subtitle = "Selected k-values: 6, 8, 9, 10, 16") +
  theme_minimal()








# quick analysis to compare number of burned and unburned pixels in each cluster
swe.df.clustered <- swe.df.clustered %>%
  mutate(burned_status = ifelse(cbibc.30m > 0, "Burned", "Unburned"))

saveRDS(swe.df.clustered, here('data', 'processed', 'processed', 'rds', 'swe_df_clustered_1524_2674.rds'))

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

# exploration of how clusters vary by topographic variable
# Select the topo variables + cluster
topo.vars <- swe.df.clustered %>%
  dplyr::select(cluster, elev, slope, hli, tpi510, aspect)

# Pivot to long format for easier plotting
topo.long <- topo.vars %>%
  pivot_longer(cols = -cluster, names_to = 'variable', values_to = 'value')

# Boxplot
ggplot(topo.long, aes(x = factor(cluster), y = value, fill = factor(cluster))) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = 'free_y') +
  labs(
    x = 'Cluster',
    y = 'Value',
    fill = 'cluster',
    title = 'Topographic Attributes by Cluster'
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = 'none')

topo.vars$northness <- cos(topo.vars$aspect) # convert radians to measure of northness
topo.vars$southness <- sin(topo.vars$aspect) # convert radians to measure of northness

cluster.profile <- topo.vars %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))


vars <- names(topo.vars)[!names(topo.vars) %in% 'cluster']

for (v in vars) {
  print(
    ggplot(topo.vars, aes(x = factor(cluster), y = .data[[v]], fill = factor(cluster))) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      labs(x = 'Cluster', y = v, fill = 'Cluster') +
      theme_minimal()
  )
}

ggplot(topo.vars, aes(x = elev, y = hli, color = factor(cluster))) +
  geom_point(alpha = 0.4) +
  labs(x = 'Elevation (m)', y = 'Heat Load Index (HLI)', color = 'Cluster') +
  theme_minimal()


