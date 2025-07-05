packages <- c( 'here', 'raster', 'sf', 'terra', 'geodata',
               'tidyverse', 'e1071')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

creek.scaled.df <- readRDS(here::here('data', 'processed', 'dataframes', 'creek_terrain_scaled.rds'))

# rename dem to elev (elevation)
names(creek.scaled.df)[names(creek.scaled.df) == 'dem'] <- 'elev'

# create matrix of just terrain variables, excluding x and y
terrain.matrix <- creek.scaled.df %>%
  dplyr::select(tpi_130, tpi_510, tpi_2010, slope, hli, elev) %>%
  as.matrix()

# run fuzzy c cluster, starting with n = 6 clusters
fcm.result <- cmeans(terrain.matrix, centers = 6, m = 2, iter.max = 100, verbose = TRUE, method = 'cmeans')

