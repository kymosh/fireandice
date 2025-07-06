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
saveRDS(terrain.matrix, here::here('data', 'processed', 'dataframes', 'creek_terrain_matrix.rds'))



# create subset of matrix to run 

set.seed(42)  # for reproducibility

# Sample size 0.0005%
sample.size <- round(nrow(terrain.matrix) * 0.0005)
sample.size1 <- round(nrow(terrain.matrix) * 0.0005)

sample.rows <- sample(nrow(terrain.matrix), sample.size)
terrain.sample <- terrain.matrix[sample.rows, ]
