packages <- c('terra', 'sf', 'mapview', 'lidR', 'aRchi', 'TreeLS', 'dplyr', 'ForestGapR', 'raster', 'future')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# read in initial data
canopy.metrics.50m <- readRDS('data/processed/processed/rds/canopy_metrics_50m_test.rds')
swe.df <- readRDS('data/processed/dataframes/filtered_swe_df.rds')

# convert canopy to df
canopy.df <- as.data.frame(canopy.metrics.50m, xy = T, na.rm = T)

swe.trim <- semi_join(swe.df, canopy.df, by = c('x', 'y'))
