packages <- c('here', 'terra', 'tidymodels', 'dplyr', 'tidyr', 'stringr')
install.packages(setdiff(packages, row.names(installed.packages())))
lapply(packages, library, character.only = T)

# get dataf
df.0 <- readRDS('data/processed/dataframes/swe_df_1524.rds')

