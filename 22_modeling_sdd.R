packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest', 'ranger', 'tictoc')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================

# get dataframe
# make sure if not on processing computer that the rds is updated!
dir <- 'data/processed/processed/rds/creek' 

df.500 <- readRDS(file.path(dir, 'creek_long_df_500m_clean.rds'))
