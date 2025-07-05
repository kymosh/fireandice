# Load packages
packages <- c( 'here', 'dplyr', 'stringr', 'terra', 'tibble', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

tif <- rast(here('data', 'raw', 'ASO', 'tif', 'ASO_SanJoaquin_Mosaic_2021May03_swe_50m.tif'))
res(tif)

#test change
