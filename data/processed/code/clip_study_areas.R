# set working directory (PC)
setwd("C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice")

# mac
setwd("/Users/kyliemosher/OneDrive/thesis/fireandice")

packages <- (c('exactextractr', 'raster', 'sf', 'terra', 'geodata', 'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr'))
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)



# study extents
## all watersheds that intersect the area of the fire
castle.extent <- st_read('data/processed/processed/study_extent_castle_32611.shp')
creek.extent <- st_read('data/processed/processed/study_extent_creek_32611.shp')

tif <- rast('data/raw/ASO/tif/ASO_SanJoaquin_2023Jan21-24_swe_50m.tif')

crs(tif, describe = T)
crs(castle.extent, describe = T)
