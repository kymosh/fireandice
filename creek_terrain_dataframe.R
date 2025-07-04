# Load packages
packages <- c( 'here', 'dplyr', 'stringr', 'terra', 'tibble', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Define base variable names and create full paths
var.names <- c('tpi_130', 'tpi_510', 'tpi_2010', 'slope', 'hli', 'dem')
creek.paths <- here('data', 'processed', 'processed', 'tif', paste0(var.names, '_creek_32611.tif'))

# Load rasters and assign names
creek.rasters <- setNames(lapply(creek.paths, rast), var.names)


###### Resample to match resolutions
# use highest resolution raster as reference raster
ref.rast <- creek.rasters[[which.min(sapply(creek.rasters, function(r) prod(res(r))))]]
# resample everything to match ref
creek.rasters.aligned <- lapply(creek.rasters, function(r) resample(r, ref.rast, method = 'bilinear'))

# Stack
creek.stack <- rast(creek.rasters.aligned)
names(creek.stack) <- var.names  # reassign names after stacking

# create df from stacked rasters
creek.df <- as.data.frame(creek.stack, xy = TRUE, na.rm = TRUE)

# copy df before scaling
creek.scaled.df <- creek.df

# scale so all variables are 0-1
vars.to.scale <- setdiff(names(creek.df), c('x', 'y', 'hli'))
creek.scaled.df[vars.to.scale] <- lapply(creek.scaled.df[vars.to.scale], function(x) (x - min(x)) / (max(x) - min(x)))

saveRDS(creek.scaled.df, here('data', 'processed', 'dataframes', 'creek_terrain_scaled.rds'))
