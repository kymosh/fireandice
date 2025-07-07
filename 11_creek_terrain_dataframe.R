# Load packages
packages <- c( 'here', 'dplyr', 'stringr', 'terra', 'tibble', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Define base variable names and create full paths
var.names <- c('tpi_130', 'tpi_510', 'tpi_2010', 'slope', 'hli', 'dem')
creek.paths <- here('data', 'processed', 'processed', 'tif', paste0(var.names, '_creek_32611.tif'))

# Load rasters and assign names
creek.rasters <- setNames(lapply(creek.paths, rast), var.names)

# check CRS of all variables
# unique(sapply(creek.variables, function(p) crs(rast(p), describe = TRUE)$code))
# all 32611, g2g

###### check resolution of all variables
#unique(sapply(creek.variables, function(p) paste(res(rast(p)), collapse = 'x')))

# resample to match resolutions
# use highest resolution raster as reference raster
ref.rast <- creek.rasters[[which.min(sapply(creek.rasters, function(r) prod(res(r))))]]
# resample everything to match ref
creek.rasters.aligned <- lapply(creek.rasters, function(r) resample(r, ref.rast, method = 'bilinear'))

####### check extents for all variables
# names(creek.variables.resampled) <- var.names
# for (name in names(creek.variables.resampled)) {
#   r <- creek.variables.resampled[[name]]
#   cat(name, '\n')
#   print(ext(r))
#   cat('\n')
# }
# g2g

# stack rasters
creek.stack <- rast(creek.rasters.aligned)
names(creek.stack) <- var.names  # reassign names after stacking

# create df from stacked rasters
creek.df <- as.data.frame(creek.stack, xy = TRUE, na.rm = TRUE)
# save
saveRDS(creek.raw.df, here('data', 'processed', 'dataframes', 'creek_terrain_raw.rds'))
# check summary
#summary(creek.df)
.
######## data visualization
# ggplot(creek.df, aes(x = tpi_130)) +
#   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
#   theme_minimal() +
#   labs(title = 'Distribution of TPI (130)', x = 'x', y = 'Count')
# # normal distribution
# 
# ggplot(creek.df, aes(x = tpi_510)) +
#   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
#   theme_minimal() +
#   labs(title = 'Distribution of TPI (510)', x = 'x', y = 'Count')
# # normal distribution
# 
# ggplot(creek.df, aes(x = tpi_2010)) +
#   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
#   theme_minimal() +
#   labs(title = 'Distribution of TPI (2010)', x = 'x', y = 'Count')
# # normal distribution
# 
# ggplot(creek.df, aes(x = slope)) +
#   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
#   theme_minimal() +
#   labs(title = 'Distribution of Slope', x = 'x', y = 'Count')
# ## skewed right
# 
# ggplot(creek.df, aes(x = hli)) +
#   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
#   theme_minimal() +
#   labs(title = 'Distribution of HLI (Heat Load Index)', x = 'x', y = 'Count')
# # normal distribution
# 
# ggplot(creek.df, aes(x = elevation)) +
#   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
#   theme_minimal() +
#   labs(title = 'Distribution of Elevation', x = 'x', y = 'Count')
## bimodal-ish

# copy df before scaling
creek.scaled.df <- creek.df

# scale so all variables are 0-1
vars.to.scale <- setdiff(names(creek.df), c('x', 'y', 'hli'))
creek.scaled.df[vars.to.scale] <- lapply(creek.scaled.df[vars.to.scale], function(x) (x - min(x)) / (max(x) - min(x)))

saveRDS(creek.scaled.df, here('data', 'processed', 'dataframes', 'creek_terrain_scaled.rds'))


