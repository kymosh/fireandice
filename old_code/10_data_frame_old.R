# Load packages
packages <- c( 'here', 'dplyr', 'stringr', 'terra', 'tibble', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# ASO
# Directory with input TIFs
tif.dir <- here('data', 'processed', 'processed', 'tif')

# List files with matching keywords
tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = TRUE)

# Filter files by basin
creek.tifs   <- tif.files[str_detect(tif.files, 'SanJoaquin.*clipped')]

file_info <- tibble(fname = basename(creek.tifs)) %>%
  mutate(
    fire       = 'creek',
    year       = str_extract(fname, '\\d{4}'),
    month      = str_extract(fname, '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)'),
    day        = str_extract(fname, '(?<=\\d{4}[A-Za-z]{3})\\d{1,2}'),  # 1- or 2-digit day
    date       = as.Date(str_c(year, month, str_pad(day, 2, pad = '0'), sep = '-'), format = '%Y-%b-%d'),
    variable   = str_extract(fname, '(?<=_)swe|depth|melt(?=_)'),  # tweak if needed
    resolution = str_extract(fname, '\\d{2,}m')
  )

# remove report tifs/superswe tifs
file_info <- file_info[-c(1, 2, 18), ]



### NOTE code below was replicated and refined in another file, creek_terrain_dataframe.r

##################### Creek Fire terrain variable stack
# # create list of all creek variable files
# # variable base names
# var.names <- c('tpi_130', 'tpi_510', 'tpi_2010', 'slope', 'hli', 'dem')
# # build full paths
# creek.variables <- here('data', 'processed', 'processed', 'tif', paste0(var.names, '_creek_32611.tif'))
# 
# 
# # check CRS of all variables
# # unique(sapply(creek.variables, function(p) crs(rast(p), describe = TRUE)$code))
# # all 32611, g2g
# 

# ###### check resolution of all variables
# #unique(sapply(creek.variables, function(p) paste(res(rast(p)), collapse = 'x')))
# 
# # choose smallest res (10m) raster as reference raster
# ref <- rast(creek.variables[which.min(sapply(creek.variables, function(p) prod(res(rast(p)))))])
# # resample rasters to match
# creek.variables.resampled <- lapply(creek.variables, function(p) {
#   r <- rast(p)
#   resample(r, ref, method = 'bilinear') 
# })
# 
# # unique(sapply(creek.variables.resampled, function(p) paste(res(rast(p)), collapse = 'x')))
# # # all 10m, g2g
# # 
# # 
# # ###### check extents for all variables
# names(creek.variables.resampled) <- var.names
# for (name in names(creek.variables.resampled)) {
#   r <- creek.variables.resampled[[name]]
#   cat(name, '\n')
#   print(ext(r))
#   cat('\n')
# }
# # # g2g
# 
# creek.stack <- rast(creek.variables.resampled)
# creek.terrain.df <- as.data.frame(creek.stack, xy = TRUE, na.rm = TRUE)
# 
# # check summary
# #summary(creek.terrain.df)
# .
# ######## data visualization
# # ggplot(creek.terrain.df, aes(x = tpi_130)) +
# #   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
# #   theme_minimal() +
# #   labs(title = 'Distribution of TPI (130)', x = 'x', y = 'Count')
# # # normal distribution
# # 
# # ggplot(creek.terrain.df, aes(x = tpi_510)) +
# #   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
# #   theme_minimal() +
# #   labs(title = 'Distribution of TPI (510)', x = 'x', y = 'Count')
# # # normal distribution
# # 
# # ggplot(creek.terrain.df, aes(x = tpi_2010)) +
# #   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
# #   theme_minimal() +
# #   labs(title = 'Distribution of TPI (2010)', x = 'x', y = 'Count')
# # # normal distribution
# # 
# # ggplot(creek.terrain.df, aes(x = slope)) +
# #   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
# #   theme_minimal() +
# #   labs(title = 'Distribution of Slope', x = 'x', y = 'Count')
# # ## skewed right
# # 
# # ggplot(creek.terrain.df, aes(x = hli)) +
# #   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
# #   theme_minimal() +
# #   labs(title = 'Distribution of HLI (Heat Load Index)', x = 'x', y = 'Count')
# # # normal distribution
# # 
# # ggplot(creek.terrain.df, aes(x = elevation)) +
# #   geom_histogram(bins = 50, fill = 'skyblue', color = 'black') +
# #   theme_minimal() +
# #   labs(title = 'Distribution of Elevation', x = 'x', y = 'Count')
# ## bimodal-ish
# 
# 
# 
# 
# 
# 
# ######### re-scale variables to 0-1
# # copy for safety
# creek.terrain.scaled.df <- creek.terrain.df
# 
# # Get variable names to rescale (excluding x, y, and hli)- hli is already rescaled
# vars.to.scale <- setdiff(names(creek.terrain.scaled.df), c('x', 'y', 'hli'))
# 
# # re-scale selected variables
# creek.terrain.scaled.df[vars.to.scale] <- lapply(creek.terrain.scaled.df[vars.to.scale], function(x) {
#   (x - min(x)) / (max(x) - min(x))
# })
# 
# # check for NAs
# #sum(is.na(creek.terrain.scaled.df))
# 
# # summary stats
# #summary(creek.terrain.scaled.df)
# 
# ######### save dataframe
# 
# # save as CSV
# write.csv(creek.terrain.scaled.df, file = here::here('data', 'processed',  'dataframes', 'creek_terrain_scaled.csv'), row.names = FALSE)
# # save as RDS
# saveRDS(creek.terrain.scaled.df, file = here::here('data', 'processed', 'dataframes', 'creek_terrain_scaled.rds'))
