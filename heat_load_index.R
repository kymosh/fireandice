packages <- c( 'here', 'terra', 
             'tidyverse')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# McCune, B., & Keon, D. (2002). Equations for potential annual direct incident radiation and heat load. Journal of Vegetation Science, 13(4), 603–606.
# https://doi.org/10.1111/j.1654-1103.2002.tb02087.x


# load in DEMs
creek_dem <- rast(here('data', 'raw', 'background_variables', 'tif', 'DEM_creek.tif'))
castle_dem <- rast(here('data', 'raw', 'background_variables', 'tif', 'DEM_castle.tif'))

# calculate slope and aspect
creek_slope <- terrain(creek_dem, v = 'slope', unit = 'radians')
castle_slope <- terrain(castle_dem, v = 'slope', unit = 'radians')
creek_aspect <- terrain(creek_dem, v = 'aspect', unit = 'radians')
castle_aspect <- terrain(castle_dem, v = 'aspect', unit = 'radians')

# calculate HLI (heat load index) (McCune & Keon 2002 formulation)
creek_adjusted_aspect <- abs(creek_aspect - (pi / 4))
castle_adjusted_aspect <- abs(castle_aspect - (pi / 4))

creek_hli <- 0.339 + 0.808 * cos(creek_adjusted_aspect) * sin(creek_slope) - 
  0.196 * sin(creek_adjusted_aspect) * sin(creek_slope) - 
  0.482 * cos(creek_slope)

castle_hli <- 0.339 + 0.808 * cos(castle_adjusted_aspect) * sin(castle_slope) - 
  0.196 * sin(castle_adjusted_aspect) * sin(castle_slope) - 
  0.482 * cos(castle_slope)

# rescale to 0-1
creek_hli_min <- minmax(creek_hli)[1]
creek_hli_max <- minmax(creek_hli)[2]
creek_hli_rescaled <- (creek_hli - creek_hli_min) / (creek_hli_max - creek_hli_min)

castle_hli_min <- minmax(castle_hli)[1]
castle_hli_max <- minmax(castle_hli)[2]
castle_hli_rescaled <- (castle_hli - castle_hli_min) / (castle_hli_max - castle_hli_min)

plot(castle_hli_rescaled)

# save 
writeRaster(creek_hli_rescaled, filename = here('data', 'processed', 'processed', 'tif', 'hli_creek.tif'), overwrite = TRUE)
writeRaster(castle_hli_rescaled, filename = here('data', 'processed', 'processed', 'tif', 'hli_castle.tif'), overwrite = TRUE)
