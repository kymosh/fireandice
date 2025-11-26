packages <- c( 'here', 'terra', 
             'tidyverse')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# McCune, B., & Keon, D. (2002). Equations for potential annual direct incident radiation and heat load. Journal of Vegetation Science, 13(4), 603–606.
# https://doi.org/10.1111/j.1654-1103.2002.tb02087.x


# load in DEMs
creek_dem <- rast(here('data', 'processed', 'processed', 'tif', 'dem_creek_32611.tif'))
castle_dem <- rast(here('data', 'processed', 'processed', 'tif', 'dem_castle_32611.tif'))

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

# Create elevation mask for >1524m
creek_mask <- creek_dem >= 1524
castle_mask <- castle_dem >= 1524

# Apply elevation mask to HLI rasters
creek_hli_5000 <- mask(creek_hli_rescaled, creek_mask, maskvalue = FALSE)
castle_hli_5000 <- mask(castle_hli_rescaled, castle_mask, maskvalue = FALSE)

# Save masked rasters
writeRaster(creek_hli_5000, filename = here('data', 'processed', 'processed', 'tif', 'hli_creek_32611_5000.tif'), overwrite = TRUE)
writeRaster(castle_hli_5000, filename = here('data', 'processed', 'processed', 'tif', 'hli_castle_32611_5000.tif'), overwrite = TRUE)

# reference swe tifs to resample to
creek.tif <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_SanJoaquin_2021_0331_swe_50m_clipped.tif'))
castle.kaweah.tif <- rast(here('data', 'processed', 'processed', 'tif', 'ASO_Kaweah_2024_0211_swe_50m_clipped.tif'))

# resample to swe tifs
creek_hli_5000_50.0001m <- resample(creek_hli_5000, creek.tif, method = 'bilinear')
castle_hli_5000_50m <- resample(castle_hli_5000, castle.kaweah.tif, method = 'bilinear')

writeRaster(creek_hli_5000_50.0001m, filename = here('data', 'processed', 'processed', 'tif', 'hli_creek_5000_50.0001m.tif'), overwrite = TRUE)
writeRaster(castle_hli_5000_50m, filename = here('data', 'processed', 'processed', 'tif', 'hli_castle_5000_50m.tif'), overwrite = TRUE)
