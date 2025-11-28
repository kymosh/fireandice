packages <- c('terra', 'sf', 'mapview', 'lidR', 'aRchi', 'TreeLS', 'dplyr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


mean.csm <- rast(here('data', 'raw', 'ALS', 'tif', 'CreekFire_2021_MeanCSM_Meters.tif'))
rm(meancsm)

summary(mean.csm)

#test

# --- 0) Read catalog & set options ---
ctg <- readLAScatalog('data/raw/ALS/laz/random_tiles')   # Load a LAScatalog from the folder of tiles
set_lidr_threads(8)         # Use 8 threads for lidR operations (parallel where supported)
opt_laz_compression(ctg) <- TRUE      # Write LAZ (compressed) outputs instead of LAS
opt_progress(ctg)        <- TRUE      # Show a progress bar for catalog operations

plot(ctg, mapview = TRUE, map.types = "Esri.WorldImagery")  # Interactive map of catalog tiles with Esri imagery basemap

tile.dtm = rasterize_terrain(ctg, res = 3.28, algorithm = tin())
