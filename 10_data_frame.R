packages <- c('terra', 'dplyr', 'here')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


#### this is preliminary code to create dfs out of all my variables. as of 10/1 it works, but there is currently no forest metrics included. 

#### SDD ####

tif.dir <- here('data', 'processed', 'processed', 'tif')
sdd.files <- list.files(here(tif.dir, '500m'), pattern = '\\.tif$', full.names = T)
sdd.files
sdd.stack <- rast(sdd.files)

# simplify names 
clean.names <- sdd.files %>%
  basename() %>%
  gsub('^creek_', '', .) %>%
  gsub('(_(\\d+m|32611))?_1524', '', ., perl = TRUE) %>% 
  gsub('\\.tif$', '', .) %>%
  gsub('_', '.', .) %>%
  gsub('terraclimate.', '', .) %>%
  gsub('topo.', '', .) %>%
  gsub('^\\.', '', .) %>%
  gsub('dem', 'elevation', .)
  
clean.names

# extract layer names from stacked rasters (the terraclimate files and add to layer names)
raster.list <- lapply(sdd.files, rast)
layer.names <- mapply(function(r, n) {
  rnames <- names(r)
  if (length(rnames) == 1) {
    n  # single-layer raster just gets cleaned file name
  } else {
    paste0(n, ".", rnames)  # multi-layer raster: base name + original band names
  }
}, raster.list, clean.names, SIMPLIFY = TRUE) %>% unlist()

layer.names

sdd.stack <- rast(raster.list)
names(sdd.stack) <- layer.names

# convert to dataframe
sdd.df <- as.data.frame(sdd.stack, xy = T, na.rm = T)

head(sdd.df)
saveRDS(sdd.df, here('data', 'processed', 'dataframes', 'sdd_dataframe_1524.rds'))




#### SWE ####


# aso.files <- list.files(here(tif.dir, '50m'), pattern = '^ASO.*_og_extents\\.tif$', full.names = T)
# 
# #some extents and resolutions are not the same. Need to crop/resample
# ref <- rast(here(tif.dir, '50m', 'nasadem_creek_50m_1524.tif'))
# 
# for(f in aso.files) {
#   r <- rast(f)
#   r.50 <- resample(r, ref, method = 'near')
#   r.crop <- crop(r.50, ref) # crop all swe files to ref file
#   new.name <- basename(f)
#   new.name <- gsub('_og_extents', '', new.name)
#   out.path <- here(tif.dir, '50m', new.name)
#   writeRaster(r.crop, out.path, overwrite = T)
# }

swe.files <- list.files(here(tif.dir, '50m'), pattern = '\\.tif$', full.names = T)
swe.stack <- rast(swe.files)



# simplify names 
clean.names <- swe.files %>%
  basename() %>%
  gsub('^creek_', '', .) %>%
  gsub('(_(\\d+m|32611))?_1524', '', ., perl = TRUE) %>% 
  gsub('\\.tif$', '', .) %>%
  gsub('_', '.', .) %>%
  gsub('terraclimate.', '', .) %>%
  gsub('topo.', '', .) %>%
  gsub('^\\.', '', .) %>%
  gsub('^ASO.SanJoaquin.', '', .) %>%
  gsub('nasadem.creek', 'elev', .)


clean.names

# extract layer names from stacked rasters (the terraclimate files and add to layer names)
raster.list <- lapply(swe.files, rast)
layer.names <- mapply(function(r, n) {
  rnames <- names(r)
  if (length(rnames) == 1) {
    n  # single-layer raster just gets cleaned file name
  } else {
    paste0(n, ".", rnames)  # multi-layer raster: base name + original band names
  }
}, raster.list, clean.names, SIMPLIFY = TRUE) %>% unlist()

layer.names

swe.stack <- rast(raster.list)
names(swe.stack) <- layer.names

# convert to dataframe
swe.df <- as.data.frame(swe.stack, xy = T, na.rm = T)

head(swe.df)

saveRDS(swe.df, here('data', 'processed', 'dataframes', 'swe_df_1524.rds'))


