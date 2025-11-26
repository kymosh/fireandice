packages <- c('terra', 'dplyr', 'here')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


#### this is preliminary code to create dfs out of all my variables. as of 10/1 it works, but there is currently no forest metrics included. 

#### SDD ####

tif.dir <- here('data', 'processed', 'processed', 'tif')
sdd.files <- list.files(here(tif.dir, '500m'), pattern = '\\.tif$', full.names = T)
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

# fix layer names for multi-band rasters
layer.names <- mapply(function(r, n) {
  rnames <- names(r)
  if (length(rnames) == 1) n else paste0(n, ".", rnames)
}, raster.list <- lapply(sdd.files, rast), clean.names, SIMPLIFY = TRUE) %>% unlist()

layer.names <- gsub('^landcover\\.fractional\\.groups\\.', '', layer.names)
layer.names <- tolower(layer.names)
layer.names <- gsub('\\.', '_', layer.names)


# assign cleaned names
names(sdd.stack) <- layer.names

# convert to dataframe
sdd.df <- as.data.frame(sdd.stack, xy = TRUE, na.rm = TRUE)

saveRDS(sdd.df, here('data', 'processed', 'dataframes', 'sdd_df_1524.rds'))




#-------------------------- SWE ------------------------------------------------

tif.dir <- here('data', 'processed', 'processed', 'tif')

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
  gsub('^ASO.SanJoaquin.', '', .) %>%
  gsub('nasadem.creek', 'elev', .)

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

layer.names <- gsub('^landcover\\.fractional\\.groups\\.', '', layer.names)
layer.names <- tolower(layer.names)
layer.names <- gsub('\\.', '_', layer.names)

swe.stack <- rast(raster.list)
names(swe.stack) <- layer.names

# convert to dataframe
swe.df <- as.data.frame(swe.stack, xy = T, na.rm = T)

saveRDS(swe.df, here('data', 'processed', 'dataframes', 'swe_df_1524.rds'))



#-------------------------- prepare df for modeling ----------------------------



df.0 <- readRDS('data/processed/dataframes/swe_df_1524.rds')

# get rid of wy2018 and 2019
df.0 <- df.0 %>%
  select(-matches('^wy2018\\.|^wy2019\\.')) 


static.vars <- c('x','y','cbibc','aspect','hli','slope','tpi1200','tpi150','tpi2010','tpi510','elev', 'undesirable',                                'temperate_subpolar_needleleaf_forest', 'temperate_subpolar_broadleaf_deciduous_forest', 'mixed_forest',                             'temperate_subpolar_shrubland', 'temperate_subpolar_grassland', 'wetland')


# pivot swe to long and get 1 max swe value per year
df.peak.swe <- df.0 %>%
  select(-matches('^wy2018\\.|^wy2019\\.')) %>% # get rid of wy2018 and 2019
  # pivot swe columns to long
  pivot_longer(
    cols = matches('^20\\d{2}.*swe'),  # all SWE columns
    names_to = 'swe_col',
    values_to = 'swe'
  ) %>%
  mutate(year = str_extract(swe_col, '^\\d{4}')) %>% # extract year from swe column
  select(-swe_col) %>%
  group_by(x, y, year) %>%
  summarize(peak_swe = max(swe, na.rm = TRUE), .groups = 'drop')
  
# pivot climate variables to long
clim.long <- df.0 %>%
  pivot_longer(
    cols = matches('^wy\\d{4}_'), # all climate cols
    names_to = c('clim_year', 'variable'),
    names_pattern = 'wy(\\d{4})_(.*)',
    values_to = 'value'
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(year = clim_year) %>%
  select(-clim_year, x, y, year, pr, tmmx, tmmn)

keep.vars <- static.vars[!static.vars %in% colnames(df.peak.swe) & !static.vars %in% colnames(clim.long)]

# join peak swe and climate
df.long <- df.peak.swe %>%
  left_join(clim.long, by = c('x', 'y', 'year')) %>%
  left_join(df.0 %>% 
              select(all_of(keep.vars), x, y), by = c('x', 'y'))

saveRDS(df.long, 'data/processed/dataframes/swe_df_1524_long.rds')

  
# quick data exploration

ggplot(df.long, aes(x = peak_swe)) +
  geom_histogram(binwidth = 0.2, fill = 'skyblue', color = 'black') +
  facet_wrap(~ year) +
  scale_x_continuous(limits = c(0, 3)) +
  labs(title = 'Histogram of Peak SWE by Year', x = 'Peak SWE', y = 'Count') +
  theme_minimal()

zero_summary <- df.long %>%
  group_by(year) %>%
  summarise(
    n_zero = sum(peak_swe == 0, na.rm = TRUE),
    n_total = n(),
    prop_zero = n_zero / n_total
  )

ggplot(zero_summary, aes(x = year, y = n_zero)) +
  geom_col(fill = 'skyblue') +
  labs(
    x = 'Year',
    y = 'Number of pixels with peak SWE = 0',
    title = 'Zero Peak SWE Counts by Year'
  ) +
  theme_minimal()

ggplot(zero_summary, aes(x = year, y = prop_zero)) +
  geom_col(fill = 'salmon') +
  labs(
    x = 'Year',
    y = 'Proportion of pixels with peak SWE = 0',
    title = 'Proportion of Zero Peak SWE by Year'
  ) +
  theme_minimal()

always_zero <- df.long %>%
  group_by(x, y) %>%
  summarise(all_zero = all(peak_swe == 0, na.rm = TRUE)) %>%
  filter(all_zero)

nrow(always_zero)
