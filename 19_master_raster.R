packages <- c('dplyr', 'tidyr', 'tools', 'terra')
lapply(packages, library, character.only = T)

# ===========================================================================================
# Create master raster
# ===========================================================================================


# read in all raster stacks and combine into single one for modeling

# ----- 500m (452m) master-raster -----
# this is just a test so far to see if everthing matches and is able to be stacked. 
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek'
files <- list.files(dir, 'creek', full.names = TRUE)
files <- files[!grepl('master', files)]
files
sdd.stack <- rast(files)
# it worked!

names(sdd.stack)

writeRaster(sdd.stack, file.path(dir, 'creek_master_500m.tif'), overwrite = T)

# ----- 50m master-raster -----
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
files <- list.files(dir, 'creek', full.names = TRUE)

swe.stack <- rast(files)
# extents don't match, unable to be stacked :(

# check extents
for (f in files) {
  r <- rast(f)
  print(ext(r))
}

files[1] # has different extent than others

# --- crop to limiting extent -----
rasters <- lapply(files, rast)
ref <- rast(files[1]) # chose the raster that has the smallest extent

# new output directory (temporary, but necessary because we can't just overwrite the same files)
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/cropped'
dir.create(dir, recursive = T, showWarnings = F)

# crop each raster to the new extent and write out 
out.files <- vapply(files, function(f) {
  r <- rast(f)
  r.crop <- crop(r, ref)
  
  out <- file.path(dir, basename(f))
  writeRaster(r.crop, out, overwrite = TRUE)
  out
}, character(1))

# try stacking again
files <- list.files(dir, pattern = '\\.tif$', full.names = T)
swe.stack <- rast(files) # now it works!
plot(swe.stack)

# save swe.stack
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
writeRaster(swe.stack, file.path(dir, 'creek_master_50m.tif'))

# ----- clean up -----
# go back to OG files
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
files <- list.files(dir, 'creek', full.names = TRUE)

old.dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek/old_versions'

moved.files <- vapply(files, function(f) {
  base <- file_path_sans_ext(basename(f))
  new.name <- paste0(base, '_before_crop.tif')
  new.path <- file.path(old.dir, new.name)
  
  file.rename(f, new.path)
  new.path
}, character(1))





# ===========================================================================================
# convert to df
# ===========================================================================================
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif'

r.50 <- rast(file.path(dir, '50m/creek/creek_master_50m.tif'))
r.500 <- rast(file.path(dir, '500m/creek/creek_master_500m.tif'))

# convert to DF
df.50 <- as.data.frame(r.50, cells = T)
df.500 <- as.data.frame(r.500, cells = T)

# keep only cells that have no NAs
df.50 <- df.50[complete.cases(df.50), ] 
df.500 <- df.500[complete.cases(df.500), ] 

# ----- 50m raster -----

# pivot long for swe
df.long.0 <- df.50 %>%
  pivot_longer(
    cols = starts_with('swe_peak_wy'),
    names_to = 'wy',
    values_to = 'swe_peak' 
  )

# clean wy column
df.long.0$wy <- as.numeric(gsub('swe_peak_wy', '', df.long.0$wy))


# pivot long for climate variables
clim.long <- df.50 %>%
  select(cell, starts_with('clim')) %>%
  pivot_longer(
    cols = -cell,
    names_to = c('.value', 'wy'),
    names_pattern = 'clim_(.*)_wy(\\d+)'
  )

clim.long$wy <- as.numeric(clim.long$wy)
clim.long <- clim.long[clim.long$wy >= 2020, ]

df.long.0 <- left_join(df.long.0, clim.long, by = c('cell', 'wy'))

range(df.long$wy, na.rm = TRUE)

df.long <- df.long.0 %>%
  select(-starts_with('clim')) %>%
  select(-starts_with('swe_20'))

saveRDS(df.long, 'J:/Fire_Snow/fireandice/data/processed/processed/rds/creek_df_50m.rds')
  

# ----- 500m raster -----

# pivot long for swe
names(df.500)

df.long.0 <- df.500 %>%
  pivot_longer(
    cols = starts_with('swe_peak_wy'),
    names_to = 'wy',
    values_to = 'swe_peak' 
  )

# clean wy column
df.long.0$wy <- as.numeric(gsub('swe_peak_wy|_500m', '', df.long.0$wy))


# pivot long for climate variables
clim.long <- df.500 %>%
  select(cell, starts_with('clim')) %>%
  pivot_longer(
    cols = -cell,
    names_to = c('.value', 'wy'),
    names_pattern = 'clim_(.*)_wy(\\d+)'
  )

clim.long$wy <- as.numeric(clim.long$wy)
clim.long <- clim.long[clim.long$wy >= 2020, ]

# pivot long for sdd
sdd.long <- df.500 %>%
  select(cell, matches('^sdd_wy\\d{4}$')) %>%
  pivot_longer(
    cols = -cell,
    names_to = 'wy',
    values_to = 'sdd'
  )

sdd.long$wy <- as.numeric(gsub('sdd_wy', '', sdd.long$wy))

# join back in to df.long
df.long.0 <- left_join(df.long.0, sdd.long, by = c('cell', 'wy'))

df.long.0 <- left_join(df.long.0, clim.long, by = c('cell', 'wy'))

df.long <- df.long.0 %>%
  select(-starts_with('clim')) %>% # remove duplicate clim cols
  select(-starts_with('swe_20')) %>% # remove duplicate swe cols
  select(-starts_with('sdd_')) %>% # remove duplicate sdd cols
  rename(clim_swe = swe) %>% # rename climate swe column to avoid confusion
  mutate(cbibc = ifelse(wy == 2020 & !is.na(cbibc), 0, cbibc)) # change cbibc in 2020 to 0 because wy2020 was before the creek fire


saveRDS(df.long, 'J:/Fire_Snow/fireandice/data/processed/processed/rds/creek_df_500m.rds')
names(df.long)
# ----- exploration -----

# -- visualize peak swe by year ----
all_swe <- c(df.50$swe_peak_wy2021,
             df.50$swe_peak_wy2022,
             df.50$swe_peak_wy2023,
             df.50$swe_peak_wy2024)

xlim <- range(all_swe, na.rm = TRUE)
breaks <- seq(xlim[1], xlim[2], length.out = 51)  # 50 bins
par(mfrow = c(2, 2))  # 2x2 layout

hist(df.50$swe_peak_wy2021,
     breaks = breaks,
     xlim = c(0, 4),
     main = 'WY2021',
     col = 'lightblue')

hist(df.50$swe_peak_wy2022,
     breaks = breaks,
     xlim = c(0, 4),
     main = 'WY2022',
     col = 'lightblue')

hist(df.50$swe_peak_wy2023,
     breaks = breaks,
     xlim = c(0, 4),
     main = 'WY2023',
     col = 'lightblue')

hist(df.50$swe_peak_wy2024,
     breaks = breaks,
     xlim = c(0, 4),
     main = 'WY2024',
     col = 'lightblue')

hist(log1p(df.50$swe_peak_wy2023))










## troubleshooting

x <- rast(files[2])
y <- rast(files[3])
names(x)
names(y)

for (f in files) {
  r <- rast(f)
  cat('\nFILE:', basename(f), '\n')
  print(names(r))
}

names(x) <- 'cbibc'
names(y) <- 'landcover'

writeRaster(x, file.path(dir, 'creek_cbibc_500m_2.tif'))
writeRaster(y, file.path(dir, 'creek_landcover_500m_2.tif'))
