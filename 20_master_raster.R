packages <- c('dplyr', 'tidyr', 'tools', 'terra')
lapply(packages, library, character.only = T)

# ===========================================================================================
# Create master raster
# ===========================================================================================


# read in all raster stacks and combine into single one for modeling

# ----- 500m (452m) master-raster -----
dir <- 'data/processed/processed/tif/500m/creek'
j <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek'
g <- 'G:/Fire_Snow_Dynamics_backup/data/processed/processed/tif/500m/creek'


files <- list.files(dir, 'creek', full.names = TRUE)
files <- files[!grepl('master', files)]
files
sdd.stack <- rast(files)
names(sdd.stack)

writeRaster(sdd.stack, file.path(dir, 'creek_master_500m.tif'), overwrite = T)
writeRaster(sdd.stack, file.path(j, 'creek_master_500m.tif'), overwrite = T)
writeRaster(sdd.stack, file.path(g, 'creek_master_500m.tif'), overwrite = T)

# ----- 50m master-raster -----

dir <- 'data/processed/processed/tif/50m/creek'
j <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
g <- 'G:/Fire_Snow_Dynamics_backup/data/processed/processed/tif/50m/creek'

#dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif/50m/creek'
files <- list.files(dir, 'creek', full.names = TRUE)
files <- files[!grepl('master', files)]
files
swe.stack <- rast(files)
names(swe.stack)

writeRaster(swe.stack, file.path(dir, 'creek_master_50m.tif'), overwrite = T)
writeRaster(swe.stack, file.path(j, 'creek_master_50m.tif'), overwrite = T)
writeRaster(swe.stack, file.path(g, 'creek_master_50m.tif'), overwrite = T)


# ----- if extents don't match... -----

# check extents
for (f in files) {
  r <- rast(f)
  print(ext(r))
}

# crop to limiting extent 
#  for multiple files 
rasters <- lapply(files, rast)
ref <- rast(files[1]) # chose the raster that has the smallest extent

# new output directory (temporary, but necessary because we can't just overwrite the same files)
dir <- 'data/processed/processed/tif/50m/creek/cropped'
dir.create(dir, recursive = T, showWarnings = F)

# crop each raster to the new extent and write out 
out.files <- vapply(files, function(f) {
  r <- rast(f)
  r.crop <- crop(r, ref)
  
  out <- file.path(dir, basename(f))
  writeRaster(r.crop, out, overwrite = TRUE)
  out
}, character(1))

# if just needing to crop 1 file 
ref <- rast(files[1]) # chose the raster that has the smallest extent
old <- rast(files[8]) # chose the raster that needs to be cropped
new <- crop(old, ref) # crop

# rename old file and move to old_versions folder so new file can be written
old.file <- file.path(dir, 'creek_topo_50m.tif')
new.file <- file.path(dir, 'old_versions/creek_topo_50m_before_crop_again.tif')
file.rename(old.file, new.file)

# write new raster
writeRaster(new, file.path(dir, 'creek_topo_50m.tif'))

# move old master file to old_version
old.file <- file.path(dir, 'creek_master_50m.tif')
new.file <- file.path(dir, 'old_versions/creek_master_50m_before_fixinglandcover_before_fixingsnowice_andNAs.tif')
file.rename(old.file, new.file)

# try stacking again
files <- list.files(dir, pattern = '\\.tif$', full.names = T)
files
swe.stack <- rast(files) # now it works!

# save swe.stack again
writeRaster(swe.stack, file.path(dir, 'creek_master_50m.tif'), overwrite = T)



# clean up 
# some of this may not be necessary since doing the file.rename thing. 
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
# convert to df, filter, and pivot long
# ===========================================================================================

# ===========================================================================================
# setup
# ===========================================================================================

forest.cols <- c(
  'Undesirable',
  'Temperate_subpolar_needleleaf_forest',
  'Temperate_subpolar_broadleaf_deciduous_forest',
  'Mixed_forest',
  'Temperate_subpolar_shrubland',
  'Temperate_subpolar_grassland',
  'Wetland'
)

remove.cols <- c(
  'Undesirable',
  'Temperate_subpolar_needleleaf_forest',
  'Temperate_subpolar_broadleaf_deciduous_forest',
  'Mixed_forest',
  'Temperate_subpolar_shrubland',
  'Temperate_subpolar_grassland',
  'Wetland',
  'forest_type',
  'forest_dom_frac'
)

keep.canopy <- c(
  'fd_fractal_dim',
  'gap_gap_pct',
  'cover_ground_frac',
  'gap_dist_to_canopy_mean',
  'ht_zskew',
  'ht_zkurt',
  'ht_zentropy',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zpcum6',
  'ht_zpcum9'
)



# ===========================================================================================
# helper functions
# ===========================================================================================

filter_landcover <- function(df, forest.cols, lc.sum.min = 0.25, undesirable.max = 0.30) {
  
  df %>%
    mutate(
      lc.all.na = rowSums(is.na(across(all_of(forest.cols)))) == length(forest.cols),
      lc.sum = rowSums(across(all_of(forest.cols)), na.rm = TRUE)
    ) %>%
    filter(
      !lc.all.na,
      lc.sum > lc.sum.min,
      Undesirable <= undesirable.max
    ) %>%
    mutate(
      forest_type = forest.cols[max.col(across(all_of(forest.cols)), ties.method = 'first')],
      forest_type = as.factor(forest_type),
      forest_dom_frac = do.call(pmax, c(across(all_of(forest.cols)), na.rm = TRUE))
    ) %>%
    select(-lc.all.na, -lc.sum)
}

pivot_climate_long <- function(df) {
  
  clim.long <- df %>%
    select(cell, starts_with('clim_')) %>%
    pivot_longer(
      cols = -cell,
      names_to = c('.value', 'wy'),
      names_pattern = 'clim_(.*)_wy(\\d+)'
    )
  
  clim.long$wy <- as.numeric(clim.long$wy)
  clim.long <- clim.long[clim.long$wy >= 2020, ]
  names(clim.long)[names(clim.long) == 'swe'] <- 'clim_swe'
  
  clim.long
}

build_long_50m <- function(df, forest.cols, remove.cols, keep.canopy) {
  
  canopy.cols <- names(df)[grepl('^(cover_|gap_|ht_|fd_)', names(df))]
  drop.canopy <- setdiff(canopy.cols, keep.canopy)
  
  swe.long <- df %>%
    pivot_longer(
      cols = starts_with('swe_peak_wy'),
      names_to = 'wy',
      values_to = 'swe_peak'
    )
  
  swe.long$wy <- as.numeric(gsub('swe_peak_wy', '', swe.long$wy))
  
  clim.long <- pivot_climate_long(df)
  
  df.long <- left_join(swe.long, clim.long, by = c('cell', 'wy')) %>%
    select(
      -starts_with('clim_'),
      -matches('^swe_\\d{8}$'),
      -all_of(remove.cols),
      -all_of(drop.canopy)
    )
  
  df.long
}

build_long_500m <- function(df, forest.cols, remove.cols, keep.canopy) {
  
  names.clean <- gsub('_500m$', '', names(df))
  canopy.cols <- names.clean[grepl('^(cover_|gap_|ht_|fd_)', names.clean)]
  drop.canopy <- setdiff(canopy.cols, keep.canopy)
  
  swe.long <- df %>%
    pivot_longer(
      cols = starts_with('swe_peak_wy'),
      names_to = 'wy',
      values_to = 'swe_peak'
    )
  
  swe.long$wy <- as.numeric(gsub('swe_peak_wy|_500m', '', swe.long$wy))
  
  clim.long <- pivot_climate_long(df)
  
  sdd.long <- df %>%
    select(cell, matches('^sdd_wy\\d{4}$')) %>%
    pivot_longer(
      cols = -cell,
      names_to = 'wy',
      values_to = 'sdd'
    )
  
  sdd.long$wy <- as.numeric(gsub('sdd_wy', '', sdd.long$wy))
  
  df.long <- swe.long %>%
    left_join(sdd.long, by = c('cell', 'wy')) %>%
    left_join(clim.long, by = c('cell', 'wy')) %>%
    rename_with(~ gsub('_500m$', '', .x)) %>%
    select(
      -starts_with('clim_'),
      -matches('^swe_\\d{8}$'),
      -starts_with('sdd_'),
      -all_of(remove.cols),
      -all_of(drop.canopy)
    ) %>%
    mutate(
      cbibc = ifelse(wy == 2020 & !is.na(cbibc), 0, cbibc)
    )
  
  df.long
}



# ===========================================================================================
# read rasters and convert to dfs
# ===========================================================================================

#dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/tif'
dir <- 'data/processed/processed/tif'

r.50 <- rast(file.path(dir, '50m/creek/creek_master_50m.tif'))
r.500 <- rast(file.path(dir, '500m/creek/creek_master_500m.tif'))

df.50 <- as.data.frame(r.50, cells = TRUE)
df.500 <- as.data.frame(r.500, cells = TRUE)


# ===========================================================================================
# filter landcover
# ===========================================================================================

df.50.f <- filter_landcover(df.50, forest.cols)
df.500.f <- filter_landcover(df.500, forest.cols)

# ===========================================================================================
# pivot long
# ===========================================================================================

df.long.50 <- build_long_50m(df.50.f, forest.cols, remove.cols, keep.canopy)
df.long.500 <- build_long_500m(df.500.f, forest.cols, remove.cols, keep.canopy)

# check
names(df.long.50)
names(df.long.500)

# ===========================================================================================
# save
# ===========================================================================================
dir <- 'data/processed/processed/rds'

saveRDS(df.long.50, file.path(dir, 'creek_long_df_50m.rds'))
saveRDS(df.long.500, file.path(dir, 'creek_long_df_500m.rds'))

# save to backup
saveRDS(df.long.50, 'G:/Active_Projects/Fire_Snow_Dynamics_backup/data/processed/processed/rds/creek_long_df_50m.rds')
saveRDS(df.long.500, 'G:/Active_Projects/Fire_Snow_Dynamics_backup/data/processed/processed/rds/creek_long_df_500m.rds')


# -------- check NAs ------------
na.counts <- colSums(is.na(df.50.cc))
na.pct <- colMeans(is.na(df.50.cc))*100

n.total <- nrow(df.50.cc)
n.complete <- sum(complete.cases(df.50.cc))

cat('Total rows:', n.total, '\n')
cat('Complete rows:', n.complete, '\n')
cat('Rows lost:', n.total - n.complete, '\n')
cat('Percent lost:', round(100 * (n.total - n.complete) / n.total, 2), '%\n')
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




# ==============================================================================
# explore
# ==============================================================================

# ----- visualize where swe = 0 for each year -----
year <- c('2021', '2022', '2023', '2024', '2025')

dev.off()
par(mfrow = c(2, 3))
for (yr in year) {
  r <- swe.stack[[paste0('swe_peak_wy', yr)]]
  r <- clamp(r, lower = 0, upper = 5, values = TRUE)
  
  plot(r,
       main = paste0('Peak SWE in WY', yr),
       zlim = c(0, 5))
  
  r.zero <- r == 0
  r.zero <- classify(r.zero, rbind(c(0, NA)))
  plot(r.zero, col = 'red', add = TRUE, legend = FALSE)
}

# ----- find what what elevation threshold we stop seeing snow for each year -----
for (yr in year) {
  r.swe <- swe.stack[[paste0('swe_peak_wy', yr)]]
  r.zero <- r.swe == 0
  r.elev <- swe.stack$topo_elev
  elev.zero <- mask(r.elev, r.zero, maskvalues = 0)
  elev.vals <- values(elev.zero, na.rm = T)

}

# find snowline

years <- c(2021, 2022, 2023, 2024, 2025)
r.elev <- swe.stack$topo_elev

snowline <- data.frame(
  wy = years,
  q25 = NA_real_,
  q50 = NA_real_,
  q75 = NA_real_,
  n.edge = NA_integer_
)

w <- matrix(1, 3, 3)

for (i in seq_along(years)) {
  
  yr <- years[i]
  
  # SWE for this year
  r.swe <- swe.stack[[paste0('swe_peak_wy', yr)]]
  
  # binary: 0 = no snow, 1 = snow
  r.bin <- ifel(r.swe > 0, 1, 0)
  
  # find cells whose 3x3 neighborhood contains both 0 and 1
  r.min <- focal(r.bin, w = w, fun = min, na.rm = TRUE)
  r.max <- focal(r.bin, w = w, fun = max, na.rm = TRUE)
  r.edge <- (r.min != r.max)
  
  # keep only the snow-side edge cells
  r.edge.snow <- r.edge * (r.bin == 1)
  r.edge.snow <- mask(r.edge.snow, r.edge.snow, maskvalues = 0)
  
  # elevation at edge
  elev.edge <- mask(r.elev, r.edge.snow)
  elev.vals <- values(elev.edge, na.rm = TRUE)
  
  # save summaries
  qs <- quantile(elev.vals, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  snowline$q25[i] <- qs[1]
  snowline$q50[i] <- qs[2]
  snowline$q75[i] <- qs[3]
  snowline$n.edge[i] <- length(elev.vals)
}

snowline

par(mfrow = c(1, 2))
plot(snowline$wy, snowline$q50, ylim = range(snowline$q25, snowline$q75))
arrows(snowline$wy, snowline$q25,
       snowline$wy, snowline$q75,
       angle = 90, code = 3, length = 0.05)

# ----- now visualize snowline -----
# ----- visualize where swe = 0 for each year -----
year <- c('2021', '2022', '2023', '2024', '2025')

dev.off()
par(mfrow = c(2, 3))
for (yr in year) {
  r <- swe.stack[[paste0('swe_peak_wy', yr)]]
  r.elev <- swe.stack$topo_elev
  r <- clamp(r, lower = 0, upper = 5, values = TRUE)
  
  # set snowline by year
  snowline <- 
    if (yr == '2021') 1843 else
    if (yr == '2022') 1748 else
    if (yr == '2023') -Inf else
    if (yr == '2024') 1723 else
    if (yr == '2025') 1883
  
  plot(r,
       main = paste0('Snowline in WY', yr),
       zlim = c(0, 5))
  
  r.sl <- r.elev < snowline
  r.sl <- mask(r.sl, r)
  r.sl <- classify(r.sl, rbind(c(0, NA)))
  plot(r.sl, col = 'red', add = TRUE, legend = FALSE)
}



# ----- visualize where sdd = 0 for each year -----
year <- c('2021', '2022', '2023', '2024', '2025')

dev.off()
par(mfrow = c(2, 3))
for (yr in year) {
  r <- sdd.stack[[paste0('sdd_wy', yr)]]
  r <- clamp(r, lower = 0, upper = 360, values = TRUE)
  
  plot(r,
       main = paste0('SDD in WY', yr),
       zlim = c(0, 360))
  
  r.zero <- r == 0
  r.zero <- classify(r.zero, rbind(c(0, NA)))
  plot(r.zero, col = 'red', add = TRUE, legend = FALSE)
}




## troubleshooting
f1 <- file.path(out.dir, 'creek_sdd_wy2023__500m.tif')
f2 <- file.path(temp.dir, 'creek_cbibc_500m_1524.tif') 
r1 <- rast(f1)
r2 <- rast(f2)

res(r1)
res(r2)
origin(r2) == origin(r1)
crs(r1, describe = T)$code == crs(r2, describe = T)$code
