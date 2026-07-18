packages <- c('dplyr', 'tidyr', 'tools', 'terra')
lapply(packages, library, character.only = T)

# ===========================================================================================
# Create master raster
# ===========================================================================================

# read in all raster stacks and combine into single one for modeling

# ----- master-raster 500m -----
fires <- c('dixie', 'castle', 'caldor')
fire <- 'creek'

for (fire in fires) {
  res <- '500m'
  dir <- paste0('data/processed/processed/tif/', res, '/', fire, '/')
  
  j <- paste0('J:/Fire_Snow/fireandice/', dir)
  g <- paste0('G:/Fire_Snow_Dynamics_backup/', dir)
  
  
  files <- list.files(dir, full.names = TRUE, pattern = '\\.tif$')
  files <- files[!grepl('master', files)]
  files
  
  # combine
  stack.0 <- rast(files)
  
  names(stack.0) <- sub(
    '_500m$',
    '',
    names(stack.0)
  )
  
  # mask all areas by canopy raster
  mask.template <- stack.0$gap_gap_pct
  
  # mask every layer in the stack
  stack <- mask(stack.0, mask.template)
  
  writeRaster(stack, paste0(dir, fire, '_master_', res, '.tif'), overwrite = T)
  writeRaster(stack, paste0(j, fire, '_master_', res, '.tif'), overwrite = T)
  writeRaster(stack, paste0(g, fire, '_master_', res, '.tif'), overwrite = T)

}

# mask t exclude Kings study area
# template <- sdd.stack[['swe_20200414_500m']]
# mask <- !is.na(template) 
# 
# sdd.stack.masked <- trim(mask(sdd.stack, mask, maskvalues = FALSE))


# ----- master-raster 50m -----

fires <- c('dixie', 'castle', 'caldor')
fire <- 'creek'

for (fire in fires) {
  res <- '50m'
  dir <- paste0('data/processed/processed/tif/', res, '/', fire, '/')
  
  j <- paste0('J:/Fire_Snow/fireandice/', dir)
  g <- paste0('G:/Fire_Snow_Dynamics_backup/', dir)
  
  
  files <- list.files(dir, full.names = TRUE, pattern = '\\.tif$')
  files <- files[!grepl('master', files)]
  files
  
  # read rasters
  rasters <- lapply(files, rast)
  
  # 50m for some reason have slightly different extents, so we need to crop them all to the common extent
  # extract extents
  exts <- lapply(rasters, ext)
  
  # common extent
  common.ext <- ext(
    max(sapply(exts, xmin)),
    min(sapply(exts, xmax)),
    max(sapply(exts, ymin)),
    min(sapply(exts, ymax))
  )
  
  # crop to common extent
  rasters.crop <- lapply(
    rasters,
    crop,
    y = common.ext
  )
  
  # combine
  stack.0 <- rast(rasters.crop)
  
  # mask all areas by canopy raster
  mask.template <- stack.0$gap_gap_pct
  
  # mask every layer in the stack
  stack <- mask(stack.0, mask.template)
  
  writeRaster(stack, paste0(dir, fire, '_master_', res, '.tif'), overwrite = T)
  writeRaster(stack, paste0(j, fire, '_master_', res, '.tif'), overwrite = T)
  writeRaster(stack, paste0(g, fire, '_master_', res, '.tif'), overwrite = T)
  
}


# ----- inspect ----- 

# check rasters
fire <- 'dixie'
res <- '500m'

dir <- paste0('data/processed/processed/tif/', res, '/', fire, '/')
master.file <- paste0(dir, fire, '_master_', res, '.tif')
master <- rast(master.file)

names(master)

par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))

n <- nlyr(master)

for (i in seq(1, n, by = 4)) {
  
  # layers to plot in this batch
  idx <- i:min(i + 3, n)
  
  plot(master[[idx]])
  
  # pause unless this is the last batch
  if (max(idx) < n) {
    readline('Press <Enter> to see the next 4 layers...')
  }
}


par(mfrow = c(1, 1))


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


# ===========================================================================================
# helper functions
# ===========================================================================================

filter_landcover <- function(df, forest.cols, lc.sum.min = 0.25, undesirable.max = 0.30) {
  
  df %>%
    #rename(cbibc = CBI_bc) %>% # only for castle/caldor/dixie
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
    dplyr::select(-lc.all.na, -lc.sum)
}

build_long_50m <- function(df, forest.cols, remove.cols, keep.canopy = NULL) {
  
  canopy.cols <- names(df)[grepl('^(cover_|gap_|ht_|fd_)', names(df))]
  
  if (is.null(keep.canopy)) {
    drop.canopy <- character(0)
  } else {
    drop.canopy <- setdiff(canopy.cols, keep.canopy)
  }
  
  df.long <- df %>%
    pivot_longer(
      cols = starts_with('swe_peak_wy'),
      names_to = 'wy',
      values_to = 'swe_peak'
    ) %>%
    mutate(
      wy = as.numeric(gsub('swe_peak_wy', '', wy))
    ) %>%
    dplyr::select(
      -matches('^swe_\\d{8}$'),
      -all_of(remove.cols),
      -all_of(drop.canopy)
    )
  
  df.long
}

build_long_500m <- function(df, forest.cols, remove.cols, keep.canopy = NULL) {
  
  names.clean <- gsub('_500m$', '', names(df))
  canopy.cols <- names.clean[grepl('^(cover_|gap_|ht_|fd_)', names.clean)]
  
  if (is.null(keep.canopy)) {
    drop.canopy <- character(0)
  } else {
    drop.canopy <- setdiff(canopy.cols, keep.canopy)
  }
  
  swe.long <- df %>%
    pivot_longer(
      cols = starts_with('swe_peak_wy'),
      names_to = 'wy',
      values_to = 'swe_peak'
    ) %>%
    mutate(
      wy = as.numeric(gsub('swe_peak_wy|_500m', '', wy))
    )
  
  sdd.long <- df %>%
    dplyr::select(cell, matches('^sdd_wy\\d{4}$')) %>%
    pivot_longer(
      cols = -cell,
      names_to = 'wy',
      values_to = 'sdd'
    ) %>%
    mutate(
      wy = as.numeric(gsub('sdd_wy', '', wy))
    )
  
  df.long <- swe.long %>%
    left_join(sdd.long, by = c('cell', 'wy')) %>%
    rename_with(~ gsub('_500m$', '', .x)) %>%
    dplyr::select(
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

fire <- 'creek'

dir <- 'data/processed/processed/tif/'

r.50 <- rast(paste0(dir, '50m/', fire, '/', fire, '_master_50m.tif'))
r.500 <- rast(paste0(dir, '500m/', fire, '/', fire, '_master_500m.tif'))

df.50 <- as.data.frame(r.50, cells = TRUE, xy = TRUE)
df.500 <- as.data.frame(r.500, cells = TRUE, xy = TRUE)

# ===========================================================================================
# filter landcover
# ===========================================================================================

df.50.f <- filter_landcover(df.50, forest.cols)

df.500.f <- filter_landcover(df.500, forest.cols)

# ===========================================================================================
# pivot long
# ===========================================================================================

# if you want to only keep the canopy defined in the setup, add in ", keep.canopy" to the function calls below

df.long.50 <- build_long_50m(df.50.f, forest.cols, remove.cols)

df.long.500 <- build_long_500m(df.500.f, forest.cols, remove.cols)

# check
names(df.long.50)
names(df.long.500)

# ===========================================================================================
# save
# ===========================================================================================
out.dir <- paste0('data/processed/processed/rds/', fire, '/')
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(df.long.50, paste0(out.dir, fire, '_long_df_50m_unfiltered.rds'))
saveRDS(df.long.500, paste0(out.dir, fire, '_long_df_500m_unfiltered.rds'))

# save to backup
saveRDS(df.long.50, paste0('G:/Fire_Snow_Dynamics_backup/', out.dir, fire, '_long_df_50m_unfiltered.rds'))
saveRDS(df.long.500, paste0('G:/Fire_Snow_Dynamics_backup/', out.dir, fire, '_long_df_500m_unfiltered.rds'))
saveRDS(df.long.50, paste0('J:/Fire_Snow/fireandice/', out.dir, fire, '_long_df_50m_unfiltered.rds'))
saveRDS(df.long.500, paste0('J:/Fire_Snow/fireandice/', out.dir, fire, '_long_df_500m_unfiltered.rds'))



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
all_swe <- c(df.50$swe_peak_wy2023,
             df.50$swe_peak_wy2024,
             df.50$swe_peak_wy2025)

xlim <- range(all_swe, na.rm = TRUE)
breaks <- seq(xlim[1], xlim[2], length.out = 51)  # 50 bins
par(mfrow = c(2, 2))  # 2x2 layout

hist(df.50$swe_peak_wy2023,
     breaks = breaks,
     xlim = c(0, 4),
     main = '2023',
     col = 'lightblue')

hist(df.50$swe_peak_wy2024,
     breaks = breaks,
     xlim = c(0, 4),
     main = '2024',
     col = 'lightblue')

hist(df.50$swe_peak_wy2025,
     breaks = breaks,
     xlim = c(0, 4),
     main = '2025',
     col = 'lightblue')


hist(log1p(df.50$swe_peak_wy2023))




# ==============================================================================
# Find Snowline and Explore Data
# ==============================================================================

fire <- 'castle'

swe <- rast(paste0('data/processed/processed/tif/50m/', fire, '/', fire, '_swe_peak_50m.tif'))
elev <- rast(paste0('data/processed/processed/tif/50m/', fire, '/', fire, '_nasadem_50m.tif'))

# get same extents
rasters <- list(
  swe = swe,
  elev = elev
)

# 50m for some reason have slightly different extents, so we need to crop them all to the common extent
# extract extents
exts <- lapply(rasters, ext)

# common extent
common.ext <- ext(
  max(sapply(exts, xmin)),
  min(sapply(exts, xmax)),
  max(sapply(exts, ymin)),
  min(sapply(exts, ymax))
)

# crop to common extent
rasters.crop <- lapply(
  rasters,
  crop,
  y = common.ext
)

# combine
swe.stack <- rast(rasters.crop)

years <- as.numeric(sub('swe_peak_wy', '', names(swe)))

names(swe.stack) <- c(
  paste0('swe_peak_wy', years),
  'topo_elev'
)


# ----- visualize where swe = 0 for each year -----
year <- sub('swe_peak_wy', '', names(swe))

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

# ----- find snowline -----

r.elev <- swe.stack$topo_elev

snowline <- data.frame(
  wy = years,
  q25 = NA_real_,
  q50 = NA_real_,
  q75 = NA_real_,
  n.edge = NA_integer_
)

w <- matrix(1, 3, 3, 4)

for (i in seq_along(years)) {
  
  yr <- years[i]
  
  # SWE for this year
  r.swe <- swe.stack[[paste0('swe_peak_wy', yr)]]
  
  # binary: 0 = no snow, 1 = snow
  # Preserve NA outside valid SWE coverage
  r.bin <- ifel(
    is.na(r.swe),
    NA,
    ifel(r.swe > 0, 1, 0)
  )
  
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

# plot snowline for each year
dev.off()
par(mfrow = c(2, 2))

for (i in seq_along(years)) {
  
  yr <- years[i]
  
  r.swe <- swe.stack[[paste0('swe_peak_wy', yr)]]
  
  r.bin <- ifel(
    is.na(r.swe),
    NA,
    ifel(r.swe > 0, 1, 0)
  )
  
  r.min <- focal(r.bin, w = w, fun = min, na.rm = TRUE)
  r.max <- focal(r.bin, w = w, fun = max, na.rm = TRUE)
  r.edge <- r.min != r.max
  
  r.edge.snow <- ifel(
    r.edge & r.bin == 1,
    1,
    NA
  )
  
  # plot elevation
  plot(
    r.elev,
    main = paste('WY', yr)
  )
  
  # overlay detected snowline
  plot(
    r.edge.snow,
    add = TRUE,
    col = 'red',
    legend = FALSE
  )
}

par(mfrow = c(1, 2))
plot(snowline$wy, snowline$q50, ylim = range(snowline$q25, snowline$q75))
arrows(snowline$wy, snowline$q25,
       snowline$wy, snowline$q75,
       angle = 90, code = 3, length = 0.05)

# ----- now visualize snowline -----
# ----- visualize where swe = 0 for each year -----

# snowline elevations by fire and water year
snowline.lookup <- data.frame(
  fire = c(
    rep('caldor', 4),
    rep('castle', 3),
    rep('dixie', 3)
  ),
  wy = c(
    2023, 2024, 2025, 2026,
    2023, 2024, 2025,
    2023, 2024, 2025
  ),
  snowline = c(
    1344, 1441, 1453, 2267,
    1722, 1574, 1937,
    1390, 1454, 1708
  )
)


year <- sub('swe_peak_wy', '', names(swe))

dev.off()
par(mfrow = c(2, 3))

for (yr in year) {
  
  r <- swe.stack[[paste0('swe_peak_wy', yr)]]
  r.elev <- swe.stack$topo_elev
  
  r <- clamp(
    r,
    lower = 0,
    upper = 5,
    values = TRUE
  )
  
  # get snowline for this fire and water year
  snowline <- snowline.lookup$snowline[
    snowline.lookup$fire == fire &
      snowline.lookup$wy == as.numeric(yr)
  ]
  
  # stop if no matching snowline was provided
  if (length(snowline) != 1) {
    stop(
      paste0(
        'No unique snowline found for ',
        fire,
        ' in WY',
        yr
      )
    )
  }
  
  plot(
    r,
    main = paste0(
      tools::toTitleCase(fire),
      ' snowline in WY',
      yr,
      ': ',
      snowline,
      ' m'
    ),
    zlim = c(0, 5)
  )
  
  # identify elevations below the snowline
  r.sl <- r.elev < snowline
  
  # retain only cells within the SWE raster
  r.sl <- mask(r.sl, r)
  
  # remove cells above the snowline
  r.sl <- classify(
    r.sl,
    rbind(c(0, NA))
  )
  
  # plot areas below the snowline in red
  plot(
    r.sl,
    col = 'red',
    add = TRUE,
    legend = FALSE
  )
}


# ----- visualize where sdd = 0 for each year -----
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

r <- rast('data/processed/processed/tif/500m/creek/creek_master_500m.tif')
plot(r)


r1 <- rast('data/processed/processed/tif/50m/creek/creek_canopy_metrics_50m.tif')
plot(r1)

# ----- explore elevation distributions -----
fire <- 'caldor'
dir <- paste0('data/processed/processed/rds/', fire, '/')
df.50.0 <- readRDS(paste0(dir, fire, '_long_df_50m_unfiltered.rds'))
caldor.50.raw <- df.50.0

fire <- 'castle'
dir <- paste0('data/processed/processed/rds/', fire, '/')
df.50.0 <- readRDS(paste0(dir, fire, '_long_df_50m_unfiltered.rds'))
castle.50.raw <- df.50.0

fire <- 'dixie'
dir <- paste0('data/processed/processed/rds/', fire, '/')
df.50.0 <- readRDS(paste0(dir, fire, '_long_df_50m_unfiltered.rds'))
dixie.50.raw <- df.50.0

fire <- 'creek'
dir <- paste0('data/processed/processed/rds/', fire, '/')
df.50.0 <- readRDS(paste0(dir, fire, '_long_df_50m_raw_unfiltered.rds'))
creek.50.raw <- df.50.0

plot.df <- bind_rows(
  caldor.50.raw %>% mutate(fire = 'caldor'),
  castle.50.raw %>% mutate(fire = 'Castle'),
  dixie.50.raw %>% mutate(fire = 'dixie'),
  creek.50.raw %>% mutate(fire = 'creek')
)

ggplot(
  plot.df,
  aes(
    x = elevation,
    color = fire,
    fill = fire
  )
) +
  geom_density(alpha = 0.2) +
  theme_bw() +
  labs(
    x = 'Elevation (m)',
    y = 'Density'
  )

plot.df %>%
  group_by(fire) %>%
  summarize(
    min = min(elevation),
    q25 = quantile(elevation, 0.25),
    median = median(elevation),
    q75 = quantile(elevation, 0.75),
    max = max(elevation)
  )


# troubleshooting
for (each in files) {
   r <- rast(each)
   print(names(r))
 }
files

test.files <- list.files('data/processed/processed/tif/500m/creek/snow_metrics', full.names = T, pattern = '\\.tif$')

x <- rast(files[4])
names(x) <- 'elevation'
names(x)

writeRaster(x, 'data/processed/processed/tif/500m/creek/creek_nasadem_500m2.tif', overwrite = T)

