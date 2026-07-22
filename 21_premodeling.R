packages <- c('dplyr', 'tidyr', 'corrplot', 'ggplot2', 'blockCV', 'sf', 'terra')
lapply(packages, library, character.only = T)

# =======================================================================================
#  filter DFs 
# =======================================================================================
#  ----- setup -----
fire <- 'creek'
dir <- paste0('data/processed/processed/rds/', fire, '/')

# --- Snowline lookup ---
if (fire == 'caldor') {
  snowline.2023 <- 1344
  snowline.2024 <- 1441
  snowline.2025 <- 1453
  snowline.2026 <- 2267
  epsg <- 32610
  selection <- 'random'
} else if (fire == 'creek') {
  snowline.2023 <- -Inf
  snowline.2024 <- 1786
  snowline.2025 <- 2021
  snowline.2026 <- NA_real_
  epsg <- 32611
  selection <- 'systematic'
} else if (fire == 'castle') {
  snowline.2023 <- 1722
  snowline.2024 <- 1574
  snowline.2025 <- 1937
  snowline.2026 <- NA_real_
  epsg <- 32611
  selection <- 'random'
} else if (fire == 'dixie') {
  snowline.2023 <- 1390
  snowline.2024 <- 1454
  snowline.2025 <- 1708
  snowline.2026 <- NA_real_
  epsg <- 32610
  selection <- 'random'
} else {
  stop('Snowline values have not been defined for this fire.')
}

# ----- 50m -----


# get dataframe
df.50.0 <- readRDS(paste0(dir, fire, '_long_df_50m_unfiltered.rds'))

if (fire == 'creek') {
df.50.0 <- df.50.0 %>%
  select(-fd_fractal_dim) %>%
  filter(!wy %in% c(2020, 2021, 2022))
}

df.50 <- df.50.0 %>% 
  select(-rad_dtm_melt, -rad_dsm_melt) %>% # melt season not relevant to snow accumulation phase
  filter(
    swe_peak > 0 # drop all cells where there was no snow
  ) %>%  
  mutate(
    snowline = case_when(
      wy == 2023 ~ snowline.2023,
      wy == 2024 ~ snowline.2024,
      wy == 2025 ~ snowline.2025,
      wy == 2026 ~ snowline.2026,
      TRUE ~ NA_real_
    ),
    
    wy = as.factor(wy),
    cell = as.factor(cell),
    
    gap_percent = gap_gap_pct * 100,
    
    burned = factor(
      if_else(cbibc > 0, 'burned', 'unburned'),
      levels = c('unburned', 'burned')
    ),
    
    # aspect classes
    aspect_class = case_when(
      aspect_cos > 0.5  ~ 'North-facing',
      aspect_cos < -0.5 ~ 'South-facing',
      TRUE                   ~ NA_character_
    ),
    
    aspect_class = factor(
      aspect_class,
      levels = c('North-facing', 'South-facing')
    )
  
  ) %>% 
  filter(
    elevation >= snowline,
    ht_zmax <= 96 # remove anything taller than what is proved to be the tallest tree in the Sierra
  ) %>% 
  select(-snowline) # remove temp column



# save raw before scaling
df.50.raw <- df.50

# scale numeric predictors
num.cols <- sapply(df.50, is.numeric)
num.cols[c('swe_peak', 'x', 'y')] <- FALSE # don't scale the response variable or coordinates
df.50[num.cols] <- scale(df.50[num.cols])

# --- Spatial Blocking ---

block.m <- 8000
test.prop <- 0.20
res <- 50
df <- df.50 

# study area polygon
study <- st_read(paste0('data/processed/processed/shp/studyarea_extents/study_extent_', fire, '_simple.shp')) %>%
  st_transform(epsg)

# your model data as points
dat.sf <- st_as_sf(df, coords = c('x', 'y'), crs = epsg, remove = FALSE)

# NOTE:
# creek used selection = 'systematic'
# castle, caldor, and dixie used selection = 'random', seed = 61 for best distribution of train/test data

sb <- cv_spatial(
  x = dat.sf,
  column = NULL,
  r = NULL,
  size = block.m,
  k = 5,
  selection = selection,
  iteration = 100,
  biomod2 = FALSE,
  progress = TRUE,
  report = TRUE,
  seed = 61
)

cv_plot(sb)

# create test/train categories
sb$blocks$split <- if_else(sb$blocks$folds == 2, 'test', 'train')

# plot test/train
ggplot(sb$blocks) +
  geom_sf(aes(fill = split)) +
  theme_bw()

# apply blocks to data
dat.blocked <- dat.sf %>%
  mutate(
    fold_id = sb$folds_ids,
    split = if_else(fold_id == 1, 'test', 'train')
  )


# --- check these numbers for new study areas ---
# proportion numbers per fold
# sb$blocks %>%
#   st_drop_geometry() %>%
#   count(folds) %>%
#   mutate(prop = n / sum(n))
# 
# # proportion of pixels per fold
# dat.blocked %>%
#   st_drop_geometry() %>%
#   count(split) %>%
#   mutate(prop = n / sum(n))
# 
# dat.blocked %>%
#   st_drop_geometry() %>%
#   count(fold_id) %>%
#   mutate(prop = n / sum(n))
# 
# # check SDD years are balanced across train/test test
# dat.blocked %>%
#   st_drop_geometry() %>%
#   count(wy, split) %>%
#   group_by(wy) %>%
#   mutate(prop = n / sum(n))
# 
# dat.blocked %>%
#   st_drop_geometry() %>%
#   distinct(x, y, wy, fold_id, split) %>%
#   count(x, y) %>%
#   filter(n > 1)
# 
# dat.blocked %>%
#   st_drop_geometry() %>%
#   group_by(x, y) %>%
#   summarize(
#     n_folds = n_distinct(fold_id),
#     n_splits = n_distinct(split),
#     .groups = 'drop'
#   ) %>%
#   filter(n_folds > 1 | n_splits > 1)


# --- save ---
dat.blocked <- st_drop_geometry(dat.blocked)
saveRDS(dat.blocked, file.path(dir, paste0(fire, '_df_', res, 'm.rds')))
saveRDS(dat.blocked, paste0('J:/Fire_Snow/fireandice/data/processed/processed/rds/', fire, '_df_', res, 'm.rds'))
saveRDS(dat.blocked, paste0('G:/Fire_Snow_Dynamics_backup/data/processed/processed/rds/', fire, '_df_', res, 'm.rds'))


# add same folds to raw

stopifnot(nrow(df.50.raw) == nrow(dat.blocked))
stopifnot(all(df.50.raw$x == dat.blocked$x))
stopifnot(all(df.50.raw$y == dat.blocked$y))
stopifnot(all(df.50.raw$wy == dat.blocked$wy))

df.50.raw <- df.50.raw %>%
  mutate(
    fold_id = dat.blocked$fold_id,
    split = dat.blocked$split
  )

# save raw + fold_ids
saveRDS(df.50.raw, paste0(dir, fire, '_df_50m_raw.rds'))
saveRDS(df.50.raw, paste0('J:/Fire_Snow/fireandice/data/processed/processed/rds/', fire, '/', fire, '_df_50m_raw.rds')) # save to J: drive
saveRDS(df.50.raw, paste0('G:/Fire_Snow_Dynamics_backup/data/processed/processed/rds/', fire, '/', fire, '_df_50m_raw.rds')) # save to G: drive backup

  


# ----- 500m -----

df.500.0 <- readRDS(paste0(dir, fire, '_long_df_500m_unfiltered.rds'))

if (fire == 'creek') {
  df.500.0 <- df.500.0 %>%
    rename_with(
      ~ gsub('^topo_', '', .x)
    ) %>%
    select(-fd_fractal_dim, -elev) %>%
    filter(!wy %in% c(2020, 2021, 2022))
}

df.500 <- df.500.0 %>% 
  filter(
    swe_peak > 0,
    sdd > 0
  ) %>%  
  mutate(
    snowline = case_when(
      wy == 2023 ~ snowline.2023,
      wy == 2024 ~ snowline.2024,
      wy == 2025 ~ snowline.2025,
      wy == 2026 ~ snowline.2026,
      TRUE ~ NA_real_
    ),
    
    wy = as.factor(wy),
    cell = as.factor(cell),
    
    gap_percent = gap_gap_pct * 100,
    
    burned = factor(
      if_else(cbibc > 0, 'burned', 'unburned'),
      levels = c('unburned', 'burned')
    ),
    
    # aspect classes
    aspect_class = case_when(
      aspect_cos > 0.5  ~ 'North-facing',
      aspect_cos < -0.5 ~ 'South-facing',
      TRUE                   ~ 'East/West'
    ),
    
    aspect_class = factor(
      aspect_class,
      levels = c(
        'North-facing',
        'East/West',
        'South-facing'
      )
    )

  ) %>% 
  filter(
    elevation >= snowline,
  ) %>% 
  select(-snowline)

# save raw values before scaling
df.500.raw <- df.500

# scale numeric predictors
num.cols <- sapply(df.500, is.numeric)
num.cols[c('sdd', 'x', 'y')] <- FALSE # don't scale the response variable'
df.500[num.cols] <- scale(df.500[num.cols])



# --- spatial blocking ---

block.m <- 8000
test.prop <- 0.20
res <- 500
df <- df.500 # or df.500

# study area polygon
study <- st_read(paste0('data/processed/processed/shp/studyarea_extents/study_extent_', fire, '_simple.shp')) %>%
  st_transform(epsg)

# your model data as points
dat.sf <- st_as_sf(df, coords = c('x', 'y'), crs = epsg, remove = FALSE)

# NOTE:
# creek used selection = 'systematic'
# castle, caldor, and dixie used selection = 'random', seed = 61 for best distribution of train/test data

sb <- cv_spatial(
  x = dat.sf,
  column = NULL,
  r = NULL,
  size = block.m,
  k = 5,
  selection = selection,
  iteration = 100,
  biomod2 = FALSE,
  progress = TRUE,
  report = TRUE,
  seed = 61
)

cv_plot(sb)

# create test/train categories
sb$blocks$split <- if_else(sb$blocks$folds == 2, 'test', 'train')

# plot test/train
ggplot(sb$blocks) +
  geom_sf(aes(fill = split)) +
  theme_bw()

# apply blocks to data
dat.blocked <- dat.sf %>%
  mutate(
    fold_id = sb$folds_ids,
    split = if_else(fold_id == 1, 'test', 'train')
  )

# --- save ---
dat.blocked <- st_drop_geometry(dat.blocked)
saveRDS(dat.blocked, file.path(dir, paste0(fire, '_df_', res, 'm.rds')))
saveRDS(dat.blocked, paste0('J:/Fire_Snow/fireandice/data/processed/processed/rds/', fire, '_df_', res, 'm.rds'))
saveRDS(dat.blocked, paste0('G:/Fire_Snow_Dynamics_backup/data/processed/processed/rds/', fire, '_df_', res, 'm.rds'))


# add same folds to raw
stopifnot(nrow(df.500.raw) == nrow(dat.blocked))
stopifnot(all(df.500.raw$x == dat.blocked$x))
stopifnot(all(df.500.raw$y == dat.blocked$y))
stopifnot(all(df.500.raw$wy == dat.blocked$wy))

df.500.raw <- df.500.raw %>%
  mutate(
    fold_id = dat.blocked$fold_id,
    split = dat.blocked$split
  )

# save raw + fold_ids
saveRDS(df.500.raw, paste0(dir, fire, '_df_500m_raw.rds'))
saveRDS(df.500.raw, paste0('J:/Fire_Snow/fireandice/data/processed/processed/rds/', fire, '/', fire, '_df_500m_raw.rds')) # save to J: drive
saveRDS(df.500.raw, paste0('G:/Fire_Snow_Dynamics_backup/data/processed/processed/rds/', fire, '/', fire, '_df_500m_raw.rds')) # save to G: drive backup







# ----- combine dfs -----

# --- raw ---
fires <- c('creek', 'castle', 'caldor', 'dixie')
rds.dir <- 'data/processed/processed/rds/'
res <- '500m' # 500m or 50m

dfs <- lapply(fires, function(fire) {
  
  readRDS(paste0(rds.dir, fire, '/', fire, '_df_', res, '_raw.rds')
          
  ) %>%
    mutate(fire = fire)
  
})

df.raw.0 <- bind_rows(dfs)

df.raw <- df.raw.0 %>%
  rename(gap_percent = gap_gap_pct) %>%
  filter(complete.cases(select(., -aspect_class)))
  

saveRDS(df.raw, paste0(rds.dir, 'df_', res, '_raw_all_metrics.rds'))


# =======================================================================================
#  Elevation Matching
# =======================================================================================
# ----- balance elevation distributions between burned and unburned -----
# --- get df ---
set.seed(61)

rds.dir <- 'data/processed/processed/rds/'
res <- '500m'
df.raw.file <- paste0(rds.dir, 'df_', res, '_raw_all_metrics.rds')

df.raw <- readRDS(df.raw.file)

df.balanced <- df.raw %>%
  mutate(
    elev_bin = floor(elevation / 50) * 50 # bin every pixel into a 50m elevation bin
  ) %>%   
  group_by(fire, wy, elev_bin) %>%        # create groups across fire, wy, and elev_bin
  filter(n_distinct(burned) == 2) %>%     # only keep elev_bins that have both burned and unburned groups 
  # run the following code separately for each group
  group_modify(~ {
    
    # .x is the dataframe for the current fire × year × elevation bin
    
    # find the size of the smaller burn class
    n.keep <- min(
      sum(.x$burned == 'burned'),
      sum(.x$burned == 'unburned')
    )
    
    # randomly keep n.keep rows from each burn class
    .x %>%
      group_by(burned) %>%
      slice_sample(n = n.keep) %>%
      ungroup()
  }) %>%
  
  ungroup()

df.balanced %>%
  count(fire, wy, elev_bin, burned) %>%
  tidyr::pivot_wider(
    names_from = burned,
    values_from = n
  )

saveRDS(df.balanced, paste0(rds.dir, 'df_', res, '_raw_balanced_all_metrics.rds'))

# scale numeric predictors
df.balanced.scaled <- df.balanced
num.cols <- sapply(df.balanced.scaled, is.numeric)

if (res == '50m') {
num.cols[c('swe_peak', 'x', 'y')] <- FALSE # don't scale the response variable or coordinates
} else if (res == '500m') {
  num.cols[c('sdd', 'x', 'y')] <- FALSE # don't scale the response variable or coordinates
} else {
  stop('wrong resolution')
}

df.balanced.scaled[num.cols] <- scale(df.balanced.scaled[num.cols])

saveRDS(df.balanced.scaled, paste0(rds.dir, 'df_', res, '_scaled_balanced_all_metrics.rds'))

# ----- check -----

vars <- c(
  'elevation',
  'rad_dtm_accum',
  'slope',
  'tpi150',
  'gap_gap_pct',
  'ht_zmax',
  'cbibc'
)

ggplot(df.balanced,
       aes(x = elevation,
           color = burned,
           fill = burned)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~fire) +
  labs(
    x = 'Elevation (m)',
    y = 'Density'
  ) +
  theme_bw()


compare_summary <- function(df, label) {
  
  df %>%
    group_by(fire, burned) %>%
    summarize(
      across(
        all_of(vars),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE)
        )
      ),
      .groups = 'drop'
    ) %>%
    mutate(dataset = label)
  
}

summary.compare <- bind_rows(
  compare_summary(df.raw, 'Original'),
  compare_summary(df.balanced, 'Balanced')
)

summary.compare

# compare radiation density plots
plot.df <- bind_rows(
  df.raw %>%
    mutate(dataset = 'Original'),
  
  df.balanced %>%
    mutate(dataset = 'Balanced')
)

ggplot(
  plot.df,
  aes(
    x = rad_dtm_accum,
    color = burned,
    fill = burned
  )
) +
  geom_density(alpha = 0.3) +
  facet_grid(fire ~ dataset) +
  labs(
    x = 'Radiation',
    y = 'Density'
  ) +
  theme_bw()

# calculate SMDs - Standardized Mean Differences
calc_smd <- function(df, variable) {
  
  burned <- df %>%
    filter(burned == 'burned') %>%
    pull({{ variable }})
  
  unburned <- df %>%
    filter(burned == 'unburned') %>%
    pull({{ variable }})
  
  (
    mean(burned, na.rm = TRUE) -
      mean(unburned, na.rm = TRUE)
  ) /
    sqrt(
      (
        var(burned, na.rm = TRUE) +
          var(unburned, na.rm = TRUE)
      ) / 2
    )
}

# calculate SMDs per fire
calc_fire_smd <- function(df, label) {
  
  bind_rows(
    lapply(unique(df$fire), function(f) {
      
      dat <- filter(df, fire == f)
      
      data.frame(
        fire = f,
        dataset = label,
        variable = vars,
        smd = sapply(
          vars,
          function(v) calc_smd(dat, !!rlang::sym(v))
        )
      )
    })
  )
  
}

smd.compare <- bind_rows(
  calc_fire_smd(df.raw, 'Original'),
  calc_fire_smd(df.balanced, 'Balanced')
)

smd.compare

# plot them
ggplot(
  smd.compare,
  aes(
    variable,
    abs(smd),
    fill = dataset
  )
) +
  geom_col(position = 'dodge') +
  facet_wrap(~fire) +
  labs(
    x = '',
    y = 'Absolute standardized mean difference'
  ) +
  theme_bw() +
  coord_flip()

# compare sample size by fire
bind_rows(
  df.raw %>%
    count(fire) %>%
    mutate(dataset = 'Original'),
  
  df.balanced %>%
    count(fire) %>%
    mutate(dataset = 'Balanced')
) %>%
  tidyr::pivot_wider(
    names_from = dataset,
    values_from = n
  ) %>%
  mutate(
    Percent_retained = round(100 * Balanced / Original, 1)
  )
# compare sample size by fire and burned status
bind_rows(
  df.raw %>%
    count(fire, burned) %>%
    mutate(dataset = 'Original'),
  
  df.balanced %>%
    count(fire, burned) %>%
    mutate(dataset = 'Balanced')
) %>%
  tidyr::pivot_wider(
    names_from = dataset,
    values_from = n
  ) %>%
  mutate(
    Percent_retained = round(100 * Balanced / Original, 1)
  )


# =======================================================================================
#  Trim Variables
# =======================================================================================
# ----- keep only list of paired-down variables -----

old.rds.dir <- 'data/processed/processed/rds/old_versions/'
new.rds.dir <- 'data/processed/processed/rds/'

files <- list.files(old.rds.dir, pattern = '\\.rds$', full.names = T)

keep.vars <- c('cell', 'x', 'y', 'fire', 'wy', 'swe_peak', 'burned', 'ht_zpcum6', 'ht_zpcum9', 'ht_zpcum1', 'ht_zpcum2', 'ht_zskew', 'ht_zkurt', 'ht_zmax', 'gap_dist_to_canopy_mean', 'gap_percent',  'cbibc', 'elevation', 'rad_dsm_accum', 'rad_dtm_accum', 'slope', 'aspect_sin', 'aspect_cos', 'aspect_class', 'tpi150', 'tpi510', 'tpi1200', 'tpi2010', 'split', 'fold_id')

for (file in files) {
  
  rds <- readRDS(file)
  # variables to keep
  keep <- keep.vars
  
  # add variables specific to 500m data
  if (grepl('500', basename(file))) {
    keep <- c(
      keep,
      'sdd',
      grep('melt', names(rds), value = TRUE)
    )
  }
  
  rds.filt <- rds %>%
    select(all_of(keep))
  
  # remove '_all_metrics' from filename
  new.name <- sub(
    '_all_metrics\\.rds$',
    '.rds',
    basename(file)
  )
  
  new.file <- file.path(new.rds.dir, new.name)
  
  # write filtered dataframe to /rds/
  saveRDS(rds.filt, new.file)
  
}

x <- readRDS(files[1])
names(x)

# =======================================================================================
# Spatial Autocorrelation
# =======================================================================================

# ----- df.50 -----
library(gstat)
df <- subset(df.50, wy == 2023)
df <- df[complete.cases(df[, c('swe_peak', 'x', 'y')]), ]

# --- semivariogram ---
set.seed(123)
df.vario <- df[sample(nrow(df), min(10000, nrow(df))), ]
vg <- variogram(
  swe_peak ~ 1,
  locations = ~ x + y,
  data = df.vario,
  width = 100   # 100 m bins
)
plot(vg)
head(vg)
tail(vg)
vg[vg$dist <= 3000, ]

# troubleshooting


# --- Morans I ---
# subset dataset for Moran's I
set.seed(14)
n.samp <- 20000
df.moran <- df[sample(nrow(df), n.samp), ]
coords <- as.matrix(df.moran[, c('x', 'y')])
t = 100 # distance threshold for neighbors (e.g., 1000 meters)
nb <- dnearneigh(coords, d1 = 0, d2 = t)
lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)
moran.test(df.moran$swe_peak, lw, zero.policy = TRUE)

thresholds <- c(100, 250, 500, 1000, 2000, 5000)

for (t in thresholds) {
  
  nb <- dnearneigh(coords, d1 = 0, d2 = t)
  lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)
  
  mt <- moran.test(df.moran$swe_peak, lw, zero.policy = TRUE)
  
  cat('-------------------------\n')
  cat('Distance:', t, 'm\n')
  cat('Moran I:', mt$estimate['Moran I statistic'], '\n')
  cat('Expected:', mt$estimate['Expectation'], '\n')
  cat('Z-score:', mt$statistic, '\n')
  cat('p-value:', mt$p.value, '\n')
}


# ----- df.50.thin ------
library(gstat)
df <- subset(df.50.thin, wy == 2023)
df <- df[complete.cases(df[, c('swe_peak', 'x', 'y')]), ]

# --- Morans I ---
coords <- as.matrix(df[, c('x', 'y')])
t = 1000 # distance threshold for neighbors (e.g., 1000 meters)
nb <- dnearneigh(coords, d1 = 0, d2 = t)
lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)
moran.test(df$swe_peak, lw, zero.policy = TRUE)

thresholds <- c(500, 1000, 2000, 5000)

for (t in thresholds) {
  
  nb <- dnearneigh(coords, d1 = 0, d2 = t)
  lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)
  
  mt <- moran.test(df$swe_peak, lw, zero.policy = TRUE)
  
  cat('-------------------------\n')
  cat('Distance:', t, 'm\n')
  cat('Moran I:', mt$estimate['Moran I statistic'], '\n')
  cat('Expected:', mt$estimate['Expectation'], '\n')
  cat('Z-score:', mt$statistic, '\n')
  cat('p-value:', mt$p.value, '\n')
}



# =======================================================================================
# Data Exploration
# =======================================================================================
# --- get df ---
fires <- c('creek', 'castle', 'caldor', 'dixie')
rds.dir <- 'data/processed/processed/rds/'
res <- '50m'
df.raw.file <- paste0(rds.dir, 'df_', res, '_raw.rds')

df.raw <- readRDS(df.raw.file)

df.raw <- df.raw %>%
  select(-x, -y, -cell)

df.by.fire <- split(df.raw, df.raw$fire)

# ----- density plot for elevation for burned/unburned -----
ggplot(df.raw,
       aes(x = elevation,
           color = burned,
           fill = burned)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~fire) +
  labs(
    x = 'Elevation (m)',
    y = 'Density'
  ) +
  theme_bw()



# -------- SWE ----------

# ----- check how many NAs -----
na.summary <- data.frame(
  variable = names(df.500),
  prop.na = sapply(df.500, function(x) mean(is.na(x)))
)
na.summary <- na.summary[order(-na.summary$prop.na), ]
print(na.summary)

# how many rows would be removed if we drop rows with NAs?
mean(complete.cases(df.50))
mean(complete.cases(df.500))


# ----- Check correlation among predictors -----
cor.mats <- lapply(df.by.fire, function(df) {
  
  num.vars <- df %>%
    select(-cell, -fire, -wy, -swe_peak) %>%
    select(where(is.numeric))
  
  cor(
    num.vars,
    use = 'pairwise.complete.obs'
  )
})

for (fire.name in names(cor.mats)) {
  
  cor.mat <- cor.mats[[fire.name]]
  
  corrplot(
    cor.mat,
    method = 'color',
    type = 'upper',
    order = 'hclust',
    tl.cex = 0.7,
    addCoef.col = 'black',
    number.cex = 0.5,
    col = colorRampPalette(c('blue', 'white', 'red'))(200),
    tl.col = 'black',
    diag = FALSE,
    title = fire.name,
    mar = c(0, 0, 2, 0)
  )
}

cor.strong <- lapply(cor.mats, function(cor.mat) {
  
  as.data.frame(as.table(cor.mat)) %>%
    filter(Var1 != Var2) %>%
    filter(abs(Freq) > 0.5) %>%
    rowwise() %>%
    mutate(
      pair = paste(
        sort(c(as.character(Var1), as.character(Var2))),
        collapse = '___'
      )
    ) %>%
    ungroup() %>%
    distinct(pair, .keep_all = TRUE) %>%
    arrange(desc(abs(Freq))) %>%
    select(-pair)
  
})

cor.strong.all <- bind_rows(
  cor.strong,
  .id = 'fire'
)

print(cor.strong.all, n = Inf)




# ------------- check relationship between predictors and response variable ---------

# ----- correlations between variable and response -----
# not actually that useful because relationships are nonlinear. use gam instead!

# print cor value for each predictor
df.50.sub <- df.50[complete.cases(df.50[.])]
vars <- c(
  'cover_ground_frac',
  'gap_gap_pct',
  'gap_dist_to_canopy_mean',
  'ht_zskew',
  'ht_zkurt',
  'ht_zentropy',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zpcum6',
  'ht_zpcum9',
  'cbibc',
  'rad_dsm_accum',
  'rad_dtm_accum',
  'topo_aspect_cos',
  'topo_aspect_sin',
  'topo_slope',
  'topo_tpi150',
  'topo_elev',
  'pr',
  'tmmn'
)

vars <- df.50 %>%
  select(-c(cell, x, y, tmmn, pr, swe_peak, wy, topo_elev, topo_slope, topo_tpi150, topo_tpi2010, rad_dtm_accum)) %>%
  names()

# group by year
cor.by.wy <- df.50 %>%
  group_by(wy) %>%
  summarise(
    across(
      all_of(vars),
      ~ cor(.x, swe_peak, use = 'complete.obs'),
      .names = 'cor_{.col}'
    )
  )

# make easier to read
cor.long <- cor.by.wy %>%
  pivot_longer(
    cols = -wy,
    names_to = 'variable',
    values_to = 'correlation'
  ) %>%
  mutate(variable = gsub('cor_', '', variable))

# this plot! shows box plots of within-yar correlations with SWE among my predictors
ggplot(cor.long, aes(x = variable, y = correlation)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  labs(title = 'Within-year correlations with SWE')

cor(df.50$swe_peak, df.50$ht_zpcum1, use = 'complete.obs')
# -0.090
cor(sqrt(df.50$swe_peak), df.50$ht_zpcum1, use = 'complete.obs')
# -0.097
cor(df.50$swe_peak, df.50$ht_zpcum2, use = 'complete.obs')
# -0.127
cor(sqrt(df.50$swe_peak), df.50$ht_zpcum2, use = 'complete.obs')
# -0.147

canopy.vars.temp <- c('ht_zpcum1', 'ht_zpcum2', 'gap_gap_pct', 'rad_dsm_accum', 'cover_ground_frac')
for (v in canopy.vars.temp) {
  cat('\n---', v, '---\n')
  
  cor1 <- cor(df.50$swe_peak, df.50[[v]], use = 'complete.obs')
  cor2 <- cor(sqrt(df.50$swe_peak), df.50[[v]], use = 'complete.obs')
  
  cat('swe:', round(cor1, 4), '\n')
  cat('sqrt swe:', round(cor1, 4), '\n')
}


# ----- plot predictor vs response -----

# SWE
vars <- c(
  'cover_ground_frac',
  'gap_gap_pct',
  'gap_dist_to_canopy_mean',
  'ht_zskew',
  'ht_zkurt',
  'ht_zentropy',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zpcum6',
  'ht_zpcum9',
  'cbibc',
  'rad_dsm_accum',
  'rad_dtm_accum',
  'topo_slope',
  'topo_tpi150',
  'topo_elev'
)

# pivot long so that it's better for plotting
plot.df <- df.50 %>%
  select(cell, wy, swe_peak, all_of(vars)) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = 'predictor',
    values_to = 'value'
  ) %>%
  filter(!is.na(value), !is.na(swe_peak))

# create sample df
set.seed(123)
# plot function
plot_var_wy <- function(df, var) {
  
  d <- df %>%
    filter(!is.na(.data[[var]]), !is.na(swe_peak))
  
  # sample to keep it fast
  set.seed(123)
  d <- d %>% slice_sample(n = min(200000, nrow(d)))
  
  ggplot(d, aes(x = .data[[var]], y = swe_peak)) +
    geom_point(alpha = 0.03, size = 0.3) +
    geom_smooth(method = 'gam', formula = y ~ s(x), se = FALSE, color = 'blue') +
    facet_wrap(~ wy) +
    coord_cartesian(ylim = c(0, 4)) +
    theme_bw() +
    labs(
      x = var,
      y = 'swe_peak',
      title = paste('SWE vs', var, 'by water year')
    )
}

plot_var_wy(df.50, 'topo_elev')
plot_var_wy(df.50, 'topo_tpi150')
plot_var_wy(df.50, 'topo_slope')
plot_var_wy(df.50, 'topo_aspect_sin')
plot_var_wy(df.50, 'topo_aspect_cos')
plot_var_wy(df.50, 'rad_dtm_accum')
plot_var_wy(df.50, 'pr')
plot_var_wy(df.50, 'tmmn')


# --- fit a GAM with multiple predictors ---
gam <- sqrt(swe) ~ topo_elev + tpi150 + rad_dtm_accum + topo_slope 


hist(df.50[["topo_elev"]])

# normalish

hist(df.50[["rad_dtm_accum"]])
# normal

hist(df.50[["topo_slope"]])
# skewed right

hist(df.50[["topo_tpi150"]])
# normal

hist(df.50[["pr"]])
# skewed right

hist(df.50[["tmmn"]])
# multimodal but somewhat normal

hist(df.50[["swe_peak"]])
# skewed right

hist(df.50[["sqrt_swe"]])
# still skewed right but better

# loop through all variables
par(mfrow = c(3, 3))  # adjust layout

for (v in vars) {
  hist(df.50[[v]], main = v, xlab = v)
}

par(mfrow = c(1, 1))  # reset



# --- plot relationships between variables and swe --
library(ggplot2)
library(patchwork)

set.seed(123)
df.sample <- df.50[sample(nrow(df.50), 10000), ]

p <- list()
for (v in vars) {
  p[[v]] <- ggplot(df.sample, aes(x = .data[[v]], y = swe_peak)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'gam') +
    labs(title = v)
}

wrap_plots(p, ncol = 4)

# look at specific correlations
cor(df.50$tmmn, df.50$swe_peak, use = 'complete.obs')
cor(df.50$pr, df.50$swe_peak, use = 'complete.obs')
cor(df.50$rad_dtm_accum, df.50$topo_aspect_cos, use = 'complete.obs')
cor(df.50$rad_dtm_accum, df.50$swe_peak, use = 'complete.obs')
cor(df.50$swe_peak, df.50$topo_aspect_cos, use = 'complete.obs')



# --- plot/explore transformations of predictors ---

# slope
hist(
  df.50$topo_slope,
  main = 'Slope distribution',
  xlab = 'Slope',
  breaks = 50
)

hist(
  (df.50$topo_slope)^2,
  main = 'Slope distribution',
  xlab = 'Slope',
  breaks = 50
)
smoothScatter(log(df.50$topo_slope + 0.01), df.50$swe_peak)
smoothScatter(sqrt(df.50$topo_slope + 0.01), df.50$swe_peak)
smoothScatter(log(df.50$topo_slope + 0.01), df.50$swe_peak)






# ----- explore GAMs for certain variables classes -----
library(mgcv)
library(dplyr)

tpi.vars <- names(df.raw)[grepl('^tpi', names(df.raw))]

tpi.results <- lapply(df.by.fire, function(df) {
  
  lapply(tpi.vars, function(var) {
    
    form <- as.formula(
      paste0('swe_peak ~ wy + s(', var, ', k = 10)')
    )
    
    mod <- bam(
      form,
      data = df,
      method = 'fREML',
      discrete = TRUE,
      nthreads = 4
    )
    
    mod.summary <- summary(mod)
    
    data.frame(
      variable = var,
      r.squared = mod.summary$r.sq,
      dev.expl = mod.summary$dev.expl,
      AIC = AIC(mod),
      edf = mod.summary$s.table[1, 'edf']
    )
    
  }) %>%
    bind_rows()
  
})

tpi.results <- bind_rows(
  tpi.results,
  .id = 'fire'
)


# ----------- SDD ------------

# ----- check how many NAs -----
# should be 0 since we already filtered them out
na.summary <- data.frame(
  variable = names(df.500),
  prop.na = sapply(df.500, function(x) mean(is.na(x)))
)
na.summary <- na.summary[order(-na.summary$prop.na), ]
print(na.summary)

# how many rows would be removed if we drop rows with NAs?
mean(complete.cases(df.50))
mean(complete.cases(df.500))


# --- Check correlation among predictors ---
num.vars <- df.500 |>
  select(-cell, -wy)

cor.mat <- cor(num.vars, use = 'pairwise.complete.obs')

corrplot(cor.mat, method = 'color')
corrplot(
  cor.mat,
  method = 'color',
  type = 'upper',
  order = 'hclust',
  tl.cex = 0.7,
  addCoef.col = 'black',
  number.cex = 0.5,
  col = colorRampPalette(c('blue', 'white', 'red'))(200),
  tl.col = 'black',
  diag = FALSE
)

cor.df <- as.data.frame(as.table(cor.mat))

cor.strong <- cor.df %>%
  filter(Var1 != Var2) %>%
  filter(abs(Freq) > 0.5) %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(as.character(Var1), as.character(Var2))), collapse = '___')) %>%
  ungroup() %>%
  distinct(pair, .keep_all = TRUE) %>%
  arrange(desc(abs(Freq))) %>%
  select(-pair)

cor.strong


#  ----- check relationship between predictors and response variable -----


# --- compare tpi ---
# since TPI are highly correlated with eachother, I want to include one in the model. Here I am seeing which one is the most correlated with SDD
# SDD
df.500.sub <- df.500[complete.cases(df.500[, c('topo_tpi150', 'topo_tpi510', 'topo_tpi1200', 'sdd')]), ]
cor(df.500.sub$topo_tpi150, df.500.sub$sdd) # -0.005
cor(df.500.sub$topo_tpi510, df.500.sub$sdd)  # -0.013
cor(df.500.sub$topo_tpi1200, df.500.sub$sdd) # -0.015

smoothScatter(df.500.sub$topo_tpi150, df.500.sub$sdd)
smoothScatter(df.500.sub$topo_tpi510, df.500.sub$sdd)
smoothScatter(df.500.sub$topo_tpi1200, df.500.sub$sdd)

cor(abs(df.500.sub$topo_tpi150), df.500.sub$sdd) # 0.072
cor(abs(df.500.sub$topo_tpi1200), df.500.sub$sdd) # 0.064

# ---- Compare correlations between variable and response ---
# print cor value for each predictor
df.500.sub <- df.500[complete.cases(df.500[.])]
vars <- c(
  'cover_ground_frac',
  'gap_gap_pct',
  'gap_dist_to_canopy_mean',
  'ht_zskew',
  'ht_zkurt',
  'ht_zentropy',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zpcum6',
  'ht_zpcum9',
  'cbibc',
  'rad_dsm_accum',
  'rad_dtm_accum',
  'topo_aspect_sin',
  'topo_slope',
  'topo_tpi1200',
  'topo_elev'
)

# group by year
cor.by.wy <- df.500 %>%
  group_by(wy) %>%
  summarise(
    across(
      all_of(vars),
      ~ cor(.x, swe_peak, use = 'complete.obs'),
      .names = 'cor_{.col}'
    )
  )

# make easier to read
cor.long <- cor.by.wy %>%
  pivot_longer(
    cols = -wy,
    names_to = 'variable',
    values_to = 'correlation'
  ) %>%
  mutate(variable = gsub('cor_', '', variable))

# this plot! shows box plots of within-yar correlations with SWE among my predictors
ggplot(cor.long, aes(x = variable, y = correlation)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  labs(title = 'Within-year correlations with SDD')

# print correlations of canopy vars with sdd
canopy.vars.temp <- c('ht_zpcum1', 'ht_zpcum2', 'gap_gap_pct', 'rad_dsm_accum', 'cover_ground_frac')
for (v in canopy.vars.temp) {
  cat('\n---', v, '---\n')
  
  cor1 <- cor(df.500$sdd, df.500[[v]], use = 'complete.obs')
 # cor2 <- cor(sqrt(df.500$sdd), df.500[[v]], use = 'complete.obs')
  
  cat('sdd:', round(cor1, 4), '\n')
#  cat('sqrt swe:', round(cor1, 4), '\n')
}

# --- compute correlations with sdd ---
cor.df <- df.500 %>%
  select(where(is.numeric)) %>%
  summarise(across(-sdd, ~ cor(.x, sdd, use = 'complete.obs'))) %>%
  tidyr::pivot_longer(cols = everything(), 
                      names_to = 'variable', 
                      values_to = 'correlation')

# sort by strength
cor.df <- cor.df %>%
  arrange(desc(abs(correlation)))

# plot
ggplot(cor.df, aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col() +
  coord_flip() +
  labs(
    title = 'Correlation with SDD',
    x = 'Variable',
    y = 'Pearson correlation'
  ) +
  theme_minimal()


# ---  plot predictor vs response ---
plot.df <- df.500 %>%
  select(cell, wy, sdd, all_of(vars)) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = 'predictor',
    values_to = 'value'
  ) %>%
  filter(!is.na(value), !is.na(sdd))

# create sample df
set.seed(123)
# plot function
plot_var_wy <- function(df, var) {
  
  d <- df %>%
    filter(!is.na(.data[[var]]), !is.na(sdd))
  
  # sample to keep it fast
  set.seed(123)
  d <- d %>% slice_sample(n = min(200000, nrow(d)))
  
  p <- ggplot(d, aes(x = .data[[var]], y = sdd)) +
    geom_point(alpha = 0.03, size = 0.3) +
    geom_smooth(method = 'gam', formula = y ~ s(x), se = FALSE, color = 'blue') +
    facet_wrap(~ wy) +
    theme_bw() +
    labs(
      x = var,
      y = 'sdd',
      title = paste('SDD vs', var, 'by water year')
    )
  
  return(p)
}

for (v in vars) {
  print(plot_var_wy(df.500, v))
}

plot_var_wy(df.500, 'topo_elev')
plot_var_wy(df.500, 'topo_tpi150')
plot_var_wy(df.500, 'topo_slope')
plot_var_wy(df.500, 'topo_aspect_sin')
plot_var_wy(df.500, 'topo_aspect_cos')
plot_var_wy(df.50, 'rad_dtm_accum')
plot_var_wy(df.50, 'pr')
plot_var_wy(df.50, 'tmmn')


# --- fit a GAM with multiple predictors ---
gam <- sdd ~ topo_elev + tpi150 + rad_dtm_accum + topo_slope 


hist(df.50[["topo_elev"]])

# normalish

hist(df.50[["rad_dtm_accum"]])
# normal

hist(df.50[["topo_slope"]])
# skewed right

hist(df.50[["topo_tpi150"]])
# normal

hist(df.50[["pr"]])
# skewed right

hist(df.50[["tmmn"]])
# multimodal but somewhat normal

hist(df.50[["swe_peak"]])
# skewed right

hist(df.50[["sqrt_swe"]])
# still skewed right but better

# loop through all variables
par(mfrow = c(3, 3))  # adjust layout

for (v in vars) {
  hist(df.50[[v]], main = v, xlab = v)
}

par(mfrow = c(1, 1))  # reset



# ---- plot relationships between variables and swe ---
library(ggplot2)
library(patchwork)

set.seed(123)
df.sample <- df.50[sample(nrow(df.50), 10000), ]

p <- list()
for (v in vars) {
  p[[v]] <- ggplot(df.sample, aes(x = .data[[v]], y = swe_peak)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'gam') +
    labs(title = v)
}

wrap_plots(p, ncol = 4)

# look at specific correlations
cor(df.50$tmmn, df.50$swe_peak, use = 'complete.obs')
cor(df.50$pr, df.50$swe_peak, use = 'complete.obs')
cor(df.50$rad_dtm_accum, df.50$topo_aspect_cos, use = 'complete.obs')
cor(df.50$rad_dtm_accum, df.50$swe_peak, use = 'complete.obs')
cor(df.50$swe_peak, df.50$topo_aspect_cos, use = 'complete.obs')



# --- plot/explore transformations of predictors ---

# slope
hist(
  df.50$topo_slope,
  main = 'Slope distribution',
  xlab = 'Slope',
  breaks = 50
)

hist(
  (df.50$topo_slope)^2,
  main = 'Slope distribution',
  xlab = 'Slope',
  breaks = 50
)
smoothScatter(log(df.50$topo_slope + 0.01), df.50$swe_peak)
smoothScatter(sqrt(df.50$topo_slope + 0.01), df.50$swe_peak)
smoothScatter(log(df.50$topo_slope + 0.01), df.50$swe_peak)



