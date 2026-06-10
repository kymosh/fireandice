packages <- c('sf', 'tidyverse', 'terra', 'ggplot2', 'blockCV')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# this code creates spatial blocks that will be used to divide the data into train/test data

# -------------------------------------------------------------------------
# Spatial blocking setup
# -------------------------------------------------------------------------

fire <- 'creek'
epsg <- 32611
block.m <- 8000
test.prop <- 0.20
set.seed(15)

# study area polygon
study <- st_read(paste0('data/processed/processed/shp/studyarea_extents/study_extent_', fire, '_simple.shp')) %>%
  st_transform(epsg)

# dataframe
dir <- paste0('data/processed/processed/rds/', fire) 
df.50 <- readRDS(file.path(dir, paste0(fire, '_long_df_50m_clean.rds')))



# your model data as points
dat.sf <- st_as_sf(df.50, coords = c('x', 'y'), crs = epsg, remove = FALSE)

sb <- cv_spatial(
  x = dat.sf,
  column = NULL,
  r = NULL,
  size = block.m,
  k = 5,
  selection = 'systematic',
  iteration = 100,
  biomod2 = FALSE,
  progress = TRUE,
  report = TRUE
)

cv_plot(sb)

# create test/train categories
sb$blocks$split <- if_else(sb$blocks$folds == 1, 'test', 'train')

# apply blocks to data
dat.blocked <- dat.sf %>%
  mutate(
    fold_id = sb$folds_ids,
    split = if_else(fold_id == 1, 'test', 'train')
  )

# plot test/train
ggplot(sb$blocks) +
  geom_sf(aes(fill = split)) +
  theme_bw()

# proportion numbers
sb$blocks %>%
  st_drop_geometry() %>%
  count(folds) %>%
  mutate(prop = n / sum(n))


dat.blocked %>%
  st_drop_geometry() %>%
  count(split) %>%
  mutate(prop = n / sum(n))

# remove geometry
dat.blocked <- st_drop_geometry(dat.blocked)
# save
saveRDS(dat.blocked, file.path(dir, paste0(fire, '_df_50m.rds')))
saveRDS(dat.blocked, paste0('J:/Fire_Snow/fireandice/data/processed/processed/rds/', fire, '_df_50m.rds'))
saveRDS(dat.blocked, paste0('G:/Fire_Snow_Dynamics_backup/data/processed/processed/rds/', fire, '_df_50m.rds'))






# ----- manual spatial blocking -----

# make square grid blocks
blocks <- st_make_grid(
  study,
  cellsize = block.m,
  square = TRUE
) %>%
  st_as_sf() %>%
  st_intersection(study) %>%
  mutate(block_id = row_number())

# systematically choose test blocks
cent <- st_centroid(blocks)
xy <- st_coordinates(cent)

blocks <- blocks %>%
  mutate(
    col_id = floor((xy[, 1] - min(xy[, 1])) / block.m),
    row_id = floor((xy[, 2] - min(xy[, 2])) / block.m),
    split = if_else((col_id + row_id) %% 5 == 0, 'test', 'train')
  )

ggplot(blocks) +
  geom_sf(aes(fill = split)) +
  theme_bw()

nrow(blocks)

ggplot(blocks) +
  geom_sf(aes(fill = split)) +
  geom_sf_text(aes(label = block_id), size = 2) +
  theme_bw()


