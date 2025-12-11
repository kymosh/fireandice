packages <- c('terra', 'dplyr', 'ggplot2', 'elsa', 'ranger', 'raster')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
#                        Create Master Raster
# ==============================================================================

# read in initial data
canopy.metrics.50m <- readRDS('data/processed/processed/rds/canopy_metrics_50m_test.rds')
swe.df <- readRDS('data/processed/dataframes/filtered_swe_df.rds')

# convert canopy to df
canopy.df <- as.data.frame(canopy.metrics.50m, xy = T, na.rm = T)

# keep only rows where year is 2020
swe.2020 <- swe.df %>%
  filter(year == 2020)

# keep only pixels that are also in canopy.df
swe.trim <- semi_join(swe.2020, canopy.df, by = c('x', 'y'))

# keep only 2023 swe data
swe.trim.clean <- swe.trim %>% 
  mutate(swe_april2020 = `2020_0414_superswe`) %>%
  dplyr::select(x, y, swe_april2020, cbibc, hli, elev, pr, tmmx, tmmn, forest_type)

# 30 pixels are missing swe data from the canopy df, probably ones that have low elevtions. 
canopy.clean <- semi_join(canopy.df, swe.trim.clean, by = c('x', 'y'))

# sanity check
nrow(canopy.clean) == nrow(swe.trim.clean)
# TRUE

# label all canopy metrics as such
canopy.clean <- canopy.clean %>%
  rename_with(~ paste0('cm_', .x), -c(x, y))

# combine DFs!
master.df <- left_join(swe.trim.clean, canopy.clean, by = c('x', 'y'))
nrow(master.df)

# convert back to raster
master.raster <- rast(master.df[, c('x', 'y', 'swe_april2020')],
                      type = 'xyz',
                      crs = 'EPSG:32611')

plot(master.raster$swe_april2020, main = 'SWE (m)')



# ==============================================================================
#                        Test/Adjust for Spatial Autocorrelation
# ==============================================================================
# must be rasterlater (not spatraster) for morans i
master.rasterlayer <- raster(master.raster)

d1 <- 0
d2 <- 75

mi <- elsa::moran(master.rasterlayer, d1, d2)
print(mi)
# it's high as we would expect

# spatial thinning to subsample points
# 1 point per 3x3 block(every ~150m)
master.df$block_id <- paste0(
  floor(master.df$x / 150),
  "_",
  floor(master.df$y / 150)
)

master.thin <- master.df %>%
  dplyr::group_by(block_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

# convert back to raster for morans i
thin.rast <- rast(master.thin[, c('x', 'y', 'swe_april2020')],
                  type = 'xyz',
                  crs = 'EPSG:32611')
thin.rast <- raster(thin.rast) # back to rasterlayer
plot(thin.rast, main = "Thinned SWE Raster")

# check morans i again
d2 <- 200
mi <- elsa::moran(thin.rast, d1, d2)
mi
# .49, moderate

# ==============================================================================
#                        Random Forest Model
# ==============================================================================

# define predictor
exclude.cols <- c('x', 'y', 'block_id', 'swe_april2020', 'cbibc')
pred.cols <- setdiff(names(master.thin), exclude.cols)

# make forest type a factor
master.thin <- master.thin %>%
  mutate(across(intersect('forest_type,', names(.)), as.factor))

set.seed(42)

# train/test split
n <- nrow(master.thin)
train.idx <- sample(seq_len(n), size = floor(0.8 * n))
train.data <- master.thin[train.idx, ]
test.data <- master.thin[-train.idx, ]

rf.formula <- as.formula(paste('swe_april2020 ~', paste(pred.cols, collapse = ' + ')))
rf.formula

rf.model <- ranger(
  formula = rf.formula,
  data = train.data,
  num.trees = 500,
  mtry = floor(sqrt(length(pred.cols))),
  importance = 'permutation',
  seed = 42
)

rf.pred <- predict(rf.model, data = test.data)$predictions
obs     <- test.data$swe_april2020

rmse <- sqrt(mean((rf.pred - obs)^2, na.rm = TRUE))
r2   <- cor(rf.pred, obs)^2

rmse
r2

plot(obs, rf.pred,
     xlab = 'Observed SWE April 2020',
     ylab = 'Predicted SWE April 2020')
abline(0, 1, lty = 2)

vi <- rf.model$variable.importance[order(rf.model$variable.importance, decreasing = TRUE)]
vi

# ------ plots
barplot(vi,
        las = 2,
        main = 'RF variable importance',
        ylab = 'Permutation importance')

library(ggplot2)
library(tibble)

vi_df <- tibble(
  variable = names(vi),
  importance = as.numeric(vi)
) %>%
  arrange(desc(importance))

ggplot(vi_df, aes(x = importance, y = reorder(variable, importance))) +
  geom_col(fill = 'steelblue') +
  labs(
    title = 'RF Variable Importance',
    x = 'Permutation Importance',
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.y = element_text(size = 10)
  )



#------------- recheck morans on full dataset -------------

master.thin$rf_pred_full <- predict(rf.model, data = master.thin)$predictions
master.thin$rf_resid     <- master.thin$rf_pred_full - master.thin$swe_april2020

resid.rast <- rast(
  master.thin[, c('x', 'y', 'rf_resid')],
  type = 'xyz',
  crs  = 'EPSG:32611'
)
resid.rast <- raster(resid.rast)

mi.resid <- elsa::moran(resid.rast, d1 = 0, d2 = 200)  # 200 m to match thinning scale
mi.resid





