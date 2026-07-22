packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest', 'ranger', 'tictoc', 'mgcv', 'plotmyGAM')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# get dataframe
set.seed(61)
dir <- 'data/processed/processed/rds/' 

df.50.raw <- readRDS(file.path(dir, 'df_50m_raw.rds'))
df.50.balanced <- readRDS(file.path(dir, 'df_50m_raw_balanced.rds')) 

df.50.raw.test <- df.50.raw %>%
  group_by(fire) %>%
  slice_sample(n = 10000) %>%
  ungroup()

df.50.balanced.test <- df.50.balanced %>%
  group_by(fire) %>%
  slice_sample(n = 10000) %>%
  ungroup()


burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)
 

# # trim to years 2023 and up
# df.raw <- df.raw.0 %>%
#   filter(wy %in% c(2023, 2024, 2025))
# 
# 
# df.50 <- df.50.0 %>%
#   semi_join(
#     df.raw %>% select(cell, wy),
#     by = c('cell', 'wy')
#   )



# ==============================================================================
#  Results DF and helper functions creation
# ==============================================================================

get.metrics <- function(fitted.model, model.name, fire.name) {
  
  s <- summary(fitted.model)
  
  data.frame(
    fire = fire.name,
    model_name = model.name,
    r.squared = s$r.sq,
    dev.expl = s$dev.expl,
    AIC = AIC(fitted.model),
    edf = sum(s$edf)
  )
}


# ==============================================================================
# Stage 1 Modeling - Single family predictors
# ==============================================================================

# ----- plot peak swe for all fires -----
plot.df <- df.50.raw %>%
  select(fire, wy, swe_peak) %>%
  mutate(
    Raw = swe_peak,
    `Square root` = sqrt(swe_peak)
  ) %>%
  pivot_longer(
    cols = c(Raw, `Square root`),
    names_to = 'Transformation',
    values_to = 'Peak_SWE'
  )

for (f in unique(plot.df$fire)) {
  
  p <- plot.df %>%
    filter(fire == f) %>%
    ggplot(aes(Peak_SWE)) +
    geom_density(fill = 'steelblue', alpha = 0.4) +
    facet_grid(wy ~ Transformation, scales = 'free_x') +
    labs(
      title = f,
      x = 'Peak SWE',
      y = 'Density'
    ) +
    theme_bw()
  
  print(p)
}


# ------------------------------ Topo-only Model ------------------------------

# topo variables beyond elevation
topo.vars <- c(
  'slope',
  'rad_dtm_accum',
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results <- data.frame()

# ----- Stepwise 1 -----

topo.vars <- c(
  'slope',
  'rad_dtm_accum',
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results <- data.frame()

for (fire.name in unique(df.50.raw.test$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation baseline
  model.elev <- bam(sqrt(swe_peak) ~
                      wy +
                      s(elevation, k = 10),
                    data = fire.df,
                    method = 'ML')
  
  topo.results <- bind_rows(
    topo.results,
    get.metrics(
      fitted.model = model.elev,
      model.name = 'elevation',
      fire.name = fire.name
    )
  )
  

  # test each additional topographic variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + s(elevation, k = 10) + s(' , var, ', k = 10)'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'ML'
    )
    
    topo.results <- bind_rows(
      topo.results,
      get.metrics(
        fitted.model = model,
        model.name = paste0('elevation + ', var), 
        fire.name = fire.name
      )
    )
    
  }
}

topo.results <- topo.results %>%
  group_by(fire) %>%
  mutate(
    AIC.elevation = AIC[model_name == 'elevation'],
    delta.AIC.elevation = AIC - AIC.elevation,
    delta.r.squared = r.squared - r.squared[model_name == 'elevation']
  ) %>%
  ungroup()

topo.results %>%
  arrange(fire, AIC) %>%
  print(n = Inf)

# shows that radiation definitely adds the most. Continue on to stepwise to see if adding additional variables improves the model

# ----- stepwise 2 -----

# updated vars
topo.vars <- c(
  'slope',
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad <- bam(
    sqrt(swe_peak) ~ wy + s(elevation) + s(rad_dtm_accum),
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics(
      fitted.model = topo.elev.rad,
      model.name = 'topo.elev.rad',
      fire.name = fire.name
    )
  )
  
 # test each additional variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sqrt(swe_peak) ~ wy + s(elevation) + s(rad_dtm_accum) + 
             s(', var, ')')
    )
    
    model <- bam(model.formula,
                 data = fire.df,
                 method = 'fREML',
                 discrete = TRUE)
    
    # add results
    topo.results.step <- bind_rows(
      topo.results.step,
      get.metrics(
        fitted.model = model,
        model.name = paste0('topo.elev.rad.', var),
        fire.name = fire.name
        
    
      )
    )
    
  }
  
}

topo.results.step %>%
  arrange(fire, AIC)

topo.results.step.2 <- topo.results.step

# slope still seems to add enough to keep it in the model

# ----- stepwise 3 ------
# updated vars
topo.vars <- c(
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad.slope <- bam(
    sqrt(swe_peak) ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope),
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics(
      fitted.model = topo.elev.rad,
      model.name = 'topo.elev.rad',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sqrt(swe_peak) ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + 
             s(', var, ')')
    )
    
    model <- bam(model.formula,
                 data = fire.df,
                 method = 'fREML',
                 discrete = TRUE)
    
    # add results
    topo.results.step <- bind_rows(
      topo.results.step,
      get.metrics(
        fitted.model = model,
        model.name = paste0('topo.elev.rad.', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

topo.results.step %>%
  arrange(fire, AIC)

topo.results.step.3 <- topo.results.step

# aspect_sin adds a decent amount still across the board

# ----------------------- Canopy-only Model -----------------------

# ------ stepwise 1 -----
# canopy variables
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'ht_zmax',
  'gap_dist_to_canopy_mean',
  'gap_percent',
  'rad_dsm_accum'
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # wy only baseline
  base.model <- bam(
    sqrt(swe_peak) ~ wy,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics(
      fitted.model = base.model,
      model.name = 'wy only',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in canopy.vars) {
    
    model.formula <- as.formula(
      paste0('sqrt(swe_peak) ~ wy +  
             s(', var, ')')
    )
    
    model <- bam(model.formula,
                 data = fire.df,
                 method = 'fREML',
                 discrete = TRUE)
    
    # add results
    canopy.results.step <- bind_rows(
      canopy.results.step,
      get.metrics(
        fitted.model = model,
        model.name = paste0('wy + ', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

canopy.results.step %>%
  arrange(fire, AIC)

canopy.results.step.1 <- canopy.results.step

# zpcum6 wins

# ------ stepwise 1 -----
# canopy variables
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'ht_zmax',
  'gap_dist_to_canopy_mean',
  'gap_percent',
  'rad_dsm_accum'
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # wy only baseline
  base.model <- bam(
    sqrt(swe_peak) ~ wy,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics(
      fitted.model = base.model,
      model.name = 'wy only',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in canopy.vars) {
    
    model.formula <- as.formula(
      paste0('sqrt(swe_peak) ~ wy +  
             s(', var, ')')
    )
    
    model <- bam(model.formula,
                 data = fire.df,
                 method = 'fREML',
                 discrete = TRUE)
    
    # add results
    canopy.results.step <- bind_rows(
      canopy.results.step,
      get.metrics(
        fitted.model = model,
        model.name = paste0('wy + ', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

canopy.results.step %>%
  arrange(fire, AIC)

canopy.results.step.2 <- canopy.results.step

#  xx wins

# ----- Elevation only, then adding 1 additional topo var -----
for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation baseline
  model.elev <- bam(sqrt(swe_peak) ~
                      wy +
                      s(elevation, k = 10),
                    data = fire.df,
                    method = 'ML')
  
  topo.results <- bind_rows(
    topo.results,
    get.metrics(
      fitted.model = model.elev,
      model.name = 'elevation',
      fire.name = fire.name
    )
  )
# ----- null model with just spatial term *old -----
gam.null <- bam(
  sqrt(swe_peak) ~ factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df.check,
  method = 'fREML',
  discrete = TRUE
)
# plot of modeled spatial structure
plot(
  gam.null,
  select = 1,
  scheme = 2,
  too.far = 0.05,
  main = "Spatial smooth: s(x, y)"
)

results <- rbind(
  results,
  get.metrics(gam.null, "Null")
)
gam.check(gam.test)

# ----- null topo *old -----
topo <- bam(
  sqrt(swe_peak) ~
    wy +
    s(elevation) +
    s(rad_dtm_accum) +
    s(slope),
  data = df.50.raw.test,
  method = "fREML",
  discrete = TRUE
)

summary(gam.topo)
gam.check(gam.topo)

# add results
results <- rbind(
  results,
  get.metrics(gam.topo, "Topo")
)

# plot residuals
plot(
  fitted(gam.topo),
  residuals(gam.topo),
  pch = 16,
  cex = 0.1
)
abline(h = 0)

qqnorm(
  sample(residuals(gam.topo), 50000)
)
qqline(
  sample(residuals(gam.topo), 50000)
)

df.check$resid.topo <- residuals(gam.topo)

ggplot(df.check,
       aes(x, y, color = resid.topo)) +
  geom_point(size = 0.3) +
  scale_color_gradient2(
    midpoint = 0,
    limits = c(-0.35, 0.35)
  ) +
  coord_equal()

summary(df.check$resid.topo)

quantile(
  df.check$resid.topo,
  probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
)


gam.topo <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  data = df.check,
  method = "fREML",
  discrete = TRUE
)

summary(gam.topo)
gam.check(gam.topo)

# add results
results <- rbind(
  results,
  get.metrics(gam.topo, "Topo")
)

# plot residuals
plot(
  fitted(gam.topo),
  residuals(gam.topo),
  pch = 16,
  cex = 0.1
)
abline(h = 0)

qqnorm(
  sample(residuals(gam.topo), 50000)
)
qqline(
  sample(residuals(gam.topo), 50000)
)

df.check$resid.topo <- residuals(gam.topo)

ggplot(df.check,
       aes(x, y, color = resid.topo)) +
  geom_point(size = 0.3) +
  scale_color_gradient2(
    midpoint = 0,
    limits = c(-0.35, 0.35)
  ) +
  coord_equal()

summary(df.check$resid.topo)

quantile(
  df.check$resid.topo,
  probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
)


# ----- null topo with spatial term *old -----
gam.topo.sxy <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(x, y, bs = "tp", k = 200),
  data = df.check,
  method = "fREML",
  discrete = TRUE
)

summary(gam.topo.sxy)
gam.check(gam.topo.sxy)

# add results
results <- rbind(
  results,
  get.metrics(gam.topo.sxy, "Topo with s(x,y)")
)


df.check$resid.topo <- residuals(gam.topo.sxy)

ggplot(df.check,
       aes(x, y, color = resid.topo)) +
  geom_point(size = 0.3) +
  scale_color_gradient2(
    midpoint = 0,
    limits = c(-0.35, 0.35)
  ) +
  coord_equal()

summary(df.check$resid.topo)

quantile(
  df.check$resid.topo,
  probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
)


gam.topo <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  data = df.check,
  method = "fREML",
  discrete = TRUE
)

summary(gam.topo)
gam.check(gam.topo)

# add results
results <- rbind(
  results,
  get.metrics(gam.topo, "Topo")
)

# plot residuals
plot(
  fitted(gam.topo),
  residuals(gam.topo),
  pch = 16,
  cex = 0.1
)
abline(h = 0)

qqnorm(
  sample(residuals(gam.topo), 50000)
)
qqline(
  sample(residuals(gam.topo), 50000)
)

df.check$resid.topo <- residuals(gam.topo)

ggplot(df.check,
       aes(x, y, color = resid.topo)) +
  geom_point(size = 0.3) +
  scale_color_gradient2(
    midpoint = 0,
    limits = c(-0.35, 0.35)
  ) +
  coord_equal()

summary(df.check$resid.topo)

quantile(
  df.check$resid.topo,
  probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
)


































# ----- ** topo + best canopy vars ** -----
gam.topo.canopy.best <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned)
    ,
  data = df.check,
  method = "fREML",
  discrete = TRUE
)

# add results
results <- rbind(
  results,
  get.metrics(gam.topo.canopy.best, "best, whole dataset")
)



plot(
  gam.topo.canopy.best,
  shade = TRUE,
  rug = FALSE,
  pages = 1,
  scale = 0
)

par(mfrow = c(1,2))
plot(
  gam.topo.canopy.best,
  select = 8,
  shade = TRUE,
  scale = 0
)

plot(
  gam.topo.canopy.best,
  select = 9,
  shade = TRUE,
  scale = 0
)

par(mfrow = c(1,2))

plot(
  gam.topo.canopy.best,
  select = 6,
  shade = TRUE,
  rug = TRUE,
  scale = 0,
  ylim = c(-0.3, 0.3)
)

plot(
  gam.topo.canopy.best,
  select = 7,
  shade = TRUE,
  rug = TRUE,
  scale = 0,
  ylim = c(-0.3, 0.3)
)

par(mfrow = c(1,1))
hist(
  df.check$gap_gap_pct[
    df.check$burned == "burned"
  ],
  breaks = 50
)

cor(df.check$gap_gap_pct,
    residuals(gam.topo))

summary(df.check$gap_gap_pct)
summary(df.check$ht_zmax)

par(mfrow = c(1,1))
boxplot(
  gap_gap_pct ~ burned,
  data = df.check
)

# calculate Morans I
idx <- sample(nrow(df.check), 5000)

df.moran <- df.check[idx, ]
df.moran$resid <- residuals(gam.topo.canopy.best)[idx]

library(spdep)
coords <- cbind(df.moran$x, df.moran$y)

nb <- knearneigh(coords, k = 8)
nb <- knn2nb(nb)

lw <- nb2listw(nb, style = "W")

moran.test(df.moran$resid, lw)



# ----- topo + te(canopyvars) -----
gam.topo.te.canopy <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    te(gap_gap_pct, ht_zmax)
  ,
  data = df.check,
  method = "fREML",
  discrete = TRUE
)

# add results
results <- rbind(
  results,
  get.metrics(gam.topo.te.canopy, "topo + te(gap_gap_pct, ht_zmax")
)

# ----- best + s(x.y) -----
gam.topo.canopy.best.sxy <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(x, y, k = 200)
  ,
  data = df.check,
  method = "fREML",
  discrete = TRUE
)

# add results
results <- rbind(
  results,
  get.metrics(gam.topo.x, "best")
)

# calculate Morans I
idx <- sample(nrow(df.check), 5000)

df.moran <- df.check[idx, ]
df.moran$resid <- residuals(gam.topo.canopy.best.sxy)[idx]

library(spdep)
coords <- cbind(df.moran$x, df.moran$y)

nb <- knearneigh(coords, k = 8)
nb <- knn2nb(nb)

lw <- nb2listw(nb, style = "W")

moran.test(df.moran$resid, lw)



# ----- best + s(x,y)
gam.topo.canopy.best.sxy <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(x, y, k = 200)
  ,
  data = df.check,
  method = "fREML",
  discrete = TRUE
)

# add results
results <- rbind(
  results,
  get.metrics(gam.topo.canopy.best.sxy, "best + s(x,y)")
)


# ----- explore with every metric -----
metrics <- c(
  "cover_canopy_open_2m",
  "cover_cover_2m",
  "cover_pzabove5",
  "cover_pzabove10",
  "cover_ground_frac",
  "gap_gap_pct",
  "gap_dist_to_gap_mean",
  "gap_dist_to_canopy_mean",
  "gap_dist_to_canopy_max",
  names(df.check)[grepl("^ht_", names(df.check))]
)

results.metrics <- data.frame()

for (metric in metrics) {
  
  message("Running ", metric)
  
  form <- as.formula(
    paste(
      "sqrt(swe_peak) ~",
      "factor(wy) +",
      "s(topo_elev) +",
      "s(rad_dtm_accum) +",
      "s(topo_slope) +",
      "s(topo_tpi150) +",
      "s(topo_tpi2010) +",
      paste0("s(", metric, ")")
    )
  )
  
  mod <- bam(
    form,
    data = df.check,
    method = "fREML",
    discrete = TRUE
  )
  
  s <- summary(mod)
  
  results.metrics <- rbind(
    results.metrics,
    data.frame(
      metric = metric,
      r.squared = s$r.sq,
      dev.expl = s$dev.expl,
      AIC = AIC(mod),
      edf = tail(s$edf, 1)
    )
  )
}

results.metrics <- results.metrics[
  order(-results.metrics$dev.expl),
]

results.metrics

# check corr
cor(
  df.check[, c(
    "gap_gap_pct",
    "cover_cover_2m",
    "ht_zmax"
  )],
  use = "complete.obs"
)
# ----- gam, sqrt swe -----
gam.test <- bam(
  sqrt(swe_peak) ~ 
    topo_elev + I(topo_elev^2) +
    topo_slope +
    topo_tpi150 +
    rad_dtm_accum +
    rad_dsm_accum +
    gap_gap_pct +
    gap_dist_to_canopy_mean +
    cover_ground_frac +
    ht_zpcum1 +
    ht_zpcum2 +
    ht_zskew +
    cbibc +
    factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df.test,
  method = 'fREML',
  discrete = TRUE
)

summary(gam.test)
gam.check(gam.test)
plot(gam.test, select = 1, scheme = 2)
vis.gam(gam.test, view = c('x', 'y'))

# residuals
df.test$gam.resid <- residuals(gam.test, type = 'pearson')
df.moran <- df.test[complete.cases(df.test[, c('x', 'y', 'gam.resid')]), ]
coords <- as.matrix(df.moran[, c('x', 'y')])

# Morans I
library(spdep)
d.vals <- c(250, 500, 1000, 2000)
for (d in d.vals) {
  
  nb <- dnearneigh(coords, 0, d)
  
  lw <- nb2listw(
    nb,
    style = 'W',
    zero.policy = TRUE
  )
  
  mi <- moran.test(
    df.moran$gam.resid,
    lw,
    zero.policy = TRUE
  )
  
  cat(
    '\n',
    'Distance:', d, 'm\n',
    '-------------------------\n',
    "Moran's I :", round(mi$estimate[['Moran I statistic']], 3), '\n',
    'Expected I:', round(mi$estimate[['Expectation']], 5), '\n',
    'Z-score   :', round(unname(mi$statistic), 2), '\n',
    'P-value   :', signif(mi$p.value, 3), '\n'
  )
}

plot(fitted(gam.test), residuals(gam.test))
qqnorm(residuals(gam.test))
qqline(residuals(gam.test))

# ----- gam, gamma family -----
gam.gamma <- bam(
  swe_peak ~ 
    topo_elev + I(topo_elev^2) +
    topo_slope + topo_tpi150 +
    rad_dtm_accum + rad_dsm_accum +
    gap_gap_pct + gap_dist_to_canopy_mean +
    cover_ground_frac +
    ht_zpcum1 + ht_zpcum2 + ht_zskew +
    cbibc +
    factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df.test,
  family = Gamma(link = 'log'),
  method = 'fREML',
  discrete = TRUE
)

summary(gam.gamma)
gam.check(gam.gamma)
plot(gam.gamma, select = 1, scheme = 2)
vis.gam(gam.gamma, view = c('x', 'y'))

# residuals
df.test$gam.resid <- residuals(gam.gamma, type = 'pearson')
df.moran <- df.test[complete.cases(df.test[, c('x', 'y', 'gam.resid')]), ]
coords <- as.matrix(df.moran[, c('x', 'y')])

# Morans I
library(spdep)
d.vals <- c(250, 500, 1000, 2000)
for (d in d.vals) {
  
  nb <- dnearneigh(coords, 0, d)
  
  lw <- nb2listw(
    nb,
    style = 'W',
    zero.policy = TRUE
  )
  
  mi <- moran.test(
    df.moran$gam.resid,
    lw,
    zero.policy = TRUE
  )
  
  cat(
    '\n',
    'Distance:', d, 'm\n',
    '-------------------------\n',
    "Moran's I :", round(mi$estimate[['Moran I statistic']], 3), '\n',
    'Expected I:', round(mi$estimate[['Expectation']], 5), '\n',
    'Z-score   :', round(unname(mi$statistic), 2), '\n',
    'P-value   :', signif(mi$p.value, 3), '\n'
  )
}

plot(fitted(gam.gamma), residuals(gam.gamma))
qqnorm(residuals(gam.gamma))
qqline(residuals(gam.gamma))



# ----- reduced canopy metrics -----
gam.test <- bam(
  sqrt(swe_peak) ~ 
    topo_elev + I(topo_elev^2) +
    topo_slope +
    topo_tpi150 +
    rad_dtm_accum +
    gap_dist_to_canopy_mean +
    cover_ground_frac +
    ht_zpcum2 +
    factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df.test,
  method = 'fREML',
  discrete = TRUE
)

summary(gam.test)
gam.check(gam.test)
df.test$gam.resid <- residuals(gam.test, type = 'pearson')
df.moran <- df.test[complete.cases(df.test[, c('x', 'y', 'gam.resid')]), ]
coords <- as.matrix(df.moran[, c('x', 'y')])

# Morans I
library(spdep)
d.vals <- c(250, 500, 1000, 2000)
for (d in d.vals) {
  
  nb <- dnearneigh(coords, 0, d)
  
  lw <- nb2listw(
    nb,
    style = 'W',
    zero.policy = TRUE
  )
  
  mi <- moran.test(
    df.moran$gam.resid,
    lw,
    zero.policy = TRUE
  )
  
  cat(
    '\n',
    'Distance:', d, 'm\n',
    '-------------------------\n',
    "Moran's I :", round(mi$estimate[['Moran I statistic']], 3), '\n',
    'Expected I:', round(mi$estimate[['Expectation']], 5), '\n',
    'Z-score   :', round(unname(mi$statistic), 2), '\n',
    'P-value   :', signif(mi$p.value, 3), '\n'
  )
}

gam.test <- bam(
  sqrt(swe_peak) ~ 
    topo_elev + I(topo_elev^2) +
    topo_slope +
    topo_tpi150 +
    rad_dtm_accum +
    gap_gap_pct +
    gap_dist_to_canopy_mean +
    cover_ground_frac +
    ht_zpcum1 +
    ht_zpcum2 +
    ht_zskew +
    factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df.test,
  method = 'fREML',
  discrete = TRUE
)

summary(gam.test)
gam.check(gam.test)














# ------------------ find best canopy-metrics-only model --------------------
# ----- stepwise 1 -----
# fit base model with the 2 variables we know we want to include
canopy.gap.ht <- bam(
  sqrt(swe_peak) ~
  wy +
  s(gap_gap_pct, by = burned) +
  s(ht_zmax, by = burned),
data = df.50,
method = "fREML",
discrete = TRUE)

model.formulas <- list(
  
  canopy.gap.ht =
    sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  canopy.gap.ht.zpcum1 =
    sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum1, by = burned)
   ,
  
  canopy.gap.ht.zpcum2 =
    sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned)
  ,
  
  canopy.gap.ht.zsd =
    sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zsd, by = burned)
  ,
  
  canopy.gap.ht.zq95 =
    sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zq95, by = burned)
  ,
 
  canopy.gap.ht.groundfrac =
    sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(cover_ground_frac, by = burned)
  ,
  
  canopy.gap.ht.disttocanopymean =
    sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned)
  ,
  
  canopy.gap.ht.zskew =
    sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zskew, by = burned)
)

# run each model 
results.canopy.stepwise <- list()

for (m in names(model.formulas)) {
    
  fit <- bam(
      model.formulas[[m]],
      data = df.50,
      method = "fREML",
      discrete = TRUE
    )
    
    pred <- predict(fit, newdata = df.50)
    obs  <- sqrt(df.50$swe_peak) 
    
    r2 <- 1 - sum((obs - pred)^2, na.rm = TRUE) /
      sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)

    results.canopy.stepwise[[length(results.canopy.stepwise) + 1]] <- data.frame(
      model = m,
      aic = AIC(fit),
      r2 = r2
    )
}



results.canopy.stepwise <- bind_rows(results.canopy.stepwise) %>%
  mutate(
    delta_aic = aic - AIC(canopy.gap.ht)
  ) %>%
  arrange(delta_aic)

results.canopy.stepwise

# ----- stepwise 2 -----
# fit base model with the 2 variables we know we want to include
base.fit <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

candidates <- c(
  'ht_zskew',
  'cover_ground_frac',
  'gap_dist_to_canopy_mean',
  'ht_zq95',
  'ht_zsd',
  'ht_zpcum1'
)

results.canopy.stepwise.2 <- list()

for (v in candidates) {
  
  f <- as.formula(
    paste0('. ~ . + s(', v, ', by = burned)')
  )
  
  fit <- update(base.fit, f)
  
  results.canopy.stepwise.2[[v]] <- data.frame(
    variable = v,
    aic = AIC(fit)
  )
}

bind_rows(results.canopy.stepwise.2) %>%
  mutate(
    delta_aic = aic - AIC(base.fit)
  ) %>%
  arrange(delta_aic)

canopy.gap.ht.zpcum2.groundfrac <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

summary(canopy.gap.ht.zpcum2.groundfrac)
concurvity(canopy.gap.ht.zpcum2.groundfrac)

plot(canopy.gap.ht.zpcum2.groundfrac,
     pages = 1)

par(mfrow = c(4, 2))

plot(
  canopy.gap.ht.zpcum2.groundfrac,
  pages = 1,
  shade = TRUE,
  scale = 0
)

# ----- stepwise 3 -----
base.fit <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

candidates <- c(
  'ht_zskew',
  'ht_zq95',
  'ht_zsd',
  'ht_zpcum1'
)

results.canopy.stepwise.3 <- list()

for (v in candidates) {
  
  f <- as.formula(
    paste0('. ~ . + s(', v, ', by = burned)')
  )
  
  fit <- update(base.fit, f)
  
  results.canopy.stepwise.3[[v]] <- data.frame(
    variable = v,
    aic = AIC(fit)
  )
}

bind_rows(results.canopy.stepwise.3) %>%
  mutate(
    delta_aic = aic - AIC(base.fit)
  ) %>%
  arrange(delta_aic)

canopy.gap.ht.zpcum2.groundfrac.disttocanopy.zskew <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned) +
    s(ht_zskew, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)


summary(canopy.gap.ht.zpcum2.groundfrac.disttocanopy.zskew)

# ----- stepwise 4 -----
base.fit <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned) +
    s(ht_zskew, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

candidates <- c(
  'ht_zq95',
  'ht_zsd',
  'ht_zpcum1'
)

results.canopy.stepwise.4 <- list()

for (v in candidates) {
  
  f <- as.formula(
    paste0('. ~ . + s(', v, ', by = burned)')
  )
  
  fit <- update(base.fit, f)
  
  results.canopy.stepwise.4[[v]] <- data.frame(
    variable = v,
    aic = AIC(fit)
  )
}

bind_rows(results.canopy.stepwise.4) %>%
  mutate(
    delta_aic = aic - AIC(base.fit)
  ) %>%
  arrange(delta_aic)




# ----- final canopy-only model after stepwise -----
canopy <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned) +
    s(ht_zskew, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)
# --------- explore interactions ---------
# ----- final canopy-only model -----
canopy <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned) +
    s(ht_zskew, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)


results <- rbind(
  results,
  get.metrics(canopy, "Canopy")
)


# gap / height interaction
canopy.int.gap.ht <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned) +
    s(ht_zskew, by = burned) +
    ti(gap_gap_pct, ht_zmax, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy.int.gap.ht, "Canopy interactions: gap / ht")
)

# gap / groundfrac interaction
canopy.int.gap.groundfrac <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned) +
    s(ht_zskew, by = burned) +
    ti(gap_gap_pct, cover_ground_frac, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy.int.gap.groundfrac, "Canopy interactions: gap / groundfrac")
)

# gap / distgap interaction
canopy.int.gap.distgap <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned) +
    s(ht_zskew, by = burned) +
    ti(gap_gap_pct, gap_dist_to_canopy_mean, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy.int.gap.distgap, "Canopy interactions: gap / groundfrac")
)

# -------------------- base models -------------------------------
# ----- canopy -----
canopy <- bam(
  sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(cover_ground_frac) +
    s(gap_dist_to_canopy_mean) +
    s(ht_zskew),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy, "Canopy")
)

# ----- cbi -----
cbi <- bam(
  sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(cbibc),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(cbi, "cbi")
)

# ----- burn status -----
burned <- bam(
  sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    burned,
  data = df.50,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(burned, "burn status")
)


# ----- topo -----
topo <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  data = df.50,
  method = "fREML",
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(topo, "topo")
)

# ----- spatial smooth -----
spatial <- bam(
  sqrt(swe_peak) ~ factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df.50,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(spatial, "spatial")
)

# ----- wy only -----

wy <- bam(
  sqrt(swe_peak) ~ wy +
    s(topo_elev),
  data = df.50,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(wy, "wy")
)

results 

singe.type.model.results <- results

# ----- canopy + severity-----
canopy.cbi <- bam(
  sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(cover_ground_frac) +
    s(gap_dist_to_canopy_mean) +
    s(cbibc) +
    s(ht_zskew),
  data = df.50,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy.cbi, "Canopy and Severity")
)

# ----- plot comparing model results -----
plot.df <- data.frame(
  model = c(
    'WY + Elevation only',
    'WY + Elevation + CBI',
    'WY + Elevation + Canopy',
    'WY + Elevation + Topography',
    'WY + Spatial'
  ),
  r2 = c(
    0.76,
    0.76,
    0.77,
    0.81,
    0.84
  )
)

plot.df$model <- factor(
  plot.df$model,
  levels = rev(plot.df$model)
)

ggplot(
  plot.df,
  aes(x = model, y = r2, fill = model)
) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      'WY + Elevation only' = 'cyan4',
      'WY + Elevation + CBI' = 'darkkhaki',
      'WY + Elevation + Canopy' = 'darkseagreen4',
      'WY + Elevation + Topography' = 'darkslategrey',
      'WY + Spatial' = 'goldenrod2'
    )
  ) +
  theme_bw() +
  theme(
    legend.position = 'none'
  ) +
  labs(
    x = NULL,
    y = expression('Adjusted '*R^2)
  )



# -------------------- find best topo-canopy model ----------------------
base.model <- bam(
  sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(base.model, "no interactions")
)

# ----- explore interactions -----
# gap / elev interaction
gap.elev <- update(
  base.model,
  . ~ . + ti(topo_elev, gap_gap_pct, by = burned)
)

results <- rbind(
  results,
  get.metrics(gap.elev, "gap*elev")
)

# gap / rad_dtm interaction
gap.rad <- update(
  base.model,
  . ~ . + ti(rad_dtm_accum, gap_gap_pct, by = burned)
)

results <- rbind(
  results,
  get.metrics(gap.rad, "gap*rad_dtm")
)

# gap / rad_dtm + rad / elev interaction
gap.rad.gap.elev <- update(
  base.model,
  . ~ . + ti(rad_dtm_accum, gap_gap_pct, by = burned) + ti(topo_elev, gap_gap_pct, by = burned)
)

results <- rbind(
  results,
  get.metrics(gap.rad.gap.elev, "gap*rad_dtm and gap*elev")
)


results

# ----- now try with k-fold -----
model.formulas.swe.interactions <- list(
  
  no.interactions = 
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  gap.elev = 
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(topo_elev, gap_gap_pct, by = burned),
  
  gap.rad = 
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_accum, gap_gap_pct, by = burned),
  
  gap.elev.gap.rad = 
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_accum, gap_gap_pct, by = burned) + 
    ti(topo_elev, gap_gap_pct, by = burned)

 )

# run each model doing 5-fold cross validation
results <- list()

# define which set of models
model.formulas.set <- model.formulas.swe.interactions

for (fold in 1:5) {
  
  train <- filter(df.50, fold_id != fold)
  test  <- filter(df.50, fold_id == fold)
  
  for (m in names(model.formulas.set)) {
    
    fit <- bam(
      model.formulas.set[[m]],
      data = train,
      method = "fREML",
      discrete = TRUE
    )
    
    pred <- predict(fit, newdata = test)
    
    # sqrt scale
    obs <- sqrt(test$swe_peak)
    
    rmse <- sqrt(mean((pred - obs)^2))
    mae  <- mean(abs(pred - obs))
    
    # original SWE scale
    pred.orig <- pred^2
    obs.orig  <- test$swe_peak
    
    rmse.orig <- sqrt(mean((pred.orig - obs.orig)^2))
    mae.orig  <- mean(abs(pred.orig - obs.orig))
    
    r2 <- 1 - sum((obs - pred)^2) /
      sum((obs - mean(obs))^2)
    
    results[[length(results)+1]] <- data.frame(
      fold = fold,
      model = m,
      r2 = r2,
      rmse = rmse,
      mae = mae,
      rmse_orig = rmse.orig,
      mae_orig = mae.orig
    )
  }
}

results <- bind_rows(results)

summary.table <- results %>%
  group_by(model) %>%
  summarise(
    r2_mean        = mean(r2),
    rmse_mean      = mean(rmse),
    mae_mean       = mean(mae),
    rmse_orig_mean = mean(rmse_orig),
    mae_orig_mean  = mean(mae_orig),
    .groups = "drop"
  ) %>%
  arrange(rmse_orig_mean)

summary.table

# ----- best topo canopy model now: -----

best.model <- bam(
  sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(topo_elev, gap_gap_pct, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE
)

# LMs ===========================================================================
#  
# --- sqrt(swe) / Fixed Effect WY / no clim ) --- 
wyfe.sqrtswe <- lm(
  sqrt(swe_peak) ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + wy,
  data = df.50
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = wyfe.sqrtswe,
  name = 'wy as fixed effect, no clim, sqrt(swe))',
  type = 'base',
  notes = 'base model with WY and log(swe), but no climate'
)

results <- out$results.df
coef.results <- out$coef.df

plot.residuals(wyfe.sqrtswe)






# --- sqrt(swe) / FE WY / high vs low canopy ---
# I want to look at if the significance of canopy cover changes whether the canopy cover is high or low

# function to create 3 groups:

make.groups <- function(x) {
  cut(x,
      breaks = quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = T),
      include.lowest = T,
      labels = c('low', 'medium', 'high'))
}

df.50.g <- df.50
canopy.vars.temp <- c('ht_zpcum1', 'ht_zpcum2', 'gap_gap_pct', 'rad_dsm_accum', 'cover_ground_frac')
groups <- c('all', 'low', 'high')

# for each canopy variable listed above, 
  # 1. create group in new df that categorizes it as high, medium, or low
  # 2. fit model using either all, low, or high 
  # 3. add results to df
  # 4. plot residuals

for (v in canopy.vars.temp) {
  cat('\nRunning', v, '...\n')
  
  # group column
  grp.name <- paste0(v, '.grp')
  df.50.g[[grp.name]] <- make.groups(df.50.g[[v]])
  
  # build model formula
  form <- as.formula(paste0('sqrt(swe_peak) ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + wy + ', v))
  
  # inner loop for all/low/high
  for (g in groups) {
    # subset data
    if (g == 'all') {
      df.sub <- df.50.g
    } else {
      df.sub <- df.50.g[df.50.g[[grp.name]] == g, ]
    }
    
    # fit model
    df.sub[[v]] <- scale(df.sub[[v]])
    mod <- lm(form, data = df.sub)
    
    # model name
    mod.name <- paste0(v, '.', g)
    
    # save model
    assign(mod.name, mod)
    
    # add results
    out <- add.model.results(
      results.df = results,
      coef.df = coef.results,
      model = mod,
      name = mod.name,
      type = 'canopy.explore',
      notes = paste(g, 'values of', v)
    )
    
    results <- out$results.df
    coef.results <- out$coef.df
    
    # plot residuals
    plot.residuals(mod, name = mod.name)
  
  }
}

# results
coef.compare.2 <- data.frame()

for (v in canopy.vars.temp) {
  
  # rows for this variable's models
  rows <- grepl(paste0('^', v, '\\.(all|low|high)$'), coef.results$model)
  temp <- coef.results[rows, ]
  
  # build column names to pull
  est.col <- paste0('est_', v)
  se.col <- paste0('se_', v)
  stat.col <- paste0('stat_', v)
  p.col <- paste0('p_', v)
  
  # make small clean table
  out <- data.frame(
    variable = v,
    group = sub(paste0('^', v, '\\.'), '', temp$model),
    model = temp$model,
    estimate = temp[[est.col]],
    se = temp[[se.col]],
    stat = temp[[stat.col]],
    p = temp[[p.col]],
    stringsAsFactors = FALSE
  )
  
  coef.compare.2 <- rbind(coef.compare.2, out)
}

coef.compare.2



# --- sqrt(swe) allll ---
full.lm <- lm(sqrt(swe_peak) ~ cover_ground_frac + gap_gap_pct+ gap_dist_to_canopy_mean + ht_zskew + ht_zkurt + ht_zentropy + ht_zpcum1 + ht_zpcum2 + ht_zpcum6 + ht_zpcum9 + cbibc + rad_dsm_accum + rad_dtm_accum + topo_slope + topo_tpi150 + topo_elev + wy,
              data = df.50)

plot.residuals(full.lm)

full.lm.elev2 <- lm(sqrt(swe_peak) ~ cover_ground_frac + gap_gap_pct+ gap_dist_to_canopy_mean + ht_zskew + ht_zkurt + ht_zentropy + ht_zpcum1 + ht_zpcum2 + ht_zpcum6 + ht_zpcum9 + cbibc + rad_dsm_accum + rad_dtm_accum + topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + wy,
              data = df.50)

plot.residuals(full.lm.elev2)

anova(wyfe.sqrtswe, full.lm.elev2)

# --- OLD EXPLORATORY LMS ---
# --- LOG(SWE) ---

# --- 1a) WY and clim ---
wy.clim.logswe <- lmer(
  swe_peak_log ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + pr + tmmn +
    (1 | wy),
  data = df.50,
  REML = FALSE 
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = wy.clim.logswe,
  name = 'wy.clim.logswe',
  type = 'base',
  notes = 'base model with WY and climate, log(swe)'
)

results <- out$results.df
coef.results <- out$coef.df

# --- 1b) WY, no clim  --- 
wy.logswe <- lm(
  log(swe_peak) ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + (1 | wy),
  data = df.50
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = wy.logswe,
  name = 'wy, no clim, log(swe)',
  type = 'base',
  notes = 'base model with WY and log(swe), but no climate'
)

results <- out$results.df
coef.results <- out$coef.df

plot.residuals(wy.sqrtswe)

# --- 1c) clim, no wy ---
clim.logswe <- lm(
  swe_peak_log ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + pr + tmmn,
  data = df.50,
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = clim.logswe,
  name = 'clim, no wy, log(swe)',
  type = 'base',
  notes = 'climate + log(swe), but getting rid of WY'
)

results <- out$results.df
coef.results <- out$coef.df


# --- UNTRANFORMED SWE ---

# --- 2a) un-logged SWE, clim w/o wy ---
clim.swe <- lm(
  swe_peak ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + pr + tmmn,
  data = df.50,
)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = clim.swe,
  name = 'clim, no log(swe)',
  type = 'base',
  notes = 'no wy, no log(swe)'
)

results <- out$results.df
coef.results <- out$coef.df

plot.residuals(clim.swe)

# --- SQRT(SWE) ---

# -- clim, no wy, sqrt(swe)---
clim.sqrtswe <- lm(
  sqrt_swe ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + pr + tmmn,
  data = df.50,
)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = clim.sqrtswe,
  name = 'clim, sqrt swe',
  type = 'base',
  notes = 'no wy, sqrt(swe)'
)

results <- out$results.df
coef.results <- out$coef.df

plot.residuals(clim.sqrtswe)

# --- WY, no clim, sqrt(swe) ---
wy.sqrtswe <- lmer(
  sqrt_swe ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + (1 | wy),
  data = df.50,
)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = wy.sqrtswe,
  name = 'wy, sqrt swe',
  type = 'base',
  notes = 'wy, no clim, sqrt(swe)'
)

results <- out$results.df
coef.results <- out$coef.df

plot.residuals(wy.sqrtswe)
# residuals look more funky

# --- WY + clim, sqrt(swe) ---
wy.clim.sqrtswe <- lmer(
  sqrt_swe ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + tmmn + pr + (1 | wy),
  data = df.50,
)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = wy.clim.sqrtswe,
  name = 'wy, clim, sqrt swe',
  type = 'base',
  notes = 'wy AND clim, sqrt(swe)'
)

results <- out$results.df
coef.results <- out$coef.df

plot.residuals(wy.clim.sqrtswe)

saveRDS(results, file.path(dir, 'modeling/base_model_results.rds'))
saveRDS(coef.results, file.path(dir, 'modeling/base_modelcoef_results.rds'))


# ==============================================================================
#  Exploratory Random Forest
# ==============================================================================


# --- initalize ---
library(ranger)
library(pdp)

dir <- 'data/processed/processed/rds/creek'
df.50 <- readRDS(file.path(dir, 'creek_long_df_50m_clean.rds'))
out.dir <- 'data/processed/processed/rds/creek/modeling'

rf.results <- data.frame()
rf.var.importance <- data.frame()

# --- create dfs ---

keep <- c(
  'ht_zq95',
  'gap_gap_pct',
  'cover_ground_frac',
  'gap_dist_to_canopy_mean',
  'ht_zskew', # this one has basically no correlation with SWE, so can probably drop
  'ht_zkurt', # this one has basically no correlation with SWE, so can probably drop
  'ht_zentropy',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zsd',
  'ht_zmax',
  'rad_dsm_accum',
  'topo_slope',
  'topo_tpi150',
  'topo_tpi2010',
  'topo_elev',
  'wy',
  'swe_peak'
)

# full df
df.50.rf.full <- df.50 %>% 
  select(all_of(keep)) %>%
  filter(
    wy != 2020) %>% # drop 2020, since it's prefire
  mutate(
    wy = as.factor(wy)) %>% # make wy a factor 
  filter(complete.cases(.)) # drop rows with any missing values

# create dfs for each year
df.2021 <- df.50.rf.full %>% filter(wy == '2021')
df.2022 <- df.50.rf.full %>% filter(wy == '2022')
df.2023 <- df.50.rf.full %>% filter(wy == '2023')
df.2024 <- df.50.rf.full %>% filter(wy == '2024')
df.2025 <- df.50.rf.full %>% filter(wy == '2025')


# --- rf helper function ---

rf.run.save <- function(name, df, out.dir, rf.results, rf.var.importance) {
  
  rf.mod <- ranger(
    swe_peak ~ .,
    data = df,
    num.trees = 500,
    importance = 'permutation',
    seed = 123
  )
  
  saveRDS(rf.mod, file.path(out.dir, paste0(name, '.rds')))
  
  rf.results <- rbind(
    rf.results,
    data.frame(
      model = name,
      n = nrow(df),
      num.trees = rf.mod$num.trees,
      mtry = rf.mod$mtry,
      min.node.size = rf.mod$min.node.size,
      prediction.error = rf.mod$prediction.error,
      r.squared = rf.mod$r.squared
    )
  )
  
  var.imp <- sort(rf.mod$variable.importance, decreasing = TRUE)
  
  rf.var.importance <- rbind(
    rf.var.importance,
    data.frame(
      model = name,
      variable = names(var.imp),
      importance = as.numeric(var.imp)
    )
  )
  
  saveRDS(rf.results, file.path(out.dir, 'rf_results.rds'))
  saveRDS(rf.var.importance, file.path(out.dir, 'rf_var_importance.rds'))
  
  list(
    rf.mod = rf.mod,
    rf.results = rf.results,
    rf.var.importance = rf.var.importance
  )
}

# --- run rf models ---

# --- 2021 ---
tic('2021 rf model')
rf.2021 <- rf.run.save(name = 'rf2021', df = df.2021, out.dir, rf.results, rf.var.importance)
toc()
# 15 threads
# 27 minutes


# --- 2022 ---
tic('2022 rf model')
rf.2022 <- rf.run.save(name = 'rf2022', df = df.2022, out.dir, rf.results, rf.var.importance)
toc()



# --- 2023 ---
tic('2023 rf model')
out.2023 <- rf.run.save(name = 'rf2023', df = df.2023, out.dir, rf.results, rf.var.importance)
toc()
rf.2023 <- out.2023$rf.mod
rf.results <- out.2023$rf.results
rf.var.importance <- out.2023$rf.var.importance


# --- 2024 ---
tic('2024 rf model')
rf.2024 <- rf.run.save(name = 'rf2024', df = df.2024, out.dir, rf.results, rf.var.importance)
toc()
# can't remember when I ended up running here, may have overwritten out.2023 but kind of don't think I did because when I told it to rewrite it with doesn't actually exist...


# --- 2025 ---
tic('2025 rf model')
rf.2025 <- rf.run.save(name = 'rf2025', df = df.2025, out.dir, rf.results, rf.var.importance)
toc()



# --- full rf model ---
tic('full rf.model')
rf.full <- rf.run.save(name = 'rf_full_50', df = df.50.rf.full, out.dir, rf.results, rf.var.importance)
toc()
# took 2 hours 20 minutes



# troubleshooting

get.rf.mod <- function(x) {
  if ('ranger' %in% class(x)) {
    return(x)
  }
  if (is.list(x) && 'rf.mod' %in% names(x)) {
    return(x$rf.mod)
  }
  stop('Object is neither a ranger model nor a list containing $rf.mod')
}

extract.rf.var.importance <- function(model.obj, model.name) {
  rf.mod <- get.rf.mod(model.obj)
  var.imp <- sort(rf.mod$variable.importance, decreasing = TRUE)
  
  data.frame(
    model = model.name,
    variable = names(var.imp),
    importance = as.numeric(var.imp)
  )
}

extract.rf.results <- function(model.obj, model.name) {
  rf.mod <- get.rf.mod(model.obj)
  
  data.frame(
    model = model.name,
    n = rf.mod$num.samples,
    num.trees = rf.mod$num.trees,
    mtry = rf.mod$mtry,
    min.node.size = rf.mod$min.node.size,
    prediction.error = rf.mod$prediction.error,
    r.squared = rf.mod$r.squared
  )
}
dir <- 'data/processed/processed/rds/creek/modeling'

rf.var.importance <- rbind(
  extract.rf.var.importance(rf.2021, 'rf.2021'),
  extract.rf.var.importance(rf.2022, 'rf.2022'),
  extract.rf.var.importance(rf.full, 'rf.full')
)

rf.results <- rbind(
  extract.rf.results(rf.2021, 'rf.2021'),
  extract.rf.results(rf.2022, 'rf.2022'),
  extract.rf.results(rf.full, 'rf.full')
)

saveRDS(rf.var.importance, file.path(dir, 'rf_var_importance.rds'))
saveRDS(rf.results, file.path(dir, 'rf.results.rds'))

rf.var.importance <- readRDS(file.path(dir, 'modeling/rf_var_importance.rds'))
rf.2021 <- readRDS(file.path(dir, 'modeling/rf2021.rds'))
rf.2022 <- readRDS(file.path(dir, 'modeling/rf2022.rds'))





# ==============================================================================
#  Exploratory GLM
# ==============================================================================

gamma.base <- glm(swe_peak ~ topo_elev + I(topo_elev^2) + wy + topo_tpi150 + topo_slope + rad_dtm_accum + topo_aspect_cos + topo_aspect_sin + topo_tpi150 + ht_zsd + ht_zq95 + ht_zpcum1 + ht_zpcum2 + ht_zmax + gap_dist_to_canopy_mean, 
                    family = Gamma(link = log), 
                    data = df.50)

# get pearson residuals
fit <- fitted(gamma.base)
est.disp <- summary(gamma.base)$dispersion
pearson.resid <- residuals(gamma.base, type = 'pearson')/sqrt(est.disp)
deviance.resid <- residuals(gamma.base, type = 'deviance')

range(predict(gamma.base, type = 'response'))

set.seed(14)
idx <- sample(seq_along(fit), 10000)

# plot residuals
plot(pearson.resid[idx] ~ fit[idx], 
     col = densCols(pearson.resid[idx], fit[idx]),
     ylab = "Pearson residuals", 
     xlab = 'Predicted SWE')
abline(h = 0)

plot(deviance.resid[idx] ~ fit[idx], 
     col = densCols(deviance.resid[idx], fit[idx]),
     ylab = "Deviance residuals", 
     xlab = 'Predicted SWE')
abline(h = 0)

summary(gamma.base)
anova(gamma.base)

# Morans I on residuals
library(spdep)
set.seed(14)
idx <- sample(nrow(df.50), 10000)

coords <- as.matrix(df.50[idx, c('x', 'y')])
res.samp <- pearson.resid[idx]

thresholds <- c(500, 1000, 2000, 5000)

for (t in thresholds) {
  
  nb <- dnearneigh(coords, d1 = 0, d2 = t)
  lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)
  
  mt <- moran.test(res.samp, lw, zero.policy = TRUE)
  
  cat('-------------------------\n')
  cat('Distance:', t, 'm\n')
  cat('Moran I:', mt$estimate['Moran I statistic'], '\n')
  cat('Expected:', mt$estimate['Expectation'], '\n')
  cat('Z-score:', mt$statistic, '\n')
  cat('p-value:', mt$p.value, '\n')
}


# troubleshooting
t = 1000
nb <- dnearneigh(coords, d1 = 0, d2 = t)
lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)
MoransResults <- moran.test(res.samp, lw, zero.policy = TRUE)
### Computation of effective sample size
n = 10000
eff_ss = round(n/(1+abs(MoransResults$estimate[1])))
print(eff_ss)

# ==============================================================================
#  Exploratory CART
# ==============================================================================
library(rpart)
cart <- rpart(swe_peak ~ cover_ground_frac +gap_gap_pct + gap_dist_to_canopy_mean + ht_zskew + ht_zkurt + ht_zentropy + ht_zpcum1 + ht_zpcum2 + ht_zpcum6 + ht_zpcum9 + cbibc + rad_dsm_accum + rad_dtm_accum + topo_slope + topo_tpi150 + topo_elev, data = df.50.samp, method = 'anova',  control = rpart.control(
  cp = 0.001,
  minsplit = 100,
  maxdepth = 10))











