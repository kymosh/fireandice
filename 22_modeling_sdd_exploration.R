packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest', 'ranger', 'tictoc', 'mgcv', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# get dataframe
set.seed(61)
dir <- 'data/processed/processed/rds/' 

df.500.raw <- readRDS(file.path(dir, 'df_500m_raw.rds'))
df.500.balanced <- readRDS(file.path(dir, 'df_500m_raw_balanced.rds')) 


burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)

# helper function
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

# ----- plot SDD for all fires -----
# ----- plot SDD distributions for all fires -----

for (f in unique(df.500.raw$fire)) {
  
  p <- df.500.raw %>%
    filter(fire == f) %>%
    ggplot(aes(x = sdd)) +
    geom_density(
      fill = 'steelblue',
      alpha = 0.4,
      na.rm = TRUE
    ) +
    facet_wrap(~wy, scales = 'free') +
    labs(
      title = tools::toTitleCase(f),
      x = 'Snow Disappearance Day (DOY)',
      y = 'Density'
    ) +
    theme_bw()
  
  print(p)
}

# ==============================================================================
# Stage 1 Modeling - Single family predictors
# ==============================================================================
# ------------------------- Topo-only Model ------------------------------
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

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation baseline
  model.elev <- bam(sdd ~
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
        'sdd ~ wy + s(elevation, k = 10) + s(' , var, ', k = 10)'
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
  arrange(fire, desc(dev.expl)) %>%
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

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad <- bam(
    sdd ~ wy + s(elevation) + s(rad_dtm_accum),
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
      paste0('sdd ~ wy + s(elevation) + s(rad_dtm_accum) + 
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
  arrange(fire, desc(dev.expl))

topo.results.step.2 <- topo.results.step

# slope performs best in 3/4

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

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad.slope <- bam(
    sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope),
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics(
      fitted.model = topo.elev.rad.slope,
      model.name = 'topo.elev.rad.slope',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + 
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
        model.name = paste0('topo.elev.rad.slope', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

topo.results.step %>%
  arrange(fire, desc(dev.expl))

topo.results.step.3 <- topo.results.step

# ----- stepwise 3 ------
# updated vars
topo.vars <- c(
  'tpi150',
  'tpi510',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad.slope.tpi <- bam(
    sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + s(tpi1200),
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics(
      fitted.model = topo.elev.rad.slope.tpi,
      model.name = 'topo.elev.rad.slope.tpi.',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + s(tpi1200) +
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
        model.name = paste0('topo.elev.rad.slope.tpi.', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

topo.results.step %>%
  arrange(fire, desc(dev.expl))

topo.results.step.4 <- topo.results.step



# ==============================================================================
# GAM
# ==============================================================================
# ----- null -----
null <- bam(
  sdd ~ wy,
  data = df.500,
  method = 'REML'
)

summary(null)
results <- rbind(
  results,
  get.metrics(null, "Null")
)

# ----- topo -----
topo <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010),
  data = df,
  method = 'REML'
)

summary(topo)
results <- rbind(
  results,
  get.metrics(topo, "Topo")
)

# ----- cbi and burned -----
name <- 'topo + cbi'
m3 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(cbibc),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m3, name)
)

name <- 'topo + burned'
m4 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    burned,
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m4, name)
)



# ----- check all canopy metrics to find best ones -----
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
  "rad_dsm_melt",
  names(df.500)[grepl("^ht_", names(df.500))]
)

results.metrics <- data.frame()

for (metric in metrics) {
  
  message("Running ", metric)
  
  form <- as.formula(
    paste(
      "sdd ~",
      "factor(wy) +",
      "s(topo_elev) +",
      "s(rad_dtm_melt) +",
      "s(rad_dtm_accum) +",
      "s(topo_slope) +",
      "s(topo_tpi1200) +",
      "s(topo_tpi2010) +",
      paste0("s(", metric, ")")
    )
  )
  
  mod <- bam(
    form,
    data = df.500,
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

cor(df.500$ht_zmax, df.500$ht_zpcum9, use = 'complete.obs')


# ----- canopy metric models -----
name <- 'topo + gap(by b)'
m5 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m5, name)
)

name <- 'topo + zmax'
m6 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(ht_zmax),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m6, name)
)

name <- 'topo + zpcum9'
m7 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(ht_zpcum9),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m7, name)
)

name <- 'topo + gap(by b) + zmax(by b)'
m8 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m8, name)
)

name <- 'topo + gap(by b) + zpcum9(by b)'
m9 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zpcum9, by = burned),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m9, name)
)

name <- 'topo + gap(by b) + zpcum9(by b) + zmax(by b)'
m10 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zpcum9, by = burned) +
    s(ht_zmax, by = burned),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m10, name)
)



# ------------------ stepwise for canopy metrics --------------------
# ----- stepwise 1 -----
# fit base model with the 2 variables we know we want to include
canopy.gap.ht <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

model.formulas <- list(
  
  canopy.gap.ht =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  canopy.gap.ht.zpcum1 =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum1, by = burned)
  ,
  
  canopy.gap.ht.zpcum2 =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned)
  ,
  
  canopy.gap.ht.zsd =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zsd, by = burned)
  ,
  
  canopy.gap.ht.zq95 =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zq95, by = burned)
  ,
  
  canopy.gap.ht.groundfrac =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(cover_ground_frac, by = burned)
  ,
  
  canopy.gap.ht.disttocanopymean =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned)
  ,
  
  canopy.gap.ht.zskew =
    sdd ~
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
    data = df.500,
    method = 'fREML',
    discrete = TRUE
  )
  
  s <- summary(fit)
  
  results.canopy.stepwise[[length(results.canopy.stepwise) + 1]] <- data.frame(
    model = m,
    aic = AIC(fit),
    r2 = s$r.sq,
    dev_expl = s$dev.expl,
    edf = sum(s$edf)
  )
}


results.canopy.stepwise <- bind_rows(results.canopy.stepwise) %>%
  mutate(
    delta_aic = aic - AIC(canopy.gap.ht)
  ) %>%
  arrange(delta_aic)

results.canopy.stepwise


# ----- stepwise 2 -----
base.fit <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned),
  data = df.500,
  method = 'fREML',
  discrete = TRUE
)

base.formula <- formula(base.fit)

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
  
  f.new <- update(
    base.formula,
    as.formula(paste0('. ~ . + s(', v, ', by = burned)'))
  )
  
  fit <- bam(
    f.new,
    data = df.500,
    method = 'fREML',
    discrete = TRUE
  )
  
  s <- summary(fit)
  
  results.canopy.stepwise.2[[v]] <- data.frame(
    variable = v,
    aic = AIC(fit),
    r2 = s$r.sq,
    dev_expl = s$dev.expl,
    edf = sum(s$edf)
  )
}

results.canopy.stepwise.2 <- bind_rows(results.canopy.stepwise.2) %>%
  mutate(
    delta_aic = aic - AIC(base.fit)
  ) %>%
  arrange(delta_aic)

results.canopy.stepwise.2

canopy.gap.ht.zpcum2.zpcum1 <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(ht_zpcum1, by = burned),
  data = df.500,
  method = "fREML",
  discrete = TRUE)


# ----- stepwise 3 -----
base.fit <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(ht_zpcum1, by = burned),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

candidates <- c(
  'ht_zskew',
  'ht_zq95',
  'ht_zsd',
  'cover_ground_frac',
  'gap_dist_to_canopy_mean'
)

results.canopy.stepwise.3 <- list()

for (v in candidates) {
  
  f.new <- update(
    base.formula,
    as.formula(paste0('. ~ . + s(', v, ', by = burned)'))
  )
  
  fit <- bam(
    f.new,
    data = df.500,
    method = 'fREML',
    discrete = TRUE
  )
  
  s <- summary(fit)
  
  results.canopy.stepwise.3[[v]] <- data.frame(
    variable = v,
    aic = AIC(fit),
    r2 = s$r.sq,
    dev_expl = s$dev.expl,
    edf = sum(s$edf)
  )
}

results.canopy.stepwise.3 <- bind_rows(results.canopy.stepwise.3) %>%
  mutate(
    delta_aic = aic - AIC(base.fit)
  ) %>%
  arrange(delta_aic)

results.canopy.stepwise.3


# ----- final canopy-only model after stepwise -----
canopy <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(ht_zpcum1, by = burned),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy, "canopy")
)

results <- data.frame()
# -------------------- base models -------------------------------
# ----- canopy -----
canopy <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(ht_zpcum1),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy, "canopy")
)

# ----- rad.dsm -----
rad.dsm <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dsm_accum) +
    s(rad_dsm_melt),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(rad.dsm, "rad.dsm")
)

# ----- cbi -----
cbi <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(cbibc),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(cbi, "cbi")
)

# ----- burn status -----
burned <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    burned,
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(burned, "burn status")
)

# ----- topo -----
topo <- bam(
  sdd ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010),
  data = df.500,
  method = "fREML",
  discrete = TRUE
)
results <- rbind(
  results,
  get.metrics(topo, "topo")
)

# ----- canopy + severity -----
canopy.cbi <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(cbibc) +
    s(ht_zpcum1),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy.cbi, "canopy + severity")
)


# ----- spatial smooth -----
spatial <- bam(
  sdd ~ factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df.500,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(spatial, "spatial")
)

# ----- wy only -----

wy <- bam(
  sdd ~ wy +
  s(topo_elev),
  data = df.500,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(wy, "wy")
)

results 

singe.type.model.results.sdd <- results

# ----- plot comparing model results -----
# ----- AFTER adding elev to base models -----
plot.df <- data.frame(
  model = c(
    'WY + Elevation only',
    'WY + Elevation + CBI',
    'WY + Elevation + Canopy',
    'WY + Elevation + Topography',
    'WY + Spatial'
  ),
  r2 = c(
    0.79,
    0.80,
    0.83,
    0.82,
    0.88
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

# ----- AFTER adding elev - plot with both SWE and SDD results-----
plot.df <- data.frame(
  model = rep(
    c(
      'WY + Elevation only',
      'WY + Elevation + CBI',
      'WY + Elevation + Canopy',
      'WY + Elevation + Topography',
      'WY + Spatial'
    ),
    2
  ),
  response = rep(
    c('SWE', 'SDD'),
    each = 5
  ),
  r2 = c(
    # SWE
    0.76,
    0.76,
    0.77,
    0.81,
    0.84,
    
    # SDD
    0.79,
    0.80,
    0.83,
    0.82,
    0.88
  )
)

plot.df$model <- factor(
  plot.df$model,
  levels = c(
    'WY + Elevation only',
    'WY + Elevation + CBI',
    'WY + Elevation + Canopy',
    'WY + Elevation + Topography',
    'WY + Spatial'
  )
)

plot.df$fill.grp <- paste(plot.df$model, plot.df$response)

ggplot(
  plot.df,
  aes(
    x = model,
    y = r2,
    fill = fill.grp
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      
      # WY
      'WY + Elevation only SWE' = 'cyan4',
      'WY + Elevation only SDD' = 'cyan2',
      
      # CBI
      'WY + Elevation + CBI SWE' = 'darkkhaki',
      'WY + Elevation + CBI SDD' = 'khaki2',
      
      # Canopy
      'WY + Elevation + Canopy SWE' = 'darkseagreen4',
      'WY + Elevation + Canopy SDD' = 'darkseagreen2',
      
      # Topography
      'WY + Elevation + Topography SWE' = 'darkslategrey',
      'WY + Elevation + Topography SDD' = 'darkslategray3',
      
      # Spatial
      'WY + Spatial SWE' = 'goldenrod4',
      'WY + Spatial SDD' = 'goldenrod2'
    ),
    labels = c(
      'WY + Elevation only (SWE)',
      'WY + Elevation only (SDD)',
      'CBI + Elevation (SWE)',
      'CBI + Elevation (SDD)',
      'Canopy + Elevation (SWE)',
      'Canopy + Elevation (SDD)',
      'Topography + Elevation (SWE)',
      'Topography + Elevation (SDD)',
      'Spatial (SWE)',
      'Spatial (SDD)'
    )
  ) +
  geom_text(
    aes(label = round(r2, 3)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  expand_limits(y = 0.95) +
  theme_bw() +
  labs(
    x = NULL,
    y = expression('Adjusted '*R^2),
    fill = NULL
  ) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )
# ----- BEFORE adding elev to base models-----
plot.df <- data.frame(
  model = c(
    'WY only',
    'WY + CBI',
    'WY + Canopy',
    'WY + Topography',
    'WY + Spatial'
  ),
  r2 = c(
    0.270,
    0.543,
    0.720,
    0.819,
    0.882
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
      'WY only' = 'cyan4',
      'WY + CBI' = 'darkkhaki',
      'WY + Canopy' = 'darkseagreen4',
      'WY + Topography' = 'darkslategrey',
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

# ----- BEFORE adding elev - plot with both SWE and SDD results-----
plot.df <- data.frame(
  model = rep(
    c(
      'WY only',
      'WY + CBI',
      'WY + Canopy',
      'WY + Topography',
      'WY + Spatial'
    ),
    2
  ),
  response = rep(
    c('SWE', 'SDD'),
    each = 5
  ),
  r2 = c(
    # SWE
    0.47,
    0.59,
    0.63,
    0.81,
    0.84,
    
    # SDD
    0.27,
    0.54,
    0.72,
    0.82,
    0.88
  )
)

plot.df$model <- factor(
  plot.df$model,
  levels = c(
    'WY only',
    'WY + CBI',
    'WY + Canopy',
    'WY + Topography',
    'WY + Spatial'
  )
)

plot.df$fill.grp <- paste(plot.df$model, plot.df$response)

ggplot(
  plot.df,
  aes(
    x = model,
    y = r2,
    fill = fill.grp
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      
      # WY
      'WY only SWE' = 'cyan4',
      'WY only SDD' = 'cyan2',
      
      # CBI
      'WY + CBI SWE' = 'darkkhaki',
      'WY + CBI SDD' = 'khaki2',
      
      # Canopy
      'WY + Canopy SWE' = 'darkseagreen4',
      'WY + Canopy SDD' = 'darkseagreen2',
      
      # Topography
      'WY + Topography SWE' = 'darkslategrey',
      'WY + Topography SDD' = 'darkslategray3',
      
      # Spatial
      'WY + Spatial SWE' = 'goldenrod4',
      'WY + Spatial SDD' = 'goldenrod2'
    ),
    labels = c(
      'WY only (SWE)',
      'WY only (SDD)',
      'CBI (SWE)',
      'CBI (SDD)',
      'Canopy (SWE)',
      'Canopy (SDD)',
      'Topography (SWE)',
      'Topography (SDD)',
      'Spatial (SWE)',
      'Spatial (SDD)'
    )
  ) +
  geom_text(
    aes(label = round(r2, 3)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  expand_limits(y = 0.95) +
  theme_bw() +
  labs(
    x = NULL,
    y = expression('Adjusted '*R^2),
    fill = NULL
  ) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )
# ------------------- figure out best topo-canopy model -----------------
# ----- models with interactions -----
model.formulas.sdd.interactions <- list(
  
  no.interactions = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  gap.elev = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(topo_elev, gap_gap_pct, by = burned),
  
  gap.rad.accum = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_accum, gap_gap_pct, by = burned),
  
  gap.rad.melt = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_melt, gap_gap_pct, by = burned),
  
  gap.rad.accum.melt = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_accum, gap_gap_pct, by = burned) +
    ti(rad_dtm_melt, gap_gap_pct, by = burned),
  
  gap.elev.gap.rad = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_accum, gap_gap_pct, by = burned) + 
    ti(topo_elev, gap_gap_pct, by = burned)
  
)


# ----- best topo-canopy model -----

topo.canopy <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(topo_elev, gap_gap_pct, by = burned),
  data = df.500,
  method = 'REML'
)

# ==============================================================================
# Model Comparisons under k-fold validation
# ==============================================================================
# ------ model formulas -----
model.formulas <- list(
  topo.cbi = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(cbibc),
  
  topo.burned = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      burned,
  
  topo.gap.b = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(gap_gap_pct, by = burned),


  topo.gap.b.zmax.b = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(gap_gap_pct, by = burned) +
      s(ht_zmax, by = burned),


  topo.gap.b.zpcum9.b = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(gap_gap_pct, by = burned) +
      s(ht_zpcum9, by = burned),


  topo.gap.b.zpcum9.b.zmax.b = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(gap_gap_pct, by = burned) +
      s(ht_zpcum9, by = burned) +
      s(ht_zmax, by = burned)
)

# ----- 5-fold cross validation -----

# set what model set you want to test
sdd.k.results <- list()

k.results <- sdd.k.results

model.formulas.set <- model.formulas.sdd.interactions

for (fold in 1:5) {
  
  train <- filter(df.500, fold_id != fold)
  test  <- filter(df.500, fold_id == fold)
  
  for (m in names(model.formulas.set)) {
    
    fit <- bam(
      model.formulas.set[[m]],
      data = train,
      method = "fREML",
      discrete = TRUE
    )
    
    pred <- predict(fit, newdata = test)
    
    obs <- test$sdd
    
    rmse <- sqrt(mean((pred - obs)^2))
    mae  <- mean(abs(pred - obs))
    
    
    r2 <- 1 - sum((obs - pred)^2) /
      sum((obs - mean(obs))^2)
    
    k.results[[length(k.results)+1]] <- data.frame(
      fold = fold,
      model = m,
      r2 = r2,
      rmse = rmse,
      mae = mae
    )
  }
}

k.results <- bind_rows(k.results)

summary.table <- k.results %>%
  group_by(model) %>%
  summarise(
    r2_mean        = mean(r2),
    rmse_mean      = mean(rmse),
    mae_mean       = mean(mae),
    .groups = "drop"
  ) %>%
  arrange(rmse_mean)

summary.table


# ----- cbi and elev -----

