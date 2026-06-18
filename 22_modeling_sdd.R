packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest', 'ranger', 'tictoc')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize
# ==============================================================================

# get dataframe
# make sure if not on processing computer that the rds is updated!
dir <- 'data/processed/processed/rds/creek' 
df.5000 <- readRDS(file.path(dir, 'creek_df_500m.rds'))

# explore sdd data
hist(df.5000$sdd, breaks = 50)
summary(df.5000$sdd)
qqnorm(df.5000$sdd)
qqline(df.5000$sdd)

# quick check to make sure there are no NAs
colSums(is.na(df.5000))

# function to add results to table
get.metrics <- function(model, name) {
  
  s <- summary(model)
  
  data.frame(
    model = name,
    r.squared = s$r.sq,
    dev.expl = s$dev.expl,
    AIC = AIC(model),
    edf = sum(s$edf)
  )
}
results <- data.frame()

# ==============================================================================
# GAM
# ==============================================================================
# ----- null -----
null <- gam(
  sdd ~ wy,
  data = df.5000,
  method = 'REML'
)

summary(null)
results <- rbind(
  results,
  get.metrics(null, "Null")
)

# ----- topo -----
topo <- gam(
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
m3 <- gam(
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
m4 <- gam(
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
  names(df.5000)[grepl("^ht_", names(df.5000))]
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
    data = df.5000,
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

cor(df.5000$ht_zmax, df.5000$ht_zpcum9, use = 'complete.obs')


# ----- canopy metric models -----
name <- 'topo + gap(by b)'
m5 <- gam(
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
m6 <- gam(
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
m7 <- gam(
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
m8 <- gam(
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
m9 <- gam(
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
m10 <- gam(
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

# ----- canopy -----
rad.dsm <- bam(
  sdd ~
    wy +
    s(rad_dsm_accum, by = burned) +
    s(rad_dsm_melt, by = burned),
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
    s(cbibc),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(cbi, "cbi")
)

# ----- topo -----
topo <- bam(
  sdd ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
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
  sdd ~ wy,
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

# ----- plot with with SWE and SDD results -----
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

# run each model doing 5-fold cross validation
k.results <- list()

for (fold in 1:5) {
  
  train <- filter(df.5000, fold_id != fold)
  test  <- filter(df.5000, fold_id == fold)
  
  for (m in names(model.formulas)) {
    
    fit <- bam(
      model.formulas[[m]],
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
