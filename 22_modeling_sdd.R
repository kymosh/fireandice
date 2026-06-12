packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest', 'ranger', 'tictoc')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize
# ==============================================================================

# get dataframe
# make sure if not on processing computer that the rds is updated!
dir <- 'data/processed/processed/rds/creek' 
df.500 <- readRDS(file.path(dir, 'creek_df_500m.rds'))

# explore sdd data
hist(df.500$sdd, breaks = 50)
summary(df.500$sdd)
qqnorm(df.500$sdd)
qqline(df.500$sdd)

# quick check to make sure there are no NAs
colSums(is.na(df.500))

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
  data = df.500,
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
  
  train <- filter(df.500, fold_id != fold)
  test  <- filter(df.500, fold_id == fold)
  
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
