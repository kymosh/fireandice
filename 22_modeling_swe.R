packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest', 'ranger', 'tictoc')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# get dataframe
# make sure if not on processing computer that the rds is updated!
dir <- 'data/processed/processed/rds/creek' 

df.50 <- readRDS(file.path(dir, 'creek_long_df_50m_clean.rds'))
#df.500 <- readRDS(file.path(dir, 'creek_long_df_500m.rds')) let's just focus on df.50 for now. 
df.50 <- df.50 %>% 
  mutate(sqrt_swe = sqrt(swe_peak))


# load results DF
results <- readRDS(file.path(dir, 'modeling/base_model_results.rds'))
coef.results <- readRDS(file.path(dir, 'modeling/base_modelcoef_results.rds'))

set.seed(12)
idx <- sample(seq_len(nrow(df.50)), 100000)
df.50.samp <- df.50[sample(nrow(df.50), 100000), ]

# ==============================================================================
#  Results DF and helper functions creation
# ==============================================================================

# create blank dataframes (commenting out so don't accidentally overwrite them)
results <- data.frame()
coef.results <- data.frame()

clean.term.names <- function(x) {
  x <- gsub('\\(Intercept\\)', 'Intercept', x)
  x <- gsub('I\\((.*)\\^2\\)', '\\1_sq', x)
  x <- gsub('[^[:alnum:]_]', '_', x)
  x <- gsub('_+', '_', x)
  x <- gsub('^_|_$', '', x)
  x
}

add.model.results <- function(results.df, coef.df, model, name, type = 'exploratory', notes = NA) {
  
  # check model class and extract coefficient matrix
  if (inherits(model, c('merMod', 'lm'))) {
    coef.mat <- summary(model)$coefficients
    n.params <- attr(logLik(model), 'df')
  } else {
    stop('Model class not supported. Use lm or lmer/glmer objects.')
  }
  
  # --------------------------
  # model-level results
  # --------------------------
  new.row <- data.frame(
    model = name,
    AIC = AIC(model),
    BIC = BIC(model),
    logLik = as.numeric(logLik(model)),
    n_params = n.params,
    type = type,
    notes = notes,
    stringsAsFactors = FALSE
  )
  
  results.df <- bind_rows(results.df, new.row)
  
  # --------------------------
  # coefficient-level results (wide format)
  # one row per model
  # --------------------------
  
  term.names <- clean.term.names(rownames(coef.mat))
  
  # estimates
  est.vec <- coef.mat[, 'Estimate']
  names(est.vec) <- term.names
  est.row <- as.data.frame(as.list(est.vec), stringsAsFactors = FALSE)
  names(est.row) <- paste0('est_', names(est.row))
  
  # standard errors
  se.vec <- coef.mat[, 'Std. Error']
  names(se.vec) <- term.names
  se.row <- as.data.frame(as.list(se.vec), stringsAsFactors = FALSE)
  names(se.row) <- paste0('se_', names(se.row))
  
  # test statistics
  if ('t value' %in% colnames(coef.mat)) {
    stat.vec <- coef.mat[, 't value']
  } else if ('z value' %in% colnames(coef.mat)) {
    stat.vec <- coef.mat[, 'z value']
  } else {
    stat.vec <- rep(NA_real_, nrow(coef.mat))
  }
  names(stat.vec) <- term.names
  stat.row <- as.data.frame(as.list(stat.vec), stringsAsFactors = FALSE)
  names(stat.row) <- paste0('stat_', names(stat.row))
  
  # p-values
  if ('Pr(>|t|)' %in% colnames(coef.mat)) {
    p.vec <- coef.mat[, 'Pr(>|t|)']
  } else if ('Pr(>|z|)' %in% colnames(coef.mat)) {
    p.vec <- coef.mat[, 'Pr(>|z|)']
  } else {
    p.vec <- rep(NA_real_, nrow(coef.mat))
  }
  names(p.vec) <- term.names
  p.row <- as.data.frame(as.list(p.vec), stringsAsFactors = FALSE)
  names(p.row) <- paste0('p_', names(p.row))
  
  # combine metadata + coefficient info into one row
  coef.out <- bind_cols(
    data.frame(
      model = name,
      type = type,
      notes = notes,
      stringsAsFactors = FALSE
    ),
    est.row,
    se.row,
    stat.row,
    p.row
  )
  
  # add to running wide coefficient table
  coef.df <- bind_rows(coef.df, coef.out)
  
  return(list(results.df = results.df, coef.df = coef.df))
}

plot.residuals <- function(model, name = NULL) {
  
  # get residuals and fitted
  res <- resid(model)
  fit <- fitted(model)

  # sample indices
  set.seed(123)
  idx <- sample(seq_along(res), min(100000, length(res)))

  res.sub <- res[idx]
  fit.sub <- fit[idx]
  
  # title
  if (is.null(name)) {
    main.title <- 'Residuals vs Fitted'
  } else {
    main.title <- paste(name, '- Residuals vs Fitted')
  }

  plot(
    fit.sub, res.sub,
    pch = 16, cex = 0.3,
    col = rgb(0, 0, 0, 0.3),  
    xlab = 'Fitted values',
    ylab = 'Residuals',
    main = main.title
  )

  abline(h = 0, col = 'red')
}


# ==============================================================================
#  LMs
# ==============================================================================
# 
# ----- sqrt(swe) / Fixed Effect WY / no clim ) ------ 
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






# ----- sqrt(swe) / FE WY / high vs low canopy -----
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



# ----- sqrt(swe) allll ----
full.lm <- lm(sqrt(swe_peak) ~ cover_ground_frac + gap_gap_pct+ gap_dist_to_canopy_mean + ht_zskew + ht_zkurt + ht_zentropy + ht_zpcum1 + ht_zpcum2 + ht_zpcum6 + ht_zpcum9 + cbibc + rad_dsm_accum + rad_dtm_accum + topo_slope + topo_tpi150 + topo_elev + wy,
              data = df.50)

plot.residuals(full.lm)

full.lm.elev2 <- lm(sqrt(swe_peak) ~ cover_ground_frac + gap_gap_pct+ gap_dist_to_canopy_mean + ht_zskew + ht_zkurt + ht_zentropy + ht_zpcum1 + ht_zpcum2 + ht_zpcum6 + ht_zpcum9 + cbibc + rad_dsm_accum + rad_dtm_accum + topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + wy,
              data = df.50)

plot.residuals(full.lm.elev2)

anova(wyfe.sqrtswe, full.lm.elev2)

# ------------------------- OLD EXPLORATORY LMS --------------------------------
# -------------- LOG(SWE) -------------------------

# ----- 1a) WY and clim -----
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

# ----- 1b) WY, no clim  ----- 
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

# ----- 1c) clim, no wy -----
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


# ------------------ UNTRANFORMED SWE ------------------

# ----- 2a) un-logged SWE, clim w/o wy -----
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

# ----------------------- SQRT(SWE) ---------------------

# ----- clim, no wy, sqrt(swe)-----
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

# ----- WY, no clim, sqrt(swe) -----
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

# ----- WY + clim, sqrt(swe) -----
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


# ------ initalize -----
library(ranger)
library(pdp)

dir <- 'data/processed/processed/rds/creek'
df.50.0 <- readRDS(file.path(dir, 'creek_long_df_50m.rds'))
out.dir <- dir <- 'data/processed/processed/rds/creek/modeling'

#rf.results <- data.frame()
#rf.var.importance <- data.frame()

# ----- create dfs -----
# full df
df.50.rf.full <- df.50 %>% 
  select(-cell) %>% # RF won't use cell
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


# ----- rf helper function -----

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

# ----- run rf models -----

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
rf.full <- rf.run.save(name = 'rf_full_50_75qsnowline', df = df.50.rf.full, out.dir, rf.results, rf.var.importance)
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

gamma.base <- glm(swe_peak ~ topo_elev + I(topo_elev^2) + wy + topo_tpi150 + topo_slope + rad_dtm_accum, 
                    family = Gamma(link = 'log'), 
                    data = df.50)

# get pearson residuals
fit <- fitted(gamma.base)
est.disp <- summary(gamma.base)$dispersion
pearson.resid <- residuals(gamma.base, type = 'pearson')/sqrt(est.disp)

set.seed(1)
idx <- sample(seq_along(fit), 5000)

par(mfrow = c(1, 1))
plot(fit[idx] ~ pearson.resid[idx], 
     ylab = "Pearson residuals", 
     xlab = 'Predicted SWE')
abline(h = 0)







summary(gamma.base)
anova(m.gamma.full)

plot(residuals(m.gamma) ~ fitted(m.gamma))
plot(fitted(m.gamma),
     residuals(m.gamma, type = 'pearson'),
     xlab = 'Fitted values',
     ylab = 'Pearson residuals')
abline(h = 0, col = 'red')


# toubleshooting
zeros <- df.50$swe_peak == 0


# ==============================================================================
#  Exploratory CART
# ==============================================================================
library(rpart)
cart <- rpart(swe_peak ~ cover_ground_frac +gap_gap_pct + gap_dist_to_canopy_mean + ht_zskew + ht_zkurt + ht_zentropy + ht_zpcum1 + ht_zpcum2 + ht_zpcum6 + ht_zpcum9 + cbibc + rad_dsm_accum + rad_dtm_accum + topo_slope + topo_tpi150 + topo_elev, data = df.50.samp, method = 'anova',  control = rpart.control(
  cp = 0.001,
  minsplit = 100,
  maxdepth = 10))









