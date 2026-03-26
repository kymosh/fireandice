packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# get dataframe
# make sure if not on processing computer that the rds is updated!
dir <- 'data/processed/processed/rds' 

df.50 <- readRDS(file.path(dir, 'creek_long_df_50m_clean.rds'))
#df.500 <- readRDS(file.path(dir, 'creek_long_df_500m.rds')) let's just focus on df.50 for now. 
df.50 <- df.50 %>% 
  mutate(sqrt_swe = sqrt(swe_peak))


# load results DF
results <- readRDS(file.path(dir, 'model_results.rds'))
coef.results <- readRDS(file.path(dir, 'model_coef_results.rds'))

set.seed(12)
idx <- sample(seq_len(nrow(df.50)), 100000)

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

plot.residuals <- function(model) {
  
  # get residuals and fitted
  res <- resid(model)
  fit <- fitted(model)

  # sample indices
  set.seed(123)
  idx <- sample(seq_along(res), min(100000, length(res)))

  res.sub <- res[idx]
  fit.sub <- fit[idx]

  plot(
    fit.sub, res.sub,
    pch = 16, cex = 0.3,
    col = rgb(0, 0, 0, 0.3),  # transparency helps A LOT
    xlab = 'Fitted values',
    ylab = 'Residuals',
    main = 'Residuals vs Fitted'
  )

  abline(h = 0, col = 'red')
}


# ==============================================================================
#  Modeling
# ==============================================================================

# -------------- LOG(SWE) -------------------------

# ----- 1a) WY and clim -----
wy.clim.logswe<- lmer(
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

# ----- 1b) WY, no clim -----
wy.logswe <- lmer(
  swe_peak_log ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum +
    (1 | wy),
  data = df.50,
  REML = FALSE 
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

plot.residuals(wy.logswe)

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

plot.residuals(clim.logswe)

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
