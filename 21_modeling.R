packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4')
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

add.model.results <- function(results.df, coef.df, model, name, type = 'exploratory', notes = NA) {
  
  # fixed-effect coefficients
  if (inherits(model, c('merMod', 'lm'))) {
    coef.mat <- summary(model)$coefficients
    n.params <- attr(logLik(model), 'df')
  } else {
    stop('Model class not supported. Use lm or lmer/glmer objects.')
  }
  
  # model-level results
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
  
  results.df <- rbind(results.df, new.row)
  
  # coefficient-level results
  coef.out <- data.frame(
    model = name,
    term = rownames(coef.mat),
    estimate = coef.mat[, 'Estimate'],
    std.error = coef.mat[, 'Std. Error'],
    stringsAsFactors = FALSE
  )
  
  # add test statistic column
  if ('t value' %in% colnames(coef.mat)) {
    coef.out$statistic <- coef.mat[, 't value']
  } else if ('z value' %in% colnames(coef.mat)) {
    coef.out$statistic <- coef.mat[, 'z value']
  } else {
    coef.out$statistic <- NA
  }
  
  # add p-values if available
  if ('Pr(>|t|)' %in% colnames(coef.mat)) {
    coef.out$p.value <- coef.mat[, 'Pr(>|t|)']
  } else if ('Pr(>|z|)' %in% colnames(coef.mat)) {
    coef.out$p.value <- coef.mat[, 'Pr(>|z|)']
  } else {
    coef.out$p.value <- NA
  }
  
  coef.out$type <- type
  coef.out$notes <- notes
  
  coef.df <- rbind(coef.df, coef.out)
  
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
# ----- WY and clim -----
wy.clim <- lmer(
  swe_peak_log ~ topo_slope + topo_tpi150 + I(topo_tpi150^2) + topo_elev + I(topo_elev^2) + rad_dtm_accum + pr + tmmn +
    (1 | wy),
  data = df.50,
  REML = FALSE # if TRUE, can't compare AIC values
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = wy.clim,
  name = 'wy.clim',
  type = 'base',
  notes = 'base model with WY and climate'
)

results <- out$results.df
coef.results <- out$coef.df

# ----- WY, no clim -----
wy <- lmer(
  swe_peak_log ~ topo_slope + topo_tpi150 + I(topo_tpi150^2) + topo_elev + I(topo_elev^2) + rad_dtm_accum +
    (1 | wy),
  data = df.50,
  REML = FALSE # if TRUE, can't compare AIC values
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = wy,
  name = 'wy, no clim',
  type = 'base',
  notes = 'base model with WY, but no climate'
)

results <- out$results.df
coef.results <- out$coef.df

# ----- clim, no wy -----
clim <- lm(
  swe_peak_log ~ topo_slope + topo_tpi150 + I(topo_tpi150^2) + topo_elev + I(topo_elev^2) + rad_dtm_accum + pr + tmmn,
  data = df.50,
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = clim,
  name = 'clim, no wy',
  type = 'base',
  notes = 'climate, but getting rid of WY'
)

results <- out$results.df
coef.results <- out$coef.df

# ----- clim, no wy -----
# getting rid of tpi^2
clim2 <- lm(
  swe_peak_log ~ topo_slope + topo_tpi150 + topo_elev + I(topo_elev^2) + rad_dtm_accum + pr + tmmn,
  data = df.50,
)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = clim2,
  name = 'clim, no wy, no tpi^2',
  type = 'base',
  notes = 'climate, but getting rid of WY  tpi^2'
)

results <- out$results.df
coef.results <- out$coef.df





