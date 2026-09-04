packages <- c('mgcv', 'ggplot2', 'pdp', 'dplyr', 'purrr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# ----- setup -----
# get dataframe
set.seed(61)
dir <- 'data/processed/processed/rds/' 

df.50.raw <- readRDS(file.path(dir, 'df_50m_raw.rds'))

fire.levels <- c('Caldor', 'Creek', 'Castle')

# years common to all fires
common.years <- df.50.raw %>%
  filter(fire != 'dixie') %>%
  distinct(fire, wy) %>%
  count(wy) %>%
  filter(n == 3) %>%   # 3 remaining fires
  pull(wy)

# remove dixie fire and non-common years
df.50 <- df.50.raw %>%
  filter(
    fire != 'dixie',
    wy %in% common.years) %>%
  mutate(
    fire = recode(
      fire,
      'caldor' = 'Caldor',
      'creek' = 'Creek'
    ),
    fire = factor(
      fire,
      levels = fire.levels
    ),
    fire_burned = interaction(
      fire,
      burned,
      sep = '_'
    )
  ) %>%
  droplevels()


# make balanced prediction sample
df.pred <- df.50 %>%
  group_by(fire, wy, burned) %>%
  slice_sample(n = 1000) %>%
  mutate(
    fire = factor(
      fire,
      levels = fire.levels)) %>%
  ungroup()

# df.50.balanced <- readRDS(file.path(dir, 'df_50m_raw_balanced.rds')) 

# str(df.50.raw)

# df.50.raw.test <- df.50.raw %>%
#   group_by(fire) %>%
#   slice_sample(n = 10000) %>%
#   ungroup()

# df.50.balanced.test <- df.50.balanced %>%
#   group_by(fire) %>%
#   slice_sample(n = 10000) %>%
#   ungroup()


burn.cols <- c(
  'unburned' = '#00868B',
  'burned' = '#EE2C2C'
)

fire.colors <- c(
  'Caldor' = '#3b435c',
  'Castle' = '#ffa600',
  'Creek' = '#c55488'
)


elev.colors <- c(
  'Caldor_< 1750 m' = '#3b435c',
  'Caldor_1750–2500 m' = '#868eaa',
  'Caldor_> 2500 m' = '#d8e1ff',
  
  'Castle_< 1750 m' = '#ffa600',
  'Castle_1750–2500 m' = '#ffc171',
  'Castle_> 2500 m' = '#fbddbe',
  
  'Creek_< 1750 m' = '#c55488',
  'Creek_1750–2500 m' = '#e396b8',
  'Creek_> 2500 m' = '#ffd5e8'
)



# ----- functions -----
cv_bam_swe <- function(formula, data, k_folds = 5) {
  
  cv.results <- data.frame()
  cv.fire.results <- data.frame()
  
  for (fold in 1:k_folds) {
    
    train <- data %>%
      filter(fold_id != fold)
    
    test <- data %>%
      filter(fold_id == fold)
    
    model <- bam(
      formula,
      data = train,
      method = 'fREML',
      discrete = TRUE
    )
    
    # prediction on sqrt(SWE) scale
    pred.sqrt <- predict(
      model,
      newdata = test,
      type = 'response'
    )
    
    # back-transform to SWE
    pred <- pred.sqrt^2
    
    # observed SWE on original scale
    obs <- test$swe_peak
    
    # ----- overall fold metrics -----
    
    rmse <- sqrt(
      mean(
        (obs - pred)^2,
        na.rm = TRUE
      )
    )
    
    r2 <- cor(
      obs,
      pred,
      use = 'complete.obs'
    )^2
    
    cv.results <- bind_rows(
      cv.results,
      data.frame(
        fold = fold,
        RMSE = rmse,
        R2 = r2
      )
    )
    
    # ----- fire-specific metrics -----
    
    fire.results <- test %>%
      mutate(
        obs = obs,
        pred = pred
      ) %>%
      group_by(fire) %>%
      summarise(
        n = n(),
        RMSE = sqrt(
          mean(
            (obs - pred)^2,
            na.rm = TRUE
          )
        ),
        R2 = cor(
          obs,
          pred,
          use = 'complete.obs'
        )^2,
        .groups = 'drop'
      ) %>%
      mutate(
        fold = fold
      )
    
    cv.fire.results <- bind_rows(
      cv.fire.results,
      fire.results
    )
  }
  
  cv.summary <- cv.results %>%
    summarise(
      RMSE_mean = mean(RMSE),
      RMSE_sd = sd(RMSE),
      R2_mean = mean(R2),
      R2_sd = sd(R2)
    )
  
  list(
    fold.results = cv.results,
    fire.results = cv.fire.results,
    summary = cv.summary
  )
}
get.metrics <- function(fitted.model, model.name, fire.name) {
  
  s <- summary(fitted.model)
  
  data.frame(
    fire = fire.name,
    model_name = model.name,
    r.squared = s$r.sq,
    dev.expl = s$dev.expl,
    AIC = AIC(fitted.model),
    BIC = BIC(fitted.model),
    edf = sum(s$edf)
  )
}
get.metrics.combined <- function(fitted.model, model.name) {
  
  s <- summary(fitted.model)
  
  data.frame(
    model_name = model.name,
    r.squared = s$r.sq,
    dev.expl = s$dev.expl,
    AIC = AIC(fitted.model),
    BIC = BIC(fitted.model),
    edf = sum(s$edf)
  )
}
# ----- models -----
model.swe <- bam(sqrt(swe_peak) ~ wy * fire
                 + s(elevation, by = wy, k = 20)
                 + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                 + s(ht_zmax, by = fire, k = 10) + s(gap_percent, by = fire, k = 10),
                 data = df.50,
                 method = 'fREML',
                 discrete = TRUE)

model.swe.burned <- bam(sqrt(swe_peak) ~ wy * fire + burned * fire
                 + s(elevation, by = wy, k = 20)
                 + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                 + s(ht_zmax, by = fire_burned, k = 10) + s(gap_percent, by = fire_burned, k = 10),
                 data = df.50,
                 method = 'fREML',
                 discrete = TRUE)

# model.swe.combined <- bam(sqrt(swe_peak) ~ wy * fire
#                  + s(elevation, by = wy, k = 20)
#                  + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
#                  + s(ht_zmax, k = 10) + s(gap_percent, k = 10),
#                  data = df.50,
#                  method = 'fREML',
#                  discrete = TRUE)



# ==============================================================================
# Model Evaluation/
# ==============================================================================
# ----- Cross-fold Validation -----
# -- simple model ---
cv.swe <- cv_bam_swe(formula = formula(model.swe),
                 data = df.50,
                 k_folds = 5)

# fire-specific summary
cv.swe.summary.byfire <- cv.swe$fire.results %>%
  group_by(fire) %>%
  summarise(
    RMSE_mean = mean(RMSE),
    RMSE_sd = sd(RMSE),
    R2_mean = mean(R2),
    R2_sd = sd(R2),
    .groups = 'drop'
  )

# overall summary
cv.swe.summary.overall <- cv.swe$fold.results %>%
  summarise(
    fire = 'Overall',
    RMSE_mean = mean(RMSE),
    RMSE_sd = sd(RMSE),
    R2_mean = mean(R2),
    R2_sd = sd(R2)
  )

# combine
cv.swe.summary <- bind_rows(
  cv.swe.summary.overall,
  cv.swe.summary.byfire %>%
    mutate(fire = as.character(fire))
)

cv.swe.summary

# -- burned model ---
cv.swe.burned <- cv_bam_swe(formula = formula(model.swe.burned),
                     data = df.50,
                     k_folds = 5)

# fire-specific summary
cv.swe.burned.summary.byfire <- cv.swe.burned$fire.results %>%
  group_by(fire) %>%
  summarise(
    RMSE_mean = mean(RMSE),
    RMSE_sd = sd(RMSE),
    R2_mean = mean(R2),
    R2_sd = sd(R2),
    .groups = 'drop'
  )

# overall summary
cv.swe.burned.summary.overall <- cv.swe.burned$fold.results %>%
  summarise(
    fire = 'Overall',
    RMSE_mean = mean(RMSE),
    RMSE_sd = sd(RMSE),
    R2_mean = mean(R2),
    R2_sd = sd(R2)
  )

# combine
cv.swe.burned.summary <- bind_rows(
  cv.swe.burned.summary.overall,
  cv.swe.burned.summary.byfire %>%
    mutate(fire = as.character(fire))
)

cv.swe.burned.summary



# explore
# --- CV without skew ---
cv.swe.noskew <- cv_bam_swe(
  formula(model.swe.noskew),
  df.50
)

cv.skew.comparison <- bind_rows(
  'With skew' = cv.swe$summary,
  'Without skew' = cv.swe.noskew$summary,
  .id = 'model'
)

cv.skew.comparison

# --- CV burned ---
cv.swe.noskew <- cv_bam_swe(
  formula(model.swe.noskew),
  df.50
)

cv.skew.comparison <- bind_rows(
  'With skew' = cv.swe$summary,
  'Without skew' = cv.swe.noskew$summary,
  .id = 'model'
)

cv.skew.comparison
# ==============================================================================
# Generate Predictions - OLD
# ==============================================================================
# ------------------------ Prediction for gap percent for * realistic canopy conditions * ---------------------
# ----- build lookup table * for gap percent * -----

# Create a lookup table describing the typical canopy structure associated with different levels of canopy gap within each fire.
# This is used later to avoid predicting SWE for unrealistic combinations of correlated canopy variables.

canopy.lookup <- df.50 %>%
  
  # Calculate gap bins separately within each fire so that the relationships
  # between gap and the other canopy metrics can differ among fires.
  group_by(fire) %>%
  
  # Divide observations within each fire into 100 approximately equal-sized
  # groups based on gap_percent.
  mutate(
    gap.bin = ntile(gap_percent, 100)
  ) %>%
  
  # Summarize canopy conditions within each gap bin and fire.
  group_by(fire, gap.bin) %>%
  summarize(
    
    # Mean gap percentage represented by each bin.
    gap_lookup = mean(gap_percent, na.rm = TRUE),
    
    # Typical maximum canopy height associated with that amount of gap.
    ht_lookup = mean(ht_zmax, na.rm = TRUE),
    
    # Typical distance to canopy associated with that amount of gap.
    dist_lookup = mean(
      gap_dist_to_canopy_mean,
      na.rm = TRUE
    ),
    
    .groups = 'drop'
  )


# ----- prediction grid for gap percent -----

# Create all combinations of fire, water year, and burn status at which predictions will be generated. Gap percentage varies from 0 to 100%.
# This creates: 3 fires x 3 water years x 2 burn classes x 101 gap values = 1818 rows.

pred.gap.real <- expand.grid(
  fire = levels(df.50$fire),
  wy = levels(df.50$wy),
  burned = levels(df.50$burned),
  gap_percent = seq(0, 100, length.out = 101)
) %>%
  as_tibble()


# static variables - representative value within each fire

# Calculate representative values for predictors that are effectively static landscape/canopy characteristics.
# Fire-specific medians are used so predictions represent typical conditions within each study area rather than one global condition.

ref.fire.real <- df.50 %>%
  group_by(fire) %>%
  summarize(
    rad_dtm_accum = median(rad_dtm_accum, na.rm = TRUE),
    slope = median(slope, na.rm = TRUE),
    aspect_sin = median(aspect_sin, na.rm = TRUE),
    tpi150 = median(tpi150, na.rm = TRUE),
    tpi2010 = median(tpi2010, na.rm = TRUE),
    ht_zskew = median(ht_zskew, na.rm = TRUE),
    .groups = 'drop'
  )


# elevation can differ by fire and water year

# Calculate representative elevation separately for each fire and water year. This allows predictions to reflect differences 
# in the elevation distribution among fire-year combinations, including differences caused by annual snowline filtering.

ref.elev.real <- df.50 %>%
  group_by(fire, wy) %>%
  summarize(
    elevation = median(elevation, na.rm = TRUE),
    .groups = 'drop'
  )


# Add the representative fire-specific and fire-by-year covariate values to every row of the prediction grid.

pred.gap.real <- pred.gap.real %>%
  left_join(ref.fire.real, by = 'fire') %>%
  left_join(ref.elev.real, by = c('fire', 'wy'))


# Assign realistic values of the canopy variables that are correlated with gap_percent. Rather than holding ht_zmax and 
# gap_dist_to_canopy_mean atfixed values while gap changes, allow them to vary according to their observed relationship with 
# gap within each fire.

pred.gap.real <- pred.gap.real %>%
  group_by(fire) %>%
  group_modify(~ {
    
    # Select the lookup values corresponding to the current fire and arrange them from lowest to highest gap percentage.
    lookup.fire <- canopy.lookup %>%
      filter(fire == .y$fire) %>%
      arrange(gap_lookup)
    
    .x %>%
      mutate(
        
        # Interpolate the typical maximum canopy height corresponding to each gap percentage in the prediction grid.
        ht_zmax = approx(
          x = lookup.fire$gap_lookup,
          y = lookup.fire$ht_lookup,
          xout = gap_percent,
          rule = 2
        )$y,
        
        # Interpolate the typical mean distance to canopy corresponding to each gap percentage in the prediction grid.
        gap_dist_to_canopy_mean = approx(
          x = lookup.fire$gap_lookup,
          y = lookup.fire$dist_lookup,
          xout = gap_percent,
          rule = 2
        )$y
        
      )
  }) %>%
  ungroup()


# ----- generate model predictions -----

# Predict peak SWE for every row of the prediction grid.
# Because the response in model.swe is sqrt(swe_peak), these predictions and their standard errors are initially on the square-root SWE scale.

p <- predict(
  model.swe,
  newdata = pred.gap.real,
  type = 'response',
  se.fit = TRUE
)


# Extract the estimated residual variance from the fitted GAM.
# This can be used to approximately correct the mean prediction when transforming predictions from sqrt(SWE) back to the original SWE scale.

sigma2 <- summary(model.swe)$scale


# Add fitted values, confidence limits, and back-transformed SWE predictions to the prediction dataset.

pred.gap.real <- pred.gap.real %>%
  mutate(
    
    # Predicted value and standard error on the sqrt(SWE) scale.
    fit.sqrt = p$fit,
    se.sqrt = p$se.fit,
    
    # Approximate 95% confidence interval on the sqrt(SWE) scale.
    lower.sqrt = fit.sqrt - 1.96 * se.sqrt,
    upper.sqrt = fit.sqrt + 1.96 * se.sqrt,
    
    # direct back-transformation
    fit.swe = fit.sqrt^2,
    
    # approximate expected SWE on original scale
    # Adds the residual variance to account for bias introduced when squaring predictions from a square-root-transformed response.
    fit.swe.mean = fit.sqrt^2 + sigma2,
    
    # Back-transform the confidence limits.
    # pmax() prevents a negative lower sqrt(SWE) bound from becoming positive again when squared.
    lower.swe = pmax(0, lower.sqrt)^2,
    upper.swe = pmax(0, upper.sqrt)^2
  )


# ----- plot predicted SWE across canopy gap -----

# Plot predicted peak SWE across the realistic canopy-gap gradient.
# Burned and unburned predictions are generated using identical canopy configurations, so separation between the two curves represents the
# remaining modeled effect of burn status after accounting for canopy structure and the other model predictors.

# -- all years --
ggplot(
  pred.gap.real,
  aes(
    x = gap_percent,
    y = fit.swe,
    color = burned,
    fill = burned
  )
) +
  geom_ribbon(
    aes(
      ymin = lower.swe,
      ymax = upper.swe
    ),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_grid(wy ~ fire) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE',
    color = 'Burn status',
    fill = 'Burn status'
  ) +
  theme_bw()

# -- 2023 --
ggplot(
  pred.gap.real %>%
    filter(wy == '2023'),
  aes(
    x = gap_percent,
    y = fit.swe,
    color = burned,
    fill = burned
  )
) +
  geom_ribbon(
    aes(
      ymin = lower.swe,
      ymax = upper.swe
    ),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~fire) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE',
    color = 'Burn status',
    fill = 'Burn status'
  ) +
  theme_bw()


# average across years
pred.gap.real.avg <- pred.gap.real %>%
  group_by(
    fire,
    burned,
    gap_percent
  ) %>%
  summarize(
    fit.swe = mean(fit.swe),
    .groups = 'drop'
  )

ggplot(
  pred.gap.real.avg,
  aes(
    x = gap_percent,
    y = fit.swe,
    color = burned
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(~fire) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Mean predicted peak SWE',
    color = 'Burn status'
  ) +
  theme_bw()














# ------------------------- Prediction for gap percent * regular pdp * --------------------------
# ----- prediction grid for gap percent -----

# Create prediction grid across the full range of gap percent for each fire, water year, and burn status
pred.gap <- expand.grid(
  fire = levels(df.50$fire),
  wy = levels(df.50$wy),
  burned = levels(df.50$burned),
  gap_percent = seq(0, 100, length.out = 101)
) %>%
  as_tibble()


# ----- representative values for static predictors -----

# Hold all other static predictors at their median value within each fire
ref.fire <- df.50 %>%
  group_by(fire) %>%
  summarize(
    rad_dtm_accum = median(rad_dtm_accum, na.rm = TRUE),
    slope = median(slope, na.rm = TRUE),
    aspect_sin = median(aspect_sin, na.rm = TRUE),
    tpi150 = median(tpi150, na.rm = TRUE),
    tpi2010 = median(tpi2010, na.rm = TRUE),
    ht_zmax = median(ht_zmax, na.rm = TRUE),
    gap_dist_to_canopy_mean = median(
      gap_dist_to_canopy_mean,
      na.rm = TRUE
    ),
    ht_zskew = median(ht_zskew, na.rm = TRUE),
    .groups = 'drop'
  )


# ----- representative elevation -----

# Use median elevation within each fire and water year
ref.elev <- df.50 %>%
  group_by(fire, wy) %>%
  summarize(
    elevation = median(elevation, na.rm = TRUE),
    .groups = 'drop'
  )


# ----- add representative values to prediction dataset -----

pred.gap <- pred.gap %>%
  left_join(ref.fire, by = 'fire') %>%
  left_join(ref.elev, by = c('fire', 'wy'))


# ----- generate predictions -----

# Predictions are initially on the sqrt(SWE) scale
p <- predict(
  model.swe.burned,
  newdata = pred.gap,
  type = 'response',
  se.fit = TRUE
)

# residual variance for approximate bias correction
sigma2 <- summary(model.swe.burned)$scale


# ----- back-transform predictions to SWE -----

pred.gap <- pred.gap %>%
  mutate(
    fit.sqrt = p$fit,
    se.sqrt = p$se.fit,
    
    lower.sqrt = fit.sqrt - 1.96 * se.sqrt,
    upper.sqrt = fit.sqrt + 1.96 * se.sqrt,
    
    # direct back-transformation
    fit.swe = fit.sqrt^2,
    
    # approximate expected SWE after back-transformation
    fit.swe.mean = fit.sqrt^2 + sigma2,
    
    lower.swe = pmax(0, lower.sqrt)^2,
    upper.swe = pmax(0, upper.sqrt)^2
  )

# ----- plot -----
# -- all years --
ggplot(
  pred.gap,
  aes(
    x = gap_percent,
    y = fit.swe,
    color = burned,
    fill = burned
  )
) +
  geom_ribbon(
    aes(
      ymin = lower.swe,
      ymax = upper.swe
    ),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_grid(wy ~ fire) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE',
    color = 'Burn status',
    fill = 'Burn status'
  ) +
  theme_bw()

# -- 2023 --
ggplot(
  pred.gap %>%
    filter(wy == '2023'),
  aes(
    x = gap_percent,
    y = fit.swe,
    color = burned,
    fill = burned
  )
) +
  geom_ribbon(
    aes(
      ymin = lower.swe,
      ymax = upper.swe
    ),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~fire) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE',
    color = 'Burn status',
    fill = 'Burn status'
  ) +
  theme_bw()


# --- average across years ---
pred.gap.avg <- pred.gap %>%
  group_by(
    fire,
    burned,
    gap_percent
  ) %>%
  summarize(
    fit.swe = mean(fit.swe),
    .groups = 'drop'
  )

ggplot(
  pred.gap.avg,
  aes(
    x = gap_percent,
    y = fit.swe,
    color = burned
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(~fire) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Mean predicted peak SWE',
    color = 'Burn status'
  ) +
  theme_bw()




# ==============================================================================
# Generate Predictions - Fire-specific
# ==============================================================================
# ----------------------------------- ** MARGINAL EFFECT PLOTS ** ------------------------------------
# --------------- gap percent ----------------

# --- simulate coefficients for CI interval ---
#  simulate 500 plausible sets of model coefficients based on the fitted coefficients and their uncertainty; used to calculate 95% confidence intervals
set.seed(61)
n.sim <- 500

beta.sim <- MASS::mvrnorm(
  n = n.sim,
  mu = coef(model.swe),
  Sigma = vcov(model.swe)
)

# --- predict ---
gap.pred <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
  # prediction sample for fire
  df.fire <- df.pred %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # create 100 gap values spanning the fire-specific 1st–99th percentile range from the full dataset
  gap.seq <- seq(
    quantile(
      df.50$gap_percent[df.50$fire == fire.name],
      0.01,
      na.rm = TRUE
    ),
    quantile(
      df.50$gap_percent[df.50$fire == fire.name],
      0.99,
      na.rm = TRUE
    ),
    length.out = 100
  )
  
  map_dfr(gap.seq, function(gap.value) {
    
    # set gap percent to the focal value for all observations while keeping all other predictors at their observed values
    newdata <- df.fire %>%
      mutate(gap_percent = gap.value)
    
    # predict sqrt(SWE), back-transform each prediction, and average across observations to get marginal predicted SWE
    pred <- predict(
      model.swe,
      newdata = newdata,
      type = 'response'
    )
    
    fit <- mean(pred^2)
    
    # create linear predictor matrix so predictions can be recalculated using each of the 500 simulated coefficient sets
    Xp <- predict(
      model.swe,
      newdata = newdata,
      type = 'lpmatrix'
    )
    
    # predict sqrt(SWE) for each observation using each of the 500 plausible coefficient sets
    sim.sqrt.swe <- Xp %*% t(beta.sim)
    
    # back-transform each observation, then average within simulation
    sim.swe <- colMeans(sim.sqrt.swe^2)
    
    # save central marginal prediction and 95% confidence interval from simulated coefficient uncertainty
    tibble(
      fire = fire.name,
      gap_percent = gap.value,
      swe_peak = fit,
      lower = quantile(sim.swe, 0.025),
      upper = quantile(sim.swe, 0.975)
    )
  })
})

# gap.rug <- df.50 %>%
#   group_by(fire) %>%
#   slice_sample(n = 3000) %>%
#   ungroup()

# --- observed density for gap percent ---

gap.density.0 <- df.50 %>%
  filter(
    !is.na(gap_percent),
    !is.na(fire)
  ) %>%
  mutate(
    gap_bin = cut(
      gap_percent,
      breaks = seq(0, 100, by = 5),
      include.lowest = TRUE
    )
  ) %>%
  group_by(
    fire,
    gap_bin
  ) %>%
  summarise(
    gap_percent = mean(gap_percent, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )

gap.density <- gap.density.0 %>%
  group_by(fire) %>%
  mutate(
    swe_peak = approx(
      x = gap.pred$gap_percent[
        gap.pred$fire == first(fire)
      ],
      y = gap.pred$swe_peak[
        gap.pred$fire == first(fire)
      ],
      xout = gap_percent,
      rule = 1
    )$y
  ) %>%
  ungroup() %>%
  filter(!is.na(swe_peak),
         fire != 'Castle') 

# --- Identify thresholds/optima ---
gap.optimum <- gap.pred %>%
  group_by(fire) %>%
  summarise(
    max.swe = max(swe_peak),
    
    gap.at.max = gap_percent[which.max(swe_peak)],
    
    threshold.95 = 0.95 * max.swe,
    
    gap.95.low = min(
      gap_percent[swe_peak >= threshold.95]
    ),
    
    gap.95.high = max(
      gap_percent[swe_peak >= threshold.95]
    )
  )

gap.optimum

# # --- Combined model ---
# gap.pred.combined <- map_dfr(
#   levels(df.pred$fire),
#   function(fire.name) {
#     
#     df.fire <- df.pred %>%
#       filter(fire == fire.name) %>%
#       droplevels()
#     
#     gap.seq <- seq(
#       quantile(
#         df.50$gap_percent[df.50$fire == fire.name],
#         0.01,
#         na.rm = TRUE
#       ),
#       quantile(
#         df.50$gap_percent[df.50$fire == fire.name],
#         0.99,
#         na.rm = TRUE
#       ),
#       length.out = 100
#     )
#     
#     map_dfr(gap.seq, function(gap.value) {
#       
#       newdata <- df.fire %>%
#         mutate(gap_percent = gap.value)
#       
#       pred <- predict(
#         model.swe.combined,
#         newdata = newdata,
#         type = 'response'
#       )
#       
#       tibble(
#         fire = fire.name,
#         gap_percent = gap.value,
#         swe_peak = mean(pred^2, na.rm = TRUE)
#       )
#     })
#   }
# )

# filter out Castle
gap.pred.plot <- gap.pred %>%
  filter(fire != 'Castle')
gap.optimum.plot <- gap.optimum %>%
  filter(fire != 'Castle') 
gap.rug.plot <- gap.rug %>%
  filter(fire != 'Castle') 

# ----- plot -----
p.gap.opt <- ggplot(
  gap.pred.plot,
  aes(
    x = gap_percent,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  
  geom_rect(
    data = gap.optimum.plot,
    aes(
      xmin = gap.95.low,
      xmax = gap.95.high,
      ymin = -Inf,
      ymax = Inf,
      fill = fire
    ),
    inherit.aes = FALSE,
    alpha = 0.08,
    color = NA
  ) +
  
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.3,
    color = NA
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  # geom_line(
  #   data = gap.pred.combined %>%
  #     filter(fire != 'Castle'),
  #   aes(
  #     x = gap_percent,
  #     y = swe_peak,
  #     group = fire
  #   ),
  #   inherit.aes = FALSE,
  #   color = 'grey',
  #   linetype = 'solid',
  #   linewidth = 0.8
  # ) +
  
  # geom_rug(
  #   data = gap.rug.plot,
  #   aes(
  #     x = gap_percent,
  #     color = fire
  #   ),
  #   inherit.aes = FALSE,
  #   sides = 'b',
  #   alpha = 0.08
  # ) +
  
  geom_point(
    data = gap.density,
    aes(
      x = gap_percent,
      y = swe_peak,
      size = n,
      color = fire
    ),
    inherit.aes = FALSE,
    alpha = 0.7
  ) +
  
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE (m)'
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1,
    drop = FALSE
  ) +
  
  geom_segment(
    data = gap.optimum.plot,
    aes(
      x = gap.95.low,
      xend = gap.95.high,
      y = threshold.95,
      yend = threshold.95,
      color = fire
    ),
    inherit.aes = FALSE,
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  
  scale_size_continuous(
    name = 'Observations',
    limits = c(0, 250000),
    breaks = c(
      50000,
      100000,
      150000,
      250000
    ),
    labels = scales::comma,
    range = c(1.5, 5)

  ) +
  
  guides(
    color = 'none',
    fill = 'none',
    size = 'none'
  ) +
  
  theme_classic() +

  theme(
    legend.position = 'none'
  )
  


p.gap.opt

# ----- summary results -----
gap.summary <- gap.pred %>%
  group_by(fire) %>%
  filter(fire != 'Castle') %>%
  summarise(
    gap_min = min(gap_percent),
    gap_max = max(gap_percent),
    
    swe_at_gap_min = swe_peak[which.min(gap_percent)],
    swe_at_gap_max = swe_peak[which.max(gap_percent)],
    
    max_swe = max(swe_peak),
    gap_at_max = gap_percent[which.max(swe_peak)],
    
    increase_to_max = max_swe - swe_at_gap_min,
    
    increase_to_max_pct =
      (max_swe - swe_at_gap_min) /
      swe_at_gap_min * 100,
    
    change_full_range =
      swe_at_gap_max - swe_at_gap_min,
    
    lower_at_max = lower[which.max(swe_peak)],
    upper_at_max = upper[which.max(swe_peak)],
    
    threshold_95 = 0.95 * max_swe,
    
    gap_95_low = min(
      gap_percent[swe_peak >= threshold_95]
    ),
    
    gap_95_high = max(
      gap_percent[swe_peak >= threshold_95]
    ),
    
    .groups = 'drop'
  )

print(gap.summary, width = Inf)
# ---------------- maximum canopy height ---------------

ht.pred <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
  # prediction sample for fire
  df.fire <- df.pred %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # fire-specific prediction range
  ht.seq <- seq(
    quantile(
      df.50$ht_zmax[df.50$fire == fire.name],
      0.01,
      na.rm = TRUE
    ),
    quantile(
      df.50$ht_zmax[df.50$fire == fire.name],
      0.99,
      na.rm = TRUE
    ),
    length.out = 100
  )
  
  map_dfr(ht.seq, function(ht.value) {
    
    newdata <- df.fire %>%
      mutate(ht_zmax = ht.value)
    
    # central prediction
    pred <- predict(
      model.swe,
      newdata = newdata,
      type = 'response'
    )
    
    fit <- mean(pred^2)
    
    # linear predictor matrix for uncertainty
    Xp <- predict(
      model.swe,
      newdata = newdata,
      type = 'lpmatrix'
    )
    
    # predictions for simulated coefficients
    sim.sqrt.swe <- Xp %*% t(beta.sim)
    
    # back-transform each observation, then average
    sim.swe <- colMeans(sim.sqrt.swe^2)
    
    tibble(
      fire = fire.name,
      ht_zmax = ht.value,
      swe_peak = fit,
      lower = quantile(sim.swe, 0.025),
      upper = quantile(sim.swe, 0.975)
    )
  })
})

# ht.rug <- df.50 %>%
#   group_by(fire) %>%
#   slice_sample(n = 3000) %>%
#   ungroup()

ht.density <- df.50 %>%
  filter(
    !is.na(ht_zmax),
    !is.na(fire)
  ) %>%
  mutate(
    ht_bin = cut(
      ht_zmax,
      breaks = seq(0, 100, by = 3),
      include.lowest = TRUE
    )
  ) %>%
  group_by(
    fire,
    ht_bin
  ) %>%
  summarise(
    ht_zmax = mean(ht_zmax, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  group_by(fire) %>%
  mutate(
    swe_peak = approx(
      x = ht.pred$ht_zmax[
        ht.pred$fire == first(fire)
      ],
      y = ht.pred$swe_peak[
        ht.pred$fire == first(fire)
      ],
      xout = ht_zmax,
      rule = 1
    )$y
  ) %>%
  ungroup() %>%
  filter(!is.na(swe_peak))

# # --- combined model ---
# ht.pred.combined <- map_dfr(
#   levels(df.pred$fire),
#   function(fire.name) {
#     
#     # prediction sample for this fire
#     df.fire <- df.pred %>%
#       filter(fire == fire.name) %>%
#       droplevels()
#     
#     # fire-specific range of ht_zmax
#     ht.seq <- seq(
#       quantile(
#         df.50$ht_zmax[df.50$fire == fire.name],
#         0.01,
#         na.rm = TRUE
#       ),
#       quantile(
#         df.50$ht_zmax[df.50$fire == fire.name],
#         0.99,
#         na.rm = TRUE
#       ),
#       length.out = 100
#     )
#     
#     map_dfr(ht.seq, function(ht.value) {
#       
#       newdata <- df.fire %>%
#         mutate(
#           ht_zmax = ht.value
#         )
#       
#       # predictions on sqrt(SWE) scale
#       pred <- predict(
#         model.swe.combined,
#         newdata = newdata,
#         type = 'response'
#       )
#       
#       tibble(
#         fire = fire.name,
#         ht_zmax = ht.value,
#         swe_peak = mean(pred^2, na.rm = TRUE)
#       )
#     })
#   }
# )
# --- identify thresholds/optima ---

ht.optimum <- ht.pred %>%
  group_by(fire) %>%
  summarise(
    max.swe = max(swe_peak),
    
    ht.at.max = ht_zmax[which.max(swe_peak)],
    
    threshold.95 = 0.95 * max.swe,
    
    ht.95.low = min(
      ht_zmax[swe_peak >= threshold.95]
    ),
    
    ht.95.high = max(
      ht_zmax[swe_peak >= threshold.95]
    )
  )


# --- reorder ---
ht.pred <- ht.pred %>%
  mutate(
    fire = factor(fire, levels = fire.levels)
  )
ht.optimum <- ht.optimum %>%
  mutate(
    fire = factor(fire, levels = fire.levels)
  )
# ht.rug <- ht.rug %>%
#   mutate(
#     fire = factor(fire, levels = fire.levels)
#   )

# ----- plot -----
p.ht.opt <- ggplot(
  ht.pred,
  aes(
    x = ht_zmax,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  
  # range producing >=95% of maximum predicted SWE
  geom_rect(
    data = ht.optimum,
    aes(
      xmin = ht.95.low,
      xmax = ht.95.high,
      ymin = -Inf,
      ymax = Inf,
      fill = fire
    ),
    inherit.aes = FALSE,
    alpha = 0.08,
    color = NA
  ) +
  
  # 95% CI
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.3,
    color = NA
  ) +
  
  # marginal prediction
  geom_line(linewidth = 1) +
  
  # 95% of maximum threshold
  geom_segment(
    data = ht.optimum,
    aes(
      x = ht.95.low,
      xend = ht.95.high,
      y = threshold.95,
      yend = threshold.95,
      color = fire
    ),
    inherit.aes = FALSE,
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  # geom_line(
  #   data = ht.pred.combined,
  #   aes(
  #     x = ht_zmax,
  #     y = swe_peak
  #   ),
  #   inherit.aes = FALSE,
  #   color = 'grey',
  #   linetype = 'solid',
  #   linewidth = 0.8
  # ) +
  
  # # observed data support
  # geom_rug(
  #   data = ht.rug,
  #   aes(x = ht_zmax, color = fire),
  #   inherit.aes = FALSE,
  #   sides = 'b',
  #   alpha = 0.08
  # ) +
  
  geom_point(
    data = ht.density,
    aes(
      x = ht_zmax,
      y = swe_peak,
      size = n,
      color = fire
    ),
    inherit.aes = FALSE,
    alpha = 0.7
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  
  scale_size_continuous(
    name = 'Observations',
    limits = c(0, 250000),
    breaks = c(
      50000,
      100000,
      150000,
      250000
    ),
    labels = scales::comma,
    range = c(1.5, 5)
  ) +
  
  scale_x_continuous(
    limits = c(0, 65),
    breaks = seq(0, 60, by = 20)) +
  
  scale_y_continuous(
    limits = c(0.38, 0.82),
    breaks = seq(0.4, 0.8, 0.1)
  ) +

  theme_classic() +
  
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Predicted peak SWE (m)',
    size = 'Observations'
  ) +
  
  guides(
    color = 'none',
    fill = 'none',
    size = 'none'
  ) 



p.ht.opt


# ----- summary results -----
ht.summary <- ht.pred %>%
  group_by(fire) %>%
  summarise(
    ht_min = min(ht_zmax),
    ht_max = max(ht_zmax),
    
    swe_at_ht_min = swe_peak[which.min(ht_zmax)],
    swe_at_ht_max = swe_peak[which.max(ht_zmax)],
    
    max_swe = max(swe_peak),
    ht_at_max = ht_zmax[which.max(swe_peak)],
    
    increase_to_max = max_swe - swe_at_ht_min,
    
    increase_to_max_pct =
      (max_swe - swe_at_ht_min) /
      swe_at_ht_min * 100,
    
    change_full_range =
      swe_at_ht_max - swe_at_ht_min,
    
    lower_at_max = lower[which.max(swe_peak)],
    upper_at_max = upper[which.max(swe_peak)],
    
    threshold_95 = 0.95 * max_swe,
    
    ht_95_low = min(
      ht_zmax[swe_peak >= threshold_95]
    ),
    
    ht_95_high = max(
      ht_zmax[swe_peak >= threshold_95]
    ),
    
    .groups = 'drop'
  )

print(ht.summary, width = Inf)
# ------------------ combined plot -------------------
library(patchwork)
library(grid)

# --- common theme ---

optimum.theme <- theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = 'bold',
      size = 10
    ),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 9),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

gap.legend <- cowplot::get_legend(
  p.gap.opt +
    guides(size = guide_legend()) +
    theme(legend.position = 'right')
)

# create spacer for where top castle panel would be and put legend there
gap.row <- (
  p.gap.opt |
    wrap_elements(gap.legend)
) +
  plot_layout(
    widths = c(2, 1),
    guides = 'keep'
  )



canopy.optimum.fig <- (
  gap.row /
    p.ht.opt
) +
  plot_layout(
    guides = 'keep'
  ) &
  optimum.theme &
  scale_y_continuous(
    limits = c(0.38, 0.82),
    breaks = seq(0.4, 0.8, 0.1)
  )

canopy.optimum.fig


# ----------------------------------- ** OPTIMUM CANOPY CONDITIONS ** ------------------------------------
# ---------- gap percent x max canopy height ----------
# ---------- creek -----

df.creek.marg <- df.pred %>%
  filter(fire == 'Creek')

# --- observed canopy combinations ---

canopy.combos <- df.creek %>%
  mutate(
    gap_bin = round(gap_percent / 2) * 2, # round gap_percent to nearest 2%
    ht_bin = round(ht_zmax / 2) * 2 # round ht_zmax to nearest 2m
  ) %>%
  count(gap_bin, ht_bin, name = 'n') %>% # count how many are in each bin 
  filter(n >= 100) # keep only when there's a sufficient number of obs

# --- marginal predictions across observed canopy combinations ---

canopy.combos$pred.swe <- NA_real_

for (i in seq_len(nrow(canopy.combos))) {
  
  # Assign one observed canopy combination to all observations in the marginalization sample.
  # All other model predictors retain their observed values.
  newdata <- df.creek.marg %>%
    mutate(
      gap_percent = canopy.combos$gap_bin[i],
      ht_zmax = canopy.combos$ht_bin[i]
    )
  
  # Predict sqrt(SWE) for all background observations.
  pred <- predict(
    model.swe,
    newdata = newdata,
    type = 'response'
  )

  # Back-transform the marginal mean to SWE.
  canopy.combos$pred.swe[i] <- mean(pred^2)
}

plot.grid <- expand.grid(
  gap_bin = seq(
    min(canopy.combos$gap_bin),
    max(canopy.combos$gap_bin),
    by = 2
  ),
  ht_bin = seq(
    min(canopy.combos$ht_bin),
    max(canopy.combos$ht_bin),
    by = 2
  )
) %>%
  left_join(
    canopy.combos,
    by = c('gap_bin', 'ht_bin'))

# ----- results summary -----

# maximum predicted SWE
max.swe <- max(canopy.combos$pred.swe, na.rm = TRUE)

# minimum predicted SWE
min.swe <- min(canopy.combos$pred.swe, na.rm = TRUE)

# 95% of maximum
threshold.95 <- 0.95 * max.swe

# exact maximum
optimum <- canopy.combos %>%
  filter(pred.swe == max.swe)

# canopy combinations within 95% of maximum
optimum.95 <- canopy.combos %>%
  filter(pred.swe >= threshold.95)

# summarize results
optimum.summary <- tibble(
  min_pred_swe = min.swe,
  max_pred_swe = max.swe,
  swe_range = max.swe - min.swe,
  
  optimum_gap = optimum$gap_bin[1],
  optimum_height = optimum$ht_bin[1],
  
  threshold_95 = threshold.95,
  
  gap_95_min = min(optimum.95$gap_bin),
  gap_95_max = max(optimum.95$gap_bin),
  
  height_95_min = min(optimum.95$ht_bin),
  height_95_max = max(optimum.95$ht_bin),
  
  n_combos_95 = nrow(optimum.95)
)

optimum.summary <- optimum.summary %>%
  mutate(
    swe_difference_pct = 
      (max_pred_swe - min_pred_swe) / min_pred_swe * 100
  )

print(optimum.summary, width = Inf)


# ----- exploring results -----
creek.interaction <- canopy.combos %>%
  mutate(
    gap_group = cut(
      gap_bin,
      breaks = c(0, 50, 75, 90, 100),
      labels = c('0–50%', '50–75%', '75–90%', '90–100%'),
      include.lowest = TRUE
    )
  ) %>%
  group_by(gap_group, ht_bin) %>%
  summarise(
    pred.swe = mean(pred.swe),
    .groups = 'drop'
  )

ggplot(
  creek.interaction,
  aes(
    x = ht_bin,
    y = pred.swe,
    color = gap_group
  )
) +
  geom_line(linewidth = 1) +
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Predicted SWE (m)',
    color = 'Gap percent'
  ) +
  theme_classic()

creek.height <- canopy.combos %>%
  filter(gap_bin %in% c(40, 60, 80, 96))

ggplot(
  creek.height,
  aes(
    x = ht_bin,
    y = pred.swe,
    color = factor(gap_bin)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Predicted SWE (m)',
    color = 'Gap percent'
  ) +
  theme_classic()
# ----- plot -----
p.creek.optimum <- ggplot(
  plot.grid,
  aes(
    x = gap_bin,
    y = ht_bin
  )
) +
  
  # marginal predicted SWE
  geom_tile(
    aes(fill = pred.swe),
    width = 2,
    height = 2
  ) +
  
  # # boundary around >= 95% of maximum
  # geom_contour(
  #   aes(z = optimum.95),
  #   breaks = 0.5,
  #   color = 'grey60',
  #   linewidth = 0.8
  # ) +
  
  # color fill
  scale_fill_viridis_c(
    option = 'viridis',
    na.value = NA,
    name = 'Predicted SWE (m)'
  ) +
  
  
  labs(
    title = 'Creek',
    x = 'Gap percent',
    y = 'Maximum canopy height (m)',
    fill = 'Predicted SWE (m)'
  ) +
  
  theme_classic() 

p.creek.optimum
# ---------- castle -----

# subset to Castle
df.castle <- df.50 %>%
  filter(fire == 'Castle') %>%
  mutate(
    fire = factor(
      'Castle',
      levels = levels(model.frame(model.swe)$fire)
    )
  )


# --- observed canopy combinations ---

canopy.combos.castle <- df.castle %>%
  mutate(
    gap_bin = round(gap_percent / 2) * 2,
    ht_bin = round(ht_zmax / 2) * 2
  ) %>%
  count(gap_bin, ht_bin, name = 'n') %>%
  filter(n >= 100)

nrow(canopy.combos.castle)
summary(canopy.combos.castle$n)

# --- marginalization sample ---

set.seed(61)

df.castle.marg <- df.castle %>%
  group_by(wy, burned) %>%
  slice_sample(n = 1000) %>%
  ungroup()


# --- marginal predictions ---

canopy.combos.castle$pred.sqrt <- NA_real_
canopy.combos.castle$pred.swe <- NA_real_

for (i in seq_len(nrow(canopy.combos.castle))) {
  
  newdata <- df.castle.marg %>%
    mutate(
      gap_percent = canopy.combos.castle$gap_bin[i],
      ht_zmax = canopy.combos.castle$ht_bin[i]
    )
  
  pred <- predict(
    model.swe,
    newdata = newdata,
    type = 'response'
  )
  
  canopy.combos.castle$pred.sqrt[i] <- mean(pred)
  canopy.combos.castle$pred.swe[i] <- mean(pred)^2
}


# --- identify optimum ---

max.swe.castle <- max(
  canopy.combos.castle$pred.swe,
  na.rm = TRUE
)

threshold.95.castle <- 0.95 * max.swe.castle

optimum.castle <- canopy.combos.castle %>%
  filter(pred.swe == max.swe.castle)

optimum.95.castle <- canopy.combos.castle %>%
  filter(pred.swe >= threshold.95.castle)

optimum.castle

optimum.95.castle %>%
  summarise(
    gap.low = min(gap_bin),
    gap.high = max(gap_bin),
    ht.low = min(ht_bin),
    ht.high = max(ht_bin),
    n.combinations = n()
  )

# --- plotting grid ---

plot.grid.castle <- expand.grid(
  gap_bin = seq(
    min(canopy.combos.castle$gap_bin),
    max(canopy.combos.castle$gap_bin),
    by = 2
  ),
  ht_bin = seq(
    min(canopy.combos.castle$ht_bin),
    max(canopy.combos.castle$ht_bin),
    by = 2
  )
) %>%
  left_join(
    canopy.combos.castle,
    by = c('gap_bin', 'ht_bin')
  ) %>%
  mutate(
    optimum.95 = ifelse(
      !is.na(pred.swe) &
        pred.swe >= threshold.95.castle,
      1,
      0
    )
  )

# --- plot ---
ggplot(
  plot.grid.castle,
  aes(
    x = gap_bin,
    y = ht_bin
  )
) +
  
  # marginal predicted SWE
  geom_tile(
    aes(fill = pred.swe),
    width = 2,
    height = 2
  ) +
  
  # boundary around >= 95% of maximum
  geom_contour(
    aes(z = optimum.95),
    breaks = 0.5,
    color = 'grey60',
    linewidth = 0.8
  ) +
  
  # color fill
  scale_fill_viridis_c(
    option = 'viridis',
    na.value = NA,
    name = 'Predicted SWE (m)'
  ) +
  
  labs(
    x = 'Gap percent',
    y = 'Maximum canopy height (m)',
    fill = 'Predicted SWE (m)'
  ) +
  
  theme_bw()




# ---------- caldor -----

# --- observed canopy combinations ---

canopy.combos.caldor <- df.caldor %>%
  mutate(
    gap_bin = round(gap_percent / 2) * 2,
    ht_bin = round(ht_zmax / 2) * 2
  ) %>%
  count(gap_bin, ht_bin, name = 'n') %>%
  filter(n >= 100)


# --- marginalization sample ---
df.caldor.marg <- df.pred %>%
  filter(fire == 'Caldor')


# --- marginal predictions ---

canopy.combos.caldor$pred.swe <- NA_real_

for (i in seq_len(nrow(canopy.combos.caldor))) {
  
  # assign one canopy combination while retaining all other observed conditions in the marginalization sample
  newdata <- df.caldor.marg %>%
    mutate(
      gap_percent = canopy.combos.caldor$gap_bin[i],
      ht_zmax = canopy.combos.caldor$ht_bin[i]
    )
  
  # predict sqrt(SWE)
  pred <- predict(
    model.swe,
    newdata = newdata,
    type = 'response'
  )
  
  # back-transform SWE
  canopy.combos.caldor$pred.swe[i] <- mean(pred^2)
}

# --- plotting grid ---

plot.grid.caldor <- expand.grid(
  gap_bin = seq(
    min(canopy.combos.caldor$gap_bin),
    max(canopy.combos.caldor$gap_bin),
    by = 2
  ),
  ht_bin = seq(
    min(canopy.combos.caldor$ht_bin),
    max(canopy.combos.caldor$ht_bin),
    by = 2
  )
) %>%
  left_join(
    canopy.combos.caldor,
    by = c('gap_bin', 'ht_bin')
  )

# ----- results summary -----

# --- identify optimum ---

max.swe.caldor <- max(
  canopy.combos.caldor$pred.swe,
  na.rm = TRUE
)

threshold.95.caldor <- 0.95 * max.swe.caldor

optimum.caldor <- canopy.combos.caldor %>%
  filter(pred.swe == max.swe.caldor)

optimum.95.caldor <- canopy.combos.caldor %>%
  filter(pred.swe >= threshold.95.caldor)

optimum.95.caldor %>%
  summarise(
    gap.low = min(gap_bin),
    gap.high = max(gap_bin),
    ht.low = min(ht_bin),
    ht.high = max(ht_bin),
    n.combinations = n()
  )

min.swe.caldor <- min(
  canopy.combos.caldor$pred.swe,
  na.rm = TRUE
)

optimum.summary.caldor <- tibble(
  min_pred_swe = min.swe.caldor,
  max_pred_swe = max.swe.caldor,
  swe_range = max.swe.caldor - min.swe.caldor,
  
  optimum_gap = optimum.caldor$gap_bin[1],
  optimum_height = optimum.caldor$ht_bin[1],
  
  threshold_95 = threshold.95.caldor,
  
  gap_95_min = min(optimum.95.caldor$gap_bin),
  gap_95_max = max(optimum.95.caldor$gap_bin),
  
  height_95_min = min(optimum.95.caldor$ht_bin),
  height_95_max = max(optimum.95.caldor$ht_bin),
  
  n_combos_95 = nrow(optimum.95.caldor)
) %>%
  mutate(
    swe_difference_pct =
      (max_pred_swe - min_pred_swe) /
      min_pred_swe * 100
  )

print(optimum.summary.caldor, width = Inf)

# ----- plot -----
p.caldor.optimum <- ggplot(
  plot.grid.caldor,
  aes(
    x = gap_bin,
    y = ht_bin
  )
) +
  
  # marginal predicted SWE
  geom_tile(
    aes(fill = pred.swe),
    width = 2,
    height = 2
  ) +
  
  # # boundary around >= 95% of maximum
  # geom_contour(
  #   aes(z = optimum.95),
  #   breaks = 0.5,
  #   color = 'grey60',
  #   linewidth = 0.8
  # ) +
  
  # color fill
  scale_fill_viridis_c(
    option = 'viridis',
    na.value = NA,
    name = 'Predicted SWE (m)'
  ) +
  
  labs(
    title = 'Caldor',
    x = 'Gap percent',
    y = 'Maximum canopy height (m)',
    fill = 'Predicted SWE (m)'
  ) +
  
  theme_classic()

p.caldor.optimum


# ----- identify contiguous optimum regions -----

# coordinates of all >= 95% optimum bins
opt.cells <- optimum.95.caldor %>%
  select(gap_bin, ht_bin, pred.swe)

# function to identify connected regions
find.regions <- function(df, bin.size = 2) {
  
  df$region <- NA_integer_
  region.id <- 0
  
  for (i in seq_len(nrow(df))) {
    
    # skip cells already assigned to a region
    if (!is.na(df$region[i])) next
    
    region.id <- region.id + 1
    
    # start a new region with this cell
    queue <- i
    df$region[i] <- region.id
    
    while (length(queue) > 0) {
      
      current <- queue[1]
      queue <- queue[-1]
      
      # find cells sharing an edge with the current cell
      neighbors <- which(
        (
          abs(df$gap_bin - df$gap_bin[current]) == bin.size &
            df$ht_bin == df$ht_bin[current]
        ) |
          (
            abs(df$ht_bin - df$ht_bin[current]) == bin.size &
              df$gap_bin == df$gap_bin[current]
          )
      )
      
      # only consider cells not already assigned
      neighbors <- neighbors[is.na(df$region[neighbors])]
      
      if (length(neighbors) > 0) {
        df$region[neighbors] <- region.id
        queue <- c(queue, neighbors)
      }
    }
  }
  
  df
}

opt.regions.caldor <- find.regions(
  opt.cells,
  bin.size = 2
)

region.summary.caldor <- opt.regions.caldor %>%
  group_by(region) %>%
  summarise(
    n.bins = n(),
    
    gap.low = min(gap_bin),
    gap.high = max(gap_bin),
    
    ht.low = min(ht_bin),
    ht.high = max(ht_bin),
    
    max.swe = max(pred.swe),
    
    gap.at.max = gap_bin[which.max(pred.swe)],
    ht.at.max = ht_bin[which.max(pred.swe)],
    
    .groups = 'drop'
  ) %>%
  arrange(desc(max.swe))

region.summary.caldor



# -------- combined plot --------
p.optimum.combined <- (
  p.caldor.optimum |
    p.creek.optimum
) &
  theme(
    plot.title = element_text(
      face = 'bold',
      size = 11,
      hjust = 0.5
    )
  )

p.optimum.combined



# ---------- gap percent x canopy height x zskew ----------
df.50 %>%
  group_by(fire) %>%
  summarise(
    min = min(ht_zskew, na.rm = TRUE),
    q01 = quantile(ht_zskew, 0.01, na.rm = TRUE),
    q25 = quantile(ht_zskew, 0.25, na.rm = TRUE),
    median = median(ht_zskew, na.rm = TRUE),
    q75 = quantile(ht_zskew, 0.75, na.rm = TRUE),
    q99 = quantile(ht_zskew, 0.99, na.rm = TRUE),
    max = max(ht_zskew, na.rm = TRUE)
  )

# ----- creek -----
# --- observed canopy combinations ---
# bin canopy variables
df.creek <- df.creek %>%
  mutate(
    # 2% gap bins
    gap_bin = round(gap_percent / 2) * 2,
    # 2 m height bins
    ht_bin = round(ht_zmax / 2) * 2,
    # 0.2 skew bins
    skew_bin = round(ht_zskew / 0.2) * 0.2
  )

# because skew doesn't occur at all parts of its range:
skew.limits <- quantile(
  df.creek$ht_zskew,
  c(0.01, 0.99),
  na.rm = TRUE
)

df.creek.3d <- df.creek %>%
  filter(
    ht_zskew >= skew.limits[1],
    ht_zskew <= skew.limits[2]
  )

canopy.combos.3d <- df.creek.3d %>%
  count(
    gap_bin,
    ht_bin,
    skew_bin,
    name = 'n'
  )

summary(canopy.combos.3d$n)

quantile(
  canopy.combos.3d$n,
  probs = c(
    0,
    0.05,
    0.10,
    0.25,
    0.50,
    0.75,
    0.90,
    0.95,
    1
  )
)

# run to determine what n to use
canopy.combos.3d %>%
  summarise(
    total.obs = sum(n),
    
    obs20 = sum(n[n >= 20]),
    obs50 = sum(n[n >= 50]),
    obs100 = sum(n[n >= 100]),
    obs200 = sum(n[n >= 200]),
    
    pct.obs20 = sum(n[n >= 20]) / sum(n) * 100,
    pct.obs50 = sum(n[n >= 50]) / sum(n) * 100,
    pct.obs100 = sum(n[n >= 100]) / sum(n) * 100,
    pct.obs200 = sum(n[n >= 200]) / sum(n) * 100
  )

# in this case 50 is best
supported.combos.3d <- canopy.combos.3d %>%
  filter(n >= 50)

creek.pred.canopy.3d <- purrr::pmap_dfr(
  supported.combos.3d,
  function(gap_bin, ht_bin, skew_bin, n) {
    
    # assign one canopy configuration to marginalization sample
    newdata <- df.creek.marg %>%
      mutate(
        gap_percent = gap_bin,
        ht_zmax = ht_bin,
        ht_zskew = skew_bin
      )
    
    # predict sqrt(SWE)
    pred <- predict(
      model.swe,
      newdata = newdata,
      type = 'response'
    )
    
    # average after back-transformation
    tibble(
      gap_bin = gap_bin,
      ht_bin = ht_bin,
      skew_bin = skew_bin,
      n = n,
      pred.swe = mean(pred^2)
    )
  }
)

# --- find optimum ---
optimum.3d <- creek.pred.canopy.3d %>%
  slice_max(pred.swe, n = 1, with_ties = FALSE)

threshold.95.3d <- max(
  creek.pred.canopy.3d$pred.swe,
  na.rm = TRUE
) * 0.95

optimum.95.3d <- creek.pred.canopy.3d %>%
  filter(pred.swe >= threshold.95.3d)

nrow(optimum.95.3d)

optimum.95.3d %>%
  arrange(desc(pred.swe)) %>%
  head(20)

# --- plot ---
library(plotly)
plot_ly(
  data = creek.pred.canopy.3d,
  x = ~gap_bin,
  y = ~ht_bin,
  z = ~skew_bin,
  color = ~pred.swe,
  colors = viridisLite::viridis(100),
  type = 'scatter3d',
  mode = 'markers',
  marker = list(
    size = 2,
    opacity = 0.25
  ),
  text = ~paste0(
    'Gap: ', gap_bin, '%',
    '<br>Height: ', ht_bin, ' m',
    '<br>Skew: ', round(skew_bin, 2),
    '<br>Predicted SWE: ', round(pred.swe, 3), ' m',
    '<br>n: ', n
  ),
  hoverinfo = 'text'
) %>%
  
  add_trace(
    data = optimum.95.3d,
    x = ~gap_bin,
    y = ~ht_bin,
    z = ~skew_bin,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      size = 4,
      color = 'pink',
      opacity = 0.8
    ),
    name = '≥95% maximum',
    inherit = FALSE
  ) %>%
  
  layout(
    scene = list(
      xaxis = list(title = 'Gap percent'),
      yaxis = list(title = 'Maximum canopy height (m)'),
      zaxis = list(title = 'Canopy height skewness')
    )
  )
# ----- caldor -----
# --- observed canopy combinations ---
# bin canopy variables
df.caldor <- df.caldor %>%
  mutate(
    # 2% gap bins
    gap_bin = round(gap_percent / 2) * 2,
    # 2 m height bins
    ht_bin = round(ht_zmax / 2) * 2,
    # 0.2 skew bins
    skew_bin = round(ht_zskew / 0.2) * 0.2
  )

# because skew doesn't occur at all parts of its range:
skew.limits <- quantile(
  df.caldor$ht_zskew,
  c(0.01, 0.99),
  na.rm = TRUE
)

df.caldor.3d <- df.caldor %>%
  filter(
    ht_zskew >= skew.limits[1],
    ht_zskew <= skew.limits[2]
  )

canopy.combos.3d <- df.caldor.3d %>%
  count(
    gap_bin,
    ht_bin,
    skew_bin,
    name = 'n'
  )

summary(canopy.combos.3d$n)

quantile(
  canopy.combos.3d$n,
  probs = c(
    0,
    0.05,
    0.10,
    0.25,
    0.50,
    0.75,
    0.90,
    0.95,
    1
  )
)

# run to determine what n to use
canopy.combos.3d %>%
  summarise(
    total.obs = sum(n),
    
    obs20 = sum(n[n >= 20]),
    obs50 = sum(n[n >= 50]),
    obs100 = sum(n[n >= 100]),
    obs200 = sum(n[n >= 200]),
    
    pct.obs20 = sum(n[n >= 20]) / sum(n) * 100,
    pct.obs50 = sum(n[n >= 50]) / sum(n) * 100,
    pct.obs100 = sum(n[n >= 100]) / sum(n) * 100,
    pct.obs200 = sum(n[n >= 200]) / sum(n) * 100
  )

# in this case 50 is best
supported.combos.3d <- canopy.combos.3d %>%
  filter(n >= 50)

caldor.pred.canopy.3d <- purrr::pmap_dfr(
  supported.combos.3d,
  function(gap_bin, ht_bin, skew_bin, n) {
    
    # assign one canopy configuration to marginalization sample
    newdata <- df.caldor.marg %>%
      mutate(
        gap_percent = gap_bin,
        ht_zmax = ht_bin,
        ht_zskew = skew_bin
      )
    
    # predict sqrt(SWE)
    pred <- predict(
      model.swe,
      newdata = newdata,
      type = 'response'
    )
    
    # average after back-transformation
    tibble(
      gap_bin = gap_bin,
      ht_bin = ht_bin,
      skew_bin = skew_bin,
      n = n,
      pred.swe = mean(pred^2)
    )
  }
)

# --- find optimum ---
optimum.3d <- caldor.pred.canopy.3d %>%
  slice_max(pred.swe, n = 1, with_ties = FALSE)

threshold.95.3d <- max(
  caldor.pred.canopy.3d$pred.swe,
  na.rm = TRUE
) * 0.95

optimum.95.3d <- caldor.pred.canopy.3d %>%
  filter(pred.swe >= threshold.95.3d)

nrow(optimum.95.3d)

optimum.95.3d %>%
  arrange(desc(pred.swe)) %>%
  head(20)

# --- plot ---
library(plotly)
plot_ly(
  data = caldor.pred.canopy.3d,
  x = ~gap_bin,
  y = ~ht_bin,
  z = ~skew_bin,
  color = ~pred.swe,
  colors = viridisLite::viridis(100),
  type = 'scatter3d',
  mode = 'markers',
  marker = list(
    size = 2,
    opacity = 0.25
  ),
  text = ~paste0(
    'Gap: ', gap_bin, '%',
    '<br>Height: ', ht_bin, ' m',
    '<br>Skew: ', round(skew_bin, 2),
    '<br>Predicted SWE: ', round(pred.swe, 3), ' m',
    '<br>n: ', n
  ),
  hoverinfo = 'text'
) %>%
  
  add_trace(
    data = optimum.95.3d,
    x = ~gap_bin,
    y = ~ht_bin,
    z = ~skew_bin,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      size = 4,
      color = 'pink',
      opacity = 0.8
    ),
    name = '≥95% maximum',
    inherit = FALSE
  ) %>%
  
  layout(
    scene = list(
      xaxis = list(title = 'Gap percent'),
      yaxis = list(title = 'Maximum canopy height (m)'),
      zaxis = list(title = 'Canopy height skewness')
    )
  )
# ----- castle -----
# --- observed canopy combinations ---
# bin canopy variables
df.castle <- df.castle %>%
  mutate(
    # 2% gap bins
    gap_bin = round(gap_percent / 2) * 2,
    # 2 m height bins
    ht_bin = round(ht_zmax / 2) * 2,
    # 0.2 skew bins
    skew_bin = round(ht_zskew / 0.2) * 0.2
  )

# because skew doesn't occur at all parts of its range:
skew.limits <- quantile(
  df.castle$ht_zskew,
  c(0.01, 0.99),
  na.rm = TRUE
)

df.castle.3d <- df.castle %>%
  filter(
    ht_zskew >= skew.limits[1],
    ht_zskew <= skew.limits[2]
  )

canopy.combos.3d <- df.castle.3d %>%
  count(
    gap_bin,
    ht_bin,
    skew_bin,
    name = 'n'
  )

summary(canopy.combos.3d$n)

quantile(
  canopy.combos.3d$n,
  probs = c(
    0,
    0.05,
    0.10,
    0.25,
    0.50,
    0.75,
    0.90,
    0.95,
    1
  )
)

# run to determine what n to use
canopy.combos.3d %>%
  summarise(
    total.obs = sum(n),
    
    obs20 = sum(n[n >= 20]),
    obs50 = sum(n[n >= 50]),
    obs100 = sum(n[n >= 100]),
    obs200 = sum(n[n >= 200]),
    
    pct.obs20 = sum(n[n >= 20]) / sum(n) * 100,
    pct.obs50 = sum(n[n >= 50]) / sum(n) * 100,
    pct.obs100 = sum(n[n >= 100]) / sum(n) * 100,
    pct.obs200 = sum(n[n >= 200]) / sum(n) * 100
  )

# in this case 50 is best
supported.combos.3d <- canopy.combos.3d %>%
  filter(n >= 50)

castle.pred.canopy.3d <- purrr::pmap_dfr(
  supported.combos.3d,
  function(gap_bin, ht_bin, skew_bin, n) {
    
    # assign one canopy configuration to marginalization sample
    newdata <- df.castle.marg %>%
      mutate(
        gap_percent = gap_bin,
        ht_zmax = ht_bin,
        ht_zskew = skew_bin
      )
    
    # predict sqrt(SWE)
    pred <- predict(
      model.swe,
      newdata = newdata,
      type = 'response'
    )
    
    # average after back-transformation
    tibble(
      gap_bin = gap_bin,
      ht_bin = ht_bin,
      skew_bin = skew_bin,
      n = n,
      pred.swe = mean(pred^2)
    )
  }
)

# --- find optimum ---
optimum.3d <- castle.pred.canopy.3d %>%
  slice_max(pred.swe, n = 1, with_ties = FALSE)

threshold.95.3d <- max(
  castle.pred.canopy.3d$pred.swe,
  na.rm = TRUE
) * 0.95

optimum.95.3d <- castle.pred.canopy.3d %>%
  filter(pred.swe >= threshold.95.3d)

nrow(optimum.95.3d)

optimum.95.3d %>%
  arrange(desc(pred.swe)) %>%
  head(20)

# --- plot ---
library(plotly)
plot_ly(
  data = castle.pred.canopy.3d,
  x = ~gap_bin,
  y = ~ht_bin,
  z = ~skew_bin,
  color = ~pred.swe,
  colors = viridisLite::viridis(100),
  type = 'scatter3d',
  mode = 'markers',
  marker = list(
    size = 2,
    opacity = 0.25
  ),
  text = ~paste0(
    'Gap: ', gap_bin, '%',
    '<br>Height: ', ht_bin, ' m',
    '<br>Skew: ', round(skew_bin, 2),
    '<br>Predicted SWE: ', round(pred.swe, 3), ' m',
    '<br>n: ', n
  ),
  hoverinfo = 'text'
) %>%
  
  add_trace(
    data = optimum.95.3d,
    x = ~gap_bin,
    y = ~ht_bin,
    z = ~skew_bin,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      size = 4,
      color = 'pink',
      opacity = 0.8
    ),
    name = '≥95% maximum',
    inherit = FALSE
  ) %>%
  
  layout(
    scene = list(
      xaxis = list(title = 'Gap percent'),
      yaxis = list(title = 'Maximum canopy height (m)'),
      zaxis = list(title = 'Canopy height skewness')
    )
  )
# ----- combine -----
creek.pred.canopy.3d <- creek.pred.canopy.3d %>%
  mutate(
    fire = 'Creek',
    relative.swe = pred.swe / max(pred.swe, na.rm = TRUE)
  )

caldor.pred.canopy.3d <- caldor.pred.canopy.3d %>%
  mutate(
    fire = 'Caldor',
    relative.swe = pred.swe / max(pred.swe, na.rm = TRUE)
  )

castle.pred.canopy.3d <- castle.pred.canopy.3d %>%
  mutate(
    fire = 'Castle',
    relative.swe = pred.swe / max(pred.swe, na.rm = TRUE)
  )

# combine
pred.3d.combined <- bind_rows(
  creek.pred.canopy.3d,
  caldor.pred.canopy.3d,
  castle.pred.canopy.3d
)

# only canopy combos that occur in all 3 fires
common.combos <- pred.3d.combined %>%
  group_by(
    gap_bin,
    ht_bin,
    skew_bin
  ) %>%
  filter(n_distinct(fire) == 3) %>%
  ungroup()

# check
n_distinct(
  interaction(
    common.combos$gap_bin,
    common.combos$ht_bin,
    common.combos$skew_bin
  )
)

# collapse the three fire predictions into one row per canopy configuration
cross.fire <- common.combos %>%
  group_by(
    gap_bin,
    ht_bin,
    skew_bin
  ) %>%
  summarise(
    mean.relative.swe = mean(relative.swe),
    min.relative.swe = min(relative.swe),
    max.relative.swe = max(relative.swe),
    .groups = 'drop'
  )

# find the most consistently good canopy configuration
best.common <- cross.fire %>%
  slice_max(
    min.relative.swe,
    n = 1,
    with_ties = FALSE
  )

best.common
# ----------------------------------- ** BURN EFFECTS ** --------------------------------------------------
# ----- set scale once -----
burn.obs.size.scale <- scale_size_continuous(
  name = 'Observations',
  limits = c(0, 150000),
  breaks = c(
    50000,
    100000,
    150000
  ),
  labels = scales::comma,
  range = c(1.5, 5)
)
# --------------- Gap percent ---------------
# --- supported gap range within each fire x burn class ---
gap.ranges.burn <- df.50 %>%
  group_by(fire, burned) %>%
  summarise(
    gap.low = quantile(
      gap_percent,
      0.01,
      na.rm = TRUE
    ),
    gap.high = quantile(
      gap_percent,
      0.99,
      na.rm = TRUE
    ),
    .groups = 'drop'
  )

# --- overlap between burned and unburned within each fire ---
gap.ranges.common <- gap.ranges.burn %>%
  group_by(fire) %>%
  summarise(
    gap.low = max(gap.low),
    gap.high = min(gap.high),
    .groups = 'drop'
  )

burn.levels <- levels(df.50$burned)

# --- predictions ---
gap.burn.pred <- map_dfr(
  levels(df.pred$fire),
  function(fire.name) {
    
    # prediction sample for this fire
    df.fire <- df.pred %>%
      filter(fire == fire.name) %>%
      droplevels()
    
    # shared supported gap range
    fire.range <- gap.ranges.common %>%
      filter(fire == fire.name)
    
    gap.seq <- seq(
      fire.range$gap.low,
      fire.range$gap.high,
      length.out = 100
    )
    
    map_dfr(
      burn.levels,
      function(burn.value) {
        
        map_dfr(
          gap.seq,
          function(gap.value) {
            
            newdata <- df.fire %>%
              mutate(
                gap_percent = gap.value,
                burned = factor(
                  burn.value,
                  levels = levels(df.50$burned)
                ),
                fire_burned = interaction(
                  fire,
                  burned,
                  sep = '_'
                ),
                fire_burned = factor(
                  fire_burned,
                  levels = levels(df.50$fire_burned)
                )
              )
            
            pred <- predict(
              model.swe.burned,
              newdata = newdata,
              type = 'response'
            )
            
            tibble(
              fire = fire.name,
              burned = burn.value,
              gap_percent = gap.value,
              
              # back-transform first, then average
              swe_peak = mean(pred^2)
            )
          }
        )
      }
    )
  }
)

# --- observed density by fire x burned x gap bin ---
gap.burn.density.0 <- df.50 %>%
  filter(
    !is.na(gap_percent),
    !is.na(burned),
    !is.na(fire)
  ) %>%
  mutate(
    gap_bin = cut(
      gap_percent,
      breaks = seq(0, 100, by = 5),
      include.lowest = TRUE
    )
  ) %>%
  group_by(
    fire,
    burned,
    gap_bin
  ) %>%
  summarise(
    gap_percent = mean(
      gap_percent,
      na.rm = TRUE
    ),
    n = n(),
    .groups = 'drop'
  )

gap.burn.density <- gap.burn.density.0 %>%
  group_by(fire, burned) %>%
  mutate(
    swe_peak = approx(
      x = gap.burn.pred$gap_percent[
        gap.burn.pred$fire == first(fire) &
          gap.burn.pred$burned == first(burned)
      ],
      y = gap.burn.pred$swe_peak[
        gap.burn.pred$fire == first(fire) &
          gap.burn.pred$burned == first(burned)
      ],
      xout = gap_percent,
      rule = 1
    )$y
  ) %>%
  ungroup() %>%
  filter(!is.na(swe_peak))

# filter out castle
gap.burn.pred.plot <- gap.burn.pred %>%
  mutate(
    fire = factor(
      fire,
      levels = fire.levels)) %>%
  filter(fire != 'Castle') 

gap.burn.density.plot <- gap.burn.density %>%
  mutate(
    fire = factor(
      fire,
      levels = fire.levels)) %>%
  filter(fire != 'Castle')
# ----- summary results -----
# --- summarize burned effect across gap percent ---

gap.burn.summary <- gap.burn.pred %>%
  select(
    fire,
    burned,
    gap_percent,
    swe_peak
  ) %>%
  pivot_wider(
    names_from = burned,
    values_from = swe_peak
  ) %>%
  mutate(
    burn_difference = burned - unburned,
    burn_difference_pct =
      burn_difference / unburned * 100
  ) %>%
  group_by(fire) %>%
  summarise(
    gap_min = min(gap_percent),
    gap_max = max(gap_percent),
    
    # burned - unburned difference across shared gap range
    mean_burn_difference = mean(burn_difference),
    min_burn_difference = min(burn_difference),
    max_burn_difference = max(burn_difference),
    
    # percent difference relative to unburned
    mean_burn_difference_pct = mean(burn_difference_pct),
    min_burn_difference_pct = min(burn_difference_pct),
    max_burn_difference_pct = max(burn_difference_pct),
    
    # difference at low and high ends of shared gap range
    difference_at_gap_min =
      burn_difference[which.min(gap_percent)],
    difference_at_gap_max =
      burn_difference[which.max(gap_percent)],
    
    .groups = 'drop'
  ) %>%
  filter(fire != 'Castle')

print(gap.burn.summary, width = Inf)

gap.burn.diff <- gap.burn.pred %>%
  select(
    fire,
    burned,
    gap_percent,
    swe_peak
  ) %>%
  pivot_wider(
    names_from = burned,
    values_from = swe_peak
  ) %>%
  mutate(
    burn_difference = burned - unburned,
    burn_difference_pct =
      burn_difference / unburned * 100
  ) %>%
  filter(fire != 'Castle')

gap.burn.crossover <- gap.burn.diff %>%
  group_by(fire) %>%
  arrange(gap_percent) %>%
  mutate(
    next_diff = lead(burn_difference),
    next_gap = lead(gap_percent)
  ) %>%
  filter(
    burn_difference * next_diff <= 0,
    !is.na(next_diff)
  ) %>%
  summarise(
    crossover_gap =
      gap_percent +
      (0 - burn_difference) *
      (next_gap - gap_percent) /
      (next_diff - burn_difference),
    .groups = 'drop'
  )

gap.burn.crossover
# ----- plot -----
p.gap.burn <- ggplot(
  gap.burn.pred.plot,
  aes(
    x = gap_percent,
    y = swe_peak,
    color = burned
  )
) +
  geom_line(linewidth = 1) +
  
  geom_point(
    data = gap.burn.density.plot,
    aes(
      x = gap_percent,
      y = swe_peak,
      size = n,
      color = burned
    ),
    inherit.aes = FALSE,
    alpha = 0.75
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  
  scale_color_manual(
    values = burn.cols
  ) +
  
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE (m)',
    color = NULL,
    size = 'Observations'
  )  +
  burn.obs.size.scale  +
  
  guides(
    color = 'none',
    fill = 'none',
    size = 'none'
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = 'none'
  )


# --------------- Zmax ---------------
# ----- supported height range within each fire x burn class -----
ht.ranges.burn <- df.50 %>%
  group_by(fire, burned) %>%
  summarise(
    ht.low = quantile(
      ht_zmax,
      0.01,
      na.rm = TRUE
    ),
    ht.high = quantile(
      ht_zmax,
      0.99,
      na.rm = TRUE
    ),
    .groups = 'drop'
  )

# ----- overlap between burned and unburned within each fire -----
ht.ranges.common <- ht.ranges.burn %>%
  group_by(fire) %>%
  summarise(
    ht.low = max(ht.low),
    ht.high = min(ht.high),
    .groups = 'drop'
  )

# ----- predictions -----
ht.burn.pred <- map_dfr(
  levels(df.pred$fire),
  function(fire.name) {
    
    # prediction sample for this fire
    df.fire <- df.pred %>%
      filter(fire == fire.name) %>%
      droplevels()
    
    # shared supported height range
    fire.range <- ht.ranges.common %>%
      filter(fire == fire.name)
    
    ht.seq <- seq(
      fire.range$ht.low,
      fire.range$ht.high,
      length.out = 100
    )
    
    map_dfr(
      burn.levels,
      function(burn.value) {
        
        map_dfr(
          ht.seq,
          function(ht.value) {
            
            newdata <- df.fire %>%
              mutate(
                ht_zmax = ht.value,
                burned = factor(
                  burn.value,
                  levels = levels(df.50$burned)
                ),
                fire_burned = interaction(
                  fire,
                  burned,
                  sep = '_'
                ),
                fire_burned = factor(
                  fire_burned,
                  levels = levels(df.50$fire_burned)
                )
              )
            
            pred <- predict(
              model.swe.burned,
              newdata = newdata,
              type = 'response'
            )
            
            tibble(
              fire = fire.name,
              burned = burn.value,
              ht_zmax = ht.value,
              swe_peak = mean(pred^2)
            )
          }
        )
      }
    )
  }
)

# restore correct order of fires
ht.burn.pred <- ht.burn.pred %>%
  mutate(
    fire = factor(
      fire,
      levels = fire.levels
    )
  )

# ----- observation-density dots -----
ht.burn.density.0 <- df.50 %>%
  filter(
    !is.na(ht_zmax),
    !is.na(burned),
    !is.na(fire)
  ) %>%
  mutate(
    ht_bin = cut(
      ht_zmax,
      breaks = seq(0, 100, by = 3),
      include.lowest = TRUE
    )
  ) %>%
  group_by(
    fire,
    burned,
    ht_bin
  ) %>%
  summarise(
    ht_zmax = mean(
      ht_zmax,
      na.rm = TRUE
    ),
    n = n(),
    .groups = 'drop'
  )

ht.burn.density <- ht.burn.density.0 %>%
  group_by(fire, burned) %>%
  mutate(
    swe_peak = approx(
      x = ht.burn.pred$ht_zmax[
        ht.burn.pred$fire == first(fire) &
          ht.burn.pred$burned == first(burned)
      ],
      y = ht.burn.pred$swe_peak[
        ht.burn.pred$fire == first(fire) &
          ht.burn.pred$burned == first(burned)
      ],
      xout = ht_zmax,
      rule = 1
    )$y
  ) %>%
  ungroup() %>%
  filter(!is.na(swe_peak)) %>%
  mutate(
    fire = factor(
      fire,
      levels = fire.levels
    )
  )

# ----- plot -----
p.ht.burn <- ggplot(
  ht.burn.pred,
  aes(
    x = ht_zmax,
    y = swe_peak,
    color = burned
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    data = ht.burn.density,
    aes(
      x = ht_zmax,
      y = swe_peak,
      size = n,
      color = burned
    ),
    inherit.aes = FALSE,
    alpha = 0.75
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  
  scale_color_manual(
    values = burn.cols
  ) +
  
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Predicted peak SWE (m)',
    color = NULL,
    size = 'Observations'
  )  +
  burn.obs.size.scale  +
  
  guides(
    color = 'none',
    fill = 'none',
    size = 'none'
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = 'none'
  )


p.ht.burn
# -------------- Combined Plot ----------------
library(patchwork)
library(grid)

# --- common theme ---

optimum.theme <- theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = 'bold',
      size = 10
    ),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 9),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

gap.legend <- cowplot::get_legend(
  p.gap.burn +
    guides(size = guide_legend()) +
    theme(legend.position = 'right')
)

# create spacer for where top castle panel would be and put legend there
gap.row <- (
  p.gap.burn |
    wrap_elements(gap.legend)
) +
  plot_layout(
    widths = c(2, 1),
    guides = 'keep'
  )



canopy.burn.fig <- (
  gap.row /
    p.ht.burn
) +
  plot_layout(
    guides = 'keep'
  ) &
  optimum.theme &
  scale_y_continuous(
    limits = c(0.38, 0.82),
    breaks = seq(0.4, 0.8, 0.1)
  )

canopy.burn.fig

# ==============================================================================
# Observed Plots
# ==============================================================================
# SWE vs Canopy Gap for each fire, by burned status
# needs refining- takes too long to plot
ggplot(
  df.50,
  aes(
    x = gap_percent,
    y = swe_peak,
    color = burned
  )
) +
  geom_point(
    alpha = 0.03
  ) +
  geom_smooth(
    method = 'gam',
    formula = y ~ s(x, k = 10),
    se = TRUE
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  scale_color_manual(
    values = burn.cols
  ) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Peak SWE',
    color = 'Burn status'
  ) +
  theme_classic()



# ----- troubleshooting -----
ht.density %>%
  filter(fire == 'Creek') %>%
  select(ht_bin, ht_zmax, n, swe_peak)

df.50 %>%
  filter(
    fire == 'Creek',
    ht_zmax >= 30,
    ht_zmax <= 45
  ) %>%
  summarise(
    n = n(),
    min = min(ht_zmax),
    max = max(ht_zmax)
  )

ht.density %>%
  filter(fire == 'Creek') %>%
  arrange(ht_zmax) %>%
  print(n = Inf)
