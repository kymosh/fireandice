packages <- c('mgcv', 'ggplot2', 'pdp', 'dplyr', 'purrr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# get dataframe
set.seed(61)
dir <- 'data/processed/processed/rds/' 

df.50.raw <- readRDS(file.path(dir, 'df_50m_raw.rds'))

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
    )
  ) %>%
  droplevels()

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
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)

fire.colors <- c(
  'Caldor' = '#009E73',
  'Castle' = '#E69F00',
  'Creek' = '#CC79A7'
)


model.swe <- bam(sqrt(swe_peak) ~ wy * fire
                 + s(elevation, by = wy, k = 20)
                 + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                 + s(ht_zmax, by = fire, k = 10) + s(gap_percent, by = fire, k = 10) 
                 + s(ht_zskew, by = fire, k = 20),
                 data = df.50,
                 method = 'fREML',
                 discrete = TRUE)

summary(model.swe)

# ----- functions -----
cv_bam <- function(formula, data, k_folds = 5) {
  
  # empty dataframes to store results
  cv.results <- data.frame()
  cv.fire.results <- data.frame()
  
  # loop through each spatial fold
  for (fold in 1:k_folds) {
    
    # use all other folds to train the model
    train <- data %>%
      filter(fold_id != fold)
    
    # hold out the current fold for model evaluation
    test <- data %>%
      filter(fold_id == fold)
    
    # fit the GAM to the training data
    model <- bam(
      formula,
      data = train,
      method = 'fREML',
      discrete = TRUE
    )
    
    # predict sqrt(SWE) for observations in the held-out fold
    pred <- predict(
      model,
      newdata = test
    )
    
    # observed response on the same sqrt scale as the model
    obs <- sqrt(test$swe_peak)
    
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
        obs = sqrt(swe_peak),
        pred = pred
      ) %>%
      group_by(fire) %>%
      summarize(
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
    
    # save fire-specific fold results
    cv.fire.results <- bind_rows(
      cv.fire.results,
      fire.results
    )
  }
  
  # summarize overall performance across folds
  cv.summary <- cv.results %>%
    summarize(
      RMSE_mean = mean(RMSE),
      RMSE_sd = sd(RMSE),
      R2_mean = mean(R2),
      R2_sd = sd(R2)
    )
  
  # return all results
  list(
    fold.results = cv.results,
    fire.results = cv.fire.results,
    summary = cv.summary
  )
}

# ==============================================================================
# Generate Predictions - All fires combined
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
# --------------- predictions using by=fire model ---------------

# make balanced prediction sample
df.pred <- df.50 %>%
  group_by(fire, wy, burned) %>%
  slice_sample(n = 500) %>%
  ungroup()


# --------------- gap percent ----------------
# this version has Confidence intervals

set.seed(61)

# simulate coefficient draws once
n.sim <- 500

beta.sim <- MASS::mvrnorm(
  n = n.sim,
  mu = coef(model.swe),
  Sigma = vcov(model.swe)
)

gap.pred <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
  # prediction sample for fire
  df.fire <- df.pred %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # prediction range
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
    
    newdata <- df.fire %>%
      mutate(gap_percent = gap.value)
    
    # central prediction
    pred <- predict(
      model.swe,
      newdata = newdata,
      type = 'response'
    )
    
    fit <- mean(pred^2)
    
    # linear predictor matrix
    Xp <- predict(
      model.swe,
      newdata = newdata,
      type = 'lpmatrix'
    )
    
    # predictions for each coefficient simulation
    sim.sqrt.swe <- Xp %*% t(beta.sim)
    
    # back-transform each observation, then average within simulation
    sim.swe <- colMeans(sim.sqrt.swe^2)
    
    tibble(
      fire = fire.name,
      gap_percent = gap.value,
      swe_peak = fit,
      lower = quantile(sim.swe, 0.025),
      upper = quantile(sim.swe, 0.975)
    )
  })
})

gap.rug <- df.50 %>%
  group_by(fire) %>%
  slice_sample(n = 3000) %>%
  ungroup()

# ----- calculate effect size -----
gap.effect.range <- gap.pred %>%
  group_by(fire) %>%
  summarise(
    swe.min = min(swe_peak),
    swe.max = max(swe_peak),
    range.m = swe.max - swe.min,
    range.cm = range.m * 100,
    percent.range = (range.m / swe.min) * 100
  )

gap.effect.range

gap.effect.range <- gap.pred %>%
  group_by(fire) %>%
  summarise(
    min.pred.swe = min(swe_peak),
    gap.at.min = gap_percent[which.min(swe_peak)],
    max.pred.swe = max(swe_peak),
    gap.at.max = gap_percent[which.max(swe_peak)],
    swe.range.cm = (max.pred.swe - min.pred.swe) * 100,
    relative.range = ((max.pred.swe - min.pred.swe) / min.pred.swe) * 100
  )

gap.effect.range

# Extract marginal predictions near these values
group_by(fire) %>%
  slice_min(abs(gap_percent - 25), n = 1, with_ties = FALSE) %>%
  mutate(gap.target = 25) %>%
  bind_rows(
    gap.pred %>%
      group_by(fire) %>%
      slice_min(abs(gap_percent - 50), n = 1, with_ties = FALSE) %>%
      mutate(gap.target = 50),
    
    gap.pred %>%
      group_by(fire) %>%
      slice_min(abs(gap_percent - 75), n = 1, with_ties = FALSE) %>%
      mutate(gap.target = 75)
  ) %>%
  arrange(fire, gap.target)

gap.contrasts

gap.values <- c(25, 50, 75)
# ----- standardized effect size -----
gap.contrast <- gap.pred %>%
  group_by(fire) %>%
  summarise(
    swe.25 = approx(
      x = gap_percent,
      y = swe_peak,
      xout = 25
    )$y,
    
    swe.75 = approx(
      x = gap_percent,
      y = swe_peak,
      xout = 75
    )$y
  ) %>%
  mutate(
    change.m = swe.75 - swe.25,
    change.cm = change.m * 100,
    change.percent = (change.m / swe.25) * 100
  )

gap.contrast

# ----- Identify thresholds/optima -----
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

# plot
p.gap.opt <- ggplot(
  gap.pred,
  aes(
    x = gap_percent,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  
  # shade gap range producing >=95% of maximum SWE
  geom_rect(
    data = gap.optimum,
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
  
  # confidence interval
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  
  # marginal prediction
  geom_line(linewidth = 1) +
  
  # rug
  geom_rug(
    data = gap.rug,
    aes(x = gap_percent, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  
  geom_segment(
    data = gap.optimum,
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
  
  guides(color = 'none', fill = 'none') +
  
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE (m)'
  ) +
  
  theme_classic()

p.gap.opt
# ----- Combined model -----
gap.pred.combined <- map_dfr(
  levels(df.pred$fire),
  function(fire.name) {
    
    df.fire <- df.pred %>%
      filter(fire == fire.name) %>%
      droplevels()
    
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
      
      newdata <- df.fire %>%
        mutate(gap_percent = gap.value)
      
      pred <- predict(
        model.swe.combined,
        newdata = newdata,
        type = 'response'
      )
      
      tibble(
        fire = fire.name,
        gap_percent = gap.value,
        swe_peak = mean(pred^2, na.rm = TRUE)
      )
    })
  }
)
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

ht.rug <- df.50 %>%
  group_by(fire) %>%
  slice_sample(n = 3000) %>%
  ungroup()

ggplot(
  ht.pred,
  aes(
    x = ht_zmax,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = ht.rug,
    aes(x = ht_zmax, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = ht.rug,
    aes(x = ht_zmax, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()

# ----- overall magnitude -----
ht.effect.range <- ht.pred %>%
  group_by(fire) %>%
  summarise(
    min.pred.swe = min(swe_peak),
    ht.at.min = ht_zmax[which.min(swe_peak)],
    
    max.pred.swe = max(swe_peak),
    ht.at.max = ht_zmax[which.max(swe_peak)],
    
    swe.range.cm = (max.pred.swe - min.pred.swe) * 100,
    relative.range = ((max.pred.swe - min.pred.swe) /
                        min.pred.swe) * 100
  )

ht.effect.range

# to determine heights to use
df.50 %>%
  group_by(fire) %>%
  summarise(
    min = min(ht_zmax, na.rm = TRUE),
    p01 = quantile(ht_zmax, 0.01, na.rm = TRUE),
    p05 = quantile(ht_zmax, 0.05, na.rm = TRUE),
    p25 = quantile(ht_zmax, 0.25, na.rm = TRUE),
    median = median(ht_zmax, na.rm = TRUE),
    p75 = quantile(ht_zmax, 0.75, na.rm = TRUE),
    p95 = quantile(ht_zmax, 0.95, na.rm = TRUE),
    p99 = quantile(ht_zmax, 0.99, na.rm = TRUE),
    max = max(ht_zmax, na.rm = TRUE)
  )

ht.contrast <- ht.pred %>%
  group_by(fire) %>%
  summarise(
    swe.20 = approx(
      x = ht_zmax,
      y = swe_peak,
      xout = 20
    )$y,
    
    swe.40 = approx(
      x = ht_zmax,
      y = swe_peak,
      xout = 40
    )$y
  ) %>%
  mutate(
    change.m = swe.40 - swe.20,
    change.cm = change.m * 100,
    change.percent = (change.m / swe.20) * 100
  )

ht.contrast

# ----- combined model ------
ht.pred.combined <- map_dfr(
  levels(df.pred$fire),
  function(fire.name) {
    
    # prediction sample for this fire
    df.fire <- df.pred %>%
      filter(fire == fire.name) %>%
      droplevels()
    
    # fire-specific range of ht_zmax
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
        mutate(
          ht_zmax = ht.value
        )
      
      # predictions on sqrt(SWE) scale
      pred <- predict(
        model.swe.combined,
        newdata = newdata,
        type = 'response'
      )
      
      tibble(
        fire = fire.name,
        ht_zmax = ht.value,
        swe_peak = mean(pred^2, na.rm = TRUE)
      )
    })
  }
)
#----- identify thresholds/optima -----

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

ht.optimum

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
    alpha = 0.15,
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
  geom_line(
    data = ht.pred.combined,
    aes(
      x = ht_zmax,
      y = swe_peak
    ),
    inherit.aes = FALSE,
    color = 'grey',
    linetype = 'solid',
    linewidth = 0.8
  ) +
  
  # observed data support
  geom_rug(
    data = ht.rug,
    aes(x = ht_zmax, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  
  scale_y_continuous(
    limits = c(0.38, 0.82),
    breaks = seq(0.4, 0.8, 0.1)
  ) +
  
  guides(color = 'none', fill = 'none') +
  
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Predicted peak SWE (m)'
  ) +
  
  theme_classic()

p.ht.opt


# ---------------- canopy height skewness ---------------

skew.pred <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
  # prediction sample for fire
  df.fire <- df.pred %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # fire-specific prediction range
  skew.seq <- seq(
    quantile(
      df.50$ht_zskew[df.50$fire == fire.name],
      0.01,
      na.rm = TRUE
    ),
    quantile(
      df.50$ht_zskew[df.50$fire == fire.name],
      0.99,
      na.rm = TRUE
    ),
    length.out = 100
  )
  
  map_dfr(skew.seq, function(skew.value) {
    
    newdata <- df.fire %>%
      mutate(ht_zskew = skew.value)
    
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
      ht_zskew = skew.value,
      swe_peak = fit,
      lower = quantile(sim.swe, 0.025),
      upper = quantile(sim.swe, 0.975)
    )
  })
})


# --- rug sample ---

set.seed(61)

skew.rug <- df.50 %>%
  filter(
    ht_zskew >= -1,
    ht_zskew <= 3
  ) %>%
  group_by(fire) %>%
  slice_sample(n = 3000) %>%
  ungroup()


# --- plot ---

ggplot(
  skew.pred,
  aes(
    x = ht_zskew,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = skew.rug,
    aes(x = ht_zskew, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  scale_x_continuous(
    limits = c(-1, 3),
    breaks = -1:3
  ) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Canopy height skewness',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()


# ----- overall magnitude -----

skew.effect.range <- skew.pred %>%
  group_by(fire) %>%
  summarise(
    min.pred.swe = min(swe_peak),
    skew.at.min = ht_zskew[which.min(swe_peak)],
    
    max.pred.swe = max(swe_peak),
    skew.at.max = ht_zskew[which.max(swe_peak)],
    
    swe.range.cm = (max.pred.swe - min.pred.swe) * 100,
    relative.range = ((max.pred.swe - min.pred.swe) /
                        min.pred.swe) * 100
  )

skew.effect.range


# --- examine distribution to choose standardized contrast ---

df.50 %>%
  group_by(fire) %>%
  summarise(
    min = min(ht_zskew, na.rm = TRUE),
    p01 = quantile(ht_zskew, 0.01, na.rm = TRUE),
    p05 = quantile(ht_zskew, 0.05, na.rm = TRUE),
    p25 = quantile(ht_zskew, 0.25, na.rm = TRUE),
    median = median(ht_zskew, na.rm = TRUE),
    p75 = quantile(ht_zskew, 0.75, na.rm = TRUE),
    p95 = quantile(ht_zskew, 0.95, na.rm = TRUE),
    p99 = quantile(ht_zskew, 0.99, na.rm = TRUE),
    max = max(ht_zskew, na.rm = TRUE)
  )

skew.contrast <- skew.pred %>%
  group_by(fire) %>%
  summarise(
    swe.0 = approx(
      x = ht_zskew,
      y = swe_peak,
      xout = 0
    )$y,
    
    swe.1 = approx(
      x = ht_zskew,
      y = swe_peak,
      xout = 1
    )$y
  ) %>%
  mutate(
    change.m = swe.1 - swe.0,
    change.cm = change.m * 100,
    change.percent = (change.m / swe.0) * 100
  )

skew.contrast

# ----- combined model -----
skew.pred.combined <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
  # prediction sample for fire
  df.fire <- df.pred %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # fire-specific prediction range
  skew.seq <- seq(
    quantile(
      df.50$ht_zskew[df.50$fire == fire.name],
      0.01,
      na.rm = TRUE
    ),
    quantile(
      df.50$ht_zskew[df.50$fire == fire.name],
      0.99,
      na.rm = TRUE
    ),
    length.out = 100
  )
  
  map_dfr(skew.seq, function(skew.value) {
    
    newdata <- df.fire %>%
      mutate(ht_zskew = skew.value)
    
    # central prediction from common-smooth model
    pred <- predict(
      model.swe.combined,
      newdata = newdata,
      type = 'response'
    )
    
    fit <- mean(pred^2)
    
    tibble(
      fire = fire.name,
      ht_zskew = skew.value,
      swe_peak = fit
    )
  })
})

# ----- thresholds/optima -----
skew.pred.stageC <- skew.pred %>%
  filter(
    ht_zskew >= -1,
    ht_zskew <= 3
  )

skew.optimum <- skew.pred.stageC %>%
  group_by(fire) %>%
  summarise(
    max.swe = max(swe_peak),
    
    skew.at.max = ht_zskew[which.max(swe_peak)],
    
    threshold.95 = 0.95 * max.swe,
    
    skew.95.low = min(
      ht_zskew[swe_peak >= threshold.95]
    ),
    
    skew.95.high = max(
      ht_zskew[swe_peak >= threshold.95]
    )
  )

skew.optimum

get_high_swe_ranges <- function(data, xvar) {
  
  data %>%
    arrange(.data[[xvar]]) %>%
    mutate(
      threshold.95 = 0.95 * max(swe_peak),
      above.95 = swe_peak >= threshold.95,
      
      # identify separate continuous runs
      run = cumsum(
        above.95 != lag(above.95, default = first(above.95))
      )
    ) %>%
    filter(above.95) %>%
    group_by(run) %>%
    summarise(
      range.low = min(.data[[xvar]]),
      range.high = max(.data[[xvar]]),
      .groups = 'drop'
    )
}

skew.ranges <- skew.pred.stageC %>%
  group_by(fire) %>%
  group_modify(
    ~ get_high_swe_ranges(.x, 'ht_zskew')
  ) %>%
  ungroup()

skew.ranges

ggplot(
  skew.pred.stageC,
  aes(
    x = ht_zskew,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_rect(
    data = skew.ranges,
    aes(
      xmin = range.low,
      xmax = range.high,
      ymin = -Inf,
      ymax = Inf,
      fill = fire
    ),
    inherit.aes = FALSE,
    alpha = 0.08,
    color = NA
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_segment(
    data = skew.optimum,
    aes(
      x = skew.95.low,
      xend = skew.95.high,
      y = threshold.95,
      yend = threshold.95,
      color = fire
    ),
    inherit.aes = FALSE,
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  geom_line(
    data = skew.pred.combined,
    aes(
      x = ht_zskew,
      y = swe_peak
    ),
    inherit.aes = FALSE,
    color = 'grey',
    linetype = 'solid',
    linewidth = 0.8
  ) +
  geom_rug(
    data = skew.rug,
    aes(x = ht_zskew, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  scale_x_continuous(
    limits = c(-1, 3),
    breaks = -1:3
  ) +
  scale_y_continuous(
    limits = c(0.38, 0.82),
    breaks = seq(0.4, 0.8, 0.1)
  ) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Canopy height skewness',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()


# ------------------ combined plot - without thresholds - ------------------
# --- gap percent ---
library(patchwork)
p.gap <- ggplot(
  gap.pred,
  aes(
    x = gap_percent,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = gap.rug,
    aes(x = gap_percent, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()


# --- maximum canopy height ---

p.ht <- ggplot(
  ht.pred,
  aes(
    x = ht_zmax,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = ht.rug,
    aes(x = ht_zmax, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()


# --- distance to canopy ---

p.dist <- ggplot(
  dist.canopy.pred,
  aes(
    x = gap_dist_to_canopy_mean,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = dist.canopy.rug,
    aes(x = gap_dist_to_canopy_mean, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  scale_x_continuous(
    limits = c(1, 10),
    breaks = seq(2, 10, 2)
  ) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Mean distance to canopy (m)',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()


# --- canopy height skewness ---

p.skew <- ggplot(
  skew.pred,
  aes(
    x = ht_zskew,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = skew.rug,
    aes(x = ht_zskew, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  scale_x_continuous(
    limits = c(-1, 3),
    breaks = -1:3
  ) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Canopy height skewness',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()

# -- common theme for combined canopy figure ---

canopy.theme <- theme_classic() +
  theme(
    axis.title.y = element_blank(),
    
    strip.background = element_blank(),
    strip.text = element_text(
      face = 'bold',
      size = 10
    ),
    
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 9),
    
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

p.gap <- p.gap + canopy.theme
p.ht   <- p.ht   + canopy.theme
p.dist <- p.dist + canopy.theme
p.skew <- p.skew + canopy.theme

p.gap <- p.gap +
  labs(tag = 'A') +
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

p.ht <- p.ht +
  labs(tag = 'B') +
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

p.dist <- p.dist +
  labs(tag = 'C') +
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

p.skew <- p.skew +
  labs(tag = 'D') +
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

# --- combine --
canopy.fig <- p.gap / p.ht / p.dist / p.skew

canopy.fig <- (
  p.gap /
    p.ht /
    p.dist /
    p.skew
) &
  scale_y_continuous(
    limits = c(0.38, 0.82),
    breaks = seq(0.4, 0.8, 0.1)
  )

library(grid)

y.title <- wrap_elements(
  grid::textGrob(
    'Predicted peak SWE (m)',
    rot = 90,
    gp = grid::gpar(fontsize = 11)
  )
)

canopy.fig.final <- y.title + canopy.fig +
  plot_layout(
    widths = c(0.04, 1)
  )

canopy.fig.final

# ------------------ combined plot - with thresholds - ------------------
library(patchwork)
library(grid)

# --- common theme ---

optimum.theme <- theme_classic() +
  theme(
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(
      face = 'bold',
      size = 10
    ),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )


# --- A. canopy gap ---

p.gap.opt <- ggplot(
  gap.pred,
  aes(
    x = gap_percent,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_rect(
    data = gap.optimum,
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
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_hline(
    data = gap.optimum,
    aes(
      yintercept = threshold.95,
      color = fire
    ),
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  geom_line(
    data = gap.pred.combined,
    aes(
      x = gap_percent,
      y = swe_peak
    ),
    inherit.aes = FALSE,
    color = 'grey',
    linetype = 'solid',
    linewidth = 0.8
  ) +
  geom_rug(
    data = gap.rug,
    aes(x = gap_percent, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Canopy gap (%)',
    tag = 'A'
  ) +
  optimum.theme


# --- B. maximum canopy height ---

p.ht.opt <- ggplot(
  ht.pred,
  aes(
    x = ht_zmax,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
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
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_hline(
    data = ht.optimum,
    aes(
      yintercept = threshold.95,
      color = fire
    ),
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  geom_line(
    data = ht.pred.combined,
    aes(
      x = ht_zmax,
      y = swe_peak
    ),
    inherit.aes = FALSE,
    color = 'grey',
    linetype = 'solid',
    linewidth = 0.8
  ) +
  geom_rug(
    data = ht.rug,
    aes(x = ht_zmax, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Maximum canopy height (m)',
    tag = 'B'
  ) +
  optimum.theme


# --- C. canopy height skewness ---

p.skew.opt <- ggplot(
  skew.pred.stageC,
  aes(
    x = ht_zskew,
    y = swe_peak,
    color = fire,
    fill = fire
  )
) +
  geom_rect(
    data = skew.ranges,
    aes(
      xmin = range.low,
      xmax = range.high,
      ymin = -Inf,
      ymax = Inf,
      fill = fire
    ),
    inherit.aes = FALSE,
    alpha = 0.08,
    color = NA
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_hline(
    data = skew.optimum,
    aes(
      yintercept = threshold.95,
      color = fire
    ),
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  geom_line(
    data = skew.pred.combined,
    aes(
      x = ht_zskew,
      y = swe_peak
    ),
    inherit.aes = FALSE,
    color = 'grey',
    linetype = 'solid',
    linewidth = 0.8
  ) +
  geom_rug(
    data = skew.rug,
    aes(x = ht_zskew, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1,
    labeller = labeller(
      fire = c(
        'caldor' = 'Caldor',
        'castle' = 'Castle',
        'creek' = 'Creek'
      )
    )
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  scale_x_continuous(
    limits = c(-1, 3),
    breaks = -1:3
  ) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Canopy height skewness',
    tag = 'C'
  ) +
  optimum.theme

# --- combine ---
canopy.optimum.fig <- (
  p.gap.opt /
    p.ht.opt /
    p.skew.opt
) &
  scale_y_continuous(
    limits = c(0.38, 0.82),
    breaks = seq(0.4, 0.8, 0.1)
  ) &
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

y.title <- wrap_elements(
  grid::textGrob(
    'Predicted peak SWE (m)',
    rot = 90,
    gp = grid::gpar(fontsize = 11)
  )
)

canopy.optimum.fig.final <- y.title + canopy.optimum.fig +
  plot_layout(
    widths = c(0.04, 1)
  )

canopy.optimum.fig.final


# ------------------ effect.table ----------------
effect.table <- bind_rows(
  
  gap.contrast %>%
    transmute(
      canopy.metric = 'Canopy gap',
      contrast = '25 to 75%',
      fire,
      change.cm,
      change.percent
    ),
  
  ht.contrast %>%
    transmute(
      canopy.metric = 'Maximum canopy height',
      contrast = '20 to 40 m',
      fire,
      change.cm,
      change.percent
    ),
  
  dist.contrast %>%
    transmute(
      canopy.metric = 'Mean distance to canopy',
      contrast = '2 to 10 m',
      fire,
      change.cm,
      change.percent
    ),
  
  skew.contrast %>%
    transmute(
      canopy.metric = 'Canopy height skewness',
      contrast = '0 to 1',
      fire,
      change.cm,
      change.percent
    )
) %>%
  mutate(
    effect = sprintf(
      '%+.1f cm (%+.1f%%)',
      change.cm,
      change.percent
    ),
    
    fire = recode(
      as.character(fire),
      'caldor' = 'Caldor',
      'castle' = 'Castle',
      'creek' = 'Creek'
    )
  ) %>%
  select(
    canopy.metric,
    contrast,
    fire,
    effect
  ) %>%
  tidyr::pivot_wider(
    names_from = fire,
    values_from = effect
  )

effect.table





# ----------------------------------- ** OPTIMUM CANOPY COMBINATIONS ** ------------------------------------
# ---------- gap percent x max canopy height ----------
# ----- creek -----

# Subset the full dataset to Creek Fire.
# droplevels() removes unused factor levels, so fire only contains 'Creek'
df.creek <- df.50 %>%
  filter(fire == 'Creek') %>%
  droplevels()

df.creek.marg <- df.creek %>%
  group_by(wy, burned) %>%
  slice_sample(n = 1000) %>%
  ungroup()

# --- observed canopy combinations ---

canopy.combos <- df.creek %>%
  mutate(
    gap_bin = round(gap_percent / 2) * 2, # round gap_percent to nearest 2%
    ht_bin = round(ht_zmax / 2) * 2 # round ht_zmax to nearest 2m
  ) %>%
  count(gap_bin, ht_bin, name = 'n') %>% # count how many are in each bin 
  filter(n >= 100)

# --- marginal predictions across observed canopy combinations ---

canopy.combos$pred.sqrt <- NA_real_
canopy.combos$pred.swe <- NA_real_

for (i in seq_len(nrow(canopy.combos))) {
  
  # Assign one observed canopy combination to all observations
  # in the marginalization sample.
  #
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
  
  # Average predictions on the model scale.
  canopy.combos$pred.sqrt[i] <- mean(pred)
  
  # Back-transform the marginal mean to SWE.
  canopy.combos$pred.swe[i] <- mean(pred)^2
}

head(canopy.combos)
range(canopy.combos$pred.swe)

# --- identify optimum canopy combinations ---

# maximum predicted SWE
max.swe <- max(canopy.combos$pred.swe, na.rm = TRUE)

# threshold for 95% of maximum
threshold.95 <- 0.95 * max.swe

# canopy bin with maximum predicted SWE
optimum <- canopy.combos %>%
  filter(pred.swe == max.swe)

# all well-supported canopy bins producing >= 95% of maximum SWE
optimum.95 <- canopy.combos %>%
  filter(pred.swe >= threshold.95)

optimum
threshold.95

canopy.combos <- canopy.combos %>%
  mutate(
    optimum.95 = pred.swe >= threshold.95
  )

# create outline of area >95%
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
    by = c('gap_bin', 'ht_bin')
  ) %>%
  mutate(
    optimum.95 = ifelse(
      !is.na(pred.swe) & pred.swe >= threshold.95,
      1,
      0
    )
  )

ggplot(
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

# ----- castle -----

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




# ----- caldor -----

# subset to Caldor
df.caldor <- df.50 %>%
  filter(fire == 'Caldor') %>%
  mutate(
    fire = factor(
      'Caldor',
      levels = levels(model.frame(model.swe)$fire)
    )
  )


# --- observed canopy combinations ---

canopy.combos.caldor <- df.caldor %>%
  mutate(
    gap_bin = round(gap_percent / 2) * 2,
    ht_bin = round(ht_zmax / 2) * 2
  ) %>%
  count(gap_bin, ht_bin, name = 'n') %>%
  filter(n >= 100)


# --- marginalization sample ---

set.seed(61)

df.caldor.marg <- df.caldor %>%
  group_by(wy, burned) %>%
  slice_sample(n = 1000) %>%
  ungroup()


# --- marginal predictions ---

canopy.combos.caldor$pred.sqrt <- NA_real_
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
  
  # marginal mean and back-transformed SWE
  canopy.combos.caldor$pred.sqrt[i] <- mean(pred)
  canopy.combos.caldor$pred.swe[i] <- mean(pred)^2
  
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
  
  optimum.caldor
  
  optimum.95.caldor %>%
    summarise(
      gap.low = min(gap_bin),
      gap.high = max(gap_bin),
      ht.low = min(ht_bin),
      ht.high = max(ht_bin),
      n.combinations = n()
    )
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
  ) %>%
  mutate(
    optimum.95 = ifelse(
      !is.na(pred.swe) &
        pred.swe >= threshold.95.caldor,
      1,
      0
    )
  )

# --- plot ---
ggplot(
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

optimum.caldor

threshold.95.caldor

optimum.95.caldor %>%
  arrange(desc(pred.swe))

nrow(optimum.95.caldor)

ggplot(
  optimum.95.caldor,
  aes(
    x = gap_bin,
    y = ht_bin
  )
) +
  geom_tile(
    width = 2,
    height = 2
  ) +
  labs(
    x = 'Gap percent',
    y = 'Maximum canopy height (m)'
  ) +
  theme_bw()

# --- identify contiguous optimum regions ---

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
model.swe <- bam(sqrt(swe_peak) ~ wy * fire
                 + s(elevation, by = wy, k = 20)
                 + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                 + s(ht_zmax, by = fire, k = 10) + s(gap_percent, by = fire, k = 10) 
                 + s(ht_zskew, by = fire, k = 20),
                 data = df.50,
                 method = 'fREML',
                 discrete = TRUE)

model.swe.burned <- model.swe <- bam(sqrt(swe_peak) ~ wy * fire + s(cbibc)
                                     + s(elevation, by = wy, k = 20)
                                     + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                                     + s(ht_zmax, by = fire, k = 10) + s(gap_percent, by = fire, k = 10) 
                                     + s(ht_zskew, by = fire, k = 20),
                                     data = df.50,
                                     method = 'fREML',
                                     discrete = TRUE)
