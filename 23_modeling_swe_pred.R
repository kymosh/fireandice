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
# df.50.balanced <- readRDS(file.path(dir, 'df_50m_raw_balanced.rds')) 

# str(df.50.raw)

df.50.raw.test <- df.50.raw %>%
  group_by(fire) %>%
  slice_sample(n = 10000) %>%
  ungroup()

# df.50.balanced.test <- df.50.balanced %>%
#   group_by(fire) %>%
#   slice_sample(n = 10000) %>%
#   ungroup()

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
    fire = factor(
      fire,
      levels = c('caldor', 'castle', 'creek'),
      labels = c('Caldor', 'Castle', 'Creek') # capitalize
    )) %>%
  droplevels()


burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)

fire.colors <- c(
  'caldor' = '#009E73',
  'castle' = '#E69F00',
  'creek' = '#CC79A7'
)


model.swe <- bam(sqrt(swe_peak) ~ wy + fire + burned 
                 + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
                 + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                 + s(ht_zmax, by = fire, k = 10) + s(gap_percent, by = fire, k = 10) + s(gap_dist_to_canopy_mean, by = fire, k = 20) + s(ht_zskew, by = fire, k = 20),
                 data = df.50,
                 method = 'fREML',
                 discrete = TRUE)


summary(model.swe)
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
# ----------------------------------- MARGINAL EFFECT PLOTS ------------------------------------
# --------------- predictions using by=fire model ---------------

# make balanced prediction sample
df.pred <- df.50 %>%
  group_by(fire, wy, burned) %>%
  slice_sample(n = 500) %>%
  ungroup()


# ----- gap percent - version 1 -----
# create marginal predictions
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
    
    pred <- predict(
      model.swe,
      newdata = newdata,
      type = 'response'
    )
    
    tibble(
      fire = fire.name,
      gap_percent = gap.value,
      swe_peak = mean(pred^2)
    )
  })
})

ggplot(
  gap.pred,
  aes(x = gap_percent, y = swe_peak)
) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()


# ----- gap percent - version 2 ----------------
# this version has Confidence intervals

set.seed(61)

# simulate coefficient draws once
n.sim <- 500

beta.sim <- MASS::mvrnorm(
  n = n.sim,
  mu = coef(model.swe),
  Sigma = vcov(model.swe)
)

gap.pred.2 <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
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

# look at CI widths, confirm that they are very small
gap.pred.2 %>%
  mutate(
    lower.diff.cm = (swe_peak - lower) * 100,
    upper.diff.cm = (upper - swe_peak) * 100
  ) %>%
  group_by(fire) %>%
  summarise(
    mean.lower.cm = mean(lower.diff.cm),
    mean.upper.cm = mean(upper.diff.cm),
    max.lower.cm = max(lower.diff.cm),
    max.upper.cm = max(upper.diff.cm)
  )

gap.rug <- df.50 %>%
  group_by(fire) %>%
  slice_sample(n = 3000) %>%
  ungroup()

ggplot(
  gap.pred.2,
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
    aes(x = ht_zmax, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ fire, nrow = 1) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()

# --- calculate effect size ---
gap.effect.range <- gap.pred.2 %>%
  group_by(fire) %>%
  summarise(
    swe.min = min(swe_peak),
    swe.max = max(swe_peak),
    range.m = swe.max - swe.min,
    range.cm = range.m * 100,
    percent.range = (range.m / swe.min) * 100
  )

gap.effect.range

gap.effect.range <- gap.pred.2 %>%
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
    gap.pred.2 %>%
      group_by(fire) %>%
      slice_min(abs(gap_percent - 50), n = 1, with_ties = FALSE) %>%
      mutate(gap.target = 50),
    
    gap.pred.2 %>%
      group_by(fire) %>%
      slice_min(abs(gap_percent - 75), n = 1, with_ties = FALSE) %>%
      mutate(gap.target = 75)
  ) %>%
  arrange(fire, gap.target)

gap.contrasts

gap.values <- c(25, 50, 75)
# --- standardized effect size ---
gap.contrast <- gap.pred.2 %>%
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
# ------ maximum canopy height ---------------

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

# --- overall magnitude ---
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
# ------ distance to canopy ---------------
dist.canopy.pred <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
  # prediction sample for fire
  df.fire <- df.pred %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # fire-specific prediction range
  dist.canopy.seq <- seq(
    1,
    10,
    length.out = 100
  )
  
  map_dfr(dist.canopy.seq, function(dist.canopy.value) {
    
    newdata <- df.fire %>%
      mutate(gap_dist_to_canopy_mean = dist.canopy.value)
    
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
      gap_dist_to_canopy_mean = dist.canopy.value,
      swe_peak = fit,
      lower = quantile(sim.swe, 0.025),
      upper = quantile(sim.swe, 0.975)
    )
  })
})

dist.canopy.rug <- df.50 %>%
  filter(gap_dist_to_canopy_mean >= 1,
         gap_dist_to_canopy_mean <= 10) %>%
  group_by(fire) %>%
  slice_sample(n = 3000) %>%
  ungroup()

ggplot(
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
    nrow = 1
  ) +
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  scale_x_continuous(
    limits = c(1, 10),
    breaks = seq(2, 10, 2)
  ) +
  guides(color = 'none', fill = 'none') +
  labs(
    x = 'Distance to canopy (m)',
    y = 'Predicted peak SWE (m)'
  ) +
  theme_classic()

# --- overall magnitude ---
dist.effect.range <- dist.canopy.pred %>%
  group_by(fire) %>%
  summarise(
    min.pred.swe = min(swe_peak),
    dist.at.min = gap_dist_to_canopy_mean[which.min(swe_peak)],
    
    max.pred.swe = max(swe_peak),
    dist.at.max = gap_dist_to_canopy_mean[which.max(swe_peak)],
    
    swe.range.cm = (max.pred.swe - min.pred.swe) * 100,
    relative.range = ((max.pred.swe - min.pred.swe) /
                        min.pred.swe) * 100
  )

dist.effect.range


dist.contrast <- dist.canopy.pred %>%
  group_by(fire) %>%
  summarise(
    swe.2 = approx(
      x = gap_dist_to_canopy_mean,
      y = swe_peak,
      xout = 2
    )$y,
    
    swe.10 = approx(
      x = gap_dist_to_canopy_mean,
      y = swe_peak,
      xout = 10
    )$y
  ) %>%
  mutate(
    change.m = swe.5 - swe.2,
    change.cm = change.m * 100,
    change.percent = (change.m / swe.2) * 100
  )

dist.contrast


# ------ canopy height skewness ---------------

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


# --- overall magnitude ---

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
# ------------------ combined plot ------------------
# --- gap percent ---
library(patchwork)
p.gap <- ggplot(
  gap.pred.2,
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
# ------------------------------------ old-ish predictions -------------------------------------
# ----- gap predictions using fire-specific model -----
fit_fire_predictions <- function(fire.name, df, model.formula) {
  
  # ----- create fire-specific dataframe -----
  
  df.fire <- df %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  
  # ----- fit fire-specific model -----
  
  model.swe.fire <- bam(model.formula,
    data = df.fire,
    method = 'fREML',
    discrete = TRUE
  )
  
  
  # ----- prediction grid for gap percent -----
  
  # Create prediction grid across the full range of gap percent
  # for each water year and burn status
  pred.gap <- expand.grid(
    wy = levels(df.fire$wy),
    burned = levels(df.fire$burned),
    gap_percent = seq(0, 100, length.out = 101)
  ) %>%
    as_tibble()
  
  
  # ----- representative values for static predictors -----
  
  # Hold all other static predictors at their median value within the fire
  ref.fire <- df.fire %>%
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
      ht_zskew = median(ht_zskew, na.rm = TRUE)
    )
  
  
  # ----- representative elevation -----
  
  # Use median elevation within each water year
  ref.elev <- df.fire %>%
    group_by(wy) %>%
    summarize(
      elevation = median(elevation, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  # ----- add representative values to prediction dataset -----
  
  # Add year-specific elevation
  pred.gap <- pred.gap %>%
    left_join(ref.elev, by = 'wy')
  
  # Add fire-wide median values for remaining predictors
  pred.gap <- pred.gap %>%
    mutate(
      rad_dtm_accum = ref.fire$rad_dtm_accum,
      slope = ref.fire$slope,
      aspect_sin = ref.fire$aspect_sin,
      tpi150 = ref.fire$tpi150,
      tpi2010 = ref.fire$tpi2010,
      ht_zmax = ref.fire$ht_zmax,
      gap_dist_to_canopy_mean = ref.fire$gap_dist_to_canopy_mean,
      ht_zskew = ref.fire$ht_zskew
    )
  
  
  # ----- generate predictions -----
  
  # Predictions are initially on the sqrt(SWE) scale
  p <- predict(
    model.swe.fire,
    newdata = pred.gap,
    type = 'response',
    se.fit = TRUE
  )
  
  # residual variance for approximate bias correction
  sigma2 <- summary(model.swe.fire)$scale
  
  
  # ----- back-transform predictions to SWE -----
  
  pred.gap <- pred.gap %>%
    mutate(
      fire = fire.name,
      
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
  
  
  # ----- return model and predictions -----
  
  list(
    model = model.swe.fire,
    predictions = pred.gap
  )
}


# run for all fires
fires <- c('caldor', 'castle', 'creek')

test.formula <- as.formula(sqrt(swe_peak) ~ wy + burned 
  + s(elevation, by = wy, k = 20) + s(elevation, k = 20) 
  + s(rad_dtm_accum, k = 10)
  + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10)
  + s(gap_percent, k = 10)
  + ti(gap_percent, gap_dist_to_canopy_mean, k = c(10, 20))
  + s(gap_dist_to_canopy_mean, k = 20)
  + s(ht_zskew, k = 20))

base.formula <- as.formula(sqrt(swe_peak) ~ wy + burned 
  + s(elevation, by = wy, k = 20)
  + s(elevation, k = 20) 
  + s(rad_dtm_accum, k = 10)
  + s(slope, k = 10)
  + s(aspect_sin, k = 10)
  + s(tpi150, k = 10)
  + s(tpi2010, k = 10) 
  + s(ht_zmax, k = 10)
  + s(gap_percent, k = 10)
  + s(gap_dist_to_canopy_mean, k = 20)
  + s(ht_zskew, k = 20))



fire.results.test <- lapply(
  fires,
  fit_fire_predictions,
  df = df.50,
  model.formula = test.formula
)

fire.results.base <- lapply(
  fires,
  fit_fire_predictions,
  df = df.50,
  model.formula = base.formula
)


names(fire.results.test) <- fires
names(fire.results.base) <- fires

summary(fire.results.base$caldor$model)
summary(fire.results.base$castle$model)
summary(fire.results.base$creek$model)

summary(fire.results.test$caldor$model)
summary(fire.results.test$castle$model)
summary(fire.results.test$creek$model)

# extract all predictions
pred.gap.all <- bind_rows(
  lapply(fire.results.test, `[[`, 'predictions')
)

# average across years
pred.gap.avg <- pred.gap.all %>%
  group_by(
    fire,
    burned,
    gap_percent
  ) %>%
  summarize(
    fit.swe = mean(fit.swe),
    .groups = 'drop'
  )

# plot
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

# ----- gap predictions - across observed canopy conditions -----
fit_fire_predictions_observed <- function(fire.name, df) {
  
  # ----- create fire-specific dataframe -----
  
  df.fire <- df %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  
  # ----- fit fire-specific model -----
  
  model.swe.fire <- bam(
    sqrt(swe_peak) ~ wy + burned 
    + s(elevation, by = wy, k = 20)
    + s(elevation, k = 20) 
    + s(rad_dtm_accum, k = 10)
    + s(slope, k = 10)
    + s(aspect_sin, k = 10)
    + s(tpi150, k = 10)
    + s(tpi2010, k = 10) 
    + s(ht_zmax, k = 10)
    + s(gap_percent, by = burned, k = 10)
    + s(gap_dist_to_canopy_mean, k = 20)
    + s(ht_zskew, k = 20),
    data = df.fire,
    method = 'fREML',
    discrete = TRUE
  )
  
  
  # ----- canopy lookup -----
  
  # Describe the typical ht_zmax and distance-to-canopy associated
  # with different levels of gap within this fire.
  
  canopy.lookup <- df.fire %>%
    mutate(
      gap.bin = ntile(gap_percent, 100)
    ) %>%
    group_by(gap.bin) %>%
    summarize(
      gap_lookup = mean(gap_percent, na.rm = TRUE),
      ht_lookup = mean(ht_zmax, na.rm = TRUE),
      dist_lookup = mean(
        gap_dist_to_canopy_mean,
        na.rm = TRUE
      ),
      .groups = 'drop'
    )
  
  
  # ----- prediction grid for gap percent -----
  
  pred.gap.real <- expand.grid(
    wy = levels(df.fire$wy),
    burned = levels(df.fire$burned),
    gap_percent = seq(0, 100, length.out = 101)
  ) %>%
    as_tibble()
  
  
  # ----- representative static predictor values -----
  
  ref.fire.real <- df.fire %>%
    summarize(
      rad_dtm_accum = median(rad_dtm_accum, na.rm = TRUE),
      slope = median(slope, na.rm = TRUE),
      aspect_sin = median(aspect_sin, na.rm = TRUE),
      tpi150 = median(tpi150, na.rm = TRUE),
      tpi2010 = median(tpi2010, na.rm = TRUE),
      ht_zskew = median(ht_zskew, na.rm = TRUE)
    )
  
  
  # ----- representative elevation by year -----
  
  ref.elev.real <- df.fire %>%
    group_by(wy) %>%
    summarize(
      elevation = median(elevation, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  # ----- add representative predictor values -----
  
  pred.gap.real <- pred.gap.real %>%
    left_join(ref.elev.real, by = 'wy') %>%
    mutate(
      rad_dtm_accum = ref.fire.real$rad_dtm_accum,
      slope = ref.fire.real$slope,
      aspect_sin = ref.fire.real$aspect_sin,
      tpi150 = ref.fire.real$tpi150,
      tpi2010 = ref.fire.real$tpi2010,
      ht_zskew = ref.fire.real$ht_zskew
    )
  
  
  # ----- assign realistic correlated canopy values -----
  
  # Allow ht_zmax and distance-to-canopy to change with gap_percent
  # according to their observed relationship within this fire.
  
  pred.gap.real <- pred.gap.real %>%
    mutate(
      
      ht_zmax = approx(
        x = canopy.lookup$gap_lookup,
        y = canopy.lookup$ht_lookup,
        xout = gap_percent,
        rule = 2
      )$y,
      
      gap_dist_to_canopy_mean = approx(
        x = canopy.lookup$gap_lookup,
        y = canopy.lookup$dist_lookup,
        xout = gap_percent,
        rule = 2
      )$y
    )
  
  
  # ----- generate predictions -----
  
  p <- predict(
    model.swe.fire,
    newdata = pred.gap.real,
    type = 'response',
    se.fit = TRUE
  )
  
  sigma2 <- summary(model.swe.fire)$scale
  
  
  # ----- back-transform predictions to SWE -----
  
  pred.gap.real <- pred.gap.real %>%
    mutate(
      fire = fire.name,
      
      fit.sqrt = p$fit,
      se.sqrt = p$se.fit,
      
      lower.sqrt = fit.sqrt - 1.96 * se.sqrt,
      upper.sqrt = fit.sqrt + 1.96 * se.sqrt,
      
      fit.swe = fit.sqrt^2,
      fit.swe.mean = fit.sqrt^2 + sigma2,
      
      lower.swe = pmax(0, lower.sqrt)^2,
      upper.swe = pmax(0, upper.sqrt)^2
    )
  
  
  # ----- return model and predictions -----
  
  list(
    model = model.swe.fire,
    predictions = pred.gap.real,
    lookup = canopy.lookup
  )
}

fires <- c('caldor', 'castle', 'creek')

fire.results.real <- lapply(
  fires,
  fit_fire_predictions_observed,
  df = df.50
)

names(fire.results.real) <- fires

pred.gap.real.all <- bind_rows(
  lapply(fire.results.real, `[[`, 'predictions')
)

pred.gap.real.avg <- pred.gap.real.all %>%
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
# ----- fire-gap scenarios (adding in other predictors) -----
fit_fire_gap_scenarios <- function(fire.name, df) {
  
  # ----- create fire-specific dataframe -----
  
  df.fire <- df %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  
  # ----- fit fire-specific model -----
  
  model.swe.fire <- bam(
    sqrt(swe_peak) ~ wy + burned 
    + s(elevation, by = wy, k = 20)
    + s(elevation, k = 20) 
    + s(rad_dtm_accum, k = 10)
    + s(slope, k = 10)
    + s(aspect_sin, k = 10)
    + s(tpi150, k = 10)
    + s(tpi2010, k = 10) 
    + s(ht_zmax, k = 10)
    + s(gap_percent, k = 10)
    + s(gap_dist_to_canopy_mean, k = 20)
    + s(ht_zskew, k = 20),
    data = df.fire,
    method = 'fREML',
    discrete = TRUE
  )
  
  
  # ----- canopy lookup -----
  
  # Describe typical canopy height and distance to canopy
  # across the observed gap gradient
  
  canopy.lookup <- df.fire %>%
    mutate(
      gap.bin = ntile(gap_percent, 100)
    ) %>%
    group_by(gap.bin) %>%
    summarize(
      gap_lookup = mean(gap_percent, na.rm = TRUE),
      ht_lookup = mean(ht_zmax, na.rm = TRUE),
      dist_lookup = mean(
        gap_dist_to_canopy_mean,
        na.rm = TRUE
      ),
      .groups = 'drop'
    )
  
  
  # ----- base prediction grid -----
  
  pred.base <- expand.grid(
    wy = levels(df.fire$wy),
    burned = levels(df.fire$burned),
    gap_percent = seq(0, 100, length.out = 101)
  ) %>%
    as_tibble()
  
  
  # ----- representative static values -----
  
  ref.fire <- df.fire %>%
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
      ht_zskew = median(ht_zskew, na.rm = TRUE)
    )
  
  
  # ----- representative elevation by water year -----
  
  ref.elev <- df.fire %>%
    group_by(wy) %>%
    summarize(
      elevation = median(elevation, na.rm = TRUE),
      .groups = 'drop'
    )
  
  
  # ----- add representative values -----
  
  pred.base <- pred.base %>%
    left_join(ref.elev, by = 'wy') %>%
    mutate(
      rad_dtm_accum = ref.fire$rad_dtm_accum,
      slope = ref.fire$slope,
      aspect_sin = ref.fire$aspect_sin,
      tpi150 = ref.fire$tpi150,
      tpi2010 = ref.fire$tpi2010,
      ht_zmax = ref.fire$ht_zmax,
      gap_dist_to_canopy_mean = ref.fire$gap_dist_to_canopy_mean,
      ht_zskew = ref.fire$ht_zskew
    )
  
  
  # ----- values following observed gap relationships -----
  
  ht.observed <- approx(
    x = canopy.lookup$gap_lookup,
    y = canopy.lookup$ht_lookup,
    xout = pred.base$gap_percent,
    rule = 2
  )$y
  
  dist.observed <- approx(
    x = canopy.lookup$gap_lookup,
    y = canopy.lookup$dist_lookup,
    xout = pred.base$gap_percent,
    rule = 2
  )$y
  
  
  # ----- scenario A: gap only -----
  
  # Both other canopy variables remain fixed at their medians
  
  pred.A <- pred.base %>%
    mutate(
      scenario = 'A: gap only'
    )
  
  
  # ----- scenario B: gap + height -----
  
  # Maximum canopy height follows its observed relationship with gap,
  # while distance to canopy remains fixed
  
  pred.B <- pred.base %>%
    mutate(
      ht_zmax = ht.observed,
      scenario = 'B: gap + height'
    )
  
  
  # ----- scenario C: gap + distance -----
  
  # Distance to canopy follows its observed relationship with gap,
  # while maximum canopy height remains fixed
  
  pred.C <- pred.base %>%
    mutate(
      gap_dist_to_canopy_mean = dist.observed,
      scenario = 'C: gap + distance'
    )
  
  
  # ----- scenario D: gap + height + distance -----
  
  # Both correlated canopy variables follow their observed
  # relationships with gap
  
  pred.D <- pred.base %>%
    mutate(
      ht_zmax = ht.observed,
      gap_dist_to_canopy_mean = dist.observed,
      scenario = 'D: all observed'
    )
  
  
  # ----- combine scenarios -----
  
  pred.all <- bind_rows(
    pred.A,
    pred.B,
    pred.C,
    pred.D
  )
  
  
  # ----- generate predictions -----
  
  p <- predict(
    model.swe.fire,
    newdata = pred.all,
    type = 'response',
    se.fit = TRUE
  )
  
  sigma2 <- summary(model.swe.fire)$scale
  
  
  # ----- back-transform predictions -----
  
  pred.all <- pred.all %>%
    mutate(
      fire = fire.name,
      
      fit.sqrt = p$fit,
      se.sqrt = p$se.fit,
      
      lower.sqrt = fit.sqrt - 1.96 * se.sqrt,
      upper.sqrt = fit.sqrt + 1.96 * se.sqrt,
      
      fit.swe = fit.sqrt^2,
      fit.swe.mean = fit.sqrt^2 + sigma2,
      
      lower.swe = pmax(0, lower.sqrt)^2,
      upper.swe = pmax(0, upper.sqrt)^2
    )
  
  
  # ----- return model and predictions -----
  
  list(
    model = model.swe.fire,
    predictions = pred.all,
    lookup = canopy.lookup
  )
}

fire.scenarios <- lapply(
  fires,
  fit_fire_gap_scenarios,
  df = df.50
)

names(fire.scenarios) <- fires

# combine
pred.scenarios <- bind_rows(
  lapply(fire.scenarios, `[[`, 'predictions')
)

# average across years
pred.scenarios.avg <- pred.scenarios %>%
  group_by(
    fire,
    scenario,
    burned,
    gap_percent
  ) %>%
  summarize(
    fit.swe = mean(fit.swe),
    .groups = 'drop'
  )

# plot
ggplot(
  pred.scenarios.avg,
  aes(
    x = gap_percent,
    y = fit.swe,
    color = burned
  )
) +
  geom_line(linewidth = 1) +
  facet_grid(scenario ~ fire) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Mean predicted peak SWE',
    color = 'Burn status'
  ) +
  theme_bw()
sapply(model.swe$smooth, function(x) x$label)


# ----------------------- OLD CODE BELOW -----------------------------------
# ==============================================================================
#  Create prediction dataset for gap pct
# ==============================================================================

# ----- this is just for 2023! -----
# create gap sequence
gap.seq <- seq(
  min(df.50$gap_percent),
  max(df.50$gap_gercent),
  length.out = 100
)

# build predication dataframe
pred.gap <- expand.grid(
  gap_gap_pct = gap.seq,
  burned = levels(df.50$burned)
)

# --- unscale gap so we can match to realistic raw height values ---
gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

pred.gap$gap_raw <- pred.gap$gap_gap_pct * gap.sd + gap.mean
pred.gap$gap_percent <- pred.gap$gap_raw * 100

# --- predict at realistic heights ---
# get typical canopy height for each gap bin and burn status
ht.lookup <- df.raw %>%
  mutate(gap.bin = ntile(gap_gap_pct, 100)) %>%
  group_by(gap.bin, burned) %>%
  summarize(
    gap_lookup = mean(gap_gap_pct, na.rm = TRUE),
    ht_lookup = mean(ht_zmax, na.rm = TRUE),
    .groups = 'drop'
  )

# assign each prediction row the typical height for that gap value and burn status
pred.gap <- pred.gap %>%
  group_by(burned) %>%
  group_modify(~ {
    lookup.b <- ht.lookup %>%
      filter(burned == .y$burned) %>%
      arrange(gap_lookup)
    
    .x %>%
      mutate(
        ht_raw = approx(
          x = lookup.b$gap_lookup,
          y = lookup.b$ht_lookup,
          xout = gap_raw,
          rule = 2
        )$y
      )
  }) %>%
  ungroup()

# convert realistic raw height into scaled model units
ht.mean <- mean(df.raw$ht_zmax, na.rm = TRUE)
ht.sd <- sd(df.raw$ht_zmax, na.rm = TRUE)

pred.gap$ht_zmax <- (pred.gap$ht_raw - ht.mean) / ht.sd

# --- hold everything else constant --- 
pred.gap$rad_dtm_accum <- 0
pred.gap$topo_slope <- 0
pred.gap$topo_tpi150 <- 0
pred.gap$topo_tpi2010 <- 0
pred.gap$topo_elev <- median(df.50$topo_elev)

# choose representative year 
pred.gap$wy <- factor(2023, levels = levels(df.50$wy))

# make sure burned matches the model levels
pred.gap$burned <- factor(pred.gap$burned, levels = levels(df.50$burned))

# --- predict ---
pred <- predict(
  best.model.swe,
  newdata = pred.gap,
  se.fit = TRUE
)

# add predictions back to df
# must back-transform since used sqrt(swe) in model
pred.gap$fit <- pred$fit^2
pred.gap$lwr <- (pred$fit - 1.96 * pred$se.fit)^2
pred.gap$upr <- (pred$fit + 1.96 * pred$se.fit)^2


# --- plot! ---
ggplot(
  pred.gap,
  aes(x = gap_percent, y = fit, color = burned, fill = burned)
) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = burn.cols) +
  scale_fill_manual(values = burn.cols) +
  theme_bw() +
  labs(
    x = 'Gap percentage',
    y = 'Predicted peak SWE (m)',
    color = NULL,
    fill = NULL,
    title = 'Water Year 2023'
  )

gap.diff <- pred.gap %>%
  select(gap_gap_pct, burned, fit) %>%
  pivot_wider(
    names_from = burned,
    values_from = fit
  ) %>%
  mutate(
    diff = burned - unburned
  )

ggplot(gap.diff, aes(x = gap_gap_pct, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  theme_bw() +
  labs(
    x = "Gap percentage (scaled)",
    y = "Predicted SWE difference\nburned - unburned"
  )


# ----- for all years -----
pred.gap <- expand.grid(
  gap_gap_pct = gap.seq,
  burned = levels(df.50$burned),
  wy = levels(df.50$wy)
)

# hold everything else constant
pred.gap$rad_dtm_accum <- 0
pred.gap$topo_slope <- 0
pred.gap$topo_tpi150 <- 0
pred.gap$topo_tpi2010 <- 0
pred.gap$ht_zmax <- 0

pred.gap$topo_elev <- median(df.50$topo_elev)

# make sure burned matches the model levels
pred.gap$burned <- factor(pred.gap$burned, levels = levels(df.50$burned))

# --- predict ---
pred <- predict(
  gam.topo.canopy.best,
  newdata = pred.gap,
  se.fit = TRUE
)

# add predictions back to df
# must back-transform since used sqrt(swe) in model
pred.gap$fit <- pred$fit^2
pred.gap$lwr <- (pred$fit - 1.96 * pred$se.fit)^2
pred.gap$upr <- (pred$fit + 1.96 * pred$se.fit)^2


# --- unscale ---
gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd   <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

pred.gap$gap_raw <-
  pred.gap$gap_gap_pct * gap.sd + gap.mean

pred.gap$gap_percent <- pred.gap$gap_raw * 100

# --- plot ---
ggplot(
  pred.gap,
  aes(x = gap_percent, y = fit, color = burned, fill = burned)
) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ wy) +
  scale_color_manual(values = c("unburned" = "lightblue", "burned" = "orange")) +
  scale_fill_manual(values = c("unburned" = "gray60", "burned" = "orange")) +
  theme_bw() +
  labs(
    x = "Gap percentage",
    y = "Predicted peak SWE (m)",
    color = NULL,
    fill = NULL
  )



# ==============================================================================
#  Create prediction dataset for ht_zmax
# ==============================================================================

# create ht sequence
ht.seq <- seq(
  quantile(df.50$ht_zmax, 0.01, na.rm = TRUE),
  quantile(df.50$ht_zmax, 0.99, na.rm = TRUE),
  length.out = 100
)

# build predication dataframe
pred.ht <- expand.grid(
  ht_zmax = ht.seq,
  burned = levels(df.50$burned)
)

# hold everything else constant
pred.ht$rad_dtm_accum <- 0
pred.ht$topo_slope <- 0
pred.ht$topo_tpi150 <- 0
pred.ht$topo_tpi2010 <- 0
pred.ht$gap_gap_pct <- 0

pred.ht$topo_elev <- median(df.50$topo_elev)

# choose representative year 
pred.ht$wy <- factor(2023, levels = levels(df.50$wy))

# make sure burned matches the model levels
pred.ht$burned <- factor(pred.ht$burned, levels = levels(df.50$burned))

# --- predict ---
pred <- predict(
  gam.topo.canopy.best,
  newdata = pred.ht,
  se.fit = TRUE
)

# add predictions back to df
# must back-transform since used sqrt(swe) in model
pred.ht$fit <- pred$fit^2
pred.ht$lwr <- (pred$fit - 1.96 * pred$se.fit)^2
pred.ht$upr <- (pred$fit + 1.96 * pred$se.fit)^2


# --- unscale ---
ht.mean <- mean(df.raw$ht_zmax, na.rm = TRUE)
ht.sd   <- sd(df.raw$ht_zmax, na.rm = TRUE)

pred.ht$ht_raw <-
  pred.ht$ht_zmax * ht.sd + ht.mean

pred.ht$max_height <- pred.ht$ht_raw

# --- plot! ---

rug.df <- df.raw %>%
  filter(ht_zmax <= quantile(df.raw$ht_zmax, 0.99, na.rm = TRUE)) %>%
  group_by(burned) %>%
  slice_sample(n = 1000) %>%
  ungroup() %>%
  rename(max_height = ht_zmax)

ggplot(
  pred.ht,
  aes(x = max_height, y = fit, color = burned, fill = burned)
) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_rug(
    data = rug.df,
    aes(x = max_height, color = burned),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.15,
    linewidth = 0.2
  ) +
  scale_color_manual(values = c("unburned" = "lightblue",
                                "burned" = "orange")) +
  scale_fill_manual(values = c("unburned" = "gray60",
                               "burned" = "orange")) +
  theme_bw() +
  labs(
    x = "Maximum canopy height (m)",
    y = "Predicted peak SWE (m)",
    color = NULL,
    fill = NULL
  )

gap.diff <- pred.gap %>%
  select(gap_gap_pct, burned, fit) %>%
  pivot_wider(
    names_from = burned,
    values_from = fit
  ) %>%
  mutate(
    diff = burned - unburned
  )

ggplot(gap.diff, aes(x = gap_gap_pct, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  theme_bw() +
  labs(
    x = "Gap percentage (scaled)",
    y = "Predicted SWE difference\nburned - unburned"
  )



# ==============================================================================
#  combined prediction surface - gap pct and max ht
# ==============================================================================

pred.surface <- expand.grid(
  gap_gap_pct = gap.seq,
  ht_zmax = ht.seq,
  burned = "unburned"
)

# hold everything else constant
pred.surface$rad_dtm_accum <- 0
pred.surface$topo_slope <- 0
pred.surface$topo_tpi150 <- 0
pred.surface$topo_tpi2010 <- 0
pred.surface$topo_elev <- median(df.50$topo_elev, na.rm = TRUE)

# choose representative year 
pred.surface$wy <- factor(2023, levels = levels(df.50$wy))

# make sure burned matches model levels
pred.surface$burned <- factor(
  pred.surface$burned,
  levels = levels(df.50$burned)
)

# --- predict ---
predicted <- predict(
  gam.topo.canopy.best,
  newdata = pred.surface,
  se.fit = TRUE
)

# back-transform because model used sqrt(swe_peak)
pred.surface$fit <- predicted$fit^2
pred.surface$lwr <- (predicted$fit - 1.96 * predicted$se.fit)^2
pred.surface$upr <- (predicted$fit + 1.96 * predicted$se.fit)^2

# --- unscale ---
ht.mean <- mean(df.raw$ht_zmax, na.rm = TRUE)
ht.sd   <- sd(df.raw$ht_zmax, na.rm = TRUE)

pred.surface$max_height <- pred.surface$ht_zmax * ht.sd + ht.mean

gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd   <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

pred.surface$gap_percent <- pred.surface$gap_gap_pct * gap.sd + gap.mean
pred.surface$gap_percent <- pred.surface$gap_percent * 100

# --- plot! ---
ggplot(pred.surface, aes(x = gap_percent, y = max_height, fill = fit)) +
  geom_raster() +
  geom_contour(aes(z = fit), color = "white", alpha = 0.5) +
  scale_fill_viridis_c(name = "Predicted\npeak SWE (m)") +
  theme_bw() +
  labs(
    x = "Gap percentage",
    y = "Maximum canopy height (m)"
  )





# ==============================================================================
#  Aspect Dependent Plots
# ==============================================================================
# not finished
df.50 <- df.50 %>%
  mutate(
    aspect_class = case_when(
      topo_aspect >= 315 | topo_aspect < 45  ~ "north-facing",
      topo_aspect >= 135 & topo_aspect < 225 ~ "south-facing",
      TRUE ~ NA_character_
    ),
    aspect_class = factor(aspect_class, levels = c("north-facing", "south-facing"))
  )

gam.burn.aspect <- bam(
  sqrt(swe_peak) ~
    wy +
    burned * aspect_class +
    s(topo_elev) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  data = df.50 %>% filter(!is.na(aspect_class)),
  method = "fREML",
  discrete = TRUE
)



# ==============================================================================
#  Elevation Dependent Plots
# ==============================================================================
# different model for elevation that doesn't include canopy
# not done
gam.elev <- bam(
  sqrt(swe_peak) ~
    wy +
    burned +
    s(topo_elev) +
    s(topo_elev, by = burned) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  data = df.50,
  method = 'fREML',
  discrete = TRUE
)

summary(gam.elev)


# ----- faceted by year ----- 
elev.seq <- seq(
  quantile(df.50$topo_elev, 0.01, na.rm = TRUE),
  quantile(df.50$topo_elev, 0.99, na.rm = TRUE),
  length.out = 100
)

pred.elev <- expand.grid(
  topo_elev = elev.seq,
  burned = levels(df.50$burned),
  wy = levels(df.50$wy)
)

# hold everything else constant
pred.elev$rad_dtm_accum <- 0
pred.elev$topo_slope <- 0
pred.elev$topo_tpi150 <- 0
pred.elev$topo_tpi2010 <- 0

pred.elev$wy <- factor(pred.elev$wy, levels = levels(df.50$wy))

pred.elev$burned <- factor(
  pred.elev$burned,
  levels = levels(df.50$burned)
)

# --- predict ---
pred <- predict(
  gam.elev,
  newdata = pred.elev,
  se.fit = TRUE
)

pred.elev$fit <- pred$fit^2
pred.elev$lwr <- (pred$fit - 1.96 * pred$se.fit)^2
pred.elev$upr <- (pred$fit + 1.96 * pred$se.fit)^2

# --- unscale elevation ---
elev.mean <- mean(df.raw$topo_elev, na.rm = TRUE)
elev.sd   <- sd(df.raw$topo_elev, na.rm = TRUE)

pred.elev$elevation <- pred.elev$topo_elev * elev.sd + elev.mean

# --- rug data ---
rug.df <- df.raw %>%
  filter(topo_elev <= quantile(topo_elev, 0.99, na.rm = TRUE)) %>%
  group_by(burned) %>%
  slice_sample(n = 1000) %>%
  ungroup() %>%
  rename(elevation = topo_elev)

# --- plot ---
ggplot(
  pred.elev,
  aes(x = elevation, y = fit, color = burned, fill = burned)
) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ wy) +
  scale_color_manual(values = c("unburned" = "lightblue",
                                "burned" = "orange")) +
  scale_fill_manual(values = c("unburned" = "gray60",
                               "burned" = "orange")) +
  theme_bw() +
  labs(
    x = "Elevation (m)",
    y = "Predicted peak SWE (m)",
    color = NULL,
    fill = NULL
  )



swe.summary <- df.raw %>%
  group_by(wy, elev_band, burned) %>%
  summarise(
    mean_swe = mean(swe_peak, na.rm = TRUE),
    sd_swe = sd(swe_peak, na.rm = TRUE),
    n = n(),
    se_swe = sd_swe / sqrt(n),
    .groups = "drop"
  )


ggplot(
  swe.summary,
  aes(
    x = elev_band,
    y = mean_swe,
    color = burned,
    group = burned
  )
) +
  geom_point(
    position = position_dodge(width = 0.3),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_swe - 1.96 * se_swe,
      ymax = mean_swe + 1.96 * se_swe
    ),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  geom_line(
    position = position_dodge(width = 0.3)
  ) +
  facet_wrap(~ wy) +
  scale_color_manual(
    values = c(
      "unburned" = "lightblue",
      "burned" = "orange"
    )
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Observed peak SWE (m)",
    color = NULL
  )

burn.diff <- swe.summary %>%
  select(wy, elev_band, burned, mean_swe) %>%
  tidyr::pivot_wider(
    names_from = burned,
    values_from = mean_swe
  ) %>%
  mutate(
    burn_effect = burned - unburned
  )

ggplot(
  burn.diff,
  aes(
    x = elev_band,
    y = burn_effect,
    group = 1
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~ wy) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Burned - unburned SWE (m)"
  )


# ==============================================================================
#  simple SWE comparison b/w burned and unburned
# ==============================================================================
# model w/o canopy
gam.fire <- bam(
  sqrt(swe_peak) ~
    wy +
    burned +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  data = df.50,
  method = 'fREML',
  discrete = TRUE
)

summary(gam.fire)

pred.fire <- data.frame(
  burned = c("unburned", "burned"),
  topo_elev = 0,
  rad_dtm_accum = 0,
  topo_slope = 0,
  topo_tpi150 = 0,
  topo_tpi2010 = 0,
  wy = factor("2023", levels = levels(df.50$wy))
)

pred.fire$burned <- factor(
  pred.fire$burned,
  levels = levels(df.50$burned)
)

pred.sqrt <- predict(gam.fire, newdata = pred.fire)

# convert back to m
pred.swe <- pred.sqrt^2
pred.results <- pred.fire %>%
  mutate(
    pred_sqrt_swe = pred.sqrt,
    pred_swe = pred.swe
  )

pred.results$pred_swe[2] - pred.results$pred_swe[1]

ggplot(
  pred.results,
  aes(x = burned, y = pred_swe, fill = burned)
) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "unburned" = "lightblue",
      "burned" = "orange"
    )
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Predicted peak SWE (m)"
  )

# ==============================================================================
#  elevation/gap/burnedunburned/wy
# ==============================================================================
elev.lookup <- df.raw %>%
  group_by(elev_band) %>%
  summarise(
    elev_raw = median(topo_elev, na.rm = TRUE),
    .groups = 'drop'
  )

elev.mean <- mean(df.raw$topo_elev, na.rm = TRUE)
elev.sd   <- sd(df.raw$topo_elev, na.rm = TRUE)

elev.lookup$topo_elev <- (elev.lookup$elev_raw - elev.mean) / elev.sd

pred.elev.gap <- expand.grid(
  gap_gap_pct = gap.seq,
  elev_band = levels(df.raw$elev_band),
  burned = levels(df.50$burned),
  wy = levels(df.50$wy)
) %>%
  left_join(elev.lookup, by = 'elev_band')

# set other values constant 
pred.elev.gap$rad_dtm_accum <- 0
pred.elev.gap$topo_slope <- 0
pred.elev.gap$topo_tpi150 <- 0
pred.elev.gap$topo_tpi2010 <- 0
pred.elev.gap$ht_zmax <- median(df.50$ht_zmax)

pred.elev.gap$burned <- factor(
  pred.elev.gap$burned,
  levels = levels(df.50$burned)
)

pred.elev.gap$wy <- factor(
  pred.elev.gap$wy,
  levels = levels(df.50$wy)
)

# --- predict ---
p <- predict(
  gam.topo.canopy.best,
  newdata = pred.elev.gap,
  se.fit = TRUE
)

# back-transform because model used sqrt(swe_peak)
pred.elev.gap$fit <- p$fit^2
pred.elev.gap$lwr <- (p$fit - 1.96 * p$se.fit)^2
pred.elev.gap$upr <- (p$fit + 1.96 * p$se.fit)^2

# --- unscale gap ---
gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd   <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

pred.elev.gap$gap_raw <-
  pred.elev.gap$gap_gap_pct * gap.sd + gap.mean

# since raw gap is stored 0-1
pred.elev.gap$gap_percent <- pred.elev.gap$gap_raw * 100

# --- plot ---
# burn status as columns, elevation bands as lines
ggplot(
  pred.elev.gap,
  aes(
    x = gap_percent,
    y = fit,
    color = elev_band,
    fill = elev_band
  )
) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1.0) +
  facet_grid(wy ~ burned) +
  theme_bw() +
  labs(
    x = 'Gap percentage (%)',
    y = 'Predicted peak SWE (m)',
    color = 'Elevation band',
    fill = 'Elevation band'
  )

# exposure as columns, burn status as lines
ggplot(
  pred.elev.gap,
  aes(
    x = gap_percent,
    y = fit,
    color = burned
  )
) +
  geom_line(linewidth = 1.0) +
  facet_grid(wy ~ elev_band) +
  scale_color_manual(
    values = c(
      'unburned' = 'lightblue',
      'burned' = 'orange'
    )
  ) +
  theme_bw() +
  labs(
    x = 'Gap percentage (%)',
    y = 'Predicted peak SWE (m)',
    color = NULL
  )


# compute differences
burn.diff <- pred.elev.gap %>%
  select(
    wy,
    elev_band,
    gap_percent,
    burned,
    fit
  ) %>%
  pivot_wider(
    names_from = burned,
    values_from = fit
  ) %>%
  mutate(
    burn_effect = burned - unburned
  )


head(burn.diff)

ggplot(
  burn.diff,
  aes(
    x = gap_percent,
    y = burn_effect
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = 2,
    color = 'gray40'
  ) +
  geom_line(
    color = 'firebrick',
    linewidth = 1
  ) +
  facet_grid(wy ~ elev_band) +
  theme_bw() +
  labs(
    x = 'Gap percentage (%)',
    y = 'Burned - unburned SWE (m)'
  )

# ==============================================================================
#  aspect/gap/burnedunburned/wy
# ==============================================================================
solar.lookup <- data.frame(
  sun_class = c('Low sun exposure', 'High sun exposure'),
  rad_raw = c(
    quantile(df.raw$rad_dtm_accum, 0.25, na.rm = TRUE),
    quantile(df.raw$rad_dtm_accum, 0.75, na.rm = TRUE)
  )
)

rad.mean <- mean(df.raw$rad_dtm_accum, na.rm = TRUE)
rad.sd   <- sd(df.raw$rad_dtm_accum, na.rm = TRUE)

solar.lookup$rad_dtm_accum <-
  (solar.lookup$rad_raw - rad.mean) / rad.sd

# create prediction dataset
pred.sun.gap <- expand.grid(
  gap_gap_pct = gap.seq,
  sun_class = c('Low sun exposure',
                'High sun exposure'),
  burned = levels(df.50$burned),
  wy = levels(df.50$wy)
) %>%
  left_join(solar.lookup, by = 'sun_class')

# set other values constant 
pred.sun.gap$topo_elev <- median(df.50$topo_elev)
pred.sun.gap$topo_slope <- 0
pred.sun.gap$topo_tpi150 <- 0
pred.sun.gap$topo_tpi2010 <- 0
pred.sun.gap$ht_zmax <- median(df.50$ht_zmax)

pred.sun.gap$burned <- factor(
  pred.sun.gap$burned,
  levels = levels(df.50$burned)
)

pred.sun.gap$wy <- factor(
  pred.sun.gap$wy,
  levels = levels(df.50$wy)
)

# --- predict ---
p <- predict(
  gam.topo.canopy.best,
  newdata = pred.sun.gap,
  se.fit = TRUE
)

# back-transform because model used sqrt(swe_peak)
pred.sun.gap$fit <- p$fit^2
pred.sun.gap$lwr <- (p$fit - 1.96 * p$se.fit)^2
pred.sun.gap$upr <- (p$fit + 1.96 * p$se.fit)^2

# --- unscale gap ---
gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd   <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

pred.sun.gap$gap_raw <-
  pred.sun.gap$gap_gap_pct * gap.sd + gap.mean

# since raw gap is stored 0-1
pred.sun.gap$gap_percent <- pred.sun.gap$gap_raw * 100

# --- plot ---
# burn status as columns, sun exposure as lines
ggplot(
  pred.sun.gap,
  aes(
    x = gap_percent,
    y = fit,
    color = sun_class,
    fill = sun_class
  )
) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1.0) +
  facet_grid(wy ~ burned) +
  theme_bw() +
  labs(
    x = 'Gap percentage (%)',
    y = 'Predicted peak SWE (m)',
    color = NULL,
    fill = NULL
  )

# exposure as columns, burn status as lines
ggplot(
  pred.sun.gap,
  aes(
    x = gap_percent,
    y = fit,
    color = burned
  )
) +
  geom_line(linewidth = 1.0) +
  facet_grid(wy ~ sun_class) +
  scale_color_manual(
    values = c(
      'unburned' = 'lightblue',
      'burned' = 'orange'
    )
  ) +
  theme_bw() +
  labs(
    x = 'Gap percentage (%)',
    y = 'Predicted peak SWE (m)',
    color = NULL
  )

# ==============================================================================
#  model comparisons!
# ==============================================================================

# ------ model formulas -----
model.formulas <- list(
  
  topo =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  
  spatial =
    sqrt(swe_peak) ~
    wy +
    s(x, y, bs = 'tp', k = 200),
  
  cbi =
    sqrt(swe_peak) ~
    wy +
    cbibc
  
  cbi.smooth =
    sqrt(swe_peak) ~
    wy +
    s(cbibc)
  
  
  topo_burned =
    sqrt(swe_peak) ~
    wy +
    burned +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  
  topo_cbi =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(cbibc),
  
  topo_burned_cbi =
    sqrt(swe_peak) ~
    wy +
    burned +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(cbibc),
  
  topo_canopy =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  topo_canopy_burned =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  topo_canopy_cbi =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(cbibc) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  topo_canopy_burned_cbi =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(cbibc) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned)
)

# ----- model set looking at if adding more canopy variables is worth it -----
model.formulas.2 <- list(
  
  topo_canopy =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  topo_canopy.zpcum2 =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned),
  
  topo_canopy.zpcum2.groundfrac =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned),
  
  topo_canopy.zpcum2.groundfrac.distcanopy =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned),
  
  topo_canopy.zpcum2.groundfrac.distcanopy.zskew =
    sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(cover_ground_frac, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned) +
    s(ht_zskew, by = burned)
  
)

# ----- 5-fold cross validation -----
# run each model doing 5-fold cross validation
results <- list()

# define which set of models
model.formulas.set <- model.formula.2

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


library(forcats)
library(tidyverse)

plot.dat <- results %>%
  mutate(
    model_label = recode(
      model,
      topo = 'Topo',
      topo_burned = 'Topo + burn class',
      topo_cbi = 'Topo + CBI',
      topo_burned_cbi = 'Topo + burn + CBI',
      topo_canopy = 'Topo + canopy',
      topo_canopy_burned = 'Topo + canopy + burn',
      topo_canopy_cbi = 'Topo + canopy + CBI',
      topo_canopy_burned_cbi = 'Topo + canopy + burn + CBI'
    )
  )

sum.dat <- plot.dat %>%
  group_by(model_label) %>%
  summarise(
    r2_mean = mean(r2),
    r2_sd = sd(r2),
    r2_se = sd(r2) / sqrt(n()),
    rmse_mean = mean(rmse_orig),
    .groups = 'drop'
  ) %>%
  arrange(r2_mean) %>%
  mutate(model_label = factor(model_label, levels = model_label))

plot.dat <- plot.dat %>%
  mutate(model_label = factor(model_label, levels = levels(sum.dat$model_label)))

ggplot(sum.dat, aes(x = r2_mean, y = model_label)) +
  geom_errorbar(
    aes(xmin = r2_mean - r2_sd, xmax = r2_mean + r2_sd),
    width = 0.2
  ) +
  geom_point(size = 3) +
  geom_point(
    data = plot.dat,
    aes(x = r2, y = model_label),
    alpha = 0.35,
    size = 1.8,
    inherit.aes = FALSE
  ) +
  labs(
    x = 'Cross-validated R²',
    y = NULL,
    title = 'Canopy structure improves SWE prediction more than burn severity'
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 13, face = 'bold')
  )




# ----- violin plot -----
ggplot(df.raw,
       aes(x = burned, y = swe_peak, fill = burned)) +
  geom_violin(alpha = 0.7, trim = TRUE) +
  geom_boxplot(width = 0.15, alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~ elev_band) +
  scale_fill_manual(values = burn.cols) +
  coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  labs(
    x = NULL,
    y = 'Observed peak SWE (m)',
    fill = NULL
  )

df.raw %>%
  group_by(elev_band, burned) %>%
  summarize(
    mean_swe = mean(swe_peak),
    se = sd(swe_peak) / sqrt(n()),
    .groups = 'drop'
  )

swe.summary <- df.raw %>%
  group_by(elev_band, burned) %>%
  summarize(
    mean_swe = mean(swe_peak),
    se = sd(swe_peak) / sqrt(n()),
    .groups = 'drop'
  )

ggplot(
  swe.summary,
  aes(
    x = elev_band,
    y = mean_swe,
    color = burned
  )
) +
  geom_point(
    position = position_dodge(width = 0.3),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_swe - 1.96 * se,
      ymax = mean_swe + 1.96 * se
    ),
    position = position_dodge(width = 0.3),
    width = 0.15
  ) +
  scale_color_manual(values = burn.cols) +
  theme_bw() +
  labs(
    x = NULL,
    y = 'Observed peak SWE (m)',
    color = NULL
  )



# ==============================================================================
#  Scenario : how does increasing gap size affect swe between burned and unburned?
# ==============================================================================
# ----- gap bins from observed raw data -----
height.by.gap <- df.raw %>%
  filter(
    !is.na(gap_gap_pct),
    !is.na(ht_zmax),
    !is.na(burned)
  ) %>%
  mutate(
    gap_percent = gap_gap_pct * 100,
    gap_bin = cut(
      gap_percent,
      breaks = seq(0, 100, by = 5),
      include.lowest = TRUE
    )
  ) %>%
  group_by(burned, gap_bin) %>%
  summarize(
    gap_gap_pct_raw = mean(gap_gap_pct, na.rm = TRUE),
    gap_percent = mean(gap_percent, na.rm = TRUE),
    ht_zmax_raw = mean(ht_zmax, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  filter(n >= 20)

range(df.raw$gap_gap_pct, na.rm = TRUE)

# ----- scaling values from raw data -----
gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

ht.mean <- mean(df.raw$ht_zmax, na.rm = TRUE)
ht.sd <- sd(df.raw$ht_zmax, na.rm = TRUE)

# ----- prediction dataframe -----
pred.scenario <- height.by.gap %>%
  mutate(
    gap_gap_pct = (gap_gap_pct_raw - gap.mean) / gap.sd,
    ht_zmax = (ht_zmax_raw - ht.mean) / ht.sd,
    
    wy = factor('2023', levels = levels(df.50$wy)),
    
    topo_elev = 0,
    rad_dtm_accum = 0,
    topo_slope = 0,
    topo_tpi150 = 0,
    topo_tpi2010 = 0,
    
    burned = factor(burned, levels = levels(df.50$burned))
  )

pred.scenario$pred_sqrt_swe <- predict(
  best.model.swe,
  newdata = pred.scenario,
  type = 'response'
)

pred.scenario <- pred.scenario %>%
  mutate(
    pred_swe = pred_sqrt_swe^2
  )

ggplot(
  pred.scenario,
  aes(x = gap_percent, y = pred_swe, color = burned, group = burned)
) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(size = n), alpha = 0.7) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Gap (%)',
    y = 'SWE (m)',
    color = NULL,
    size = 'n'
  ) +
  theme_bw()



cor(df.raw$cbibc, df.raw$ht_zmax)
