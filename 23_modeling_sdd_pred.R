fire <- 'creek'

dir <- paste0('data/processed/processed/rds/', fire)
df.500.0 <- readRDS(file.path(paste0(dir, '/', fire, '_df_500m.rds')))
df.raw.0 <- readRDS(file.path(paste0(dir, '/', fire, '_long_df_500m_raw.rds')))

# trim to years 2023 and up
df.raw <- df.raw.0 %>%
  filter(wy %in% c(2023, 2024, 2025))


df.500 <- df.500.0 %>%
  semi_join(
    df.raw %>% select(cell, wy),
    by = c('cell', 'wy')
  )

# best model
best.model.sdd <- bam(
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

burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)

# ==============================================================================
#  gap pct * at typical observed canopy height at that gap percentage
# ==============================================================================
# ----- for 2023 -----
# --- Create prediction grid ---

# Sequence of gap values spanning the observed range
gap.seq <- seq(
  min(df.500$gap_gap_pct, na.rm = TRUE),
  max(df.500$gap_gap_pct, na.rm = TRUE),
  length.out = 100
)

# Create all combinations of:
# - gap percentage
# - burn status
predict.df <- expand.grid(
  gap_gap_pct = gap.seq,
  burned = levels(df.500$burned)
)

# --- Convert scaled gap values back to raw values ---

# Mean and SD from original (unscaled) dataframe
gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

# Convert scaled gap values back to original units
predict.df$gap_raw <- predict.df$gap_gap_pct * gap.sd + gap.mean

# --- predict at realistic heights ---
# can't just hold ht at mean because that becomes unrealistic at higher gap pcts  

# Get typical canopy height for each gap bin and burn status
ht.lookup <- df.raw %>%
  mutate(gap.bin = ntile(gap_gap_pct, 100)) %>%
  group_by(gap.bin, burned) %>%
  summarize(
    gap_lookup = mean(gap_gap_pct, na.rm = TRUE),
    ht_lookup = mean(ht_zmax, na.rm = TRUE),
    .groups = 'drop'
  )

# Smooth/interpolate height along the observed gap gradient
predict.df <- predict.df %>%
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

# Convert realistic raw height into scaled model units
predict.df$ht_zmax <- (predict.df$ht_raw - mean(df.raw$ht_zmax, na.rm = TRUE)) /
  sd(df.raw$ht_zmax, na.rm = TRUE)

# --- hold all other predictors constant --- 

# Set continuous predictors to their centered value (0)
predict.df$rad_dtm_accum <- 0
predict.df$rad_dtm_melt <- 0
predict.df$topo_slope <- 0
predict.df$topo_tpi1200 <- 0
predict.df$topo_tpi2010 <- 0
predict.df$topo_elev <- median(df.500$topo_elev) # Hold elevation at the median value


# Hold water year constant at 2023
predict.df$wy <- factor(
  '2023',
  levels = levels(df.500$wy)
)

# Make sure factors match model factor levels
predict.df$burned <- factor(predict.df$burned, levels = levels(df.500$burned))

# --- predict SDD from the GAM ---
pred <- predict(
  best.model.sdd,
  newdata = predict.df,
  se.fit = TRUE)

# Add predictions and uncertainty to dataframe
predict.df$fit <- pred$fit
predict.df$se <- pred$se.fit

# Calculate 95% confidence intervals
predict.df$lwr <- predict.df$fit - 1.96 * predict.df$se
predict.df$upr <- predict.df$fit + 1.96 * predict.df$se

# convert fraction to precent
predict.df$gap_percent <- predict.df$gap_raw * 100

# --- plot ---

ggplot(
  predict.df,
  aes(
    x = gap_percent,
    y = fit,
    color = burned,
    fill = burned
  )
) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = burn.cols) +
  scale_fill_manual(values = burn.cols) +
  theme_bw() +
  labs(
    x = 'Gap percentage',
    y = 'Predicted snow disappearance date (DOY)',
    color = NULL,
    fill = NULL,
    title = 'Water Year 2023'
  )


# ==============================================================================
#  max height 
# ==============================================================================
# ----- partial dependence plot - 2023 -----
# create gap sequence
# trim to where burned vlass actually has data
ht.seq <- seq(
  quantile(df.500$ht_zmax, 0.025, na.rm = TRUE),
  quantile(df.500$ht_zmax, 0.975, na.rm = TRUE),
  length.out = 100
)

# build predication dataframe
predict.df <- expand.grid(
  ht_zmax = ht.seq,
  burned = levels(df.500$burned)
)

# hold everything else constant
predict.df$rad_dtm_accum <- 0
predict.df$rad_dtm_melt <- 0
predict.df$topo_slope <- 0
predict.df$topo_tpi1200 <- 0
predict.df$topo_tpi2010 <- 0
predict.df$gap_gap_pct <- 0

predict.df$topo_elev <- median(df.500$topo_elev)

# choose representative year 
predict.df$wy <- factor(2023, levels = levels(df.500$wy))

# make sure burned matches the model levels
predict.df$burned <- factor(predict.df$burned, levels = levels(df.500$burned))

# --- predict ---
pred <- predict(
  sdd.best,
  newdata = predict.df,
  se.fit = TRUE
)

# Add predictions and uncertainty to dataframe
predict.df$fit <- pred$fit
predict.df$se <- pred$se.fit

# Calculate 95% confidence intervals
predict.df$lwr <- predict.df$fit - 1.96 * predict.df$se
predict.df$upr <- predict.df$fit + 1.96 * predict.df$se

# --- unscale ---
var.mean <- mean(df.raw$ht_zmax, na.rm = TRUE)
var.sd   <- sd(df.raw$ht_zmax, na.rm = TRUE)

predict.df$ht_raw <- predict.df$ht_zmax * var.sd + var.mean


# --- plot! ---
ggplot(
  predict.df,
  aes(x = ht_raw, y = fit, color = burned, fill = burned)
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
    x = 'Maximum canopy height (m)',
    y = 'Predicted snow disappearance date (DOY)',
    color = NULL,
    fill = NULL,
    title = 'Water Year 2023'
  )

# ==============================================================================
#  Scenario : how does increasing gap size affect swe between burned and unburned?
# ==============================================================================
# --- gap bins from observed raw data ---
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

# --- scaling values from raw data ---
gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

ht.mean <- mean(df.raw$ht_zmax, na.rm = TRUE)
ht.sd <- sd(df.raw$ht_zmax, na.rm = TRUE)

# --- prediction dataframe ---
pred.scenario.sdd <- height.by.gap %>%
  mutate(
    gap_gap_pct = (gap_gap_pct_raw - gap.mean) / gap.sd,
    ht_zmax = (ht_zmax_raw - ht.mean) / ht.sd,
    
    wy = factor('2023', levels = levels(df.500$wy)),
    
    topo_elev = 0,
    rad_dtm_accum = 0,
    rad_dtm_melt = 0,
    topo_slope = 0,
    topo_tpi2010 = 0,
    topo_tpi1200 = 0,
    
    burned = factor(burned, levels = levels(df.500$burned))
  )

# --- predict SDD ---
pred.scenario.sdd$pred_sdd <- predict(
  best.model.sdd,
  newdata = pred.scenario.sdd,
  type = 'response'
)

# --- plot ---
ggplot(
  pred.scenario.sdd,
  aes(x = gap_percent, y = pred_sdd, color = burned)
) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(size = n), alpha = 0.7) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Gap (%)',
    y = 'Predicted snow disappearance date (DOY)',
    color = NULL,
    size = 'n'
  ) +
  theme_bw()

# ==============================================================================
#  Scenario : Across the observed range of topography and forest structure, how much later would snow disappear if every location were unburned versus burned
# ==============================================================================
# create 2 copies of data
dat.unburned <- df.500
dat.burned <- df.500

dat.unburned$burned <- 'unburned'
dat.burned$burned <- 'burned'

dat.unburned$burned <- factor(
  dat.unburned$burned,
  levels = levels(df.500$burned)
)

dat.burned$burned <- factor(
  dat.burned$burned,
  levels = levels(df.500$burned)
)

# predict both
pred.unburned <- predict(
  best.model.sdd,
  newdata = dat.unburned,
  type = 'response'
)

pred.burned <- predict(
  best.model.sdd,
  newdata = dat.burned,
  type = 'response'
)

mean(pred.unburned - pred.burned)
quantile(
  pred.unburned - pred.burned,
  probs = c(.05, .25, .5, .75, .95)
)

