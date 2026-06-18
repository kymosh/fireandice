packages <- c('tidyr', 'mgcv', 'purrr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

set.seed(14)

# ----- initialize dataset -----
fire <- 'creek'

dir <- paste0('data/processed/processed/rds/', fire) 

# df.50 as scaled values
df.50 <- readRDS(file.path(dir, paste0(fire, '_df_50m.rds')))
df.50$wy <- factor(df.50$wy)

# df.raw has unscaled values
df.raw <- readRDS(file.path(dir, paste0(fire, '_long_df_50m_raw.rds')))
df.raw$wy <- factor(df.raw$wy)

# add aspect class
df.raw <- df.raw %>%
  mutate(
    aspect_class = case_when(
      topo_aspect_cos > 0.5  ~ "North-facing",
      topo_aspect_cos < -0.5 ~ "South-facing",
      TRUE                   ~ NA_character_
    )
  )

# add elev bands
df.raw <- df.raw %>%
  mutate(
    elev_band = case_when(
      topo_elev < 2000 ~ "Low (<2000 m)",
      topo_elev < 2600 ~ "Mid (2000-2600 m)",
      TRUE             ~ "High (>2600 m)"
    ),
    elev_band = factor(
      elev_band,
      levels = c(
        "Low (<2000 m)",
        "Mid (2000-2600 m)",
        "High (>2600 m)"
      )
    )
  )

burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)



gam.topo.canopy.best <- bam(
  sqrt(swe_peak) ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  data = df.50,
  method = "fREML",
  discrete = TRUE
)

# whole dataset
df.test <- df.50

# if sampling
df.check <- df.test |>
  dplyr::slice_sample(n = 100000)

# best model
gam.topo.canopy.best <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned)
  ,
  data = df.50, # select what dataset to use, sample or not
  method = "fREML",
  discrete = TRUE
)
# ==============================================================================
#  Create prediction dataset for gap pct
# ==============================================================================

# ----- this is just for 2023! -----
# create gap sequence
gap.seq <- seq(
  min(df.50$gap_gap_pct),
  max(df.50$gap_gap_pct),
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
  gam.topo.canopy.best,
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
    burned +
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
    burned +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010) +
    s(cbibc) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned)
)

# run each model doing 5-fold cross validation
results <- list()

for (fold in 1:5) {
  
  train <- filter(df.50, fold_id != fold)
  test  <- filter(df.50, fold_id == fold)
  
  for (m in names(model.formulas)) {
    
    fit <- bam(
      model.formulas[[m]],
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

