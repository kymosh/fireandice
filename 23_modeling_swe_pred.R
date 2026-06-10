library(mgcv)
library(plotmyGAM)

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

# hold everything else constant
pred.gap$rad_dtm_accum <- 0
pred.gap$topo_slope <- 0
pred.gap$topo_tpi150 <- 0
pred.gap$topo_tpi2010 <- 0
pred.gap$ht_zmax <- 0

pred.gap$topo_elev <- median(df.50$topo_elev)

# choose representative year 
pred.gap$wy <- factor(2023, levels = levels(df.50$wy))

# make sure burned matches the model levels
pred.gap$burned <- factor(pred.gap$burned, levels = levels(df.50$burned))

# ----- predict -----
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


# ----- unscale -----
gap.mean <- mean(df.raw$gap_gap_pct, na.rm = TRUE)
gap.sd   <- sd(df.raw$gap_gap_pct, na.rm = TRUE)

pred.gap$gap_raw <-
  pred.gap$gap_gap_pct * gap.sd + gap.mean

pred.gap$gap_percent <- pred.gap$gap_raw * 100

# ----- plot! -----
library(ggplot2)

ggplot(
  pred.gap,
  aes(x = gap_percent, y = fit, color = burned, fill = burned)
) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("unburned" = "lightblue", "burned" = "orange")) +
  scale_fill_manual(values = c("unburned" = "gray60", "burned" = "orange")) +
  theme_bw() +
  labs(
    x = "Gap percentage",
    y = "Predicted peak SWE",
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
#  Create prediction dataset for ht_zmax
# ==============================================================================

# create gap sequence
ht.seq <- seq(
  min(df.50$ht_zmax),
  max(df.50$ht_zmax),
  length.out = 100
)

# build predication dataframe
pred.gap <- expand.grid(
  ht_zmax = ht.seq,
  burned = levels(df.50$burned)
)

# hold everything else constant
pred.gap$rad_dtm_accum <- 0
pred.gap$topo_slope <- 0
pred.gap$topo_tpi150 <- 0
pred.gap$topo_tpi2010 <- 0
pred.gap$gap_gap_pct <- 0

pred.gap$topo_elev <- median(df.50$topo_elev)

# choose representative year 
pred.gap$wy <- factor(2023, levels = levels(df.50$wy))

# make sure burned matches the model levels
pred.gap$burned <- factor(pred.gap$burned, levels = levels(df.50$burned))

# ----- predict -----
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


# ----- unscale -----
ht.mean <- mean(df.raw$ht_zmax, na.rm = TRUE)
ht.sd   <- sd(df.raw$ht_zmax, na.rm = TRUE)

pred.gap$ht_raw <-
  pred.gap$ht_zmax * ht.sd + ht.mean

pred.gap$max_height <- pred.gap$ht_raw * 100

# ----- plot! -----
library(ggplot2)

ggplot(
  pred.gap,
  aes(x = max_height, y = fit, color = burned, fill = burned)
) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("unburned" = "lightblue", "burned" = "orange")) +
  scale_fill_manual(values = c("unburned" = "gray60", "burned" = "orange")) +
  theme_bw() +
  labs(
    x = "Max Height",
    y = "Predicted peak SWE",
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
