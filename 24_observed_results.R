# ==============================================================================
#  SDD
# ==============================================================================

# ----- SDD - initialize dataframe -----
fire <- 'creek'

dir <- paste0('data/processed/processed/rds/', fire)

# pick one
df.500 <- readRDS(file.path(paste0(dir, '/', fire, '_df_500m.rds')))
df.raw.0 <- readRDS(file.path(paste0(dir, '/', fire, '_long_df_500m_raw.rds')))
   
df.raw <- df.raw.0 %>% 
  mutate(gap_gap_pct = gap_gap_pct * 100)

# quick check to make sure there are no NAs
#colSums(is.na(df.raw))


# ----- SWE - initialize df -----

dir <- 'data/processed/processed/rds/creek' 
df.50 <- readRDS(file.path(dir, 'creek_df_50m.rds'))
df.50$wy <- factor(df.50$wy)
fire <- 'creek'
set.seed(14)

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

# add aspect class
df.50$aspect_class <- df.raw$aspect_class

# add elevation band
df.50$elev_band <- df.raw$elev_band

# ----- explore -----

burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)

# ----- plot that shows raw SDD vs canopy gap, for both burned and unburned -----
df.plot <- df.raw %>%
  mutate(
    gap.bin = ntile(gap_gap_pct, 20)
  ) %>%
  group_by(gap.bin, burned) %>%
  summarize(
    gap = mean(gap_gap_pct),
    sdd = mean(sdd),
    se = sd(sdd) / sqrt(n()),
    .groups = 'drop'
  )

ggplot(df.plot,
       aes(gap, sdd, color = burned, group = burned)) +
  geom_ribbon(
    aes(
      ymin = sdd - 1.96 * se,
      ymax = sdd + 1.96 * se,
      fill = burned
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = burn.cols) +
  scale_fill_manual(values = burn.cols) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Snow disappearance date (DOY)',
    color = NULL,
    fill = NULL
  ) +
  theme_bw()


# ----- Violin plot just of burned vs unburned SDD -----
ggplot(df.raw,
       aes(burned, sdd,
           fill = burned)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.15,
               alpha = 0.7) +
  scale_fill_manual(values = burn.cols) +
  labs(
    x = NULL,
    y = 'Snow disappearance date (DOY)'
  ) +
  theme_bw()

# faceted by elevation
ggplot(
  df.raw,
  aes(
    burned,
    sdd,
    fill = burned
  )
) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(
    width = 0.15,
    alpha = 0.7
  ) +
  facet_wrap(
    ~ elev_band,
    nrow = 1
  ) +
  scale_fill_manual(values = burn.cols) +
  labs(
    x = NULL,
    y = 'Snow disappearance date (DOY)'
  ) +
  theme_bw()

# SDD vs Elevation
ggplot(df.raw,
       aes(topo_elev, sdd,
           color = burned)) +
  geom_smooth(se = TRUE) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Elevation (m)',
    y = 'Snow disappearance date (DOY)'
  ) +
  theme_bw()

# ----- SDD vs Canopy height -----
df.ht <- df.raw %>%
  mutate(
    ht.bin = ntile(ht_zmax, 20)
  ) %>%
  group_by(ht.bin, burned) %>%
  summarize(
    ht = mean(ht_zmax),
    sdd = mean(sdd),
    .groups = 'drop'
  )

ggplot(df.ht,
       aes(ht, sdd,
           color = burned)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Snow disappearance date (DOY)'
  ) +
  theme_bw()

# ----- Gap effect by elevation -----
df.gap.elev <- df.raw %>%
  mutate(
    gap.bin = ntile(gap_gap_pct, 20)
  ) %>%
  group_by(gap.bin, elev_band, burned) %>%
  summarize(
    gap = mean(gap_gap_pct),
    sdd = mean(sdd),
    .groups = 'drop'
  )

ggplot(df.gap.elev,
       aes(gap, sdd,
           color = burned)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Snow disappearance date (DOY)'
  ) +
  facet_wrap(~elev_band) +
  theme_bw()

# -----Gap aeffect by aspect - doesn't really show much -----
df.gap.aspect <- df.raw %>%
  mutate(
    gap.bin = ntile(gap_gap_pct, 20)
  ) %>%
  group_by(gap.bin, aspect_class, burned) %>%
  summarize(
    gap = mean(gap_gap_pct),
    sdd = mean(sdd),
    .groups = 'drop'
  )

ggplot(df.gap.aspect,
       aes(gap, sdd,
           color = burned)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Snow disappearance date (DOY)'
  ) +
  facet_wrap(~aspect_class) +
  theme_bw()

# ----- zmax vs gap pct -----
ggplot(df.raw, aes(gap_gap_pct, ht_zmax, color = burned)) +
  geom_point(alpha = 0.02) +
  scale_color_manual(values = burn.cols) +
  theme_bw()

# ----- binned gap pct -----
df.gap <- df.raw %>%
  mutate(
    gap.bin = ntile(gap_gap_pct, 20)
  ) %>%
  group_by(gap.bin, burned) %>%
  summarize(
    gap = mean(gap_gap_pct),
    sdd = mean(sdd),
    n = n(),
    .groups = 'drop'
  )

ggplot(df.gap,
       aes(gap, sdd,
           color = burned)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = burn.cols) +
  theme_bw()

cor(
  df.raw$gap_gap_pct,
  df.raw$ht_zmax,
  use = 'complete.obs'
)

ggplot(df.raw,
       aes(ht_zmax, sdd,
           color = burned)) +
  geom_smooth()

# ----- max height -----
df.ht <- df.raw %>%
  mutate(
    ht.bin = ntile(ht_zmax, 20)
  ) %>%
  group_by(ht.bin, burned) %>%
  summarize(
    ht = mean(ht_zmax),
    sdd = mean(sdd),
    n = n(),
    .groups = 'drop'
  )

ggplot(
  df.ht,
  aes(ht, sdd, color = burned)
) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_color_manual(values = burn.cols) +
  theme_bw()

# ----- statistics -----
df.raw %>%
  group_by(burned) %>%
  summarize(
    min_ht = min(ht_zmax),
    q05 = quantile(ht_zmax, .05),
    median_ht = median(ht_zmax),
    q95 = quantile(ht_zmax, .95),
    max_ht = max(ht_zmax)
  )

df.raw %>%
  group_by(elev_band, burned) %>%
  summarize(
    mean_sdd = mean(sdd)
  )

# ----- x -----
df.plot <- df.raw %>%
  mutate(
    gap.bin = ntile(gap_gap_pct, 20)
  ) %>%
  group_by(gap.bin, elev_band, burned) %>%
  summarize(
    gap = mean(gap_gap_pct),
    sdd = mean(sdd),
    .groups = 'drop'
  )

ggplot(
  df.plot,
  aes(gap, sdd, color = burned)
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ elev_band) +
  scale_color_manual(values = burn.cols) +
  theme_bw()

# ----- distribution plots faceted by variable -----
library(tidyverse)

# distribution plot of gap% and maxht between burned and unburned
plot.df <- df.raw %>%
  select(
    burned,
    gap_gap_pct,
    ht_zmax
  ) %>%
  pivot_longer(
    cols = c(gap_gap_pct, ht_zmax),
    names_to = 'variable',
    values_to = 'value'
  ) %>%
  mutate(
    variable = recode(
      variable,
      gap_gap_pct = 'Gap (%)',
      ht_zmax = 'Maximum canopy height (m)'
    ),
    burned = factor(
      burned,
      levels = c('unburned', 'burned')
    )
  )

# distribution of elevation and radiation
plot.df <- df.raw %>%
  select(
    burned,
    topo_elev,
    rad_dtm_accum
  ) %>%
  pivot_longer(
    cols = c(topo_elev, rad_dtm_accum),
    names_to = 'variable',
    values_to = 'value'
  ) %>%
  mutate(
    variable = recode(
      variable,
      topo_elev = 'Elevation (m)',
      ht_zmax = 'Radiation Value'
    ),
    burned = factor(
      burned,
      levels = c('unburned', 'burned')
    )
  )

# plug in either of above
# distribution plot
ggplot(
  plot.df,
  aes(
    x = value,
    fill = burned,
    color = burned
  )
) +
  geom_density(
    alpha = 0.5,
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ variable,
    scales = 'free_x',
    nrow = 1
  ) +
  scale_fill_manual(values = burn.cols) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = NULL,
    y = 'Density',
    fill = NULL,
    color = NULL
  ) +
  theme_bw()

# ----- gap and ht distributions across CBI -----
# --- gap ---

df.plot <- df.raw %>%
  mutate(
    cbi.class = cut(
      cbibc,
      breaks = c(-Inf, 0.1, 1.25, 2.25, Inf),
      labels = c(
        'Unburned',
        'Low',
        'Moderate',
        'High'
      )
    )
  )

ggplot(
  df.plot,
  aes(
    x = gap_gap_pct,
    fill = cbi.class
  )
) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c(
      'Unburned' = 'grey60',
      'Low' = '#7CAE00',
      'Moderate' = '#00BFC4',
      'High' = '#C77CFF'
    )
  ) +
  theme_bw() +
  labs(
    x = 'Gap (%)',
    y = 'Density',
    fill = 'Fire Severity'
  )

# --- ht ---
ggplot(
  df.plot,
  aes(
    x = ht_zmax,
    fill = cbi.class
  )
) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c(
      'Unburned' = 'grey60',
      'Low' = '#7CAE00',
      'Moderate' = '#00BFC4',
      'High' = '#C77CFF'
    )
  ) +
  theme_bw() +
  labs(
    x = 'Maximum Height',
    y = 'Density',
    fill = 'Fire Severity'
  )



# ----- observed response plot (similar to pdp) -----
plot.df <- df.500 %>%
  filter(wy == 2023) %>%
  mutate(
    gap_percent = gap_gap_pct * 100
  )

ggplot(
  plot.df,
  aes(
    x = gap_percent,
    y = sdd,
    color = burned,
    fill = burned
  )
) +
  geom_smooth(
    method = 'gam',
    formula = y ~ s(x, k = 6),
    linewidth = 1.2,
    alpha = 0.2
  ) +
  facet_wrap(
    ~ elev_band,
    nrow = 1
  ) +
  scale_color_manual(values = burn.cols) +
  scale_fill_manual(values = burn.cols) +
  theme_bw() +
  labs(
    x = 'Gap (%)',
    y = 'SDD',
    color = NULL,
    fill = NULL
  )

# SWE
df.50 %>%
  count(elev_band)

plot.df <- df.50 %>%
  filter(wy == 2023) %>%
  mutate(
    gap_percent = gap_gap_pct * 100
  ) %>%
  filter(
    !is.na(swe_peak),
    !is.na(gap_percent),
    !is.na(elev_band),
    !is.na(burned)
  )

ggplot(
  plot.df,
  aes(
    x = gap_percent,
    y = swe_peak,
    color = burned,
    fill = burned
  )
) +
  geom_smooth(
    method = 'gam',
    formula = y ~ s(x, k = 6),
    linewidth = 1.2,
    alpha = 0.2
  ) +
  facet_wrap(
    ~ elev_band,
    nrow = 1
  ) +
  scale_color_manual(values = burn.cols) +
  scale_fill_manual(values = burn.cols) +
  theme_bw() +
  labs(
    x = 'Gap (%)',
    y = 'Peak SWE',
    color = NULL,
    fill = NULL
  )


# troubleshooting
df.plot %>%
  group_by(cbi.class) %>%
  summarize(
    zmean = median(ht_zmean, na.rm = TRUE),
    zsd = median(ht_zsd, na.rm = TRUE),
    zq95 = median(ht_zq95, na.rm = TRUE),
    zpcum9 = median(ht_zpcum9, na.rm = TRUE),
    zmax = median(ht_zmax, na.rm = TRUE)
  )


df.plot %>%
  group_by(cbi.class) %>%
  summarize(
    elev = mean(topo_elev, na.rm = TRUE)
  )

ggplot(
  df.plot,
  aes(topo_elev, ht_zmax)
) +
  geom_point(alpha = 0.01)

ggplot(
  df.plot,
  aes(ht_zmax, fill = cbi.class)
) +
  geom_density(alpha = 0.4) +
  facet_wrap(~elev_band)

summary(df.raw$ht_zmax)

quantile(
  df.raw$ht_zmax,
  probs = c(0.95, 0.99, 0.999, 0.9999, 1),
  na.rm = TRUE
)

df.raw %>%
  filter(ht_zmax > 75) %>%
  select(ht_zmax, x, y)

ggplot(df.plot) +
  geom_point(
    aes(x, y, color = ht_zmax),
    size = 0.1
  )
