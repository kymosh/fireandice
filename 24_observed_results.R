# ==============================================================================
#  SDD
# ==============================================================================

# ----- initialize dataframe -----
fire <- 'creek'

dir <- paste0('data/processed/processed/rds/', fire)
df.500 <- readRDS(file.path(paste0(dir, '/', fire, '_df_500m.rds')))
df.raw.0 <- readRDS(file.path(paste0(dir, '/', fire, '_long_df_500m_raw.rds')))

   
df.raw <- df.raw.0 %>% 
  mutate(gap_gap_pct = gap_gap_pct * 100)

# quick check to make sure there are no NAs
colSums(is.na(df.raw))


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
