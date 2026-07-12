packages <- c('tidymodels', 'tidyverse', 'lme4', 'lmtest', 'ranger', 'tictoc', 'mgcv')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize
# ==============================================================================
set.seed(14)
fire <- 'creek'
res <- 500

# make sure if not on processing computer that the rds is updated!
dir <- 'data/processed/processed/rds/creek' 
df.0 <- readRDS(file.path(paste0(dir, '/creek_df_', res, 'm.rds')))
# df.raw has unscaled values
df.raw.0 <- readRDS(file.path(dir, paste0(fire, '_long_df_', res, 'm_raw.rds')))

burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)

results <- data.frame()

get.metrics <- function(model, name) {
  
  s <- summary(model)
  
  data.frame(
    model = name,
    r.squared = s$r.sq,
    dev.expl = s$dev.expl,
    AIC = AIC(model),
    edf = sum(s$edf)
  )
}

# trim to mid elevations
df.raw <- df.raw.0 %>%
  filter(topo_elev > 2300, topo_elev < 2550) %>%
  filter(wy %in% c(2023, 2024, 2025))
  
        
df <- df.0 %>%
  semi_join(
    df.raw %>% select(cell, wy),
    by = c('cell', 'wy')
  )



# ==============================================================================
# observed plots
# ==============================================================================
# ----- distribution plot of gap% and maxht between burned and unburned -----
plot.df <- df.raw %>%
  select(
    burned,
    gap_percent,
    ht_zmax
  ) %>%
  pivot_longer(
    cols = c(gap_percent, ht_zmax),
    names_to = 'variable',
    values_to = 'value'
  ) %>%
  mutate(
    variable = recode(
      variable,
      gap_percent = 'Gap (%)',
      ht_zmax = 'Maximum canopy height (m)'
    ),
    burned = factor(
      burned,
      levels = c('unburned', 'burned')
    )
  )

# distribution of elevation and radiation
plot.df <- df.raw %>%
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

# ----- comparing elev plots before and after trim -----
library(patchwork)

p1 <- ggplot(
  df.raw.0,
  aes(
    topo_elev,
    fill = burned,
    color = burned
  )
) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = burn.cols) +
  scale_color_manual(values = burn.cols) +
  labs(
    title = 'Untrimmed Dataset',
    x = 'Elevation (m)',
    y = 'Density'
  ) +
  theme_bw() +
  theme(
    legend.position = 'none'
  )

p2 <- ggplot(
  df.raw,
  aes(
    topo_elev,
    fill = burned,
    color = burned
  )
) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = burn.cols) +
  scale_color_manual(values = burn.cols) +
  labs(
    title = 'Trimmed Dataset',
    x = 'Elevation (m)',
    y = 'Density'
  ) +
  theme_bw()

p1 + p2 +
  plot_layout(guides = 'collect') &
  theme(
    legend.position = 'right'
  )


# ----- cbi and elev distributions and relationship -----
library(ggplot2)
library(ggExtra)

p <- ggplot(
  df.raw,
  aes(x = topo_elev, y = cbibc, color = burned)
) +
  geom_point(alpha = 0.35, size = 0.8) +
  geom_smooth(method = 'gam', formula = y ~ s(x), se = TRUE) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Elevation (m)',
    y = 'CBI',
    color = NULL
  ) +
  theme_bw()

ggMarginal(
  p,
  type = 'density',
  margins = 'both',
  groupColour = TRUE,
  groupFill = FALSE
)
# ==============================================================================
# SWE modeling
# ==============================================================================
# ----- base models -----  

# --- canopy ---
canopy <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(cover_ground_frac) +
    s(gap_dist_to_canopy_mean) +
    s(ht_zskew),
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy, "Canopy")
)

# --- cbi ---
cbi <- bam(
  sqrt(swe_peak) ~
    wy +
    s(cbibc),
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(cbi, "cbi")
)

# --- burn status ---
burned <- bam(
  sqrt(swe_peak) ~
    wy +
    burned,
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(burned, "burn status")
)

# --- topo ---
topo <- bam(
  sqrt(swe_peak) ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(topo_slope) +
    s(topo_tpi150) +
    s(topo_tpi2010),
  data = df,
  method = "fREML",
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(topo, "topo")
)

# --- spatial smooth ---
spatial <- bam(
  sqrt(swe_peak) ~ factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(spatial, "spatial")
)

# --- wy only ---

wy <- bam(
  sqrt(swe_peak) ~ wy,
  data = df,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(wy, "wy")
)

# --- canopy + severity ---
canopy.cbi <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(cover_ground_frac) +
    s(gap_dist_to_canopy_mean) +
    s(cbibc) +
    s(ht_zskew),
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy.cbi, "Canopy and Severity")
)


results 

single.model.results.50m <- results


# ----- violin plot *yucky* -----
ggplot(df.raw,
       aes(burned, swe_peak,
           fill = burned)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.15,
               alpha = 0.7) +
  scale_fill_manual(values = burn.cols) +
  labs(
    x = NULL,
    y = 'Peak SWE (m)'
  ) +
  theme_bw()

# ==============================================================================
# SDD modeling
# ==============================================================================
# ----- base models -----
# --- canopy ---
canopy <- bam(
  sdd ~
    wy +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(ht_zpcum1),
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy, "canopy")
)

# --- rad.dsm ---
rad.dsm <- bam(
  sdd ~
    wy +
    s(rad_dsm_accum) +
    s(rad_dsm_melt),
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(rad.dsm, "rad.dsm")
)

# --- cbi ---
cbi <- bam(
  sdd ~
    wy +
    s(cbibc),
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(cbi, "cbi")
)

# --- burn status ---
burned <- bam(
  sdd ~
    wy +
    burned,
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(burned, "burn status")
)

# --- topo ---
topo <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010),
  data = df,
  method = "fREML",
  discrete = TRUE
)
results <- rbind(
  results,
  get.metrics(topo, "topo")
)

# --- canopy + severity ---
canopy.cbi <- bam(
  sdd ~
    wy +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(cbibc) +
    s(ht_zpcum1),
  data = df,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy.cbi, "canopy + severity")
)


# --- spatial smooth ---
spatial <- bam(
  sdd ~ wy +
    s(x, y, bs = 'tp', k = 200),
  data = df,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(spatial, "spatial")
)

# --- wy only ---

wy <- bam(
  sdd ~ wy,
  data = df,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(wy, "wy")
)

results 

single.model.results.sdd <- results

# ----- violin plot -----
ggplot(df.raw,
       aes(burned, sdd,
           fill = burned)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.15,
               alpha = 0.7) +
  scale_fill_manual(values = burn.cols) +
  labs(
    x = NULL,
    y = 'Snow Disappearance Date (DOY)'
  ) +
  theme_bw()
