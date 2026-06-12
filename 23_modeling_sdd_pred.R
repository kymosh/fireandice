fire <- 'creek'

dir <- paste0('data/processed/processed/rds/', fire)
df.500 <- readRDS(file.path(paste0(dir, '/', fire, '_df_500m.rds')))
df.raw <- readRDS(file.path(paste0(dir, '/', fire, '_long_df_500m_raw.rds')))

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

# best model
sdd.best <- gam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  data = df.500,
  method = 'REML'
)
