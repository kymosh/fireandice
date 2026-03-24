packages <- c('dplyr', 'tidyr', 'corrplot', 'ggplot2')
lapply(packages, library, character.only = T)

# make sure if not on processing computer that the rds is updated!
dir <- 'data/processed/processed/rds'

# get dataframe
df.50.0 <- readRDS(file.path(dir, 'creek_long_df_50m.rds'))
df.500.0 <- readRDS(file.path(dir, 'creek_long_df_500m.rds'))

# drop tpi1200, too many NAs
df.50 <- df.50.0 %>% 
  select(-fd_fractal_dim) %>% # basically same as gap_pct and gap_pct has way less NAs than fractal_dim
  select(-tmmx) %>% # just using tmin for modeling swe (tmmn and tmmx are highly correlated 0.99)
  select(-topo_tpi510, -topo_tpi1200) %>% # not as correlated/strong relationship to swe
  select(-rad_dtm_melt, -rad_dsm_melt) # melt season not relevent to snow accumulation phase

df.500 <- df.500.0 %>% 
  select(-fd_fractal_dim) %>% # basically same as gap_pct and gap_pct has way less NAs than fractal_dim
  select(-tmmn) # just using tmmx to model sdd

saveRDS(df.50, file.path(dir, 'creek_long_df_50m_filt.rds'))
saveRDS(df.500, file.path(dir, 'creek_long_df_500m_filt.rds'))

# save to J: drive
saveRDS(df.50, 'J:/Fire_Snow/fireandice/data/processed/processed/rds/creek_long_df_50m_filt.rds')


# ----- check how many NAs (do for both 50 and 500) -----
na.summary <- data.frame(
  variable = names(df.500),
  prop.na = sapply(df.500, function(x) mean(is.na(x)))
)
na.summary <- na.summary[order(-na.summary$prop.na), ]
print(na.summary)

# how many rows would be removed if we drop rows with NAs?
mean(complete.cases(df.50))
mean(complete.cases(df.500))


# ----- Check correlation among predictors -----
num.vars <- df.500 |>
  select(-cell, -wy, -swe_peak)

cor.mat <- cor(num.vars, use = 'pairwise.complete.obs')

corrplot(cor.mat, method = 'color')
corrplot(
  cor.mat,
  method = 'color',
  type = 'upper',
  order = 'hclust',
  tl.cex = 0.7,
  addCoef.col = 'black',
  number.cex = 0.5,
  col = colorRampPalette(c('blue', 'white', 'red'))(200),
  tl.col = 'black',
  diag = FALSE
)

cor.df <- as.data.frame(as.table(cor.mat))

cor.strong <- cor.df %>%
  filter(Var1 != Var2) %>%
  filter(abs(Freq) > 0.8) %>%
  arrange(desc(abs(Freq)))

cor.strong

# ==============================================================================
#  check relationship between predictors and response variable 
# ==============================================================================

# ----- tmmx or tmmn ----
# here I am comparing tmmn and tmmx (with SWE only) to help determine which one I want to include in each model
df.50.sub <- df.50[complete.cases(df.50[, c('tmmx', 'tmmn', 'swe_peak')]), ]

cor(df.50.sub$tmmx, df.50.sub$swe_peak, use = 'complete.obs')
cor(df.50.sub$tmmn, df.50.sub$swe_peak, use = 'complete.obs')

smoothScatter(df.50$tmmx, df.50$swe_peak)
smoothScatter(df.50$tmmn, df.50$swe_peak)

cor(df.50$tmmx, df.50$tmmn, use = 'complete.obs')
# correlation between tmmn and tmmx is 0.99 so they are effectively the same. Won't make much difference regardless of which one I pick. 
# choosing tmmx for sdd modeling and tmmn for swe_peak modeling

# ----- compare tpi -----
# since TPI are highly correlated with eachother, I want to include one in the model. Here I am seeing which one is the most correlated with SWE
# SWE
df.50.sub <- df.50[complete.cases(df.50[, c('topo_tpi150', 'topo_tpi510', 'topo_tpi1200', 'swe_peak')]), ]
cor(df.50.sub$topo_tpi150, df.50.sub$swe_peak) # -0.042
cor(df.50.sub$topo_tpi510, df.50.sub$swe_peak) # -0.027
cor(df.50.sub$topo_tpi1200, df.50.sub$swe_peak) # 0.017

smoothScatter(df.50.sub$topo_tpi150, df.50.sub$swe_peak)
smoothScatter(df.50.sub$topo_tpi510, df.50.sub$swe_peak)
smoothScatter(df.50.sub$topo_tpi1200, df.50.sub$swe_peak)

cor(abs(df.50.sub$topo_tpi150), df.50.sub$swe_peak) # 0.035

# TPI showing nonlinear relationship. Relationship strength descreases as you increase TPI window size
# NOTE: I only did this for SWE not for SDD, and likely could be different for SDD!

# ------ Compare correlations between variable and response -----
# print cor value for each predictor
df.50.sub <- df.50[complete.cases(df.50[.])]
vars <- c(
  'cover_ground_frac',
  'gap_gap_pct',
  'gap_dist_to_canopy_mean',
  'ht_zskew',
  'ht_zkurt',
  'ht_zentropy',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zpcum6',
  'ht_zpcum9',
  'cbibc',
  'rad_dsm_accum',
  'rad_dtm_accum',
  'topo_aspect_cos',
  'topo_aspect_sin',
  'topo_slope',
  'topo_tpi150',
  'topo_elev',
  'pr',
  'tmmn'
)

# group by year
cor.by.wy <- df.50 %>%
  group_by(wy) %>%
  summarise(
    across(
      all_of(vars),
      ~ cor(.x, swe_peak, use = 'complete.obs'),
      .names = 'cor_{.col}'
    )
  )

# make easier to read
cor.long <- cor.by.wy %>%
  pivot_longer(
    cols = -wy,
    names_to = 'variable',
    values_to = 'correlation'
  ) %>%
  mutate(variable = gsub('cor_', '', variable))

# this plot! shows box plots of within-yar correlations with SWE among my predictors
ggplot(cor.long, aes(x = variable, y = correlation)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  labs(title = 'Within-year correlations with SWE')

# ----- plot predictor vs response -----

# SWE
vars <- c(
  'cover_ground_frac',
  'gap_gap_pct',
  'gap_dist_to_canopy_mean',
  'ht_zskew',
  'ht_zkurt',
  'ht_zentropy',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zpcum6',
  'ht_zpcum9',
  'cbibc',
  'rad_dsm_accum',
  'rad_dtm_accum',
  'topo_aspect_cos',
  'topo_aspect_sin',
  'topo_slope',
  'topo_tpi150',
  'topo_elev',
  'pr',
  'tmmn'
)

# pivot long so that it's better for plotting
plot.df <- df.50 %>%
  select(cell, wy, swe_peak, all_of(vars)) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = 'predictor',
    values_to = 'value'
  ) %>%
  filter(!is.na(value), !is.na(swe_peak))

# create sample df
set.seed(123)
# plot function
plot_var_wy <- function(df, var) {
  
  d <- df %>%
    filter(!is.na(.data[[var]]), !is.na(swe_peak))
  
  # sample to keep it fast
  set.seed(123)
  d <- d %>% slice_sample(n = min(200000, nrow(d)))
  
  ggplot(d, aes(x = .data[[var]], y = swe_peak)) +
    geom_point(alpha = 0.03, size = 0.3) +
    geom_smooth(method = 'gam', formula = y ~ s(x), se = FALSE, color = 'blue') +
    facet_wrap(~ wy) +
    coord_cartesian(ylim = c(0, 4)) +
    theme_bw() +
    labs(
      x = var,
      y = 'swe_peak',
      title = paste('SWE vs', var, 'by water year')
    )
}

plot_var_wy(df.50, 'topo_elev')
plot_var_wy(df.50, 'topo_tpi150')
plot_var_wy(df.50, 'topo_slope')
plot_var_wy(df.50, 'topo_aspect_sin')
plot_var_wy(df.50, 'topo_aspect_cos')
plot_var_wy(df.50, 'rad_dtm_accum')
plot_var_wy(df.50, 'pr')
plot_var_wy(df.50, 'tmmn')


# ----- fit a GAM with multiple predictors -----
library(mgcv)

gam.test <- gam(
  swe_peak ~ 
    s(topo_elev) +
    s(pr) +
    s(tmmn) +
    s(rad_dtm_accum),
  data = df.50
)

plot(gam.test, pages = 1)

gam.test <- gam(
  swe_peak ~ 
    s(topo_elev) +
    s(pr) +
    s(tmmn) +
    s(rad_dtm_accum) +
    s(topo_tpi150) +
    s(topo_slope),
  data = df.50
)

plot(gam.test, pages = 1)



















 # troubleshooting

