packages <- c('dplyr', 'tidyr', 'corrplot', 'ggplot2', 'terra')
lapply(packages, library, character.only = T)

# make sure if not on processing computer that the rds is updated!
dir <- 'data/processed/processed/rds/creek'

# =======================================================================================
# Inialize DFs
# =======================================================================================

# ----- df.50 SWE -----

# get dataframe
df.50.0 <- readRDS(file.path(dir, 'creek_long_df_50m.rds'))

df.50 <- df.50.0 %>% 
  select(-fd_fractal_dim) %>% # basically same as gap_pct and gap_pct has way less NAs than fractal_dim
  select(-tmmx) %>% # just using tmin for modeling swe (tmmn and tmmx are highly correlated 0.99)
  select(-topo_tpi510, -topo_tpi1200) %>% # not as correlated/strong relationship to swe
  select(-rad_dtm_melt, -rad_dsm_melt) %>% # melt season not relevent to snow accumulation phase
  select(-topo_aspect_cos, -topo_aspect_sin) %>% # drop these because they're really just proxies for what rad_dtm gets
  filter(
    wy != 2020, # drop 2020, since it's prefire
    swe_peak > 0 # drop all cells where there was no snow
    ) %>%  
  mutate(
    snowline = case_when( # remove pixels that fall below that year's snowline
      wy == 2021 ~ 2145,
      wy == 2022 ~ 1956,
      wy == 2023 ~ -Inf, # snow covers whole area for 2023, no need to filter 
      wy == 2024 ~ 1786,
      wy == 2025 ~ 2021),
    wy = as.factor(wy), # make wy a factor
    cell = as.factor(cell) # make cell a factor
    ) %>% 
  filter(topo_elev >= snowline,
    complete.cases(.)) %>% # drop rows with any missing values
  select(-snowline)

# scale numeric predictors
num.cols <- sapply(df.50, is.numeric)
num.cols['swe_peak'] <- FALSE # don't scale the response variable'
df.50[num.cols] <- scale(df.50[num.cols])

saveRDS(df.50, file.path(dir, 'creek_long_df_50m_clean.rds'))
saveRDS(df.50, 'J:/Fire_Snow/fireandice/data/processed/processed/rds/creek/creek_long_df_50m_clean.rds') # save to J: drive
  
# ----- df.500 SDD -----

df.500.0 <- readRDS(file.path(dir, 'creek_long_df_500m.rds'))
df.500 <- df.500.0 %>% 
  select(-fd_fractal_dim) %>% # super correlated with gap_pct but with way more NAs
  select(-tmmn) %>% # just using tmmx to model sdd
  select(-topo_aspect_cos) %>% # redundant with dtm_accum
  select(-topo_tpi150, -topo_tpi510) %>% # redundant with tpi1200
  filter(
    wy != 2020, # drop 2020, since it's prefire
    swe_peak > 0 # drop all cells where there was no snow
  ) %>%  
  mutate(
    snowline = case_when( # remove pixels that fall below that year's snowline
      wy == 2021 ~ 2145,
      wy == 2022 ~ 1956,
      wy == 2023 ~ -Inf, # snow covers whole area for 2023, no need to filter 
      wy == 2024 ~ 1786,
      wy == 2025 ~ 2021),
    wy = as.factor(wy), # make wy a factor
    cell = as.factor(cell) # make cell a factor
  ) %>% 
  filter(topo_elev >= snowline,
         complete.cases(.)) %>% # drop rows with any missing values
  select(-snowline)

# scale numeric predictors
num.cols <- sapply(df.500, is.numeric)
num.cols['sdd'] <- FALSE # don't scale the response variable'
df.500[num.cols] <- scale(df.500[num.cols])

# save to mosher comp
saveRDS(df.500, file.path(dir, 'creek_long_df_500m_clean.rds'))
# save to J: drive
saveRDS(df.500, 'J:/Fire_Snow/fireandice/data/processed/processed/rds/creek/creek_long_df_500m_clean.rds')

# =======================================================================================
# Spatial Autocorrelation
# =======================================================================================
library(gstat)

# get x and y coordinates
r.template <- rast('data/processed/processed/tif/50m/creek/creek_swe_50m.tif')

df.2021 <- subset(df.50, wy == 2021)
df.2021$cell <- as.numeric(as.character(df.2021$cell))


coords <- xyFromCell(r.template, df.2021$cell)
df.2021$x <- coords[,1]
df.2021$y <- coords[,2]
df.2021 <- df.2021[complete.cases(df.2021[, c('swe_peak', 'x', 'y')]), ]

# ----- semivariogram -----
set.seed(123)
df.vario <- df.2021[sample(nrow(df.2021), min(10000, nrow(df.2021))), ]
vg <- variogram(
  swe_peak ~ 1,
  locations = ~ x + y,
  data = df.vario,
  width = 100   # 100 m bins
)
plot(vg)
plot(vg, xlim = c(0, 3000))
plot(vg, xlim = c(0, 1000))
head(vg)
tail(vg)
vg[vg$dist <= 3000, ]

# ------ Morans I ------
# subset dataset for Moran's I
set.seed(123)

n.samp <- min(20000, nrow(df.2021))
df.moran <- df.2021[sample(nrow(df.2021), n.samp), ]

d.vals <- c(250, 500, 1000, 2000)
library(spdep)

for (d in d.vals) {
  nb <- dnearneigh(coords.mat, d1 = 0, d2 = d)
  lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)
  
  cat('\n----- Distance:', d, 'm -----\n')
  print(summary(nb))
  print(moran.test(df.moran$swe_peak, lw, zero.policy = TRUE))
}

# =======================================================================================
# Data Exploration
# =======================================================================================

# ----------------------- SWE --------------------------

# ----- check how many NAs -----
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
num.vars <- df.50 |>
  select(-cell, -wy, -swe_peak, -sqrt_swe, -swe_peak_log)

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
  filter(abs(Freq) > 0.5) %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(as.character(Var1), as.character(Var2))), collapse = '___')) %>%
  ungroup() %>%
  distinct(pair, .keep_all = TRUE) %>%
  arrange(desc(abs(Freq))) %>%
  select(-pair)

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

cor(df.50$swe_peak, df.50$ht_zpcum1, use = 'complete.obs')
# -0.090
cor(sqrt(df.50$swe_peak), df.50$ht_zpcum1, use = 'complete.obs')
# -0.097
cor(df.50$swe_peak, df.50$ht_zpcum2, use = 'complete.obs')
# -0.127
cor(sqrt(df.50$swe_peak), df.50$ht_zpcum2, use = 'complete.obs')
# -0.147

canopy.vars.temp <- c('ht_zpcum1', 'ht_zpcum2', 'gap_gap_pct', 'rad_dsm_accum', 'cover_ground_frac')
for (v in canopy.vars.temp) {
  cat('\n---', v, '---\n')
  
  cor1 <- cor(df.50$swe_peak, df.50[[v]], use = 'complete.obs')
  cor2 <- cor(sqrt(df.50$swe_peak), df.50[[v]], use = 'complete.obs')
  
  cat('swe:', round(cor1, 4), '\n')
  cat('sqrt swe:', round(cor1, 4), '\n')
}


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
  'topo_slope',
  'topo_tpi150',
  'topo_elev'
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
gam <- sqrt(swe) ~ topo_elev + tpi150 + rad_dtm_accum + topo_slope 


hist(df.50[["topo_elev"]])

# normalish

hist(df.50[["rad_dtm_accum"]])
# normal

hist(df.50[["topo_slope"]])
# skewed right

hist(df.50[["topo_tpi150"]])
# normal

hist(df.50[["pr"]])
# skewed right

hist(df.50[["tmmn"]])
# multimodal but somewhat normal

hist(df.50[["swe_peak"]])
# skewed right

hist(df.50[["sqrt_swe"]])
# still skewed right but better

# loop through all variables
par(mfrow = c(3, 3))  # adjust layout

for (v in vars) {
  hist(df.50[[v]], main = v, xlab = v)
}

par(mfrow = c(1, 1))  # reset



# ------ plot relationships between variables and swe -----
library(ggplot2)
library(patchwork)

set.seed(123)
df.sample <- df.50[sample(nrow(df.50), 10000), ]

p <- list()
for (v in vars) {
  p[[v]] <- ggplot(df.sample, aes(x = .data[[v]], y = swe_peak)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'gam') +
    labs(title = v)
}

wrap_plots(p, ncol = 4)

# look at specific correlations
cor(df.50$tmmn, df.50$swe_peak, use = 'complete.obs')
cor(df.50$pr, df.50$swe_peak, use = 'complete.obs')
cor(df.50$rad_dtm_accum, df.50$topo_aspect_cos, use = 'complete.obs')
cor(df.50$rad_dtm_accum, df.50$swe_peak, use = 'complete.obs')
cor(df.50$swe_peak, df.50$topo_aspect_cos, use = 'complete.obs')



# ----- plot/explore transformations of predictors -----

# slope
hist(
  df.50$topo_slope,
  main = 'Slope distribution',
  xlab = 'Slope',
  breaks = 50
)

hist(
  (df.50$topo_slope)^2,
  main = 'Slope distribution',
  xlab = 'Slope',
  breaks = 50
)
smoothScatter(log(df.50$topo_slope + 0.01), df.50$swe_peak)
smoothScatter(sqrt(df.50$topo_slope + 0.01), df.50$swe_peak)
smoothScatter(log(df.50$topo_slope + 0.01), df.50$swe_peak)



 # troubleshooting
# ----- turn back to raster -----
template <- swe.stack[['swe_peak_wy2021']]

# make an empty raster with same geometry
r <- rast(template)
values(r) <- NA

# if cell is a factor, convert back to numeric
cells <- as.numeric(as.character(df.50$cell))

# write values into those cells
r[cells] <- df.50$swe_peak

plot(r)


df.2021 <- df.50[df.50$wy == '2021', ]

r.2021 <- rast(template)
values(r.2021) <- NA

cells <- as.numeric(as.character(df.2021$cell))
r.2021[cells] <- df.2021$swe_peak

plot(r.2021)
r.zero <- r.2021 == 0
r.zero <- classify(r.zero, rbind(c(0, NA)))
plot(r.zero, col = 'red', add = TRUE, legend = FALSE)


# ----------------------- SDD --------------------------

# ----- check how many NAs -----
# should be 0 since we already filtered them out
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
  select(-cell, -wy)

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
  filter(abs(Freq) > 0.5) %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(as.character(Var1), as.character(Var2))), collapse = '___')) %>%
  ungroup() %>%
  distinct(pair, .keep_all = TRUE) %>%
  arrange(desc(abs(Freq))) %>%
  select(-pair)

cor.strong

# ==============================================================================
#  check relationship between predictors and response variable 
# ==============================================================================

# ----- compare tpi -----
# since TPI are highly correlated with eachother, I want to include one in the model. Here I am seeing which one is the most correlated with SDD
# SDD
df.500.sub <- df.500[complete.cases(df.500[, c('topo_tpi150', 'topo_tpi510', 'topo_tpi1200', 'sdd')]), ]
cor(df.500.sub$topo_tpi150, df.500.sub$sdd) # -0.005
cor(df.500.sub$topo_tpi510, df.500.sub$sdd)  # -0.013
cor(df.500.sub$topo_tpi1200, df.500.sub$sdd) # -0.015

smoothScatter(df.500.sub$topo_tpi150, df.500.sub$sdd)
smoothScatter(df.500.sub$topo_tpi510, df.500.sub$sdd)
smoothScatter(df.500.sub$topo_tpi1200, df.500.sub$sdd)

cor(abs(df.500.sub$topo_tpi150), df.500.sub$sdd) # 0.072
cor(abs(df.500.sub$topo_tpi1200), df.500.sub$sdd) # 0.064

# ------ Compare correlations between variable and response -----
# print cor value for each predictor
df.500.sub <- df.500[complete.cases(df.500[.])]
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
  'topo_aspect_sin',
  'topo_slope',
  'topo_tpi1200',
  'topo_elev'
)

# group by year
cor.by.wy <- df.500 %>%
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
  labs(title = 'Within-year correlations with SDD')

# print correlations of canopy vars with sdd
canopy.vars.temp <- c('ht_zpcum1', 'ht_zpcum2', 'gap_gap_pct', 'rad_dsm_accum', 'cover_ground_frac')
for (v in canopy.vars.temp) {
  cat('\n---', v, '---\n')
  
  cor1 <- cor(df.500$sdd, df.500[[v]], use = 'complete.obs')
 # cor2 <- cor(sqrt(df.500$sdd), df.500[[v]], use = 'complete.obs')
  
  cat('sdd:', round(cor1, 4), '\n')
#  cat('sqrt swe:', round(cor1, 4), '\n')
}

# ----- compute correlations with sdd -----
cor.df <- df.500 %>%
  select(where(is.numeric)) %>%
  summarise(across(-sdd, ~ cor(.x, sdd, use = 'complete.obs'))) %>%
  tidyr::pivot_longer(cols = everything(), 
                      names_to = 'variable', 
                      values_to = 'correlation')

# sort by strength
cor.df <- cor.df %>%
  arrange(desc(abs(correlation)))

# plot
ggplot(cor.df, aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col() +
  coord_flip() +
  labs(
    title = 'Correlation with SDD',
    x = 'Variable',
    y = 'Pearson correlation'
  ) +
  theme_minimal()


# -----  plot predictor vs response -----
plot.df <- df.500 %>%
  select(cell, wy, sdd, all_of(vars)) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = 'predictor',
    values_to = 'value'
  ) %>%
  filter(!is.na(value), !is.na(sdd))

# create sample df
set.seed(123)
# plot function
plot_var_wy <- function(df, var) {
  
  d <- df %>%
    filter(!is.na(.data[[var]]), !is.na(sdd))
  
  # sample to keep it fast
  set.seed(123)
  d <- d %>% slice_sample(n = min(200000, nrow(d)))
  
  p <- ggplot(d, aes(x = .data[[var]], y = sdd)) +
    geom_point(alpha = 0.03, size = 0.3) +
    geom_smooth(method = 'gam', formula = y ~ s(x), se = FALSE, color = 'blue') +
    facet_wrap(~ wy) +
    theme_bw() +
    labs(
      x = var,
      y = 'sdd',
      title = paste('SDD vs', var, 'by water year')
    )
  
  return(p)
}

for (v in vars) {
  print(plot_var_wy(df.500, v))
}

plot_var_wy(df.500, 'topo_elev')
plot_var_wy(df.500, 'topo_tpi150')
plot_var_wy(df.500, 'topo_slope')
plot_var_wy(df.500, 'topo_aspect_sin')
plot_var_wy(df.500, 'topo_aspect_cos')
plot_var_wy(df.50, 'rad_dtm_accum')
plot_var_wy(df.50, 'pr')
plot_var_wy(df.50, 'tmmn')


# ----- fit a GAM with multiple predictors -----
gam <- sdd ~ topo_elev + tpi150 + rad_dtm_accum + topo_slope 


hist(df.50[["topo_elev"]])

# normalish

hist(df.50[["rad_dtm_accum"]])
# normal

hist(df.50[["topo_slope"]])
# skewed right

hist(df.50[["topo_tpi150"]])
# normal

hist(df.50[["pr"]])
# skewed right

hist(df.50[["tmmn"]])
# multimodal but somewhat normal

hist(df.50[["swe_peak"]])
# skewed right

hist(df.50[["sqrt_swe"]])
# still skewed right but better

# loop through all variables
par(mfrow = c(3, 3))  # adjust layout

for (v in vars) {
  hist(df.50[[v]], main = v, xlab = v)
}

par(mfrow = c(1, 1))  # reset



# ------ plot relationships between variables and swe -----
library(ggplot2)
library(patchwork)

set.seed(123)
df.sample <- df.50[sample(nrow(df.50), 10000), ]

p <- list()
for (v in vars) {
  p[[v]] <- ggplot(df.sample, aes(x = .data[[v]], y = swe_peak)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'gam') +
    labs(title = v)
}

wrap_plots(p, ncol = 4)

# look at specific correlations
cor(df.50$tmmn, df.50$swe_peak, use = 'complete.obs')
cor(df.50$pr, df.50$swe_peak, use = 'complete.obs')
cor(df.50$rad_dtm_accum, df.50$topo_aspect_cos, use = 'complete.obs')
cor(df.50$rad_dtm_accum, df.50$swe_peak, use = 'complete.obs')
cor(df.50$swe_peak, df.50$topo_aspect_cos, use = 'complete.obs')



# ----- plot/explore transformations of predictors -----

# slope
hist(
  df.50$topo_slope,
  main = 'Slope distribution',
  xlab = 'Slope',
  breaks = 50
)

hist(
  (df.50$topo_slope)^2,
  main = 'Slope distribution',
  xlab = 'Slope',
  breaks = 50
)
smoothScatter(log(df.50$topo_slope + 0.01), df.50$swe_peak)
smoothScatter(sqrt(df.50$topo_slope + 0.01), df.50$swe_peak)
smoothScatter(log(df.50$topo_slope + 0.01), df.50$swe_peak)



# troubleshooting
# ----- turn back to raster -----
template <- swe.stack[['swe_peak_wy2021']]

# make an empty raster with same geometry
r <- rast(template)
values(r) <- NA

# if cell is a factor, convert back to numeric
cells <- as.numeric(as.character(df.50$cell))

# write values into those cells
r[cells] <- df.50$swe_peak

plot(r)


df.2021 <- df.50[df.50$wy == '2021', ]

r.2021 <- rast(template)
values(r.2021) <- NA

cells <- as.numeric(as.character(df.2021$cell))
r.2021[cells] <- df.2021$swe_peak

plot(r.2021)
r.zero <- r.2021 == 0
r.zero <- classify(r.zero, rbind(c(0, NA)))
plot(r.zero, col = 'red', add = TRUE, legend = FALSE)