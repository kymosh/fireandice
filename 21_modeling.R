packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# get dataframe
#dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/rds' # my computer
dir <- 'data/processed/processed/rds' # processing computer

df.50 <- readRDS(file.path(dir, 'creek_long_df_50m.rds'))
#df.500 <- readRDS(file.path(dir, 'creek_long_df_500m.rds')) let's just focus on df.50 for now. 

# make complete-case DF 
df.50.cc <- df.50 %>% 
  filter(wy != 2020) %>% # drop 2020, since it's prefire
  select(-fd_fractal_dim) %>% # drop fractal_dim because very highly correlated with gap_pct but gas large amount of NAs
  mutate(
    cell = as.factor(cell), # make cell a factor
    wy = as.factor(wy))  %>% # make wy a factor
  filter(complete.cases(.)) # drop rows with any missing values

# scale numeric predictors
num.cols <- sapply(df.50.cc, is.numeric)
num.cols['swe_peak'] <- FALSE # don't scale the response variable'
df.50.cc[num.cols] <- scale(df.50.cc[num.cols])

# load results DF
results <- readRDS(file.path(dir, 'model_results.rds'))
coef.results <- readRDS(file.path(dir, 'model_coef_results.rds'))

set.seed(12)
idx <- sample(seq_len(nrow(df.50.cc)), 100000)

# ==============================================================================
#  Results DF and function creation
# ==============================================================================

# create blank dataframes (commenting out so don't accidentally overwrite them)
results <- data.frame(
  model = character(),
  AIC = numeric(),
  BIC = numeric(),
  logLik = numeric(),
  n_params = numeric(),
  type = character(),
  notes = character()
)

coef.results <- data.frame(
  model = character(),
  term = character(),
  estimate = numeric(),
  std.error = numeric(),
  t.value = numeric(),
  type = character(),
  notes = character()
)

add.model.results <- function(results.df, coef.df, model, name, type = 'exploratory', notes = NA) {
  # model-level results
  new.row <- data.frame(
    model = name, AIC = AIC(model),
    BIC = BIC(model),
    logLik = as.numeric(logLik(model)),
    n_params = length(fixef(model)),
    type = type,
    notes = notes
  )
  
  results.df <- rbind(results.df, new.row)
  
  # coefficient-level results
  coef.mat <- coef(summary(model))
  coef.new <- data.frame(
    model = name,
    term = rownames(coef.mat),
    estimate = coef.mat[, 'Estimate'],
    std.error = coef.mat[, 'Std. Error'],
    t.value = coef.mat[, 't value'],
    type = type,
    notes = notes,
    row.names = NULL
  )
  
  coef.df <- rbind(coef.df, coef.new)
  
  return(list(results = results.df, coefs = coef.df))
}


# ==============================================================================
#  Modeling
# ==============================================================================

# ----- Topo + Clim + Rad -----
topo.rad.clim <- lmer(
  swe_peak ~ topo_aspect_cos +topo_aspect_sin + topo_slope + topo_tpi150 +
    topo_tpi510 + topo_elev + tmmx + pr + rad_dtm_accum + rad_dtm_melt +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE # if TRUE, can't compare AIC values
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = topo.rad.clim,
  name = 'topo.rad.clim',
  type = 'base',
  notes = 'base model with topography, dtm radiation, and climate'
)

results <- out$results
coef.results <- out$coefs

# ----- Topo + Clim -----
topo.clim <- lmer(
  swe_peak ~ topo_aspect_cos +topo_aspect_sin + topo_slope + topo_tpi150 +
    topo_tpi510 + topo_elev + tmmx + pr + (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE 
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = topo.clim,
  name = 'topo.clim',
  type = 'base',
  notes = 'base model with topography and climate'
)

results <- out$results
coef.results <- out$coefs

# ----- Rad + Clim -------
# only radiation(from DTM) and climatic predictors
rad.clim <- lmer(
  swe_peak ~ topo_elev + tmmx + pr + rad_dtm_accum + rad_dtm_melt +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE 
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = rad.clim,
  name = 'rad.clim',
  type = 'base',
  notes = 'base model dtm radiation and climate'
)

results <- out$results
coef.results <- out$coefs

# save dfs
saveRDS(results, file.path(dir, 'model_results.rds'))
saveRDS(coef.results, file.path(dir, 'model_coef_results.rds'))

# ----- clean with tpi510 -----
# removed potentially redundant slope/aspect and picked a single TPI value
topo.rad.clim.510 <- lmer(
  swe_peak ~ topo_elev + topo_tpi510 + tmmx + pr + rad_dtm_accum + rad_dtm_melt +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = topo.rad.clim.510,
  name = 'topo.rad.clim.510',
  type = 'base',
  notes = 'removed slope, aspect, and tpi150 from full base model'
)

results <- out$results
coef.results <- out$coefs

saveRDS(results, file.path(dir, 'model_results.rds'))
saveRDS(coef.results, file.path(dir, 'model_coef_results.rds'))

# ----- clean with tpi150 -----
# removed potentially redundant slope/aspect and picked a single TPI value
topo.rad.clim.150 <- lmer(
  swe_peak ~ topo_elev + topo_tpi150 + tmmx + pr + rad_dtm_accum + rad_dtm_melt +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE
)

# add results
out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = topo.rad.clim.150,
  name = 'topo.rad.clim.150',
  type = 'base',
  notes = 'removed slope, aspect, and tpi510 from full base model'
)

results <- out$results
coef.results <- out$coefs

# ----- full model but with just tpi150 -----
# so same as topo.rad.clim but minus tpi510
full.150 <- lmer(
  swe_peak ~ topo_aspect_cos + topo_aspect_sin + topo_slope + topo_tpi150 + topo_elev + tmmx + pr + rad_dtm_accum + rad_dtm_melt +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE
)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = full.150,
  name = 'full.150',
  type = 'base',
  notes = 'full model but with just tpi150')

results <- out$results
coef.results <- out$coefs



plot(fitted(full.150)[idx], resid(full.150)[idx])



# backward stepwise
drop1(full.150, test = 'Chisq')
# took about 40 minutes

# since aspect_cos barely improved model, remove
full.150.minus.aspectcos <- lmer(
  swe_peak ~ topo_aspect_sin + topo_slope + topo_tpi150 + topo_elev + tmmx + pr + rad_dtm_accum + rad_dtm_melt +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE
)
AIC(full.150, full.150.minus.aspectcos)
plot(fitted(full.150.minus.aspectcos)[idx], resid(full.150.minus.aspectcos)[idx])


out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = m1,
  name = 'full.150.minus.aspectcos',
  type = 'base',
  notes = 'full model, using tpi 150, but removed aspect_cos')

results <- out$results
coef.results <- out$coefs

# then try removing slope as well
m2 <- update(m1, . ~ . - topo_slope)
AIC(m1, m2, full.150)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = m2,
  name = 'full.150.minus.aspectcos.slope',
  type = 'base',
  notes = 'full model, using tpi 150, but removed aspect_cos AND slope')

results <- out$results
coef.results <- out$coefs


drop1(m1, test = 'Chisq')



# EXPLORATORY
# ---- temperature x precip interaction -----
tmmx.pr.int <- lmer(
  swe_peak ~ topo_elev + tmmx:pr + tmmx + pr +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE
)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = tmmx.pr.int,
  name = 'tmmx:pr.interaction',
  type = 'exploratory',
  notes = 'temp x precip interaction'
)

results <- out$results
coef.results <- out$coefs

# save dfs
saveRDS(results, file.path(dir, 'model_results.rds'))
saveRDS(coef.results, file.path(dir, 'model_coef_results.rds'))

# introduce interaction between precip and tmmx
topo.rad.clim.prtmx <- lmer(
  swe_peak ~ topo_aspect_cos + topo_aspect_sin + topo_slope + topo_tpi150 +
    topo_tpi510 + topo_elev + tmmx + pr + tmmx:pr + rad_dtm_accum + rad_dtm_melt +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE 
)

summary(topo.rad.clim.prtmx)

out <- add.model.results(
  results.df = results,
  coef.df = coef.results,
  model = topo.rad.clim.prtmx,
  name = 'topo.rad.clim.tmmx:pr',
  type = 'base',
  notes = 'adding temp x precip interaction to base model'
)

results <- out$results
coef.results <- out$coefs

saveRDS(results, file.path(dir, 'model_results.rds'))
saveRDS(coef.results, file.path(dir, 'model_coef_results.rds'))











# troubleshooting
set.seed(42)
idx <- sample(nrow(df.50.cc), 50000)

plot(
  df.50.cc$pr[idx],
  df.50.cc$swe_peak[idx],
  pch = 16,
  cex = 0.3
)

library(ggplot2)

ggplot(df.50.cc[sample(nrow(df.50.cc), 100000), ],
       aes(pr, swe_peak)) +
  geom_hex() +
  scale_fill_viridis_c()

cor(df.50.cc$pr, df.50.cc$swe_peak, use = 'complete.obs')

df.sample <- df.50.cc[sample(nrow(df.50.cc), 100000), ]

df.sample$pr.bin <- cut(df.sample$pr, breaks = 30)

df.bin <- aggregate(swe_peak ~ pr.bin, data = df.sample, FUN = mean)

plot(df.bin$swe_peak, type = 'l')


summary(mod.int)


summary(df.50$topo_aspect)

plot(df.sample$topo_aspect, df.sample$rad_dtm_melt)

m.rad <- lm(rad_dtm_accum ~ topo_slope + topo_aspect, data = df.50.cc)
plot(m.rad)

summary(m.rad)

m.rad <- lm(rad_dtm_accum ~ topo_slope + aspect_sin + aspect_cos, data = df.50.cc)
summary(m.rad)

cor(df.50.cc[, c(
  'topo_slope',
  'topo_tpi150',
  'topo_tpi510',
  'topo_elev',
  'topo_aspect_cos',
  'topo_aspect_sin',
  'rad_dtm_accum',
  'rad_dtm_melt'
)], use = 'complete.obs')

