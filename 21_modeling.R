packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4')
lapply(packages, library, character.only = T)


# get dataframe
dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/rds'
df.50 <- readRDS(file.path(dir, 'creek_long_df_50m_filt.rds'))
#df.500 <- readRDS(file.path(dir, 'creek_long_df_500m_filt.rds')) let's just focus on df.50 for now. 

# make complete-case DF
df.50.cc <- df.50 %>% 
  filter(wy != 2020) %>% # drop 2020, since it's prefire
  mutate(cell = as.factor(cell)) %>% # make cell a factor
  mutate(wy = as.factor(wy)) # make wy a factor

# scale numeric predictors
num.cols <- sapply(df.50.cc, is.numeric)
num.cols['swe_peak'] <- FALSE # don't scale the response variable'
df.50.cc[num.cols] <- scale(df.50.cc[num.cols])

# ----- create dataframes for results and function to save results -----

results <- data.frame(
  model = character(),
  AIC = numeric(),
  BIC = numeric(),
  logLik = numeric(),
  n_params = numeric(),
  type = character(),
  notes = character()
)
coef.results <- data.frame()

add.model.result <- function(results.df, model, name, type = 'exploratory', notes = NA) {
  
  new.row <- data.frame(
    model = name,
    AIC = AIC(model),
    BIC = BIC(model),
    logLik = as.numeric(logLik(model)),
    n_params = length(fixef(model)),
    type = type,
    notes = notes
  )
  
  results.df <- rbind(results.df, new.row)
  
  return(results.df)
}

# ----- swe.base -----
# only topographic and climatic predictors
swe.base <- lmer(
  swe_peak ~ topo_aspect + topo_slope + topo_tpi150 +
    topo_tpi510 + topo_elev + tmmx + pr + rad_dtm_accum + rad_dtm_melt +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE # if TRUE, can't compare AIC values
)

# add results
out <- add.model.results(
  model = swe.base,
  model.name = 'topo',
  results.df = results,
  coef.df = coef.results
)

results <- out$results
coef.results <- out$coefs

# ---- elevation:precip interaction -----
elev.pr.int <- lmer(
  swe_peak ~ topo_elev:pr + tmmx + pr + topo_elev +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE
)


tmmx.pr.int <- lmer(
  swe_peak ~ topo_elev + tmmx:pr + tmmx + pr +
    (1 | cell) + (1 | wy),
  data = df.50.cc,
  REML = FALSE
)






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









