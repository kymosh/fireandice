packages <- c('tidymodels', 'dplyr', 'tidyr')
install.packages(setdiff(packages, row.names(installed.packages())))
lapply(packages, library, character.only = T)

# get dataframe
df <- readRDS('data/processed/processed/rds/creek_long_df_50m.rds')
df.wide <- readRDS('data/processed/processed/rds/creek_df_50m.rds')

head(df)


# ---------------- EDA ---------------------------------------------------------
topo.vars <- c('aspect', 'slope', 'tpi1200', 'tpi150', 'tpi2010', 'tpi510', 'hli', 'elev')

# histograms of topo variables
df.filtered %>%
  select(all_of(topo.vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Topographic Variables")

clim.vars <- c('tmmn', 'tmmx', 'pr')

# histograms of clim variables
df.filtered %>%
  select(all_of(clim.vars)) %>%
  pivot_longer(everything(), names_to = 'variable', values_to = 'value') %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = 'darkorange', color = 'black') +
  facet_wrap(~variable, scales = 'free') +
  theme_minimal() +
  labs(title = 'Histograms of Raw Climate Variables')

# histogram of just precip
df.filtered %>%
  select(pr) %>%
  pivot_longer(everything(), names_to = 'variable', values_to = 'value') %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = 'darkorange', color = 'black') +
  facet_wrap(~variable, scales = 'free') +
  theme_minimal() +
  labs(title = 'Histograms of Raw pr')

# histogram of cbibc
df.filtered %>%
  select(cbibc) %>%
  pivot_longer(everything(), names_to = 'variable', values_to = 'value') %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = 'darkorange2', color = 'black') +
  facet_wrap(~variable, scales = 'free') +
  theme_minimal() +
  labs(title = 'Histograms of Raw CBIBC')


# ---------------- base recipe ------------------------------------------------------

swe.recipe <- recipe(peak_swe ~ x + y + year + cbibc + aspect + slope + elev + hli + tpi150 + tpi2010 + tpi510 + tpi1200 + pr + tmmx + tmmn + forest_type, data = df.filtered) %>%
  update_role(x, y, new_role = 'id') %>% # preserve x and y
  
  # --- topo variables---
  step_mutate(
    northness = cos(aspect), # calculate northness and eastness
    eastness = sin(aspect)
  ) %>%
  step_log(slope, offset = 0.1) %>% # log transform slope ---> normalized distribution
  
  # --- clim variables---
  step_log(pr, offset = 0.1) %>% # log transform precip ---> becomes mulitmodal, but more normal
  
  step_normalize(all_numeric_predictors()) 


# prep and bake the recipe 
prep.recipe <- prep(swe.recipe, training = df.filtered)
df.transformed <- bake(prep.recipe, new_data = df.filtered)

# visualize transformed variables

df.transformed %>%
  select(cbibc) %>% # change to what variable(s) to look at
  pivot_longer(everything(), names_to = 'variable', values_to = 'value') %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = 'steelblue', color = 'black') +
  facet_wrap(~variable, scales = 'free') +
  theme_minimal() +
  labs(title = 'Histograms of Transformed Topo Variables')

# plots precip before and after transformation
pr.df <- df.filtered %>%
  select(pr) %>%
  mutate(stage = 'original') %>%
  bind_rows(
    df.transformed %>%
      select(pr) %>%
      mutate(stage = 'log-normalized'))

ggplot(pr.df, aes(x = pr, fill = stage)) +
  geom_histogram(bins = 30, color = 'black', alpha = 0.6, position = 'identity') +
  facet_wrap(~stage, scales = 'free') +
  theme_minimal() +
  labs(title = 'Precipitation Before and After Log Transform',
       x = 'Precipitation', y = 'Count')

# visualize cbibc transformations including the untransformed version
df.transformed %>%
  select(cbibc_original = cbibc, 
         cbibc_YeoJohnson = cbibc, 
         cbibc_sqrt) %>% 
  pivot_longer(everything(), names_to = 'variable', values_to = 'value') %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = 'steelblue', color = 'black') +
  facet_wrap(~variable, scales = 'free') +
  theme_minimal() +
  labs(title = 'Comparison of cbibc Transformations')


# ---------------- GLM -------------------------------------------------------

# create tibble to hold model results
model.results <- tibble(
  model_name = character(),
  formula = character(),
  aic = numeric(),
  rsq = numeric(),
  rmse = numeric(),
  coefficients = list(),
  summary = list())

# ---------------- recipe ----------------
swe.recipe <- recipe(peak_swe ~ year + cbibc + hli + elev + tpi510 + pr + tmmx + tmmn,
                     data = df.filtered) %>%
  step_log(pr, offset = 0.1) %>%
  step_normalize(all_numeric_predictors())  # normalize predictors

# ------------- models -----------------

# normal GLM
glm.normal <- linear_reg() %>%
  set_engine('glm') %>%
  set_mode('regression') # don't need to put this, this is the only available mode; # identity link by default

# gamma GLM
glm.gamma.log <- linear_reg() %>%
  set_engine('glm', family = Gamma(link = 'log')) %>%
  set_mode('regression') # don't need to put this, this is the only available mode

# random forest
rf.ranger <- rand_forest() %>%
  set_mode('regression') %>%
  set_engine('ranger')


# fit models
set.seed(1)
glm.normal.fit <- glm.normal %>% fit(peak_swe ~ ., data = df.filtered)
predict(glm.normal.fit, df.filtered)

#----- workflow -----
swe.wf <- workflow() %>%
  add_formula(peak_swe ~ .)

# ----- run models -----

glm.normal.rs  <- swe.wf %>%
  add_model(glm.normal) %>%
  fit
  
glm.gamma.log.rs <- swe.wf %>%
  add_model(glm.gamma.log) %>%

# ----- evaluate -----
collect_metrics(glm.normal.rs)
collect_metrics(glm.gamma.log.rs)










