packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest', 'ranger', 'tictoc', 'mgcv', 'ggplot2', 'pdp')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# get dataframe
set.seed(61)
dir <- 'data/processed/processed/rds/' 

df.50.raw <- readRDS(file.path(dir, 'df_50m_raw.rds'))
df.50.balanced <- readRDS(file.path(dir, 'df_50m_raw_balanced.rds')) 

# str(df.50.raw)

df.50.raw.test <- df.50.raw %>%
  group_by(fire) %>%
  slice_sample(n = 10000) %>%
  ungroup()

df.50.balanced.test <- df.50.balanced %>%
  group_by(fire) %>%
  slice_sample(n = 10000) %>%
  ungroup()


burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)
 
# ==============================================================================
#  Results DF and helper functions creation
# ==============================================================================

# individual fires
get.metrics <- function(fitted.model, model.name, fire.name) {
  
  s <- summary(fitted.model)
  
  data.frame(
    fire = fire.name,
    model_name = model.name,
    r.squared = s$r.sq,
    dev.expl = s$dev.expl,
    AIC = AIC(fitted.model),
    BIC = BIC(fitted.model),
    edf = sum(s$edf)
  )
}

# combined model 
get.metrics.combined <- function(fitted.model, model.name) {
  
  s <- summary(fitted.model)
  
  data.frame(
    model_name = model.name,
    r.squared = s$r.sq,
    dev.expl = s$dev.expl,
    AIC = AIC(fitted.model),
    BIC = BIC(fitted.model),
    edf = sum(s$edf)
  )
}


# ==============================================================================
# Stage 1 Modeling - Single family predictors
# ==============================================================================

# ----- plot peak swe for all fires -----
plot.df <- df.50.raw %>%
  select(fire, wy, swe_peak) %>%
  mutate(
    Raw = swe_peak,
    `Square root` = sqrt(swe_peak)
  ) %>%
  pivot_longer(
    cols = c(Raw, `Square root`),
    names_to = 'Transformation',
    values_to = 'Peak_SWE'
  )

for (f in unique(plot.df$fire)) {
  
  p <- plot.df %>%
    filter(fire == f) %>%
    ggplot(aes(Peak_SWE)) +
    geom_density(fill = 'steelblue', alpha = 0.4) +
    facet_grid(wy ~ Transformation, scales = 'free_x') +
    labs(
      title = f,
      x = 'Peak SWE',
      y = 'Density'
    ) +
    theme_bw()
  
  print(p)
}


# ------------------------- Topo-only Model ------------------------------
# ----- Stepwise 1 -----

topo.vars <- c(
  'slope',
  'rad_dtm_accum',
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results <- data.frame()

for (fire.name in unique(df.50.raw.test$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation baseline
  model.elev <- bam(sqrt(swe_peak) ~
                      wy +
                      s(elevation, k = 10),
                    data = fire.df,
                    method = 'ML')
  
  topo.results <- bind_rows(
    topo.results,
    get.metrics(
      fitted.model = model.elev,
      model.name = 'elevation',
      fire.name = fire.name
    )
  )
  

  # test each additional topographic variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + s(elevation, k = 10) + s(' , var, ', k = 10)'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'ML'
    )
    
    topo.results <- bind_rows(
      topo.results,
      get.metrics(
        fitted.model = model,
        model.name = paste0('elevation + ', var), 
        fire.name = fire.name
      )
    )
    
  }
}

topo.results <- topo.results %>%
  group_by(fire) %>%
  mutate(
    AIC.elevation = AIC[model_name == 'elevation'],
    delta.AIC.elevation = AIC - AIC.elevation,
    delta.r.squared = r.squared - r.squared[model_name == 'elevation']
  ) %>%
  ungroup()

topo.results %>%
  arrange(fire, AIC) %>%
  print(n = Inf)

# shows that radiation definitely adds the most. Continue on to stepwise to see if adding additional variables improves the model

# ----- stepwise 2 -----

# updated vars
topo.vars <- c(
  'slope',
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad <- bam(
    sqrt(swe_peak) ~ wy + s(elevation) + s(rad_dtm_accum),
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics(
      fitted.model = topo.elev.rad,
      model.name = 'topo.elev.rad',
      fire.name = fire.name
    )
  )
  
 # test each additional variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sqrt(swe_peak) ~ wy + s(elevation) + s(rad_dtm_accum) + 
             s(', var, ')')
    )
    
    model <- bam(model.formula,
                 data = fire.df,
                 method = 'fREML',
                 discrete = TRUE)
    
    # add results
    topo.results.step <- bind_rows(
      topo.results.step,
      get.metrics(
        fitted.model = model,
        model.name = paste0('topo.elev.rad.', var),
        fire.name = fire.name
        
    
      )
    )
    
  }
  
}

topo.results.step %>%
  arrange(fire, AIC)

topo.results.step.2 <- topo.results.step

# slope still seems to add enough to keep it in the model

# ----- stepwise 3 ------
# updated vars
topo.vars <- c(
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad.slope <- bam(
    sqrt(swe_peak) ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope),
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics(
      fitted.model = topo.elev.rad,
      model.name = 'topo.elev.rad',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sqrt(swe_peak) ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + 
             s(', var, ')')
    )
    
    model <- bam(model.formula,
                 data = fire.df,
                 method = 'fREML',
                 discrete = TRUE)
    
    # add results
    topo.results.step <- bind_rows(
      topo.results.step,
      get.metrics(
        fitted.model = model,
        model.name = paste0('topo.elev.rad.', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

topo.results.step %>%
  arrange(fire, AIC)

topo.results.step.3 <- topo.results.step

# aspect_sin adds a decent amount still across the board

# ------------------------- Canopy-only Model -----------------------
# ------ stepwise 1 -----
# canopy variables
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'ht_zmax',
  'gap_dist_to_canopy_mean',
  'gap_percent'
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # wy only baseline
  base.model <- bam(
    sqrt(swe_peak) ~ wy,
    data = fire.df,
    method = 'ML',
    discrete = TRUE
  )
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics(
      fitted.model = base.model,
      model.name = 'wy only',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in canopy.vars) {
    
    model.formula <- as.formula(
      paste0('sqrt(swe_peak) ~ wy +  
             s(', var, ')')
    )
    
    model <- bam(model.formula,
                 data = fire.df,
                 method = 'ML',
                 discrete = TRUE)
    
    # add results
    canopy.results.step <- bind_rows(
      canopy.results.step,
      get.metrics(
        fitted.model = model,
        model.name = paste0('wy + ', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

canopy.results.step %>%
  arrange(fire, AIC)

canopy.results.step.1 <- canopy.results.step



# ------ stepwise 2 -----

best.var.lookup <- list(
  caldor = c('gap_percent'),
  castle = c('ht_zpcum2'),
  creek = c('ht_zpcum2'),
  dixie = c('ht_zmax')
)


canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sqrt(swe_peak) ~ wy + ',
      's(', best.vars[1], ')'
    )
  )
  
  base.model <- bam(
    base.formula,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  # get base-model metrics
  base.metrics <- get.metrics(
    fitted.model = base.model,
    model.name = paste0(
      'wy + ',
      best.vars[1]
    ),
    fire.name = fire.name
  ) %>%
    mutate(
      added_var = NA_character_,
      delta_dev_expl = 0
    )
  
  # save base-model deviance explained
  base.dev.expl <- base.metrics$dev.expl
  base.AIC <- base.metrics$AIC
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + ',
        's(', best.vars[1], ') + ',
        's(', var, ')'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'fREML',
      discrete = TRUE
    )
    
    model.metrics <- get.metrics(
      fitted.model = model,
      model.name = paste0(
        'wy + ',
        best.vars[1],
        ' + ',
        var
      ),
      fire.name = fire.name
    ) %>%
      mutate(
        added_var = var,
        delta_dev_expl = dev.expl - base.dev.expl,
        delta_AIC = AIC - base.AIC
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, delta_AIC) %>%
  select(-model_name)

canopy.results.step.2 <- canopy.results.step

# ------ stepwise 3 -----
# canopy variables

best.var.lookup <- list(
  caldor = c('gap_percent', 'ht_zpcum2'),
  castle = c('ht_zpcum2', 'gap_percent'),
  creek = c('ht_zpcum2', 'ht_zmax'),
  dixie = c('ht_zmax', 'gap_percent')
)


canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sqrt(swe_peak) ~ wy + ',
      's(', best.vars[1], ') + ',
      's(', best.vars[2], ')'
    )
  )
  
  base.model <- bam(
    base.formula,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  # get base-model metrics
  base.metrics <- get.metrics(
    fitted.model = base.model,
    model.name = paste0(
      'wy + ',
      best.vars[1],
      ' + ',
      best.vars[2]
    ),
    fire.name = fire.name
  ) %>%
    mutate(
      added_var = NA_character_,
      delta_dev_expl = 0
    )
  
  # save base-model deviance explained
  base.dev.expl <- base.metrics$dev.expl
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + ',
        's(', best.vars[1], ') + ',
        's(', best.vars[2], ') + ',
        's(', var, ')'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'fREML',
      discrete = TRUE
    )
    
    model.metrics <- get.metrics(
      fitted.model = model,
      model.name = paste0(
        'wy + ',
        best.vars[1],
        ' + ',
        best.vars[2],
        ' + ',
        var
      ),
      fire.name = fire.name
    ) %>%
      mutate(
        added_var = var,
        delta_dev_expl = dev.expl - base.dev.expl
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, desc(delta_dev_expl))

canopy.results.step.3 <- canopy.results.step

# ------ stepwise 4 -----
# canopy variables

best.var.lookup <- list(
  caldor = c('gap_percent', 'ht_zpcum2', 'ht_zmax'),
  castle = c('ht_zpcum2', 'gap_percent', 'ht_zmax'),
  creek = c('ht_zpcum2', 'ht_zmax', 'ht_zskew'),
  dixie = c('ht_zmax', 'gap_percent', 'gap_dist_to_canopy_mean')
)


canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sqrt(swe_peak) ~ wy + ',
      's(', best.vars[1], ') + ',
      's(', best.vars[2], ') + ',
      's(', best.vars[3], ')'
    )
  )
  
  base.model <- bam(
    base.formula,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  # get base-model metrics
  base.metrics <- get.metrics(
    fitted.model = base.model,
    model.name = paste0(
      'wy + ',
      best.vars[1],
      ' + ',
      best.vars[2],
      ' + ',
      best.vars[3]
    ),
    fire.name = fire.name
  ) %>%
    mutate(
      added_var = NA_character_,
      delta_dev_expl = 0
    )
  
  # save base-model deviance explained
  base.dev.expl <- base.metrics$dev.expl
  base.AIC <- base.metrics$AIC
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + ',
        's(', best.vars[1], ') + ',
        's(', best.vars[2], ') + ',
        's(', best.vars[3], ') + ',
        's(', var, ')'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'fREML',
      discrete = TRUE
    )
    
    model.metrics <- get.metrics(
      fitted.model = model,
      model.name = paste0(
        'wy + ',
        best.vars[1],
        ' + ',
        best.vars[2],
        ' + ',
        best.vars[3],
        ' + ',
        var
      ),
      fire.name = fire.name
    ) %>%
      mutate(
        added_var = var,
        delta_dev_expl = dev.expl - base.dev.expl,
        delta_AIC = AIC - base.AIC
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, desc(delta_dev_expl)) %>%
  select(-model_name)

canopy.results.step.4 <- canopy.results.step

# ------ stepwise 5 -----

best.var.lookup <- list(
  caldor = c('gap_percent', 'ht_zpcum2', 'ht_zmax', 'gap_dist_to_canopy_mean'),
  castle = c('ht_zpcum2', 'gap_percent', 'ht_zmax', 'ht_zskew'),
  creek = c('ht_zpcum2', 'ht_zmax', 'ht_zskew', 'gap_dist_to_canopy_mean'),
  dixie = c('ht_zmax', 'gap_percent', 'gap_dist_to_canopy_mean', 'ht_zskew')
)


canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sqrt(swe_peak) ~ wy + ',
      's(', best.vars[1], ') + ',
      's(', best.vars[2], ') + ',
      's(', best.vars[3], ') + ',
      's(', best.vars[4], ')'
    )
  )
  
  base.model <- bam(
    base.formula,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  # get base-model metrics
  base.metrics <- get.metrics(
    fitted.model = base.model,
    model.name = paste0(
      'wy + ',
      best.vars[1],
      ' + ',
      best.vars[2],
      ' + ',
      best.vars[3],
      ' + ',
      best.vars[4]
    ),
    fire.name = fire.name
  ) %>%
    mutate(
      added_var = NA_character_,
      delta_dev_expl = 0
    )
  
  # save base-model deviance explained
  base.dev.expl <- base.metrics$dev.expl
  base.AIC <- base.metrics$AIC
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + ',
        's(', best.vars[1], ') + ',
        's(', best.vars[2], ') + ',
        's(', best.vars[3], ') + ',
        's(', best.vars[4], ') + ',
        's(', var, ')'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'fREML',
      discrete = TRUE
    )
    
    model.metrics <- get.metrics(
      fitted.model = model,
      model.name = paste0(
        'wy + ',
        best.vars[1],
        ' + ',
        best.vars[2],
        ' + ',
        best.vars[3],
        ' + ',
        best.vars[4],
        ' + ',
        var
      ),
      fire.name = fire.name
    ) %>%
      mutate(
        added_var = var,
        delta_dev_expl = dev.expl - base.dev.expl,
        delta_AIC = AIC - base.AIC
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, delta_AIC) %>%
  select(-model_name)

canopy.results.step.5 <- canopy.results.step

# ------ stepwise 6 -----

best.var.lookup <- list(
  caldor = c('gap_percent', 'ht_zpcum2', 'ht_zmax', 'gap_dist_to_canopy_mean', 'ht_zpcum6'),
  castle = c('ht_zpcum2', 'gap_percent', 'ht_zmax', 'ht_zskew', 'gap_dist_to_canopy_mean'),
  creek = c('ht_zpcum2', 'ht_zmax', 'ht_zskew', 'gap_dist_to_canopy_mean', 'ht_zpcum1'),
  dixie = c('ht_zmax', 'gap_percent', 'gap_dist_to_canopy_mean', 'ht_zskew', 'ht_zpcum1')
)


canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sqrt(swe_peak) ~ wy + ',
      's(', best.vars[1], ') + ',
      's(', best.vars[2], ') + ',
      's(', best.vars[3], ') + ',
      's(', best.vars[4], ') + ',
      's(', best.vars[5], ')'
    )
  )
  
  base.model <- bam(
    base.formula,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  # get base-model metrics
  base.metrics <- get.metrics(
    fitted.model = base.model,
    model.name = paste0(
      'wy + ',
      best.vars[1],
      ' + ',
      best.vars[2],
      ' + ',
      best.vars[3],
      ' + ',
      best.vars[4],
      ' + ',
      best.vars[5]
    ),
    fire.name = fire.name
  ) %>%
    mutate(
      added_var = NA_character_,
      delta_dev_expl = 0
    )
  
  # save base-model deviance explained
  base.dev.expl <- base.metrics$dev.expl
  base.AIC <- base.metrics$AIC
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + ',
        's(', best.vars[1], ') + ',
        's(', best.vars[2], ') + ',
        's(', best.vars[3], ') + ',
        's(', best.vars[4], ') + ',
        's(', best.vars[5], ') + ',
        's(', var, ')'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'fREML',
      discrete = TRUE
    )
    
    model.metrics <- get.metrics(
      fitted.model = model,
      model.name = paste0(
        'wy + ',
        best.vars[1],
        ' + ',
        best.vars[2],
        ' + ',
        best.vars[3],
        ' + ',
        best.vars[4],
        ' + ',
        best.vars[5],
        ' + ',
        var
      ),
      fire.name = fire.name
    ) %>%
      mutate(
        added_var = var,
        delta_dev_expl = dev.expl - base.dev.expl,
        delta_AIC = AIC - base.AIC
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, delta_AIC) %>%
  select(-model_name)

canopy.results.step.6 <- canopy.results.step

# ------ stepwise 7 -----

best.var.lookup <- list(
  caldor = c('gap_percent', 'ht_zpcum2', 'ht_zmax', 'gap_dist_to_canopy_mean', 'ht_zpcum6', 'ht_zpcum9'),
  castle = c('ht_zpcum2', 'gap_percent', 'ht_zmax', 'ht_zskew', 'gap_dist_to_canopy_mean', 'ht_zpcum6'),
  creek = c('ht_zpcum2', 'ht_zmax', 'ht_zskew', 'gap_dist_to_canopy_mean', 'ht_zpcum1', 'ht_zkurt'),
  dixie = c('ht_zmax', 'gap_percent', 'gap_dist_to_canopy_mean', 'ht_zskew', 'ht_zpcum1', 'ht_zpcum2')
)


canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sqrt(swe_peak) ~ wy + ',
      's(', best.vars[1], ') + ',
      's(', best.vars[2], ') + ',
      's(', best.vars[3], ') + ',
      's(', best.vars[4], ') + ',
      's(', best.vars[5], ') + ',
      's(', best.vars[6], ')'
    )
  )
  
  base.model <- bam(
    base.formula,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  # get base-model metrics
  base.metrics <- get.metrics(
    fitted.model = base.model,
    model.name = paste0(
      'wy + ',
      best.vars[1],
      ' + ',
      best.vars[2],
      ' + ',
      best.vars[3],
      ' + ',
      best.vars[4],
      ' + ',
      best.vars[5],
      ' + ',
      best.vars[6]
    ),
    fire.name = fire.name
  ) %>%
    mutate(
      added_var = NA_character_,
      delta_dev_expl = 0
    )
  
  # save base-model deviance explained
  base.dev.expl <- base.metrics$dev.expl
  base.AIC <- base.metrics$AIC
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + ',
        's(', best.vars[1], ') + ',
        's(', best.vars[2], ') + ',
        's(', best.vars[3], ') + ',
        's(', best.vars[4], ') + ',
        's(', best.vars[5], ') + ',
        's(', best.vars[6], ') + ',
        's(', var, ')'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'fREML',
      discrete = TRUE
    )
    
    model.metrics <- get.metrics(
      fitted.model = model,
      model.name = paste0(
        'wy + ',
        best.vars[1],
        ' + ',
        best.vars[2],
        ' + ',
        best.vars[3],
        ' + ',
        best.vars[4],
        ' + ',
        best.vars[5],
        ' + ',
        best.vars[6],
        ' + ',
        var
      ),
      fire.name = fire.name
    ) %>%
      mutate(
        added_var = var,
        delta_dev_expl = dev.expl - base.dev.expl,
        delta_AIC = AIC - base.AIC
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, delta_AIC) %>%
  select(-model_name)

canopy.results.step.7 <- canopy.results.step
# ------ stepwise 8 -----

best.var.lookup <- list(
  caldor = c('gap_percent', 'ht_zpcum2', 'ht_zmax', 'gap_dist_to_canopy_mean', 'ht_zpcum6', 'ht_zpcum9', 'ht_zskew'),
  castle = c('ht_zpcum2', 'gap_percent', 'ht_zmax', 'ht_zskew', 'gap_dist_to_canopy_mean', 'ht_zpcum6', 'ht_zpcum9'),
  creek = c('ht_zpcum2', 'ht_zmax', 'ht_zskew', 'gap_dist_to_canopy_mean', 'ht_zpcum1', 'ht_zkurt', 'gap_percent'),
  dixie = c('ht_zmax', 'gap_percent', 'gap_dist_to_canopy_mean', 'ht_zskew', 'ht_zpcum1', 'ht_zpcum2', 'ht_zpcum6')
)


canopy.results.step <- data.frame()

for (fire.name in unique(df.50.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.50.raw.test %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sqrt(swe_peak) ~ wy + ',
      's(', best.vars[1], ') + ',
      's(', best.vars[2], ') + ',
      's(', best.vars[3], ') + ',
      's(', best.vars[4], ') + ',
      's(', best.vars[5], ') + ',
      's(', best.vars[6], ') + ',
      's(', best.vars[7], ')'
    )
  )
  
  base.model <- bam(
    base.formula,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  # get base-model metrics
  base.metrics <- get.metrics(
    fitted.model = base.model,
    model.name = paste0(
      'wy + ',
      best.vars[1],
      ' + ',
      best.vars[2],
      ' + ',
      best.vars[3],
      ' + ',
      best.vars[4],
      ' + ',
      best.vars[5],
      ' + ',
      best.vars[6],
      ' + ',
      best.vars[7]
    ),
    fire.name = fire.name
  ) %>%
    mutate(
      added_var = NA_character_,
      delta_dev_expl = 0
    )
  
  # save base-model deviance explained
  base.dev.expl <- base.metrics$dev.expl
  base.AIC <- base.metrics$AIC
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + ',
        's(', best.vars[1], ') + ',
        's(', best.vars[2], ') + ',
        's(', best.vars[3], ') + ',
        's(', best.vars[4], ') + ',
        's(', best.vars[5], ') + ',
        's(', best.vars[6], ') + ',
        's(', best.vars[7], ') + ',
        's(', var, ')'
      )
    )
    
    model <- bam(
      model.formula,
      data = fire.df,
      method = 'fREML',
      discrete = TRUE
    )
    
    model.metrics <- get.metrics(
      fitted.model = model,
      model.name = paste0(
        'wy + ',
        best.vars[1],
        ' + ',
        best.vars[2],
        ' + ',
        best.vars[3],
        ' + ',
        best.vars[4],
        ' + ',
        best.vars[5],
        ' + ',
        best.vars[6],
        ' + ',
        best.vars[7],
        ' + ',
        var
      ),
      fire.name = fire.name
    ) %>%
      mutate(
        added_var = var,
        delta_dev_expl = dev.expl - base.dev.expl,
        delta_AIC = AIC - base.AIC
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, delta_AIC) %>%
  select(-model_name)

canopy.results.step.8 <- canopy.results.step

# ---- plot BIC -----

# Put all saved stepwise tables in order
step.results <- list(
  `1` = canopy.results.step.1,
  `2` = canopy.results.step.2,
  `3` = canopy.results.step.3,
  `4` = canopy.results.step.4,
  `5` = canopy.results.step.5,
  `6` = canopy.results.step.6,
  `7` = canopy.results.step.7,
  `8` = canopy.results.step.8
)

model.path <- imap_dfr(
  step.results,
  function(results, step.number) {
    
    if (step.number == '1') {
      
      results %>%
        filter(model_name == 'wy only')
      
    } else {
      
      results %>%
        filter(is.na(added_var))
    }
    
  },
  .id = 'step'
) %>%
  mutate(
    step = as.integer(step),
    
    # Step 1 has zero canopy predictors,
    # Step 2 has one, Step 3 has two, etc.
    canopy_n = step - 1
  )

# check to make sure 4 base models were identified for each step
imap_dfr(
  step.results,
  ~ tibble(
    step = .y,
    n_base_rows = sum(.x$delta_dev_expl == 0, na.rm = TRUE)
  )
)

final.selected <- step.results[[length(step.results)]] %>%
  filter(delta_dev_expl != 0) %>%
  group_by(fire) %>%
  slice_min(
    BIC,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    step = length(step.results) + 1,
    canopy_n = length(step.results)
  )

model.path <- bind_rows(
  model.path,
  final.selected
)

model.path <- model.path %>%
  group_by(fire) %>%
  mutate(
    full_n = max(canopy_n),
    variables_removed = full_n - canopy_n
  ) %>%
  ungroup()

ggplot(
  model.path,
  aes(
    x = variables_removed,
    y = BIC,
    group = 1
  )
) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(
    ~fire,
    scales = 'free_y'
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      max(model.path$variables_removed),
      by = 1
    )
  ) +
  labs(
    x = 'Number of canopy variables removed',
    y = 'BIC',
    title = 'Change in BIC as canopy variables are removed'
  ) +
  theme_bw()

# ----- exploratino using mgcv's build-in shrinkage selection -----
fire.name <- 'castle'

fire.df <- df.50.raw.test %>%
  filter(fire == fire.name) %>%
  droplevels()

selection.model <- bam(
  sqrt(swe_peak) ~
    wy +
    s(gap_percent, k = 10) +
    s(ht_zmax, k = 10) +
    s(ht_zpcum2, k = 10) +
    s(ht_zpcum6, k = 10) +
    s(ht_zpcum9, k = 10) +
    s(ht_zpcum1, k = 10) +
    s(ht_zskew, k = 10) +
    s(ht_zkurt, k = 10) +
    s(gap_dist_to_canopy_mean, k = 10),
  data = fire.df,
  method = 'fREML',
  discrete = TRUE,
  select = TRUE
)

summary(selection.model)

df.50.raw.test %>%
  filter(fire == 'castle') %>%
  distinct(wy)

# basically confirms what we've been seeing

# ----- determine k value -----
fires <- c('castle', 'caldor', 'creek')
# increase k values and compare to determine correct value

# --- topo ---
for (fire.name in fires) {
  
  # create fire-specific df
  fire.df <- df.50.balanced %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  topo <- bam(sqrt(swe_peak) ~ wy + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20),
              data = fire.df,
              method = 'fREML',
              discrete = TRUE)
  print(fire.name)
  print(k.check(topo, subsample = 10000, n.rep = 400))
  plot(topo, pages = 1)
}

# --- canopy --- 
for (fire.name in fires) {
  
  # create fire-specific df
  fire.df <- df.50.balanced %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  
  if (fire.name %in% c('caldor')) {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(gap_percent, k = 20) + s(ht_zpcum2, k = 20) + s(ht_zmax, k = 20) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zpcum6, k = 20),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)
    
  } else if (fire.name == 'castle') {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(ht_zpcum2, k = 20) + s(gap_percent, k = 20) + s(ht_zmax, k = 20) + s(ht_zskew, k = 20),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)
    
  } else if (fire.name == 'creek') {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(ht_zpcum2, k = 20) + s(ht_zmax, k = 20) + s(ht_zskew, k = 20),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE) }
  
  print(fire.name)
  print(k.check(canopy))
  plot(canopy, pages = 1)
}

# --- cbi ---
for (fire.name in fires) {
  
  # create fire-specific df
  fire.df <- df.50.balanced %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  cbi <- bam(sqrt(swe_peak) ~ wy + s(cbibc, k = 20),
             data = fire.df,
             method = 'fREML',
             discrete = TRUE)
  print(fire.name)
  print(k.check(cbi, subsample = 10000, n.rep = 400))
  plot(cbi, pages = 1)
}

# ------------------------- Final Model Comparisons -----------------------
# ----- compare models -----
stage.one.results <- data.frame()

# NOTE: when comparing GAMs with different fixed effects (not smoothed models), use ML as method instead of fREML! fREML can only compare well between models
# that have the same fixed effects
fires <- c('castle', 'caldor', 'creek')

for (fire.name in fires) {
  
  # create fire-specific df
  fire.df <- df.50.balanced %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # --- topo ---
  topo <- bam(sqrt(swe_peak) ~ wy + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20),
              data = fire.df,
              method = 'fREML',
              discrete = TRUE)
  
  # --- canopy --- 
  if (fire.name %in% c('caldor')) {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(gap_percent, k = 20) + s(ht_zpcum2, k = 20) + s(ht_zmax, k = 20) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zpcum6, k = 20),
                                data = fire.df,
                                method = 'fREML',
                                discrete = TRUE)
    
  } else if (fire.name == 'castle') {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(ht_zpcum2, k = 20) + s(gap_percent, k = 20) + s(ht_zmax, k = 20) + s(ht_zskew, k = 20),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)
    
  } else if (fire.name == 'creek') {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(ht_zpcum2, k = 20) + s(ht_zmax, k = 20) + s(ht_zskew, k = 20),
                        data = fire.df,
                        method = 'fREML',
                        discrete = TRUE)
    
  } else if (fire.name == 'dixie') {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(ht_zmax, k = 20) + s(gap_percent, k = 20) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20),
                        data = fire.df,
                        method = 'fREML',
                        discrete = TRUE)
  }
  
  # burned <- bam(sqrt(swe_peak) ~ wy + burned,
  #               data = fire.df,
  #               method = 'fREML',
  #               discrete = TRUE)
  
  cbi <- bam(sqrt(swe_peak) ~ wy + s(cbibc, k = 20),
             data = fire.df,
             method = 'fREML',
             discrete = TRUE)
  
  stage.one.results <- bind_rows(
    stage.one.results,
    get.metrics(topo, 'Topography', fire.name),
    get.metrics(canopy, 'Canopy', fire.name),
    # get.metrics(burned, 'Burned Status', fire.name),
    get.metrics(cbi, 'Burned Severity', fire.name)
  )
  
}

stage.one.results %>%
  arrange(fire, desc(dev.expl))
  
saveRDS(stage.one.results, paste0(dir, 'stage_one_results_swe_k20.rds'))

# plot
ggplot(
  stage.one.results,
  aes(
    x = fire,
    y = dev.expl * 100,
    fill = model_name
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      'Topography' = 'steelblue3',
      'Canopy' = 'forestgreen',
     # 'Burned Status' = 'darkorange2',
      'Burned Severity' = 'firebrick3'
    )
  ) +
  labs(
    x = NULL,
    y = 'Deviance Explained (%)',
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = 'top'
  )












# ==============================================================================
# Stage 2 Modeling - Combined Model
# ==============================================================================
# years common to all fires
common.years <- df.50.raw %>%
  filter(fire != 'dixie') %>%
  distinct(fire, wy) %>%
  count(wy) %>%
  filter(n == 3) %>%   # 3 remaining fires
  pull(wy)

# remove dixie fire and non-common years
df.50 <- df.50.raw %>%
  filter(
    fire != 'dixie',
    wy %in% common.years) %>%
  droplevels()




# --------------- Random Forest to explore interactions ---------------
# ----- create reduced dataset for RF run -----
# create a unique spatial frame
sampling.frame <- df.50 %>%
  distinct(
    fire,
    cell_id,
    x,
    y)

library(purrr)
library(raster)
library(dismo)

grid.size <- 500

set.seed(61)

selected.cells <- sampling.frame %>%
  split(.$fire) %>%
  imap_dfr(
    function(fire.df, fire.name) {
      
      sampling.grid <- raster(
        xmn = min(fire.df$x),
        xmx = max(fire.df$x),
        ymn = min(fire.df$y),
        ymx = max(fire.df$y),
        res = grid.size
      )
      
      selected.xy <- dismo::gridSample(
        x = as.matrix(fire.df[, c('x', 'y')]),
        r = sampling.grid,
        n = 1
      )
      
      fire.df %>%
        semi_join(
          as.data.frame(selected.xy),
          by = c('x', 'y')
        )
    }
  )

# create RF dataset using these selected cells
# dataset is proportional to number of cells per fire

keep.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'ht_zmax',
  'gap_dist_to_canopy_mean',
  'gap_percent',
  'elevation',
  'rad_dtm_accum',
  'slope',
  'aspect_sin',
  'aspect_cos',
  'tpi150',
  'tpi1200',
  'wy',
  'burned',
  'fire',
  'swe_peak'
)

df.rf <- df.50 %>%
  semi_join(
    selected.cells,
    by = c("fire", "cell_id")) %>%
  dplyr::select(all_of(keep.vars)) %>%
  droplevels()


saveRDS(df.rf, 'data/processed/processed/rds/df_rf_50.rds')

# ----- run model and save results -----
library(ranger)

dim(df.rf)
str(df.rf[c('fire', 'wy', 'burned')])
sum(!complete.cases(df.rf))

rf.50 <- ranger(
  sqrt(swe_peak) ~ .,
  data = df.rf,
  num.trees = 500,
  importance = 'permutation',
  num.threads = 2,
  seed = 61
)

saveRDS(rf.50, 'data/processed/processed/rds/rf_50.rds')
rf.50 <- readRDS('data/processed/processed/rds/rf_50.rds')
df.rf <- readRDS('data/processed/processed/rds/df_rf_50.rds')

# variable importance
sort(rf.50$variable.importance, decreasing = TRUE)

# ----- use iml to identify interactions -----
library(iml)

# subset data to speed things up
iml <- df.rf %>%
  group_by(fire, wy, burned) %>%
  slice_sample(n = 80) %>%
  ungroup()

# separate predictors and response
# remove swe peak from predictors
x <- iml %>%
  dplyr::select(-swe_peak)

# create just the response
y <- sqrt(iml$swe_peak)

# Define a prediction function that tells iml how to obtain predictions from a ranger model.
# iml is model-agnostic, so it needs this wrapper function.
ranger.predict <- function(model, newdata) {
  predict(
    model,
    data = newdata)$predictions
}

# Wrap the fitted ranger model in an iml Predictor object.
# This object links together:
#   - the fitted model,
#   - the predictor data,
#   - the response,
#   - and the prediction function.
# It serves as the input for all subsequent iml interpretation methods.
predictor <- Predictor$new(
  model = rf.50,
  data = x,
  y = y,
  predict.function = ranger.predict,
  batch.size = 1000
)





# ----- Calculate Friedman's H-statistic for each predictor -----
# This measures how strongly each variable interacts with all other predictors in the random forest

# settings
options(
  future.globals.maxSize = 15 * 1024^3
)
future::plan(future::sequential)

interaction <- Interaction$new(
  predictor
)

# arrange from high to low
interaction$results %>%
  arrange(desc(.interaction))

interaction <- Interaction$new(
  predictor
)

# arrange from high to low
interaction$results %>%
  arrange(desc(.interaction))

# ----- test interactions -----
# settings
options(
  future.globals.maxSize = 15 * 1024^3
)
future::plan(future::sequential)

# single predictor
Interaction$new(
  predictor.prop,
  feature = 'rad_dtm_accum'
)$results %>%
  arrange(desc(.interaction))

Interaction$new(
  predictor.equal,
  feature = 'elevation'
)$results %>%
  arrange(desc(.interaction))

# function to do a bunch at once
vars <- c(
  'elevation',
  'rad_dtm_accum',
  'ht_zmax',
  'aspect_cos',
  'aspect_sin',
  'gap_percent',
  'ht_zpcum2'
)

interaction.list <- lapply(
  vars,
  function(v) {

    Interaction$new(
      predictor,
      feature = v
    )$results %>%
      mutate(feature = v)

  }
)

interaction.df <- bind_rows(interaction.list)

saveRDS(interaction.df, 'data/processed/processed/rds/rf_interactions_50.rds')

interaction.df <- readRDS('data/processed/processed/rds/rf_equal_interactions_50.rds')

# list top 10 interactions
interaction.df %>%
  filter(.feature != feature) %>%
  arrange(desc(.interaction)) %>%
  slice_head(n = 10)

# ----- pdp plots -----
library(pdp)
library(future)
library(doFuture)

# subset data to speed things up
pdp.prop <- df.rf.prop %>%
  group_by(fire, wy, burned) %>%
  slice_sample(n = 200) %>%
  ungroup()
nrow(pdp.prop)

pdp.equal <- df.rf.equal %>%
  group_by(fire, wy, burned) %>%
  slice_sample(n = 200) %>%
  ungroup()

# make prediction wrapper
pred.ranger <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}


var1 <- 'elevation'
var2 <- 'ht_zmax'

pdp.vars <- pdp::partial(
  object = rf.prop,
  pred.var = c(var1, var2),
  train = pdp.prop,
  pred.fun = pred.ranger,
  grid.resolution = 15,
  ice = FALSE,
  progress = 'text'
)

# only do this if previous gives .id name
pdp.vars.avg <- pdp.vars %>%
  group_by(
    across(all_of(c(var1, var2)))
  ) %>%
  summarize(
    yhat = mean(yhat),
    .groups = 'drop'
  )

names(pdp.vars.avg)

# for continous vars! also may need fixing since assigning var2 and var1
ggplot(
  pdp.elev.zmax.avg,
  aes(
    x = var1,
    y = var2,
    fill = yhat
  )
) +
  geom_tile() +
  geom_contour(
    aes(z = yhat),
    color = 'white',
    linewidth = 0.3
  ) +
  scale_fill_viridis_c(
    name = expression('Predicted ' * sqrt(SWE))
  ) +
  labs(
    x = paste0(var1),
    y = paste0(var2),
    title = paste0('Partial dependence: ', var1, ' × ', var2)
  ) +
  theme_minimal()

names(pdp.elev.zmax)



# plot as surface
library(plotly)

pdp.surface <- pdp.vars.avg %>%
  arrange(
    .data[[var2]],
    .data[[var1]]
  ) %>%
  pivot_wider(
    names_from = all_of(var1),
    values_from = yhat
  )

x.values <- as.numeric(names(pdp.surface)[-1])
y.values <- pdp.surface[[var2]]
z.values <- as.matrix(pdp.surface[, -1])

plot_ly(
  x = x.values,
  y = y.values,
  z = z.values,
  type = 'surface'
) %>%
  layout(
    title = 'Partial dependence: elevation × maximum canopy height',
    scene = list(
      xaxis = list(title = 'Elevation (m)'),
      yaxis = list(title = 'Maximum canopy height (m)'),
      zaxis = list(title = 'Predicted sqrt(SWE)')
    )
  )
# -------------------------- Build Model --------------------------

# ----- create dataset -----
# years common to all fires
common.years <- df.50.raw %>%
  filter(fire != 'dixie') %>%
  distinct(fire, wy) %>%
  count(wy) %>%
  filter(n == 3) %>%   # 3 remaining fires
  pull(wy)

keep.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'ht_zmax',
  'gap_dist_to_canopy_mean',
  'gap_percent',
  'elevation',
  'rad_dtm_accum',
  'slope',
  'aspect_sin',
  'aspect_cos',
  'tpi150',
  'tpi510',
  'tpi2010',
  'tpi1200',
  'wy',
  'burned',
  'fire',
  'swe_peak',
  'fold_id'
)

# remove dixie fire, non-common years, and other variables
df.50 <- df.50.raw %>%
  filter(
    fire != 'dixie',
    wy %in% common.years) %>%
  
  droplevels() %>%
  
  dplyr::select(all_of(keep.vars))



base <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

summary(base)
k.check(base, subsample = 10000, n.rep = 400)
plot(base)

# methods to follow:
# 1. do stepwise with just topo, to determine topo metrics X
# 2. do same with canopy metrics X 
# 3. determine k values for each X
# 4. combine into single model X
# 5. experiment with adding interactions and fixed variables
  # ** make sure if adding fixed variables you change from fREML to ML !


# --------------- Topo Stepwise Selection ---------------
# ----- Stepwise 1 -----


topo.vars <- c(
  'slope',
  'rad_dtm_accum',
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

# elevation baseline
base <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)
  
topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics.combined(
      fitted.model = base,
      model.name = 'elevation'
    ) %>%
      mutate(added_var = NA_character_)
  )
  
  
# test each additional topographic variable
for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(' , var, ', k = 20)'
      )
    )
    
    model <- bam(
      model.formula,
      data = df.50,
      method = 'fREML',
      discrete = TRUE
    )
    
    topo.results.step <- bind_rows(
      topo.results.step,
      get.metrics.combined(
        fitted.model = model,
        model.name = paste0('elevation + ', var)
      ) %>%
        mutate(
          added_var = var
        )
    )
    
  }

topo.results.step <- topo.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared - r.squared[is.na(added_var)]
  )

topo.results.step %>%
  arrange(BIC) 

topo.results.step.1 <- topo.results.step


# ----- stepwise 2 -----

# updated vars
topo.vars <- c(
  'slope',
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

# elevation + radiation baseline
base2 <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20),
  data = df.50,
  method = 'fREML',
  discrete = TRUE
)
  
topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics.combined(
      fitted.model = base,
      model.name = 'topo.elev.rad'
    ) %>%
      mutate(
        added_var = NA_character_
      )
  )
  
# test each additional variable
for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(', var, ', k = 20)')
    )
    
    model <- bam(model.formula,
                 data = df.50,
                 method = 'fREML',
                 discrete = TRUE)
    
    # add results
    topo.results.step <- bind_rows(
      topo.results.step,
      get.metrics.combined(
        fitted.model = model,
        model.name = paste0('topo.elev.rad.', var)
      ) %>%
        mutate(
          added_var = var
        )
    )
    
  }
  
topo.results.step <- topo.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


topo.results.step %>%
  arrange(BIC)

topo.results.step.2 <- topo.results.step



# ----- stepwise 3 -----

# updated vars
topo.vars <- c(
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

# new baseline
base <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE
)

topo.results.step <- bind_rows(
  topo.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in topo.vars) {
  
  model.formula <- as.formula(
    paste0('sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0('base + ', var) 
        ) %>%
      mutate(
        added_var = var
    )
  )
  
}

topo.results.step <- topo.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


topo.results.step %>%
  arrange(BIC)

topo.results.step.3 <- topo.results.step

# ----- stepwise 4 -----

# updated vars
topo.vars <- c(
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_cos'
)

topo.results.step <- data.frame()

# new baseline
base <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE
)

topo.results.step <- bind_rows(
  topo.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in topo.vars) {
  
  model.formula <- as.formula(
    paste0('sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0('base + ', var) 
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

topo.results.step <- topo.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


topo.results.step %>%
  arrange(BIC)

topo.results.step.4 <- topo.results.step

# ----- stepwise 5 -----

# updated vars
topo.vars <- c(
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_cos'
)

topo.results.step <- data.frame()

# new baseline
base <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(tpi150, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE
)

topo.results.step <- bind_rows(
  topo.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in topo.vars) {
  
  model.formula <- as.formula(
    paste0('sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(tpi150, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0('base + ', var)
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

topo.results.step <- topo.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


topo.results.step %>%
  arrange(BIC)

topo.results.step.5 <- topo.results.step

# ----- stepwise 6 -----

# updated vars
topo.vars <- c(
  'tpi510',
  'tpi1200',
  'aspect_cos'
)

topo.results.step <- data.frame()

# new baseline
base <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(tpi150, k = 20) + s(tpi2010, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE
)

topo.results.step <- bind_rows(
  topo.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in topo.vars) {
  
  model.formula <- as.formula(
    paste0('sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(tpi150, k = 20) + s(tpi2010, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0('base + ', var)
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

topo.results.step <- topo.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


topo.results.step %>%
  arrange(BIC)

topo.results.step.6 <- topo.results.step



# ----- plot BIC -----

# saved stepwise tables in order
step.results <- list(
  `1` = topo.results.step.1,
  `2` = topo.results.step.2,
  `3` = topo.results.step.3,
  `4` = topo.results.step.4,
  `5` = topo.results.step.5,
  `6` = topo.results.step.6
)

# extract baseline model from each step
model.path <- imap_dfr(
  step.results,
  function(results, step.number) {
    
    results %>%
      filter(is.na(added_var))
    
  },
  .id = 'step'
) %>%
  mutate(
    step = as.integer(step),
    
    # step 1 = elevation
    # step 2 = elevation + radiation
    # etc.
    predictor_n = step
  )

# check that exactly one baseline model was identified per step
imap_dfr(
  step.results,
  ~ tibble(
    step = .y,
    n_base_rows = sum(is.na(.x$added_var))
  )
)

# best candidate from final step
final.selected <- step.results[[length(step.results)]] %>%
  filter(!is.na(added_var)) %>%
  slice_min(
    BIC,
    n = 1,
    with_ties = FALSE
  ) %>%
  mutate(
    step = length(step.results) + 1,
    predictor_n = length(step.results) + 1
  )

# add final candidate to model path
model.path <- bind_rows(
  model.path,
  final.selected
)

# plot
ggplot(
  model.path,
  aes(
    x = predictor_n,
    y = BIC,
    group = 1
  )
) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(
      1,
      max(model.path$predictor_n),
      by = 1
    )
  ) +
  labs(
    x = 'Number of topographic predictors',
    y = 'BIC',
    title = 'BIC by number of topographic predictors'
  ) +
  theme_bw()









# ---------- Topo k-value Selection ----------
topo <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) 
            + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

summary(topo)
k.check(topo, subsample = 10000, n.rep = 400)
plot(topo, pages = 1)

# --------------- Canopy Stepwise Selection ---------------
# ----- Stepwise 1 -----
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'ht_zmax',
  'gap_dist_to_canopy_mean',
  'gap_percent'
)

canopy.results.step <- data.frame()

# baseline
base.formula <- 'sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 20)'

base <- bam(as.formula(base.formula),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

canopy.results.step <- bind_rows(
  canopy.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(added_var = NA_character_)
)


# test each additional canopy variable
for (var in canopy.vars) {
  
  model.formula <- as.formula(
    paste0(base.formula, ' + s(' , var, ', k = 20)'
    )
  )
  
  model <- bam(
    model.formula,
    data = df.50,
    method = 'fREML',
    discrete = TRUE
  )
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0('base + ', var)
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

canopy.results.step <- canopy.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared - r.squared[is.na(added_var)]
  )

canopy.results.step %>%
  arrange(BIC) 

canopy.results.step.1 <- canopy.results.step


# ----- stepwise 2 -----

# updated vars
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'gap_dist_to_canopy_mean',
  'gap_percent'
)


canopy.results.step <- data.frame()

# new baseline formula
new.base.formula <- paste0(base.formula, ' + s(ht_zmax, k = 20)')
# new baseline model
base7 <- bam(as.formula(new.base.formula),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

canopy.results.step <- bind_rows(
  canopy.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in canopy.vars) {
  
  model.formula <- as.formula(
    paste0(new.base.formula, ' + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0(' + ', var)
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

canopy.results.step <- canopy.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


canopy.results.step %>%
  arrange(BIC)

canopy.results.step.2 <- canopy.results.step



# ----- stepwise 3 -----

# updated vars
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'gap_dist_to_canopy_mean'
)


canopy.results.step <- data.frame()

# new baseline formula
new.base.formula <- paste0(base.formula, ' + s(ht_zmax, k = 20) + s(gap_percent, k = 20)')
# new baseline model
base8 <- bam(as.formula(new.base.formula),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

canopy.results.step <- bind_rows(
  canopy.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in canopy.vars) {
  
  model.formula <- as.formula(
    paste0(new.base.formula, ' + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0(' + ', var)
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

canopy.results.step <- canopy.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


canopy.results.step %>%
  arrange(BIC)

canopy.results.step.3 <- canopy.results.step

# ----- stepwise 4 -----

# updated vars
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt'
)


canopy.results.step <- data.frame()

# new baseline formula
new.base.formula <- paste0(base.formula, ' + s(ht_zmax, k = 20) + s(gap_percent, k = 20) + s(gap_dist_to_canopy_mean, k = 20)')
# new baseline model
base9 <- bam(as.formula(new.base.formula),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

canopy.results.step <- bind_rows(
  canopy.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in canopy.vars) {
  
  model.formula <- as.formula(
    paste0(new.base.formula, ' + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0(' + ', var)
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

canopy.results.step <- canopy.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


canopy.results.step %>%
  arrange(BIC)

canopy.results.step.4 <- canopy.results.step

# ----- stepwise 5 -----

# updated vars
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zkurt'
)


canopy.results.step <- data.frame()

# new baseline formula
new.base.formula <- paste0(base.formula, ' + s(ht_zmax, k = 20) + s(gap_percent, k = 20) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20)')
# new baseline model
base10 <- bam(as.formula(new.base.formula),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

canopy.results.step <- bind_rows(
  canopy.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in canopy.vars) {
  
  model.formula <- as.formula(
    paste0(new.base.formula, ' + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0(' + ', var)
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

canopy.results.step <- canopy.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


canopy.results.step %>%
  arrange(BIC)

canopy.results.step.5 <- canopy.results.step

# ----- stepwise 6 -----

# updated vars
canopy.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zkurt'
)


canopy.results.step <- data.frame()

# new baseline formula
new.base.formula <- paste0(base.formula, ' + s(ht_zmax, k = 20) + s(gap_percent, k = 20)', 
                           ' + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20) + s(ht_zpcum2, k = 20)')
# new baseline model
base11 <- bam(as.formula(new.base.formula),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

canopy.results.step <- bind_rows(
  canopy.results.step,
  get.metrics.combined(
    fitted.model = base,
    model.name = 'base'
  ) %>%
    mutate(
      added_var = NA_character_
    )
)

# test each additional variable
for (var in canopy.vars) {
  
  model.formula <- as.formula(
    paste0(new.base.formula, ' + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.50,
               method = 'fREML',
               discrete = TRUE)
  
  # add results
  canopy.results.step <- bind_rows(
    canopy.results.step,
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0(' + ', var)
    ) %>%
      mutate(
        added_var = var
      )
  )
  
}

canopy.results.step <- canopy.results.step %>%
  mutate(
    BIC.base = BIC[is.na(added_var)],
    delta.BIC = BIC - BIC.base,
    delta.r.squared =
      r.squared -
      r.squared[is.na(added_var)]
  )


canopy.results.step %>%
  arrange(BIC)

canopy.results.step.6 <- canopy.results.step

# ----- plot BIC -----

# saved stepwise tables in order
step.results <- list(
  `1` = canopy.results.step.1,
  `2` = canopy.results.step.2,
  `3` = canopy.results.step.3,
  `4` = canopy.results.step.4,
  `5` = canopy.results.step.5,
  `6` = canopy.results.step.6
 # `7` = canopy.results.step.7
)

# extract baseline model from each step
model.path <- imap_dfr(
  step.results,
  function(results, step.number) {
    
    results %>%
      filter(is.na(added_var))
    
  },
  .id = 'step'
) %>%
  mutate(
    step = as.integer(step),
    
    # step 1 = elevation
    # step 2 = elevation + radiation
    # etc.
    predictor_n = step
  )

# check that exactly one baseline model was identified per step
imap_dfr(
  step.results,
  ~ tibble(
    step = .y,
    n_base_rows = sum(is.na(.x$added_var))
  )
)

# best candidate from final step
final.selected <- step.results[[length(step.results)]] %>%
  filter(!is.na(added_var)) %>%
  slice_min(
    BIC,
    n = 1,
    with_ties = FALSE
  ) %>%
  mutate(
    step = length(step.results) + 1,
    predictor_n = length(step.results) + 1
  )

# add final candidate to model path
model.path <- bind_rows(
  model.path,
  final.selected
)

# plot
ggplot(
  model.path,
  aes(
    x = predictor_n,
    y = BIC,
    group = 1
  )
) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(
      1,
      max(model.path$predictor_n),
      by = 1
    )
  ) +
  labs(
    x = 'Number of canopy predictors',
    y = 'BIC',
    title = 'BIC by number of canopy predictors'
  ) +
  theme_bw()









# ---- plot BIC improvements through whole stepwise -----
# saved stepwise tables in order
step.models <- list(
  `1` = base,
  `2` = base2,
  `3` = base3,
  `4` = base4,
  `5` = base5,
  `6` = base.topo,
  `7` = base7,
  `8` = base8,
  `9` = base9,
  `10` = base10,
  `11` = base11,
  `12` = canopy
)

# extract metrics from each fitted model
model.path <- imap_dfr(
  step.models,
  function(model, step.number) {
    
    get.metrics.combined(
      fitted.model = model,
      model.name = paste0('model_', step.number)
    ) %>%
      mutate(
        predictor_n = as.integer(step.number),
        stage = if_else(predictor_n <= 6,
                        'Topography',
                        'Canopy')
      )
  }
)

ggplot(
  model.path,
  aes(
    x = predictor_n,
    y = BIC,
    color = stage
  )
) +
  geom_line(
    aes(group = 1),
    color = 'grey50'
  ) +
  geom_point(size = 3) +
  geom_vline(
    xintercept = 6.5,
    linetype = 'dashed'
  ) +
  scale_x_continuous(
    breaks = seq(
      1,
      max(model.path$predictor_n),
      by = 1
    )
  ) +
  labs(
    x = 'Model Step',
    y = 'BIC',
    color = 'Predictor group',
    title = 'BIC during combined-model variable selection'
  ) +
  theme_bw()

# --- Delta BIC ---
model.path <- model.path %>%
  arrange(predictor_n) %>%
  mutate(
    delta.BIC.previous = BIC - lag(BIC)
  )

ggplot(
  model.path %>% filter(!is.na(delta.BIC.previous)),
  aes(
    x = predictor_n,
    y = delta.BIC.previous,
    fill = stage
  )
) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(
    breaks = 2:max(model.path$predictor_n)
  ) +
  labs(
    x = 'Model step',
    y = expression(Delta*'BIC from previous model'),
    fill = 'Predictor group',
    title = 'Change in BIC during combined-model variable selection'
  ) +
  theme_bw()

# quantify
model.path %>%
  arrange(predictor_n) %>%
  mutate(
    delta.BIC.previous = BIC - lag(BIC),
    delta.r2.previous = r.squared - lag(r.squared),
    delta.dev.previous = dev.expl - lag(dev.expl)
  ) %>%
  dplyr::select(
    predictor_n,
    stage,
    BIC,
    delta.BIC.previous,
    r.squared,
    delta.r2.previous,
    dev.expl,
    delta.dev.previous
  )

# ---------- Canopy k-value Selection ----------
canopy <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10)
              + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20)
              + s(ht_zskew, k = 20),
              data = df.50,
              method = 'fREML',
              discrete = TRUE)

k.check(canopy, subsample = 10000, n.rep = 10000)
plot(canopy, pages = 3)

# ------------------- new base model -----------------------------
canopy <- bam(sqrt(swe_peak) ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10)
              + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20)
              + s(ht_zskew, k = 20),
              data = df.50,
              method = 'fREML',
              discrete = TRUE)
# ------------------- Model Diagnostics ----------------------------

# --- concurvity ---
concurvity(canopy, full = TRUE) 
conc <- concurvity(canopy, full = FALSE)
round(conc$estimate, 2)

# --- Residuals check ---
set.seed(61)

resid.df <- tibble(
  fitted = fitted(canopy),
  residual = residuals(canopy, type = 'deviance')
) %>%
  slice_sample(n = 20000)

ggplot(
  resid.df,
  aes(
    x = fitted,
    y = residual
  )
) +
  geom_point(alpha = 0.15) +
  geom_hline(yintercept = 0) +
  geom_smooth(
    method = 'loess',
    se = FALSE
  ) +
  labs(
    x = 'Fitted values',
    y = 'Deviance residuals'
  ) +
  theme_bw()




# ------------------- Testing Interactions ------------------------------
# ----- base -----
interaction.results <- data.frame()

base.A <- bam(sqrt(swe_peak) ~ wy + fire + burned + s(elevation, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10)
            + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20)
            + s(ht_zskew, k = 20),
              data = df.50,
              method = 'fREML',
              discrete = TRUE)



# baseline main-effects model
interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = base.A,
    model.name = 'base'
  )
)

rf.interactions <- readRDS('data/processed/processed/rds/rf_interactions_50.rds')
rf.interactions <-  rf.interactions %>%
  filter(.feature != feature) %>%
  arrange(desc(.interaction))

rf.interactions[1:20]

# ----- Primary Interactions -----

# fire-specific elevation relationship
# s(elevation, by = fire, k = 20)
elev.fire <- bam(sqrt(swe_peak) ~ wy + fire + burned + s(elevation, by = fire, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10)
            + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20)
            + s(ht_zskew, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = elev.fire,
    model.name = 'fire x elevation'
  )
)

# water-year-specific elevation relationship
# s(elevation, by = wy, k = 20)
elev.wy <- bam(sqrt(swe_peak) ~ wy + fire + burned + s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10)
            + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20)
            + s(ht_zskew, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = elev.wy,
    model.name = 'wy x elevation'
  )
)

# elevation by wy and fire
# s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20)
elev.fire.wy <- bam(sqrt(swe_peak) ~ wy + fire + burned + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10)
                    + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20)
                    + s(ht_zskew, k = 20),
                    data = df.50,
                    method = 'fREML',
                    discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = elev.fire.wy,
    model.name = 'elevation x wy, elevation x fire'
  )
)

# keep this model going forward as new base

# ----- Canopy-Specific interactions -----
interaction.results <- data.frame()

base.B <- bam(sqrt(swe_peak) ~ wy + fire + burned 
              + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
              + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
              + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20),
                    data = df.50,
                    method = 'fREML',
                    discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = base.B,
    model.name = 'new base with elevation x wy & fire'
  )
)

# par(mfrow = c(1,1))
# summary(base.2)
plot(base.2, scale = 0, select = 15)

# fire-specific canopy-height relationship
# s(ht_zmax, by = fire, k = 10)
zmax.fire <- bam(sqrt(swe_peak) ~ wy + fire + burned 
            + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
            + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
            + s(ht_zmax, by = fire, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20),
            data = df.50,
            method = 'fREML',
            discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = zmax.fire,
    model.name = 'zmax x fire'
  )
)

# adding percent gap & burned interaction
# + burned + s(gap_percent, by = burned)
gap.burned <- bam(sqrt(swe_peak) ~ wy + fire + burned
                         + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
                         + s(rad_dtm_accum, k = 10)  + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                         + s(ht_zmax, k = 10) + s(gap_percent, by = burned, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20),
                         data = df.50,
                         method = 'fREML',
                         discrete = TRUE)

# summary(gap.burned)
# 
par(mfrow = c(2, 2))
plot(
  gap.burned,
  pages = 5,
  scale = 0
)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = gap.burned,
    model.name = 'gap x burned'
  )
)

interaction.results

# add percent gap/ gap dist to canopy interaction
pgap.gapdist <- bam(sqrt(swe_peak) ~ wy + fire + burned
                             + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
                             + s(rad_dtm_accum, k = 10)  + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                             + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20) 
                             + ti(gap_dist_to_canopy_mean, gap_percent, k = c(10, 10)),
                             data = df.50,
                             method = 'fREML',
                             discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = pgap.gapdist,
    model.name = 'percent gap x dist to canopy'
  )
)

par(mfrow = c(1, 1))
plot.gam(pgap.gapdist, select = 17, scheme = 1, too.far = 0.05, n2 = 80, theta = 0)
vis.gam(x = pgap.gapdist, view = c('gap_dist_to_canopy_mean', 'gap_percent'), plot.type = 'persp', too.far = 0.05, theta = 90)
vis.gam(x = pgap.gapdist, view = c('gap_dist_to_canopy_mean', 'gap_percent'), plot.type = 'persp', too.far = 0.05, theta = 180)
vis.gam(x = pgap.gapdist, view = c('gap_dist_to_canopy_mean', 'gap_percent'), plot.type = 'persp', too.far = 0.05, theta = 270)
vis.gam(x = pgap.gapdist, view = c('gap_dist_to_canopy_mean', 'gap_percent'), plot.type = 'persp', too.far = 0.05, theta = 360)

interaction.results


# zmax x burned
zmax.burned <- bam(sqrt(swe_peak) ~ wy + fire + burned 
              + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
              + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
              + s(ht_zmax, by = burned, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20),
              data = df.50,
              method = 'fREML',
              discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = zmax.burned,
    model.name = 'zmax x burned'
  )
)

tpi1200.zmax <- bam(sqrt(swe_peak) ~ wy + fire + burned 
              + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
              + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
              + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20)
              + ti(tpi1200, ht_zmax, k = c(10, 10)),
              data = df.50,
              method = 'fREML',
              discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = tpi1200.zmax,
    model.name = 'tpi1200 x zmax'
  )
)

gapdist.zmax <- bam(sqrt(swe_peak) ~ wy + fire + burned 
              + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
              + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
              + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20)
              + ti(gap_dist_to_canopy_mean, ht_zmax, k = c(10, 10)),
              data = df.50,
              method = 'fREML',
              discrete = TRUE)

interaction.results <- bind_rows(
  interaction.results,
  get.metrics.combined(
    fitted.model = gapdist.zmax,
    model.name = 'gap dist to canopy x zmax'
  )
)

interaction.results


# -------------------- Model Evaluation ------------------------

# ----- verify structure -----
table(df.50$fold_id)

df.50 %>%
  count(fire, fold_id)
# ----- 5-fold spatial cross-validation function -----

cv_bam <- function(formula, data, k_folds = 5) {
  
  # empty dataframes to store results
  cv.results <- data.frame()
  cv.fire.results <- data.frame()
  
  # loop through each spatial fold
  for (fold in 1:k_folds) {
    
    # use all other folds to train the model
    train <- data %>%
      filter(fold_id != fold)
    
    # hold out the current fold for model evaluation
    test <- data %>%
      filter(fold_id == fold)
    
    # fit the GAM to the training data
    model <- bam(
      formula,
      data = train,
      method = 'fREML',
      discrete = TRUE
    )
    
    # predict sqrt(SWE) for observations in the held-out fold
    pred <- predict(
      model,
      newdata = test
    )
    
    # observed response on the same sqrt scale as the model
    obs <- sqrt(test$swe_peak)
    
    # ----- overall fold metrics -----
    
    rmse <- sqrt(
      mean(
        (obs - pred)^2,
        na.rm = TRUE
      )
    )
    
    r2 <- cor(
      obs,
      pred,
      use = 'complete.obs'
    )^2
    
    cv.results <- bind_rows(
      cv.results,
      data.frame(
        fold = fold,
        RMSE = rmse,
        R2 = r2
      )
    )
    
    # ----- fire-specific metrics -----
    
    fire.results <- test %>%
      mutate(
        obs = sqrt(swe_peak),
        pred = pred
      ) %>%
      group_by(fire) %>%
      summarize(
        n = n(),
        RMSE = sqrt(
          mean(
            (obs - pred)^2,
            na.rm = TRUE
          )
        ),
        R2 = cor(
          obs,
          pred,
          use = 'complete.obs'
        )^2,
        .groups = 'drop'
      ) %>%
      mutate(
        fold = fold
      )
    
    # save fire-specific fold results
    cv.fire.results <- bind_rows(
      cv.fire.results,
      fire.results
    )
  }
  
  # summarize overall performance across folds
  cv.summary <- cv.results %>%
    summarize(
      RMSE_mean = mean(RMSE),
      RMSE_sd = sd(RMSE),
      R2_mean = mean(R2),
      R2_sd = sd(R2)
    )
  
  # return all results
  list(
    fold.results = cv.results,
    fire.results = cv.fire.results,
    summary = cv.summary
  )
}

# ----- model formulas to test -----
formula.A <- as.formula('sqrt(swe_peak) ~ wy + fire + burned + s(elevation, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20)')

formula.B <- as.formula('sqrt(swe_peak) ~ wy + fire + burned + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20)')

formula.C <- as.formula('sqrt(swe_peak) ~ wy + fire + burned + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, by = fire, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20)')

formula.D <- as.formula('sqrt(swe_peak) ~ wy + fire + burned + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) + s(rad_dtm_accum, k = 10)  + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, by = burned, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20)')

# run 
cv.A <- cv_bam(
  formula = formula.A,
  data = df.50
)

cv.B <- cv_bam(
  formula = formula.B,
  data = df.50
)

cv.C <- cv_bam(
  formula = formula.C,
  data = df.50
)

cv.D <- cv_bam(
  formula = formula.D,
  data = df.50
)

cv.A
cv.B
cv.C
cv.D

# ==============================================================================
# Stage 2 Modeling - Model Results
# ==============================================================================
# ----- winning model -----
model.swe <- bam(sqrt(swe_peak) ~ wy + fire + burned 
              + s(elevation, by = wy, k = 20) + s(elevation, by = fire, k = 20) 
              + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
              + s(ht_zmax, by = fire, k = 10) + s(gap_percent, by = fire, k = 10) + s(gap_dist_to_canopy_mean, by = fire, k = 20) + s(ht_zskew, by = fire, k = 20),
              data = df.50,
              method = 'fREML',
              discrete = TRUE)

# ----- get stats -----
summary(model.swe)

plot(
  model.swe,
  pages = 2,
  scheme = 1,
  scale = 0,
  residuals = FALSE
)

k.check(
  model.swe,
  subsample = 10000,
  n.rep = 400
)

# ----- test with smooth spatial term -----
fires <- c('caldor', 'castle', 'creek')
models <- list()
models.xy <- list()

for (fire.name in fires) {
  
  df <- df.50 %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  model.swe <- bam(sqrt(swe_peak) ~ wy + burned 
                   + s(elevation, by = wy, k = 20) + s(elevation, k = 20)
                   + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                   + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20),
                   data = df,
                   method = 'fREML',
                   discrete = TRUE)

  model.swe.xy <- bam(sqrt(swe_peak) ~ wy + burned 
                    + s(elevation, by = wy, k = 20) + s(elevation, k = 20) + s(x,y, k = 100)
                    + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10) + s(tpi150, k = 10) + s(tpi2010, k = 10) 
                    + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20) + s(ht_zskew, k = 20),
                    data = df,
                    method = 'fREML',
                    discrete = TRUE)
  
  # save models by fire
  models[[fire.name]] <- model.swe
  models.xy[[fire.name]] <- model.swe.xy
  
  # diagnostics
  print(fire.name)
  
  print(summary(model.swe))
  print(summary(model.swe.xy))
  
  print(k.check(
    model.swe,
    subsample = 10000,
    n.rep = 10000
  ))
  
  print(k.check(
    model.swe.xy,
    subsample = 10000,
    n.rep = 10000
  ))
  
}

# ----- corr plot for predictors -----
# years common to all fires
common.years <- df.50.raw %>%
  filter(fire != 'dixie') %>%
  distinct(fire, wy) %>%
  count(wy) %>%
  filter(n == 3) %>%   # 3 remaining fires
  pull(wy)

keep.vars <- c(
  'ht_zpcum2',
  'ht_zmax',
  'gap_dist_to_canopy_mean',
  'gap_percent',
  'elevation',
  'rad_dtm_accum',
  'slope',
  'aspect_sin',
  'tpi150',
  'tpi2010'
)

# remove dixie fire, non-common years, and other variables
df.50.corr <- df.50.raw %>%
  filter(
    fire != 'dixie',
    wy %in% common.years) %>%
  
  droplevels() %>%
  
  dplyr::select(all_of(keep.vars))

cor.mat <- cor(
  df.50.corr,
  use = 'complete.obs',
  method = 'pearson'
)

round(cor.mat, 2)

library(corrplot)

corrplot(
  cor.mat,
  method = 'color',
  type = 'upper',
  order = 'hclust',
  addCoef.col = 'black',
  tl.col = 'black',
  tl.srt = 45,
  diag = FALSE
)


# ----- concurvity again -----
concurvity(model.swe, full = TRUE) 
conc <- concurvity(model.swe, full = FALSE)
round(conc$estimate, 2)
