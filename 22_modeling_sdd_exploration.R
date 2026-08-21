packages <- c('tidymodels', 'dplyr', 'tidyr', 'lme4', 'lmtest', 'ranger', 'tictoc', 'mgcv', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# Initialize Dataframe
# ==============================================================================
# get dataframe
set.seed(61)
dir <- 'data/processed/processed/rds/' 

df.500.raw <- readRDS(file.path(dir, 'df_500m_raw.rds'))
df.500.balanced.0 <- readRDS(file.path(dir, 'df_500m_raw_balanced.rds')) 

df.500 <- df.500.raw %>%
  mutate(
    fire = factor(
      fire,
      levels = c('caldor', 'castle', 'creek'),
      labels = c('Caldor', 'Castle', 'Creek') # capitalize
    )) %>%
  droplevels()

df.500.balanced <- df.500.balanced.0 %>%
  mutate(
    fire = factor(
      fire,
      levels = c('caldor', 'castle', 'creek'),
      labels = c('Caldor', 'Castle', 'Creek') # capitalize
    )) %>%
  droplevels()



burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)

# helper function
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

# ----- plot SDD distributions for all fires -----

for (f in unique(df.500.raw$fire)) {
  
  p <- df.500.raw %>%
    filter(fire == f) %>%
    ggplot(aes(x = sdd)) +
    geom_density(
      fill = 'steelblue',
      alpha = 0.4,
      na.rm = TRUE
    ) +
    facet_wrap(~wy, scales = 'free') +
    labs(
      title = tools::toTitleCase(f),
      x = 'Snow Disappearance Day (DOY)',
      y = 'Density'
    ) +
    theme_bw()
  
  print(p)
}

# ==============================================================================
# Stage 1 Modeling - Single family predictors
# ==============================================================================
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

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation baseline
  model.elev <- bam(sdd ~
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
        'sdd ~ wy + s(elevation, k = 10) + s(' , var, ', k = 10)'
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
  arrange(fire, desc(dev.expl)) %>%
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

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad <- bam(
    sdd ~ wy + s(elevation) + s(rad_dtm_accum),
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
      paste0('sdd ~ wy + s(elevation) + s(rad_dtm_accum) + 
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
  arrange(fire, desc(dev.expl))

topo.results.step.2 <- topo.results.step

# slope performs best in 3/4

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

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad.slope <- bam(
    sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope),
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics(
      fitted.model = topo.elev.rad.slope,
      model.name = 'topo.elev.rad.slope',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + 
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
        model.name = paste0('topo.elev.rad.slope', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

topo.results.step %>%
  arrange(fire, desc(dev.expl))

topo.results.step.3 <- topo.results.step

# ----- stepwise 4 ------
# updated vars
topo.vars <- c(
  'tpi150',
  'tpi510',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

topo.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # elevation + radiation baseline
  topo.elev.rad.slope.tpi <- bam(
    sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + s(tpi1200),
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  topo.results.step <- bind_rows(
    topo.results.step,
    get.metrics(
      fitted.model = topo.elev.rad.slope.tpi,
      model.name = 'topo.elev.rad.slope.tpi.',
      fire.name = fire.name
    )
  )
  
  # test each additional variable
  for (var in topo.vars) {
    
    model.formula <- as.formula(
      paste0('sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + s(tpi1200) +
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
        model.name = paste0('topo.elev.rad.slope.tpi.', var),
        fire.name = fire.name
        
        
      )
    )
    
  }
  
}

topo.results.step %>%
  arrange(fire, desc(dev.expl))

topo.results.step.4 <- topo.results.step






# ------------------------- Canopy-only Model ------------------------------
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

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # wy only baseline
  base.model <- bam(
    sdd ~ wy,
    data = fire.df,
    method = 'fREML',
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
      paste0('sdd ~ wy +  
             s(', var, ')')
    )
    
    model <- bam(model.formula,
                 data = fire.df,
                 method = 'fREML',
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
canopy.results.step <- canopy.results.step %>%
  group_by(fire) %>%
  mutate(
    base.dev.expl = dev.expl[model_name == 'wy only'],
    delta_dev_expl = dev.expl - base.dev.expl
  ) %>%
  ungroup()

canopy.results.step.1 <- canopy.results.step

canopy.results.step %>%
  arrange(fire, desc(delta_dev_expl)) %>%
  print(n = Inf)

canopy.results.step.1 <- canopy.results.step

# ----- stepwise 2 -----

best.var.lookup <- c(
  caldor = 'gap_dist_to_canopy_mean',
  castle = 'ht_zkurt',
  creek = 'ht_zpcum2',
  dixie = 'ht_zskew'
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify first-selected canopy variable
  best.var <- unname(best.var.lookup[fire.name])
  
  # remove selected variable from candidates
  remaining.vars <- setdiff(canopy.vars, best.var)
  
  # base formula: WY + selected canopy variable
  base.formula <- as.formula(
    paste0(
      'sdd ~ wy + s(', best.var, ')'
    )
  )
  
  # fit base model
  base.model <- bam(
    base.formula,
    data = fire.df,
    method = 'fREML',
    discrete = TRUE
  )
  
  # get base-model metrics
  base.metrics <- get.metrics(
    fitted.model = base.model,
    model.name = paste0('wy + ', best.var),
    fire.name = fire.name
  ) %>%
    mutate(
      added_var = NA_character_,
      delta_dev_expl = 0
    )
  
  # save base deviance explained
  base.dev.expl <- base.metrics$dev.expl
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sdd ~ wy + ',
        's(', best.var, ') + ',
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
        best.var,
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

canopy.results.step.2 <- canopy.results.step


# ----- stepwise 3 -----

best.var.lookup <- list(
  caldor = c('gap_dist_to_canopy_mean', 'ht_zkurt'),
  castle = c('ht_zkurt','gap_percent'),
  creek = c('ht_zpcum2','ht_zmax'),
  dixie = c('ht_zskew', 'ht_zpcum1')
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sdd ~ wy + ',
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
        'sdd ~ wy + ',
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


# ----- stepwise 4 -----

best.var.lookup <- list(
  caldor = c('gap_dist_to_canopy_mean', 'ht_zkurt', 'ht_zpcum6'),
  castle = c('ht_zkurt','gap_percent', 'ht_zmax'),
  creek = c('ht_zpcum2', 'ht_zmax', 'gap_percent'),
  dixie = c('ht_zskew', 'ht_zpcum1', 'gap_percent')
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sdd ~ wy + ',
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
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sdd ~ wy + ',
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

canopy.results.step.4 <- canopy.results.step





# ----- stepwise 5 -----

best.var.lookup <- list(
  caldor = c('gap_dist_to_canopy_mean', 'ht_zkurt', 'ht_zpcum6', 'ht_zmax'),
  castle = c('ht_zkurt','gap_percent', 'ht_zmax', 'ht_zskew'),
  creek = c('ht_zpcum2', 'ht_zmax', 'gap_percent', 'ht_zpcum6'),
  dixie = c('ht_zskew', 'ht_zpcum1', 'gap_percent', 'ht_zpcum2')
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sdd ~ wy + ',
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
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sdd ~ wy + ',
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
        delta_dev_expl = dev.expl - base.dev.expl
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

canopy.results.step.5 <- canopy.results.step

# ----- stepwise 6 -----

best.var.lookup <- list(
  caldor = c('gap_dist_to_canopy_mean', 'ht_zkurt', 'ht_zpcum6', 'ht_zmax', 'gap_percent'),
  castle = c('ht_zkurt','gap_percent', 'ht_zmax', 'ht_zskew', 'ht_zpcum2'),
  creek = c('ht_zpcum2', 'ht_zmax', 'gap_percent', 'ht_zpcum6', 'ht_zskew'),
  dixie = c('ht_zskew', 'ht_zpcum1', 'gap_percent', 'ht_zpcum2', 'ht_zkurt')
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sdd ~ wy + ',
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
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sdd ~ wy + ',
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
        delta_dev_expl = dev.expl - base.dev.expl
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, BIC) %>%
  select(-model_name)

canopy.results.step.6 <- canopy.results.step

# ----- stepwise 7 -----

best.var.lookup <- list(
  caldor = c('gap_dist_to_canopy_mean', 'ht_zkurt', 'ht_zpcum6', 'ht_zmax', 'gap_percent', 'ht_zpcum9'),
  castle = c('ht_zkurt','gap_percent', 'ht_zmax', 'ht_zskew', 'ht_zpcum2', 'ht_zpcum6'),
  creek = c('ht_zpcum2', 'ht_zmax', 'gap_percent', 'ht_zpcum6', 'ht_zskew', 'ht_zkurt'),
  dixie = c('ht_zskew', 'ht_zpcum1', 'gap_percent', 'ht_zpcum2', 'ht_zkurt', 'gap_dist_to_canopy_mean')
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sdd ~ wy + ',
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
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sdd ~ wy + ',
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
        delta_dev_expl = dev.expl - base.dev.expl
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step.7 <- canopy.results.step

canopy.results.step.7 %>%
  arrange(fire, BIC) %>%
  select(-model_name)

canopy.results.step.7 <- canopy.results.step

# ----- stepwise 8 -----

best.var.lookup <- list(
  caldor = c('gap_dist_to_canopy_mean', 'ht_zkurt', 'ht_zpcum6', 'ht_zmax', 'gap_percent', 'ht_zpcum9', 'ht_zskew'),
  castle = c('ht_zkurt','gap_percent', 'ht_zmax', 'ht_zskew', 'ht_zpcum2', 'ht_zpcum6', 'gap_dist_to_canopy_mean'),
  creek = c('ht_zpcum2', 'ht_zmax', 'gap_percent', 'ht_zpcum6', 'ht_zskew', 'ht_zkurt', 'gap_dist_to_canopy_mean'),
  dixie = c('ht_zskew', 'ht_zpcum1', 'gap_percent', 'ht_zpcum2', 'ht_zkurt', 'gap_dist_to_canopy_mean', 'ht_zmax')
)

canopy.results.step <- data.frame()

for (fire.name in unique(df.500.raw$fire)) {
  
  # create fire-specific df
  fire.df <- df.500.raw %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # identify the best-selected canopy vars for each fire
  best.vars <- best.var.lookup[[fire.name]]
  
  # remove the selected variables from the candidate variables
  remaining.vars <- setdiff(canopy.vars, best.vars)
  
  # base formula: WY + previously selected canopy variables
  base.formula <- as.formula(
    paste0(
      'sdd ~ wy + ',
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
  
  canopy.results.step <- bind_rows(
    canopy.results.step,
    base.metrics
  )
  
  # models with each additional variable
  for (var in remaining.vars) {
    
    model.formula <- as.formula(
      paste0(
        'sdd ~ wy + ',
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
        delta_dev_expl = dev.expl - base.dev.expl
      )
    
    canopy.results.step <- bind_rows(
      canopy.results.step,
      model.metrics
    )
  }
}

canopy.results.step %>%
  arrange(fire, BIC)%>%
  select(-model_name)

canopy.results.step.8 <- canopy.results.step

# ---- plot AIC / BIC -----

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


# ------------------------- Final Model Comparisons -----------------------
# ----- compare models -----
fires <- c('Castle', 'Caldor', 'Creek')

df <- df.500 # df.500 OR df.500.balanced
stage.one.results <- data.frame()

for (fire.name in fires) {
  
  # create fire-specific df
  fire.df <- df %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # --- topo ---
  topo <- bam(sdd ~ wy + s(elevation) + s(rad_dtm_accum) + s(slope) + s(aspect_sin) + s(tpi1200),
              data = fire.df,
              method = 'fREML',
              discrete = TRUE)
  
  # --- canopy --- 
   
  if (fire.name == 'Caldor') {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(gap_dist_to_canopy_mean) + s(ht_zkurt) + s(ht_zpcum6) + s(ht_zmax) + s(gap_percent),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)
    
  } else if (fire.name == 'Castle') {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(ht_zkurt) + s(gap_percent) + s(ht_zmax) + s(ht_zskew) + s(ht_zpcum2),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)
    
  } else if (fire.name == 'Creek') {
    
    canopy <- bam(sqrt(swe_peak) ~ wy + s(ht_zpcum2) + s(ht_zmax) + s(gap_percent) + s(ht_zpcum6),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)  
  }
  # } else if (fire.name == 'dixie') {
  #   canopy <- bam(sqrt(swe_peak) ~ wy + s(ht_zskew) + s(ht_zpcum1) + s(gap_percent) + s(ht_zpcum2) + s(ht_zkurt) + s(gap_dist_to_canopy_mean),
  #                 data = fire.df,
  #                 method = 'fREML',
  #                 discrete = TRUE)
  # }
  
  # burned <- bam(sdd ~ wy + burned,
  #               data = fire.df,
  #               method = 'fREML',
  #               discrete = TRUE)
  
  cbi <- bam(sdd ~ wy + s(cbibc),
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

# saveRDS(stage.one.results, paste0(dir, 'stage_one_results_sdd.rds'))

# --------------- plot results ---------------
# ----- deviance explained -----
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
      'Burned Status' = 'darkorange2',
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

# ----- R2 -----
ggplot(
  stage.one.results,
  aes(
    x = fire,
    y = r.squared * 100,
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
      'Burned Status' = 'darkorange2',
      'Burned Severity' = 'firebrick3'
    )
  ) +
  labs(
    x = NULL,
    y = 'R Squared',
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = 'top'
  )

# ==============================================================================
# GAM
# ==============================================================================
# ----- null -----
null <- bam(
  sdd ~ wy,
  data = df.500,
  method = 'REML'
)

summary(null)
results <- rbind(
  results,
  get.metrics(null, "Null")
)

# ----- topo -----
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
  method = 'REML'
)

summary(topo)
results <- rbind(
  results,
  get.metrics(topo, "Topo")
)

# ----- cbi and burned -----
name <- 'topo + cbi'
m3 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(cbibc),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m3, name)
)

name <- 'topo + burned'
m4 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    burned,
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m4, name)
)



# ----- check all canopy metrics to find best ones -----
metrics <- c(
  "cover_canopy_open_2m",
  "cover_cover_2m",
  "cover_pzabove5",
  "cover_pzabove10",
  "cover_ground_frac",
  "gap_gap_pct",
  "gap_dist_to_gap_mean",
  "gap_dist_to_canopy_mean",
  "gap_dist_to_canopy_max",
  "rad_dsm_melt",
  names(df.500)[grepl("^ht_", names(df.500))]
)

results.metrics <- data.frame()

for (metric in metrics) {
  
  message("Running ", metric)
  
  form <- as.formula(
    paste(
      "sdd ~",
      "factor(wy) +",
      "s(topo_elev) +",
      "s(rad_dtm_melt) +",
      "s(rad_dtm_accum) +",
      "s(topo_slope) +",
      "s(topo_tpi1200) +",
      "s(topo_tpi2010) +",
      paste0("s(", metric, ")")
    )
  )
  
  mod <- bam(
    form,
    data = df.500,
    method = "fREML",
    discrete = TRUE
  )
  
  s <- summary(mod)
  
  results.metrics <- rbind(
    results.metrics,
    data.frame(
      metric = metric,
      r.squared = s$r.sq,
      dev.expl = s$dev.expl,
      AIC = AIC(mod),
      edf = tail(s$edf, 1)
    )
  )
}

results.metrics <- results.metrics[
  order(-results.metrics$dev.expl),
]

results.metrics

cor(df.500$ht_zmax, df.500$ht_zpcum9, use = 'complete.obs')


# ----- canopy metric models -----
name <- 'topo + gap(by b)'
m5 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m5, name)
)

name <- 'topo + zmax'
m6 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(ht_zmax),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m6, name)
)

name <- 'topo + zpcum9'
m7 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(ht_zpcum9),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m7, name)
)

name <- 'topo + gap(by b) + zmax(by b)'
m8 <- bam(
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
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m8, name)
)

name <- 'topo + gap(by b) + zpcum9(by b)'
m9 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zpcum9, by = burned),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m9, name)
)

name <- 'topo + gap(by b) + zpcum9(by b) + zmax(by b)'
m10 <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zpcum9, by = burned) +
    s(ht_zmax, by = burned),
  data = df,
  method = 'REML'
)
results <- rbind(
  results,
  get.metrics(m10, name)
)



# ------------------ stepwise for canopy metrics --------------------
# ----- stepwise 1 -----
# fit base model with the 2 variables we know we want to include
canopy.gap.ht <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

model.formulas <- list(
  
  canopy.gap.ht =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned),
  
  canopy.gap.ht.zpcum1 =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum1, by = burned)
  ,
  
  canopy.gap.ht.zpcum2 =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned)
  ,
  
  canopy.gap.ht.zsd =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zsd, by = burned)
  ,
  
  canopy.gap.ht.zq95 =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zq95, by = burned)
  ,
  
  canopy.gap.ht.groundfrac =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(cover_ground_frac, by = burned)
  ,
  
  canopy.gap.ht.disttocanopymean =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(gap_dist_to_canopy_mean, by = burned)
  ,
  
  canopy.gap.ht.zskew =
    sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zskew, by = burned)
)

# run each model 
results.canopy.stepwise <- list()
for (m in names(model.formulas)) {
  
  fit <- bam(
    model.formulas[[m]],
    data = df.500,
    method = 'fREML',
    discrete = TRUE
  )
  
  s <- summary(fit)
  
  results.canopy.stepwise[[length(results.canopy.stepwise) + 1]] <- data.frame(
    model = m,
    aic = AIC(fit),
    r2 = s$r.sq,
    dev_expl = s$dev.expl,
    edf = sum(s$edf)
  )
}


results.canopy.stepwise <- bind_rows(results.canopy.stepwise) %>%
  mutate(
    delta_aic = aic - AIC(canopy.gap.ht)
  ) %>%
  arrange(delta_aic)

results.canopy.stepwise


# ----- stepwise 2 -----
base.fit <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned),
  data = df.500,
  method = 'fREML',
  discrete = TRUE
)

base.formula <- formula(base.fit)

candidates <- c(
  'ht_zskew',
  'cover_ground_frac',
  'gap_dist_to_canopy_mean',
  'ht_zq95',
  'ht_zsd',
  'ht_zpcum1'
)

results.canopy.stepwise.2 <- list()

for (v in candidates) {
  
  f.new <- update(
    base.formula,
    as.formula(paste0('. ~ . + s(', v, ', by = burned)'))
  )
  
  fit <- bam(
    f.new,
    data = df.500,
    method = 'fREML',
    discrete = TRUE
  )
  
  s <- summary(fit)
  
  results.canopy.stepwise.2[[v]] <- data.frame(
    variable = v,
    aic = AIC(fit),
    r2 = s$r.sq,
    dev_expl = s$dev.expl,
    edf = sum(s$edf)
  )
}

results.canopy.stepwise.2 <- bind_rows(results.canopy.stepwise.2) %>%
  mutate(
    delta_aic = aic - AIC(base.fit)
  ) %>%
  arrange(delta_aic)

results.canopy.stepwise.2

canopy.gap.ht.zpcum2.zpcum1 <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(ht_zpcum1, by = burned),
  data = df.500,
  method = "fREML",
  discrete = TRUE)


# ----- stepwise 3 -----
base.fit <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(ht_zpcum1, by = burned),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

candidates <- c(
  'ht_zskew',
  'ht_zq95',
  'ht_zsd',
  'cover_ground_frac',
  'gap_dist_to_canopy_mean'
)

results.canopy.stepwise.3 <- list()

for (v in candidates) {
  
  f.new <- update(
    base.formula,
    as.formula(paste0('. ~ . + s(', v, ', by = burned)'))
  )
  
  fit <- bam(
    f.new,
    data = df.500,
    method = 'fREML',
    discrete = TRUE
  )
  
  s <- summary(fit)
  
  results.canopy.stepwise.3[[v]] <- data.frame(
    variable = v,
    aic = AIC(fit),
    r2 = s$r.sq,
    dev_expl = s$dev.expl,
    edf = sum(s$edf)
  )
}

results.canopy.stepwise.3 <- bind_rows(results.canopy.stepwise.3) %>%
  mutate(
    delta_aic = aic - AIC(base.fit)
  ) %>%
  arrange(delta_aic)

results.canopy.stepwise.3


# ----- final canopy-only model after stepwise -----
canopy <- bam(
  sdd ~
    wy +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    s(ht_zpcum2, by = burned) +
    s(ht_zpcum1, by = burned),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy, "canopy")
)

results <- data.frame()
# -------------------- base models -------------------------------
# ----- canopy -----
canopy <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(ht_zpcum1),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy, "canopy")
)

# ----- rad.dsm -----
rad.dsm <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dsm_accum) +
    s(rad_dsm_melt),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(rad.dsm, "rad.dsm")
)

# ----- cbi -----
cbi <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(cbibc),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(cbi, "cbi")
)

# ----- burn status -----
burned <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    burned,
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(burned, "burn status")
)

# ----- topo -----
topo <- bam(
  sdd ~
    factor(wy) +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010),
  data = df.500,
  method = "fREML",
  discrete = TRUE
)
results <- rbind(
  results,
  get.metrics(topo, "topo")
)

# ----- canopy + severity -----
canopy.cbi <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(gap_gap_pct) +
    s(ht_zmax) +
    s(ht_zpcum2) +
    s(cbibc) +
    s(ht_zpcum1),
  data = df.500,
  method = "fREML",
  discrete = TRUE)

results <- rbind(
  results,
  get.metrics(canopy.cbi, "canopy + severity")
)


# ----- spatial smooth -----
spatial <- bam(
  sdd ~ factor(wy) +
    s(x, y, bs = 'tp', k = 200),
  data = df.500,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(spatial, "spatial")
)

# ----- wy only -----

wy <- bam(
  sdd ~ wy +
  s(topo_elev),
  data = df.500,
  method = 'fREML',
  discrete = TRUE
)

results <- rbind(
  results,
  get.metrics(wy, "wy")
)

results 

singe.type.model.results.sdd <- results

# ----- plot comparing model results -----
# ----- AFTER adding elev to base models -----
plot.df <- data.frame(
  model = c(
    'WY + Elevation only',
    'WY + Elevation + CBI',
    'WY + Elevation + Canopy',
    'WY + Elevation + Topography',
    'WY + Spatial'
  ),
  r2 = c(
    0.79,
    0.80,
    0.83,
    0.82,
    0.88
  )
)

plot.df$model <- factor(
  plot.df$model,
  levels = rev(plot.df$model)
)

ggplot(
  plot.df,
  aes(x = model, y = r2, fill = model)
) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      'WY + Elevation only' = 'cyan4',
      'WY + Elevation + CBI' = 'darkkhaki',
      'WY + Elevation + Canopy' = 'darkseagreen4',
      'WY + Elevation + Topography' = 'darkslategrey',
      'WY + Spatial' = 'goldenrod2'
    )
  ) +
  theme_bw() +
  theme(
    legend.position = 'none'
  ) +
  labs(
    x = NULL,
    y = expression('Adjusted '*R^2)
  )

# ----- AFTER adding elev - plot with both SWE and SDD results-----
plot.df <- data.frame(
  model = rep(
    c(
      'WY + Elevation only',
      'WY + Elevation + CBI',
      'WY + Elevation + Canopy',
      'WY + Elevation + Topography',
      'WY + Spatial'
    ),
    2
  ),
  response = rep(
    c('SWE', 'SDD'),
    each = 5
  ),
  r2 = c(
    # SWE
    0.76,
    0.76,
    0.77,
    0.81,
    0.84,
    
    # SDD
    0.79,
    0.80,
    0.83,
    0.82,
    0.88
  )
)

plot.df$model <- factor(
  plot.df$model,
  levels = c(
    'WY + Elevation only',
    'WY + Elevation + CBI',
    'WY + Elevation + Canopy',
    'WY + Elevation + Topography',
    'WY + Spatial'
  )
)

plot.df$fill.grp <- paste(plot.df$model, plot.df$response)

ggplot(
  plot.df,
  aes(
    x = model,
    y = r2,
    fill = fill.grp
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      
      # WY
      'WY + Elevation only SWE' = 'cyan4',
      'WY + Elevation only SDD' = 'cyan2',
      
      # CBI
      'WY + Elevation + CBI SWE' = 'darkkhaki',
      'WY + Elevation + CBI SDD' = 'khaki2',
      
      # Canopy
      'WY + Elevation + Canopy SWE' = 'darkseagreen4',
      'WY + Elevation + Canopy SDD' = 'darkseagreen2',
      
      # Topography
      'WY + Elevation + Topography SWE' = 'darkslategrey',
      'WY + Elevation + Topography SDD' = 'darkslategray3',
      
      # Spatial
      'WY + Spatial SWE' = 'goldenrod4',
      'WY + Spatial SDD' = 'goldenrod2'
    ),
    labels = c(
      'WY + Elevation only (SWE)',
      'WY + Elevation only (SDD)',
      'CBI + Elevation (SWE)',
      'CBI + Elevation (SDD)',
      'Canopy + Elevation (SWE)',
      'Canopy + Elevation (SDD)',
      'Topography + Elevation (SWE)',
      'Topography + Elevation (SDD)',
      'Spatial (SWE)',
      'Spatial (SDD)'
    )
  ) +
  geom_text(
    aes(label = round(r2, 3)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  expand_limits(y = 0.95) +
  theme_bw() +
  labs(
    x = NULL,
    y = expression('Adjusted '*R^2),
    fill = NULL
  ) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )
# ----- BEFORE adding elev to base models-----
plot.df <- data.frame(
  model = c(
    'WY only',
    'WY + CBI',
    'WY + Canopy',
    'WY + Topography',
    'WY + Spatial'
  ),
  r2 = c(
    0.270,
    0.543,
    0.720,
    0.819,
    0.882
  )
)

plot.df$model <- factor(
  plot.df$model,
  levels = rev(plot.df$model)
)

ggplot(
  plot.df,
  aes(x = model, y = r2, fill = model)
) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      'WY only' = 'cyan4',
      'WY + CBI' = 'darkkhaki',
      'WY + Canopy' = 'darkseagreen4',
      'WY + Topography' = 'darkslategrey',
      'WY + Spatial' = 'goldenrod2'
    )
  ) +
  theme_bw() +
  theme(
    legend.position = 'none'
  ) +
  labs(
    x = NULL,
    y = expression('Adjusted '*R^2)
  )

# ----- BEFORE adding elev - plot with both SWE and SDD results-----
plot.df <- data.frame(
  model = rep(
    c(
      'WY only',
      'WY + CBI',
      'WY + Canopy',
      'WY + Topography',
      'WY + Spatial'
    ),
    2
  ),
  response = rep(
    c('SWE', 'SDD'),
    each = 5
  ),
  r2 = c(
    # SWE
    0.47,
    0.59,
    0.63,
    0.81,
    0.84,
    
    # SDD
    0.27,
    0.54,
    0.72,
    0.82,
    0.88
  )
)

plot.df$model <- factor(
  plot.df$model,
  levels = c(
    'WY only',
    'WY + CBI',
    'WY + Canopy',
    'WY + Topography',
    'WY + Spatial'
  )
)

plot.df$fill.grp <- paste(plot.df$model, plot.df$response)

ggplot(
  plot.df,
  aes(
    x = model,
    y = r2,
    fill = fill.grp
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      
      # WY
      'WY only SWE' = 'cyan4',
      'WY only SDD' = 'cyan2',
      
      # CBI
      'WY + CBI SWE' = 'darkkhaki',
      'WY + CBI SDD' = 'khaki2',
      
      # Canopy
      'WY + Canopy SWE' = 'darkseagreen4',
      'WY + Canopy SDD' = 'darkseagreen2',
      
      # Topography
      'WY + Topography SWE' = 'darkslategrey',
      'WY + Topography SDD' = 'darkslategray3',
      
      # Spatial
      'WY + Spatial SWE' = 'goldenrod4',
      'WY + Spatial SDD' = 'goldenrod2'
    ),
    labels = c(
      'WY only (SWE)',
      'WY only (SDD)',
      'CBI (SWE)',
      'CBI (SDD)',
      'Canopy (SWE)',
      'Canopy (SDD)',
      'Topography (SWE)',
      'Topography (SDD)',
      'Spatial (SWE)',
      'Spatial (SDD)'
    )
  ) +
  geom_text(
    aes(label = round(r2, 3)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  expand_limits(y = 0.95) +
  theme_bw() +
  labs(
    x = NULL,
    y = expression('Adjusted '*R^2),
    fill = NULL
  ) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )
# ------------------- figure out best topo-canopy model -----------------
# ----- models with interactions -----
model.formulas.sdd.interactions <- list(
  
  no.interactions = 
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
  
  gap.elev = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(topo_elev, gap_gap_pct, by = burned),
  
  gap.rad.accum = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_accum, gap_gap_pct, by = burned),
  
  gap.rad.melt = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_melt, gap_gap_pct, by = burned),
  
  gap.rad.accum.melt = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_accum, gap_gap_pct, by = burned) +
    ti(rad_dtm_melt, gap_gap_pct, by = burned),
  
  gap.elev.gap.rad = 
    sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) +
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) + 
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(rad_dtm_accum, gap_gap_pct, by = burned) + 
    ti(topo_elev, gap_gap_pct, by = burned)
  
)


# ----- best topo-canopy model -----

topo.canopy <- bam(
  sdd ~
    wy +
    s(topo_elev) +
    s(rad_dtm_accum) + 
    s(rad_dtm_melt) +
    s(topo_slope) +
    s(topo_tpi1200) +
    s(topo_tpi2010) +
    s(gap_gap_pct, by = burned) +
    s(ht_zmax, by = burned) +
    ti(topo_elev, gap_gap_pct, by = burned),
  data = df.500,
  method = 'REML'
)

# ==============================================================================
# Model Comparisons under k-fold validation
# ==============================================================================
# ------ model formulas -----
model.formulas <- list(
  topo.cbi = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(cbibc),
  
  topo.burned = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      burned,
  
  topo.gap.b = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(gap_gap_pct, by = burned),


  topo.gap.b.zmax.b = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(gap_gap_pct, by = burned) +
      s(ht_zmax, by = burned),


  topo.gap.b.zpcum9.b = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(gap_gap_pct, by = burned) +
      s(ht_zpcum9, by = burned),


  topo.gap.b.zpcum9.b.zmax.b = sdd ~
      wy +
      s(topo_elev) +
      s(rad_dtm_accum) + 
      s(rad_dtm_melt) +
      s(topo_slope) +
      s(topo_tpi1200) +
      s(topo_tpi2010) +
      s(gap_gap_pct, by = burned) +
      s(ht_zpcum9, by = burned) +
      s(ht_zmax, by = burned)
)

# ----- 5-fold cross validation -----

# set what model set you want to test
sdd.k.results <- list()

k.results <- sdd.k.results

model.formulas.set <- model.formulas.sdd.interactions

for (fold in 1:5) {
  
  train <- filter(df.500, fold_id != fold)
  test  <- filter(df.500, fold_id == fold)
  
  for (m in names(model.formulas.set)) {
    
    fit <- bam(
      model.formulas.set[[m]],
      data = train,
      method = "fREML",
      discrete = TRUE
    )
    
    pred <- predict(fit, newdata = test)
    
    obs <- test$sdd
    
    rmse <- sqrt(mean((pred - obs)^2))
    mae  <- mean(abs(pred - obs))
    
    
    r2 <- 1 - sum((obs - pred)^2) /
      sum((obs - mean(obs))^2)
    
    k.results[[length(k.results)+1]] <- data.frame(
      fold = fold,
      model = m,
      r2 = r2,
      rmse = rmse,
      mae = mae
    )
  }
}

k.results <- bind_rows(k.results)

summary.table <- k.results %>%
  group_by(model) %>%
  summarise(
    r2_mean        = mean(r2),
    rmse_mean      = mean(rmse),
    mae_mean       = mean(mae),
    .groups = "drop"
  ) %>%
  arrange(rmse_mean)

summary.table


# ----- correlation matrix -----
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

cor.mat <- cor(
  df.500.raw[, canopy.vars],
  use = 'complete.obs',
  method = 'pearson'
)

round(cor.mat, 2)

library(corrplot)

cor.mat <- cor(
  df.500.raw[, canopy.vars],
  use = 'complete.obs'
)

corrplot(
  cor.mat,
  method = 'color',
  type = 'upper',
  order = 'hclust',
  addCoef.col = 'black',
  tl.col = 'black',
  tl.srt = 45,
  number.cex = 0.7
)


# extra
swe <- rast('data/processed/processed/tif/50m/dixie/dixie_swe_peak_50m.tif')
names(swe)
plot(swe[[1]])
writeRaster(swe[[1]], 'data/processed/processed/tif/50m/dixie/dixie_swe_peak_50m_2023_temp.tif')
