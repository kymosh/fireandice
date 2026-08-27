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

# years common to all fires
common.years <- df.500.raw %>%
  filter(fire != 'dixie') %>%
  distinct(fire, wy) %>%
  count(wy) %>%
  filter(n == 3) %>%   # 3 remaining fires
  pull(wy)

# remove dixie fire and non-common years
df.500 <- df.500.raw %>%
  filter(
    fire != 'dixie',
    wy %in% common.years) %>%
  mutate(
    fire = recode(
      fire,
      'caldor' = 'Caldor',
      'creek' = 'Creek'
    )
  ) %>%
  droplevels()

df.500.balanced <- df.500.balanced.0 %>%
  filter(fire != 'dixie') %>%
  mutate(
    fire = factor(
      fire,
      levels = c('caldor', 'Castle', 'creek'),
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

# ----- correlation matrix -----
all.vars <- c(
  'ht_zpcum6',
  'ht_zpcum9',
  'ht_zpcum1',
  'ht_zpcum2',
  'ht_zskew',
  'ht_zkurt',
  'ht_zmax',
  'gap_dist_to_canopy_mean',
  'gap_percent',
  'slope',
  'rad_dtm_accum',
  'tpi150',
  'tpi510',
  'tpi1200',
  'tpi2010',
  'aspect_sin',
  'aspect_cos'
)

df.500.matrix <- df.500 %>%
  select(all_of(all.vars))

cor.matrix <- cor(df.500.matrix, use = 'pairwise.complete.obs', method = 'pearson')

library(corrplot)

corrplot(
  cor.matrix,
  method = 'color',
  type = 'upper',
  order = 'hclust',
  addCoef.col = 'black',
  number.cex = 0.6,
  tl.cex = 0.8,
  tl.col = 'black',
  diag = FALSE
)
  

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
    
    canopy <- bam(sdd ~ wy + s(gap_dist_to_canopy_mean) + s(ht_zkurt) + s(ht_zpcum6) + s(ht_zmax) + s(gap_percent),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)
    
  } else if (fire.name == 'Castle') {
    
    canopy <- bam(sdd ~ wy + s(ht_zkurt) + s(gap_percent) + s(ht_zmax) + s(ht_zskew) + s(ht_zpcum2),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)
    
  } else if (fire.name == 'Creek') {
    
    canopy <- bam(sdd ~ wy + s(ht_zpcum2) + s(ht_zmax) + s(gap_percent) + s(ht_zpcum6),
                  data = fire.df,
                  method = 'fREML',
                  discrete = TRUE)  
  }
  # } else if (fire.name == 'dixie') {
  #   canopy <- bam(sdd ~ wy + s(ht_zskew) + s(ht_zpcum1) + s(gap_percent) + s(ht_zpcum2) + s(ht_zkurt) + s(gap_dist_to_canopy_mean),
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
# Stage 2 Modeling - Combined Model
# ==============================================================================
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
base <- bam(sdd ~ wy + fire + s(elevation, k = 20),
            data = df.500,
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
      'sdd ~ wy + fire + s(elevation, k = 20) + s(' , var, ', k = 20)'
    )
  )
  
  model <- bam(
    model.formula,
    data = df.500,
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
base <- bam(sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20),
             data = df.500,
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
    paste0('sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.500,
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
  'tpi2010',
  'aspect_sin',
  'aspect_cos',
  'slope'
)

topo.results.step <- data.frame()

# new baseline
base <- bam(sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(tpi1200, k = 20),
            data = df.500,
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
    paste0('sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(tpi1200, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.500,
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
  'aspect_cos',
  'slope'
)

topo.results.step <- data.frame()

# new baseline
base <- bam(sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(tpi1200, k = 20) + s(aspect_sin, k = 20),
            data = df.500,
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
    paste0('sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(tpi1200, k = 20) + s(aspect_sin, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.500,
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
  'aspect_cos'
)

topo.results.step <- data.frame()

# new baseline
base <- bam(sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 20),
            data = df.500,
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
    paste0('sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.500,
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
base <- bam(sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(aspect_cos, k = 20) + s(tpi2010, k = 20),
            data = df.500,
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
    paste0('sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 20) + s(slope, k = 20) + s(aspect_sin, k = 20) + s(tpi150, k = 20) + s(tpi2010, k = 20) + s(', var, ', k = 20)')
  )
  
  model <- bam(model.formula,
               data = df.500,
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
topo <- bam(sdd ~ wy + fire + s(elevation, k = 10) + s(rad_dtm_accum, k = 10) + s(tpi1200, k = 10) + s(aspect_sin, k = 10),
            data = df.500,
            method = 'fREML',
            discrete = TRUE)

summary(topo)
k.check(topo, subsample = 10000, n.rep = 400)
plot(topo, pages = 1, scale = 0)

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
base.formula <- 'sdd ~ wy + fire + s(elevation, k = 10) + s(rad_dtm_accum, k = 10) + s(aspect_sin, k = 10) + s(tpi1200, k = 10)'

base <- bam(as.formula(base.formula),
            data = df.500,
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
    data = df.500,
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
base <- bam(as.formula(new.base.formula),
             data = df.500,
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
               data = df.500,
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
  'gap_percent'
)


canopy.results.step <- data.frame()

# new baseline formula
new.base.formula <- paste0(base.formula, ' + s(ht_zmax, k = 20) + s(gap_dist_to_canopy_mean, k = 20)')
# new baseline model
base <- bam(as.formula(new.base.formula),
             data = df.500,
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
               data = df.500,
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
base <- bam(as.formula(new.base.formula),
             data = df.500,
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
               data = df.500,
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
base <- bam(as.formula(new.base.formula),
              data = df.500,
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
               data = df.500,
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
base <- bam(as.formula(new.base.formula),
              data = df.500,
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
               data = df.500,
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
canopy <- bam(sdd ~ wy + fire + s(elevation, k = 20) + s(rad_dtm_accum, k = 10) + s(slope, k = 10) + s(aspect_sin, k = 10)
              + s(tpi150, k = 10) + s(tpi2010, k = 10) + s(ht_zmax, k = 10) + s(gap_percent, k = 10) + s(gap_dist_to_canopy_mean, k = 20)
              + s(ht_zskew, k = 20),
              data = df.500,
              method = 'fREML',
              discrete = TRUE)

k.check(canopy, subsample = 10000, n.rep = 10000)
plot(canopy, pages = 3)







