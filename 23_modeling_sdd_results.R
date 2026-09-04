# ==============================================================================
# Setup
# ==============================================================================
# ----- read in and initialze dfs -----
# get dataframe
set.seed(61)
dir <- 'data/processed/processed/rds/' 

df.500.raw <- readRDS(file.path(dir, 'df_500m_raw.rds'))

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
      'creek' = 'Creek'),
    fire_burned = interaction(fire, burned, sep = '_')
  ) %>%
  droplevels()

# make balanced prediction sample
df.pred <- df.500 %>%
  group_by(fire, wy, burned) %>%
  slice_sample(n = 500) %>%
  ungroup()

# --- new df with the 3 elevation bands ---
df.elev <- df.500 %>%
  mutate(
    elev_band = cut(
      elevation,
      breaks = c(-Inf, 1750, 2500, Inf),
      labels = c(
        '< 1750 m',
        '1750–2500 m',
        '> 2500 m'
      ),
      right = FALSE
    )
  )

# --- representative elevation from all 3 fires ---
elev.values <- df.elev %>%
  group_by(elev_band) %>%
  summarise(
    elevation = median(elevation, na.rm = TRUE),
    .groups = 'drop'
  )




burn.cols <- c(
  'unburned' = '#00868B',
  'burned' = '#EE2C2C'
)

fire.colors <- c(
  'Caldor' = '#3b435c',
  'Castle' = '#ffa600',
  'Creek' = '#c55488'
)

model.colors <- c(
  'Fire Severity' = '#915984',
  'Canopy' = '#98ba3c',
  'Topography' = '#009ec4'
)

elev.colors <- c(
  'Caldor_< 1750 m' = '#3b435c',
  'Caldor_1750–2500 m' = '#868eaa',
  'Caldor_> 2500 m' = '#d8e1ff',
  
  'Castle_< 1750 m' = '#ffa600',
  'Castle_1750–2500 m' = '#ffc171',
  'Castle_> 2500 m' = '#fbddbe',
  
  'Creek_< 1750 m' = '#c55488',
  'Creek_1750–2500 m' = '#e396b8',
  'Creek_> 2500 m' = '#ffd5e8'
)

# ----- helper functions -----
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

cv_bam_sdd <- function(formula, data, k_folds = 5) {
  
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
    
    # fit GAM to training data
    model <- bam(
      formula,
      data = train,
      method = 'fREML',
      discrete = TRUE
    )
    
    # predict response for held-out fold
    pred <- predict(
      model,
      newdata = test,
      type = 'response'
    )
    
    # get observed response exactly as specified
    # on the left side of the model formula
    obs <- eval(
      formula[[2]],
      envir = test
    )
    
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
        obs = obs,
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

# ----- balanced dataset -----
df.500.balanced.0 <- readRDS(file.path(dir, 'df_500m_raw_balanced.rds')) 
df.500.balanced <- df.500.balanced.0 %>%
  filter(fire != 'dixie') %>%
  mutate(
    fire = factor(
      fire,
      levels = c('caldor', 'Castle', 'creek'),
      labels = c('Caldor', 'Castle', 'Creek') # capitalize
    )) %>%
  droplevels()



# ----- models -----
# model.sdd <- bam(sdd ~ wy * fire +
#                    s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) + 
#                    s(ht_zmax, by = fire, k = 20) + s(gap_percent, by = fire, k = 20) + 
#                    s(swe_peak, k = 20),
#                  data = df.500,
#                  method = 'fREML',
#                  discrete = TRUE)
# 
# model.sdd.combined <- bam(sdd ~ wy * fire + 
#                    s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) + 
#                    s(ht_zmax, k = 20) + s(gap_percent, k = 20) +
#                    s(swe_peak, k = 20),
#                  data = df.500,
#                  method = 'fREML',
#                  discrete = TRUE)
# 
# model.sdd.burned <- bam(sdd ~ wy * fire + burned * fire +
#                   s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) +
#                   s(ht_zmax, by = fire_burned, k = 20) + s(gap_percent, by = fire_burned, k = 20) +
#                   s(swe_peak, k = 20),
#                 data = df.500,
#                 method = 'fREML',
#                 discrete = TRUE)
# 
# model.sdd.burned.simple <- bam(sdd ~ wy * fire + burned * fire +
#                           s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) +
#                           s(ht_zmax, by = fire, k = 20) + s(gap_percent, by = fire, k = 20) +
#                           s(swe_peak, k = 20),
#                         data = df.500,
#                         method = 'fREML',
#                         discrete = TRUE)

# OLD
# model.sdd.gapbyfire <- bam(sdd ~ wy * fire +
#                    s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) +
#                    s(gap_percent, by = fire, k = 20) +
#                    ti(gap_percent, elevation, k = c(10, 10)) +
#                    s(swe_peak, k = 20),
#                  data = df.500,
#                  method = 'fREML',
#                  discrete = TRUE)

# FINAL
model.sdd <- bam(sdd ~ wy * fire +
                   s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) + 
                   + s(gap_percent, k = 20) +
                   ti(gap_percent, elevation, k = c(10, 10)) +
                   s(swe_peak, k = 20),
                 data = df.500,
                 method = 'fREML',
                 discrete = TRUE)

model.sdd.burned <- bam(sdd ~ wy * fire + burned * fire +
                   s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) + 
                   + s(gap_percent, by = burned, k = 20) +
                   ti(gap_percent, elevation, k = c(10, 10)) +
                   s(swe_peak, k = 20),
                 data = df.500,
                 method = 'fREML',
                 discrete = TRUE)



# ==============================================================================
# Model Evaluation/
# ==============================================================================
# ----- Cross-fold Validation -----
# -- simple model ---
cv.sdd <- cv_bam_sdd(formula = formula(model.sdd),
                 data = df.500,
                 k_folds = 5)

# fire-specific summary
cv.sdd.summary.byfire <- cv.sdd$fire.results %>%
  group_by(fire) %>%
  summarise(
    RMSE_mean = mean(RMSE),
    RMSE_sd = sd(RMSE),
    R2_mean = mean(R2),
    R2_sd = sd(R2),
    .groups = 'drop'
  )

# overall summary
cv.sdd.summary.overall <- cv.sdd$fold.results %>%
  summarise(
    fire = 'Overall',
    RMSE_mean = mean(RMSE),
    RMSE_sd = sd(RMSE),
    R2_mean = mean(R2),
    R2_sd = sd(R2)
  )

# combine
cv.sdd.summary <- bind_rows(
  cv.sdd.summary.overall,
  cv.sdd.summary.byfire %>%
    mutate(fire = as.character(fire))
)

cv.sdd.summary

# --- burned model ---
cv.sdd <- cv_bam_sdd(formula = formula(model.sdd.burned),
                 data = df.500,
                 k_folds = 5)

# fire-specific summary
cv.sdd.summary.byfire <- cv.sdd$fire.results %>%
  group_by(fire) %>%
  summarise(
    RMSE_mean = mean(RMSE),
    RMSE_sd = sd(RMSE),
    R2_mean = mean(R2),
    R2_sd = sd(R2),
    .groups = 'drop'
  )

# overall summary
cv.sdd.summary.overall <- cv.sdd$fold.results %>%
  summarise(
    fire = 'Overall',
    RMSE_mean = mean(RMSE),
    RMSE_sd = sd(RMSE),
    R2_mean = mean(R2),
    R2_sd = sd(R2)
  )

# combine
cv.sdd.summary <- bind_rows(
  cv.sdd.summary.overall,
  cv.sdd.summary.byfire %>%
    mutate(fire = as.character(fire))
)

cv.sdd.summary


# ==============================================================================
# Generate Predictions - Fire-specific
# ==============================================================================
# ----------------------------------- ** MARGINAL EFFECT PLOTS ** ------------------------------------
# --------------- predictions using by=fire model ---------------
# --------------- gap percent ----------------
set.seed(61)

# simulate coefficient draws once
n.sim <- 500

beta.sim <- MASS::mvrnorm(
  n = n.sim,
  mu = coef(model.sdd),
  Sigma = vcov(model.sdd)
)

gap.pred <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
  # prediction sample for fire
  df.fire <- df.pred %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # prediction range
  gap.seq <- seq(
    quantile(
      df.500$gap_percent[df.500$fire == fire.name],
      0.01,
      na.rm = TRUE
    ),
    quantile(
      df.500$gap_percent[df.500$fire == fire.name],
      0.99,
      na.rm = TRUE
    ),
    length.out = 100
  )
  
  map_dfr(gap.seq, function(gap.value) {
    
    newdata <- df.fire %>%
      mutate(gap_percent = gap.value)
    
    # central prediction
    pred <- predict(
      model.sdd,
      newdata = newdata,
      type = 'response'
    )
    
    fit <- mean(pred)
    
    # linear predictor matrix
    Xp <- predict(
      model.sdd,
      newdata = newdata,
      type = 'lpmatrix'
    )
    
    sim.pred <- Xp %*% t(beta.sim)
    
    # average predictions across the prediction sample
    sim.sdd <- colMeans(sim.pred)
    
    tibble(
      fire = fire.name,
      gap_percent = gap.value,
      sdd = fit,
      lower = quantile(sim.sdd, 0.025),
      upper = quantile(sim.sdd, 0.975)
    )
  })
})

gap.rug <- df.500 %>%
  group_by(fire) %>%
  slice_sample(n = 3000) %>%
  ungroup()


# --- Combined model ---
gap.pred.combined <- map_dfr(
  levels(df.pred$fire),
  function(fire.name) {
    
    df.fire <- df.pred %>%
      filter(fire == fire.name) %>%
      droplevels()
    
    gap.seq <- seq(
      quantile(
        df.500$gap_percent[df.500$fire == fire.name],
        0.01,
        na.rm = TRUE
      ),
      quantile(
        df.500$gap_percent[df.500$fire == fire.name],
        0.99,
        na.rm = TRUE
      ),
      length.out = 100
    )
    
    map_dfr(gap.seq, function(gap.value) {
      
      newdata <- df.fire %>%
        mutate(gap_percent = gap.value)
      
      pred <- predict(
        model.sdd.combined,
        newdata = newdata,
        type = 'response'
      )
      
      tibble(
        fire = fire.name,
        gap_percent = gap.value,
        sdd = mean(pred, na.rm = TRUE)
      )
    })
  }
)

# --- plot ---
p.gap <- ggplot(
  gap.pred,
  aes(
    x = gap_percent,
    y = sdd,
    color = fire,
    fill = fire
  )
) +
  
  # 95% confidence interval
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  
  # fire-specific marginal prediction
  geom_line(linewidth = 1) +
  
  # combined-effect prediction
  geom_line(
    data = gap.pred.combined,
    aes(
      x = gap_percent,
      y = sdd
    ),
    inherit.aes = FALSE,
    color = 'grey40',
    linetype = 'dashed',
    linewidth = 0.8
  ) +
  
  geom_rug(
    data = gap.rug,
    aes(x = gap_percent, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  
  guides(
    color = 'none',
    fill = 'none'
  ) +
  
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted snow disappearance date (day of year)'
  ) +
  
  theme_classic()

# --------------- maximum canopy height ---------------

ht.pred <- map_dfr(levels(df.pred$fire), function(fire.name) {
  
  # prediction sample for fire
  df.fire <- df.pred %>%
    filter(fire == fire.name) %>%
    droplevels()
  
  # fire-specific prediction range
  ht.seq <- seq(
    quantile(
      df.500$ht_zmax[df.500$fire == fire.name],
      0.01,
      na.rm = TRUE
    ),
    quantile(
      df.500$ht_zmax[df.500$fire == fire.name],
      0.99,
      na.rm = TRUE
    ),
    length.out = 100
  )
  
  map_dfr(ht.seq, function(ht.value) {
    
    newdata <- df.fire %>%
      mutate(ht_zmax = ht.value)
    
    # central prediction
    pred <- predict(
      model.sdd,
      newdata = newdata,
      type = 'response'
    )
    
    fit <- mean(pred)
    
    # linear predictor matrix for uncertainty
    Xp <- predict(
      model.sdd,
      newdata = newdata,
      type = 'lpmatrix'
    )
    
    # predictions for simulated coefficients
    sim.pred <- Xp %*% t(beta.sim)
    
    # average predictions across the prediction sample
    sim.sdd <- colMeans(sim.pred)
    
    tibble(
      fire = fire.name,
      ht_zmax = ht.value,
      sdd = fit,
      lower = quantile(sim.sdd, 0.025),
      upper = quantile(sim.sdd, 0.975)
    )
  })
})

ht.rug <- df.500 %>%
  group_by(fire) %>%
  slice_sample(n = 3000) %>%
  ungroup()

# --- combined model ---
ht.pred.combined <- map_dfr(
  levels(df.pred$fire),
  function(fire.name) {
    
    # prediction sample for this fire
    df.fire <- df.pred %>%
      filter(fire == fire.name) %>%
      droplevels()
    
    # fire-specific range of ht_zmax
    ht.seq <- seq(
      quantile(
        df.500$ht_zmax[df.500$fire == fire.name],
        0.01,
        na.rm = TRUE
      ),
      quantile(
        df.500$ht_zmax[df.500$fire == fire.name],
        0.99,
        na.rm = TRUE
      ),
      length.out = 100
    )
    
    map_dfr(ht.seq, function(ht.value) {
      
      newdata <- df.fire %>%
        mutate(
          ht_zmax = ht.value
        )
      
      # predictions on sqrt(sdd) scale
      pred <- predict(
        model.sdd.combined,
        newdata = newdata,
        type = 'response'
      )
      
      tibble(
        fire = fire.name,
        ht_zmax = ht.value,
        sdd = mean(pred, na.rm = TRUE)
      )
    })
  }
)


# --- plot ---
p.ht <- ggplot(
  ht.pred,
  aes(
    x = ht_zmax,
    y = sdd,
    color = fire,
    fill = fire
  )
) +
  
  # 95% confidence interval
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.15,
    color = NA
  ) +
  
  # fire-specific marginal prediction
  geom_line(linewidth = 1) +
  
  # combined-effect prediction
  geom_line(
    data = ht.pred.combined,
    aes(
      x = ht_zmax,
      y = sdd
    ),
    inherit.aes = FALSE,
    color = 'grey40',
    linetype = 'dashed',
    linewidth = 0.8
  ) +
  
  geom_rug(
    data = ht.rug,
    aes(x = ht_zmax, color = fire),
    inherit.aes = FALSE,
    sides = 'b',
    alpha = 0.08
  ) +
  
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  
  scale_color_manual(values = fire.colors) +
  scale_fill_manual(values = fire.colors) +
  
  guides(
    color = 'none',
    fill = 'none'
  ) +
  
  labs(
    x = 'Max Height',
    y = 'Predicted snow disappearance date (day of year)'
  ) +
  
  theme_classic()




# ------------------ combined plot ------------------
library(patchwork)
# common theme for combined canopy figure
canopy.theme <- theme_classic() +
  theme(
    axis.title.y = element_blank(),
    
    strip.background = element_blank(),
    strip.text = element_text(
      face = 'bold',
      size = 10
    ),
    
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 9),
    
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

p.gap <- p.gap + canopy.theme
p.ht  <- p.ht + canopy.theme

p.gap <- p.gap +
  labs(tag = 'A') +
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

p.ht <- p.ht +
  labs(tag = 'B') +
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

# common y-axis range
sdd.range <- range(
  c(
    gap.pred$lower,
    gap.pred$upper,
    ht.pred$lower,
    ht.pred$upper
  ),
  na.rm = TRUE
)

# combine plots
canopy.fig <- (
  p.gap /
    p.ht
) &
  scale_y_continuous(
    limits = sdd.range
  )

library(grid)

y.title <- wrap_elements(
  grid::textGrob(
    'Predicted snow disappearance date (day of year)',
    rot = 90,
    gp = grid::gpar(fontsize = 11)
  )
)

canopy.fig.final <- y.title + canopy.fig +
  plot_layout(
    widths = c(0.04, 1)
  )

canopy.fig.final


# 
# ------------------------------- adding elev --------------
# --- elevation groups ----------------
df.elev <- df.500 %>%
  mutate(
    elev_band = cut(
      elevation,
      breaks = c(-Inf, 1750, 2500, Inf),
      labels = c(
        '< 1750 m',
        '1750–2500 m',
        '> 2500 m'
      ),
      right = FALSE
    )
  )

# representative elevation within each band and fire
elev.values <- df.elev %>%
  group_by(fire, elev_band) %>%
  summarise(
    elevation = median(elevation, na.rm = TRUE),
    .groups = 'drop'
  )


# ----- gap percent by elevation -----

set.seed(61)

n.sim <- 500

beta.sim <- MASS::mvrnorm(
  n = n.sim,
  mu = coef(model.sdd),
  Sigma = vcov(model.sdd)
)

gap.elev.pred <- map_dfr(
  levels(df.pred$fire),
  function(fire.name) {
    
    # prediction sample for fire
    df.fire <- df.pred %>%
      filter(fire == fire.name) %>%
      droplevels()
    
    # prediction range
    gap.seq <- seq(
      quantile(
        df.500$gap_percent[df.500$fire == fire.name],
        0.01,
        na.rm = TRUE
      ),
      quantile(
        df.500$gap_percent[df.500$fire == fire.name],
        0.99,
        na.rm = TRUE
      ),
      length.out = 100
    )
    
    # representative elevations for this fire
    fire.elev <- elev.values %>%
      filter(fire == fire.name)
    
    map_dfr(seq_len(nrow(fire.elev)), function(i) {
      
      elev.value <- fire.elev$elevation[i]
      band.name <- fire.elev$elev_band[i]
      
      map_dfr(gap.seq, function(gap.value) {
        
        newdata <- df.fire %>%
          mutate(
            gap_percent = gap.value,
            elevation = elev.value
          )
        
        # central prediction
        pred <- predict(
          model.sdd,
          newdata = newdata,
          type = 'response'
        )
        
        fit <- mean(pred)
        
        # linear predictor matrix
        Xp <- predict(
          model.sdd,
          newdata = newdata,
          type = 'lpmatrix'
        )
        
        sim.pred <- Xp %*% t(beta.sim)
        
        # average predictions across prediction sample
        sim.sdd <- colMeans(sim.pred)
        
        tibble(
          fire = fire.name,
          elev_band = band.name,
          elevation = elev.value,
          gap_percent = gap.value,
          sdd = fit,
          lower = quantile(sim.sdd, 0.025),
          upper = quantile(sim.sdd, 0.975)
        )
      })
    })
  }
)

gap.elev.pred <- gap.elev.pred %>%
  mutate(
    fire_elev = paste(fire, elev_band, sep = '_')
  )

# --- combined-model predictions by elevation ---

gap.elev.pred.combined <- map_dfr(
  levels(df.pred$fire),
  function(fire.name) {
    
    # prediction sample for fire
    df.fire <- df.pred %>%
      filter(fire == fire.name) %>%
      droplevels()
    
    # representative elevations for this fire
    fire.elev <- elev.values %>%
      filter(fire == fire.name)
    
    map_dfr(seq_len(nrow(fire.elev)), function(i) {
      
      elev.value <- fire.elev$elevation[i]
      band.name <- fire.elev$elev_band[i]
      
      # same gap range used for this fire
      gap.seq <- seq(
        quantile(
          df.500$gap_percent[df.500$fire == fire.name],
          0.01,
          na.rm = TRUE
        ),
        quantile(
          df.500$gap_percent[df.500$fire == fire.name],
          0.99,
          na.rm = TRUE
        ),
        length.out = 100
      )
      
      map_dfr(gap.seq, function(gap.value) {
        
        newdata <- df.fire %>%
          mutate(
            gap_percent = gap.value,
            elevation = elev.value
          )
        
        pred <- predict(
          model.sdd.combined,
          newdata = newdata,
          type = 'response'
        )
        
        tibble(
          fire = fire.name,
          elev_band = band.name,
          elevation = elev.value,
          gap_percent = gap.value,
          sdd = mean(pred, na.rm = TRUE)
        )
      })
    })
  }
)

p.gap.elev <- ggplot(
  gap.elev.pred,
  aes(
    x = gap_percent,
    y = sdd,
    color = fire_elev,
    fill = fire_elev,
    linetype = elev_band
  )
) +
  
  # 95% confidence interval
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.15,
    color = NA,
    show.legend = FALSE
  ) +
  
  # marginal prediction
  geom_line(
    linewidth = 1
  ) +
  
  # fire panels
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  
  # add combined smooths to plot
  geom_line(
    data = gap.elev.pred.combined,
    aes(
      x = gap_percent,
      y = sdd,
      group = elev_band
    ),
    inherit.aes = FALSE,
    color = 'grey40',
    linetype = 'dashed',
    linewidth = 0.8
  ) +
  
  # actual fire × elevation colors
  scale_color_manual(
    values = elev.colors,
    guide = 'none'
  ) +
  
  scale_fill_manual(
    values = elev.colors,
    guide = 'none'
  ) +
  
  # use linetype only to generate a simple elevation legend
  scale_linetype_manual(
    name = 'Elevation',
    values = c(
      '< 1750 m' = 'solid',
      '1750–2500 m' = 'solid',
      '> 2500 m' = 'solid'
    ),
    labels = c(
      '< 1750 m' = 'Low (< 1750 m)',
      '1750–2500 m' = 'Mid (1750–2500 m)',
      '> 2500 m' = 'High (> 2500 m)'
    ),
    guide = guide_legend(
      override.aes = list(
        color = c(
          '#3b435c',
          '#868eaa',
          '#d8e1ff'
        ),
        linewidth = 1.2
      )
    )
  ) +
  
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted snow disappearance date (day of year)'
  ) +
  
  theme_classic() +
  
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = 'bold'
    ),
    legend.position = 'right'
  )

p.gap.elev
# ------------------ combined plot * don't need anymore * ------------------
library(patchwork)
# common theme for combined canopy figure
canopy.theme <- theme_classic() +
  theme(
    axis.title.y = element_blank(),
    
    strip.background = element_blank(),
    strip.text = element_text(
      face = 'bold',
      size = 10
    ),
    
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 9),
    
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

p.gap.elev <- p.gap.elev + canopy.theme
p.ht  <- p.ht + canopy.theme

p.gap.elev <- p.gap.elev +
  labs(tag = 'A') +
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

p.ht <- p.ht +
  labs(tag = 'B') +
  theme(
    plot.tag = element_text(
      face = 'bold',
      size = 14
    ),
    plot.tag.position = c(0.01, 0.98)
  )

# common y-axis range
sdd.range <- range(
  c(
    gap.elev.pred$lower,
    gap.elev.pred$upper,
    ht.pred$lower,
    ht.pred$upper
  ),
  na.rm = TRUE
)

# combine plots
canopy.fig <- (
  p.gap.elev /
    p.ht
) &
  scale_y_continuous(
    limits = sdd.range
  )

library(grid)

y.title <- wrap_elements(
  grid::textGrob(
    'Predicted snow disappearance date (day of year)',
    rot = 90,
    gp = grid::gpar(fontsize = 11)
  )
)

canopy.fig.final <- y.title + canopy.fig +
  plot_layout(
    widths = c(0.04, 1)
  )

canopy.fig.final
# ------------------------ UPDATED MODEL * USE THIS ONE * ------------------
# model:
  # gap_percent is combined for all fires
  # removed ht_zmax
  # interaction with gap and elev


# --- get supported gap ranges within each elevation band --- 
gap.ranges <- df.elev %>%
  group_by(elev_band) %>%
  summarise(
    gap.low = quantile(
      gap_percent,
      0.01,
      na.rm = TRUE
    ),
    gap.high = quantile(
      gap_percent,
      0.99,
      na.rm = TRUE
    ),
    .groups = 'drop'
  )

elev.values <- elev.values %>%
  left_join(
    gap.ranges,
    by = 'elev_band'
  )

elev.values

# --- marginal predictions ---
n.sim <- 500

beta.sim <- MASS::mvrnorm(
  n = n.sim,
  mu = coef(model.sdd),
  Sigma = vcov(model.sdd)
)

gap.elev.pred <- map_dfr(
  seq_len(nrow(elev.values)),
  function(i) {
    
    elev.value <- elev.values$elevation[i]
    band.name <- elev.values$elev_band[i]
    
    # supported gap range within elevation band
    gap.seq <- seq(
      elev.values$gap.low[i],
      elev.values$gap.high[i],
      length.out = 100
    )
    
    map_dfr(gap.seq, function(gap.value) {
      
      # set gap and elevation for entire prediction sample
      newdata <- df.pred %>%
        mutate(
          gap_percent = gap.value,
          elevation = elev.value
        )
      
      # central prediction
      pred <- predict(
        model.sdd,
        newdata = newdata,
        type = 'response'
      )
      
      fit <- mean(pred)
      
      # linear predictor matrix
      Xp <- predict(
        model.sdd,
        newdata = newdata,
        type = 'lpmatrix'
      )
      
      # coefficient simulations
      sim.pred <- Xp %*% t(beta.sim)
      
      # average across prediction sample
      sim.sdd <- colMeans(sim.pred)
      
      tibble(
        elev_band = band.name,
        elevation = elev.value,
        gap_percent = gap.value,
        sdd = fit,
        lower = quantile(sim.sdd, 0.025),
        upper = quantile(sim.sdd, 0.975)
      )
    })
  }
)

# --- plot ---
elevation.colors <- c(
  '< 1750 m' = '#3c7510',
  '1750–2500 m' = '#71b43e',
  '> 2500 m' = '#a9f76c'
)

p.gap <- ggplot(
  gap.elev.pred,
  aes(
    x = gap_percent,
    y = sdd,
    color = elev_band,
    fill = elev_band
  )
) +
  
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.15,
    color = NA
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  scale_color_manual(
    name = 'Elevation',
    values = elevation.colors,
    labels = c(
      '< 1750 m' = 'Low (< 1750 m)',
      '1750–2500 m' = 'Mid (1750–2500 m)',
      '> 2500 m' = 'High (> 2500 m)'
    )
  ) +
  
  scale_fill_manual(
    name = 'Elevation',
    values = elevation.colors,
    labels = c(
      '< 1750 m' = 'Low (< 1750 m)',
      '1750–2500 m' = 'Mid (1750–2500 m)',
      '> 2500 m' = 'High (> 2500 m)'
    )
  ) +
  
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted snow disappearance date (day of year)'
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = 'right'
  )

p.gap


# ---------------- gap predictions by elevation band ----------------

set.seed(61)

n.sim <- 500

beta.sim <- MASS::mvrnorm(
  n = n.sim,
  mu = coef(model.sdd),
  Sigma = vcov(model.sdd)
)

gap.elev.pred <- map_dfr(
  levels(df.pred$elev_band),
  function(band.name) {
    
    # prediction sample within elevation band
    df.band <- df.pred %>%
      filter(elev_band == band.name)
    
    # supported gap range within elevation band
    gap.seq <- seq(
      quantile(
        df.500$gap_percent[
          df.500$elev_band == band.name
        ],
        0.01,
        na.rm = TRUE
      ),
      quantile(
        df.500$gap_percent[
          df.500$elev_band == band.name
        ],
        0.99,
        na.rm = TRUE
      ),
      length.out = 100
    )
    
    map_dfr(gap.seq, function(gap.value) {
      
      # retain actual elevation of every observation
      newdata <- df.band %>%
        mutate(
          gap_percent = gap.value
        )
      
      # central prediction
      pred <- predict(
        model.sdd,
        newdata = newdata,
        type = 'response'
      )
      
      fit <- mean(pred)
      
      # uncertainty
      Xp <- predict(
        model.sdd,
        newdata = newdata,
        type = 'lpmatrix'
      )
      
      sim.pred <- Xp %*% t(beta.sim)
      sim.sdd <- colMeans(sim.pred)
      
      tibble(
        elev_band = band.name,
        gap_percent = gap.value,
        sdd = fit,
        lower = quantile(sim.sdd, 0.025),
        upper = quantile(sim.sdd, 0.975)
      )
    })
  }
)
# ----------------------------------- ** BURN EFFECTS ** ------------------------------------
# ---------- ** Predicted SDD across realistic canopy structure ** --------------
# Burned vs unburned within each fire

# Find shared gap range within each fire
gap.range <- df.500 %>%
  group_by(fire, burned) %>%
  summarise(
    gap.min = quantile(gap_percent, 0.01, na.rm = TRUE),
    gap.max = quantile(gap_percent, 0.99, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(fire) %>%
  summarise(
    gap.min = max(gap.min),
    gap.max = min(gap.max),
    .groups = 'drop'
  )

# Get realistic height associated with gap percent
ht.lookup <- df.500 %>%
  group_by(fire, burned) %>%
  mutate(
    gap.bin = ntile(gap_percent, 100)
  ) %>%
  group_by(fire, burned, gap.bin) %>%
  summarise(
    gap_lookup = mean(gap_percent, na.rm = TRUE),
    ht_lookup = mean(ht_zmax, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )


# Build prediction grid
pred.gap <- gap.range %>%
  rowwise() %>%
  mutate(
    gap_percent = list(
      seq(gap.min, gap.max, length.out = 100)
    )
  ) %>%
  tidyr::unnest(gap_percent) %>%
  select(fire, gap_percent) %>%
  tidyr::crossing(
    burned = levels(df.500$burned)
  ) %>%
  mutate(
    fire = factor(
      fire,
      levels = levels(df.500$fire)
    ),
    burned = factor(
      burned,
      levels = levels(df.500$burned)
    )
  )

# Assign realistic height for each fire x burn-status trajectory
pred.gap <- pred.gap %>%
  group_by(fire, burned) %>%
  group_modify(~ {
    
    lookup <- ht.lookup %>%
      filter(
        fire == .y$fire,
        burned == .y$burned
      ) %>%
      arrange(gap_lookup)
    
    .x %>%
      mutate(
        ht_zmax = approx(
          x = lookup$gap_lookup,
          y = lookup$ht_lookup,
          xout = gap_percent,
          rule = 2
        )$y
      )
    
  }) %>%
  ungroup()

# sanity check
ggplot(
  pred.gap,
  aes(
    x = gap_percent,
    y = ht_zmax,
    color = burned
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ fire) +
  scale_color_manual(values = burn.cols) +
  theme_bw() +
  labs(
    x = 'Gap percentage',
    y = 'Typical maximum canopy height (m)',
    color = NULL
  )

# --- population-averaged predictions ---

fire.medians <- df.500 %>%
  filter(wy == 2023) %>%
  group_by(fire) %>%
  summarise(
    elevation = median(elevation, na.rm = TRUE),
    rad_dtm_accum = median(rad_dtm_accum, na.rm = TRUE),
    aspect_sin = median(aspect_sin, na.rm = TRUE),
    tpi1200 = median(tpi1200, na.rm = TRUE),
    swe_peak = median(swe_peak, na.rm = TRUE),
    .groups = 'drop'
  )

# add those values to prediction dataset
pred.gap <- pred.gap %>%
  left_join(
    fire.medians,
    by = 'fire'
  ) %>%
  mutate(
    wy = factor(
      2023,
      levels = levels(df.500$wy)
    ),
    
    fire_burned = interaction(
      fire,
      burned,
      sep = '_'
    ),
    
    fire_burned = factor(
      fire_burned,
      levels = levels(df.500$fire_burned)
    )
  )

# predict
pred <- predict(
  model.sdd.burned,
  newdata = pred.gap,
  se.fit = TRUE
)

pred.gap <- pred.gap %>%
  mutate(
    fit = pred$fit,
    lwr = pred$fit - 1.96 * pred$se.fit,
    upr = pred$fit + 1.96 * pred$se.fit
  )

# plot
ggplot(
  pred.gap,
  aes(
    x = gap_percent,
    y = fit,
    color = burned,
    fill = burned
  )
) +
  geom_ribbon(
    aes(
      ymin = lwr,
      ymax = upr
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  scale_color_manual(values = burn.cols) +
  scale_fill_manual(values = burn.cols) +
  theme_bw() +
  labs(
    x = 'Gap percentage',
    y = 'Predicted snow disappearance date',
    color = NULL,
    fill = NULL,
    title = 'Water Year 2023'
  )
# ----- gap percent -----

# --- gap bins from observed data ---

height.by.gap <- df.500 %>%
  filter(
    !is.na(gap_percent),
    !is.na(ht_zmax),
    !is.na(burned),
    !is.na(fire)
  ) %>%
  mutate(
    gap_bin = cut(
      gap_percent,
      breaks = seq(0, 100, by = 5),
      include.lowest = TRUE
    )
  ) %>%
  group_by(fire, burned, gap_bin) %>%
  summarise(
    gap_percent = mean(gap_percent, na.rm = TRUE),
    ht_zmax = mean(ht_zmax, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  filter(n >= 20)

# --- retain gap bins represented in both burn classes ---

common.gap.bins <- height.by.gap %>%
  distinct(fire, burned, gap_bin) %>%
  count(fire, gap_bin) %>%
  filter(n == 2) %>%
  select(fire, gap_bin)

height.by.gap <- height.by.gap %>%
  semi_join(
    common.gap.bins,
    by = c('fire', 'gap_bin')
  )

# --- build prediction data ---
pred.scenario.gap <- height.by.gap %>%
  tidyr::crossing(
    wy = levels(df.500$wy)
  ) %>%
  mutate(
    wy = factor(
      wy,
      levels = levels(df.500$wy)
    ),
    
    fire = factor(
      fire,
      levels = levels(df.500$fire)
    ),
    
    burned = factor(
      burned,
      levels = levels(df.500$burned)
    ),
    
    fire_burned = interaction(
      fire,
      burned,
      sep = '_'
    ),
    
    fire_burned = factor(
      fire_burned,
      levels = levels(df.500$fire_burned)
    )
  )

# use fire x wy medians for other covariate values
scenario.values <- df.500 %>%
  group_by(fire, wy) %>%
  summarise(
    elevation = median(elevation, na.rm = TRUE),
    rad_dtm_accum = median(rad_dtm_accum, na.rm = TRUE),
    aspect_sin = median(aspect_sin, na.rm = TRUE),
    tpi1200 = median(tpi1200, na.rm = TRUE),
    swe_peak = median(swe_peak, na.rm = TRUE),
    .groups = 'drop'
  )

pred.scenario.gap <- pred.scenario.gap %>%
  left_join(
    scenario.values,
    by = c('fire', 'wy')
  )

# --- predict ---
pred.scenario.gap$pred_sdd <- predict(
  model.sdd.burned.simple,
  newdata = pred.scenario.gap,
  type = 'response'
)

# average across WY
pred.scenario.gap.mean <- pred.scenario.gap %>%
  group_by(
    fire,
    burned,
    gap_bin,
    gap_percent,
    ht_zmax,
    n
  ) %>%
  summarise(
    pred_sdd = mean(pred_sdd),
    .groups = 'drop'
  )

# --- restrict to 99th percentile --- 
gap.limits <- df.500 %>%
  group_by(fire) %>%
  summarise(
    gap.max = quantile(gap_percent, 0.99, na.rm = TRUE),
    .groups = 'drop'
  )

pred.scenario.gap.mean <- pred.scenario.gap.mean %>%
  left_join(
    gap.limits,
    by = 'fire'
  ) %>%
  filter(gap_percent <= gap.max)


# --- plot ---
ggplot(
  pred.scenario.gap.mean,
  aes(
    x = gap_percent,
    y = pred_sdd,
    color = burned,
    group = burned
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(
    aes(size = n),
    alpha = 0.7
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Gap (%)',
    y = 'Predicted SDD',
    color = NULL,
    size = 'n'
  ) +
  theme_bw()


# ----- zmax -----
# --- height bins from observed data ---

gap.by.height <- df.500 %>%
  filter(
    !is.na(gap_percent),
    !is.na(ht_zmax),
    !is.na(burned),
    !is.na(fire)
  ) %>%
  mutate(
    ht_bin = cut(
      ht_zmax,
      breaks = seq(
        floor(min(ht_zmax, na.rm = TRUE)),
        ceiling(max(ht_zmax, na.rm = TRUE)),
        by = 5
      ),
      include.lowest = TRUE
    )
  ) %>%
  group_by(fire, burned, ht_bin) %>%
  summarise(
    ht_zmax = mean(ht_zmax, na.rm = TRUE),
    gap_percent = mean(gap_percent, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  filter(n >= 20)


# --- retain height bins represented in both burn classes ---

common.ht.bins <- gap.by.height %>%
  distinct(fire, burned, ht_bin) %>%
  count(fire, ht_bin) %>%
  filter(n == 2) %>%
  select(fire, ht_bin)

gap.by.height <- gap.by.height %>%
  semi_join(
    common.ht.bins,
    by = c('fire', 'ht_bin')
  )


# --- build prediction data ---

pred.scenario.ht <- gap.by.height %>%
  tidyr::crossing(
    wy = levels(df.500$wy)
  ) %>%
  mutate(
    wy = factor(
      wy,
      levels = levels(df.500$wy)
    ),
    
    fire = factor(
      fire,
      levels = levels(df.500$fire)
    ),
    
    burned = factor(
      burned,
      levels = levels(df.500$burned)
    ),
    
    fire_burned = interaction(
      fire,
      burned,
      sep = '_'
    ),
    
    fire_burned = factor(
      fire_burned,
      levels = levels(df.500$fire_burned)
    )
  )


# --- fire x WY medians for other covariates ---

scenario.values <- df.500 %>%
  group_by(fire, wy) %>%
  summarise(
    elevation = median(elevation, na.rm = TRUE),
    rad_dtm_accum = median(rad_dtm_accum, na.rm = TRUE),
    aspect_sin = median(aspect_sin, na.rm = TRUE),
    tpi1200 = median(tpi1200, na.rm = TRUE),
    swe_peak = median(swe_peak, na.rm = TRUE),
    .groups = 'drop'
  )

pred.scenario.ht <- pred.scenario.ht %>%
  left_join(
    scenario.values,
    by = c('fire', 'wy')
  )


# --- predict ---

pred.scenario.ht$pred_sdd <- predict(
  model.sdd.burned.simple,
  newdata = pred.scenario.ht,
  type = 'response'
)


# --- average across WY ---

pred.scenario.ht.mean <- pred.scenario.ht %>%
  group_by(
    fire,
    burned,
    ht_bin,
    ht_zmax,
    gap_percent,
    n
  ) %>%
  summarise(
    pred_sdd = mean(pred_sdd),
    .groups = 'drop'
  )

# --- restrict to 99th percentile --- 
ht.limits <- df.500 %>%
  group_by(fire) %>%
  summarise(
    ht.max = quantile(ht_zmax, 0.99, na.rm = TRUE),
    .groups = 'drop'
  )

pred.scenario.ht.mean <- pred.scenario.ht.mean %>%
  left_join(
    ht.limits,
    by = 'fire'
  ) %>%
  filter(ht_zmax <= ht.max)


# --- plot ---

ggplot(
  pred.scenario.ht.mean,
  aes(
    x = ht_zmax,
    y = pred_sdd,
    color = burned,
    group = burned
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(
    aes(size = n),
    alpha = 0.7
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Maximum canopy height (m)',
    y = 'Predicted SDD',
    color = NULL,
    size = 'n'
  ) +
  theme_bw()
# ----------------- UPDATED MODEL * USE THIS ONE * ------------------------
# --- supported gap range within each elevation band x burn class ---
gap.ranges.burn <- df.elev %>%
  group_by(elev_band, burned) %>%
  summarise(
    gap.low = quantile(gap_percent, 0.01, na.rm = TRUE),
    gap.high = quantile(gap_percent, 0.99, na.rm = TRUE),
    .groups = 'drop'
  )

# --- overlap between burned and unburned ---
gap.ranges.common <- gap.ranges.burn %>%
  group_by(elev_band) %>%
  summarise(
    gap.low = max(gap.low),
    gap.high = min(gap.high),
    .groups = 'drop'
  )

elev.values.burn <- elev.values %>%
  select(elev_band, elevation) %>%
  left_join(
    gap.ranges.common,
    by = 'elev_band'
  )

burn.levels <- levels(df.500$burned)

gap.elev.burn.pred <- map_dfr(
  seq_len(nrow(elev.values.burn)),
  function(i) {
    
    elev.value <- elev.values.burn$elevation[i]
    band.name <- elev.values.burn$elev_band[i]
    
    gap.seq <- seq(
      elev.values.burn$gap.low[i],
      elev.values.burn$gap.high[i],
      length.out = 100
    )
    
    map_dfr(burn.levels, function(burn.value) {
      
      map_dfr(gap.seq, function(gap.value) {
        
        newdata <- df.pred %>%
          mutate(
            gap_percent = gap.value,
            elevation = elev.value,
            burned = factor(
              burn.value,
              levels = levels(df.500$burned)
            )
          )
        
        pred <- predict(
          model.sdd.burned,
          newdata = newdata,
          type = 'response'
        )
        
        tibble(
          elev_band = band.name,
          elevation = elev.value,
          burned = burn.value,
          gap_percent = gap.value,
          sdd = mean(pred)
        )
      })
    })
  }
)

# ----- observed density by elevation band x burned x gap bin -----
gap.density <- df.elev %>%
  filter(
    !is.na(gap_percent),
    !is.na(burned),
    !is.na(elev_band)
  ) %>%
  mutate(
    gap_bin = cut(
      gap_percent,
      breaks = seq(0, 100, by = 5),
      include.lowest = TRUE
    )
  ) %>%
  group_by(
    elev_band,
    burned,
    gap_bin
  ) %>%
  summarise(
    gap_percent = mean(gap_percent, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )

gap.density <- gap.density %>%
  group_by(elev_band, burned) %>%
  mutate(
    sdd = approx(
      x = gap.elev.burn.pred$gap_percent[
        gap.elev.burn.pred$elev_band == first(elev_band) &
          gap.elev.burn.pred$burned == first(burned)
      ],
      y = gap.elev.burn.pred$sdd[
        gap.elev.burn.pred$elev_band == first(elev_band) &
          gap.elev.burn.pred$burned == first(burned)
      ],
      xout = gap_percent,
      rule = 1
    )$y
  ) %>%
  ungroup() %>%
  filter(!is.na(sdd))

# --- plot ---
ggplot(
  gap.elev.burn.pred,
  aes(
    x = gap_percent,
    y = sdd,
    color = burned
  )
) +
  geom_line(linewidth = 1) +
  geom_point(
    data = gap.density,
    aes(
      x = gap_percent,
      y = sdd,
      size = n,
      color = burned
    ),
    alpha = 0.75
  ) +
  facet_wrap(
    ~ elev_band,
    nrow = 1
  ) +
  scale_color_manual(values = burn.cols) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Predicted snow disappearance date (DOY)',
    color = NULL,
    size = 'Observations'
  ) +
  theme_classic()
# ------------------ Plotting Predicted Burn Differences ---------------
# --- predict all observations as unburned ---

pred.unburned <- df.500 %>%
  mutate(
    burned = factor(
      'unburned',
      levels = levels(df.500$burned)
    )
  )

pred.unburned$pred_sdd <- predict(
  model.sdd.burned.simple,
  newdata = pred.unburned,
  type = 'response'
)


# --- predict all observations as burned ---

pred.burned <- df.500 %>%
  mutate(
    burned = factor(
      'burned',
      levels = levels(df.500$burned)
    )
  )

pred.burned$pred_sdd <- predict(
  model.sdd.burned.simple,
  newdata = pred.burned,
  type = 'response'
)

# --- combine prediction scenarios ---
pred.violin <- bind_rows(
  pred.unburned %>%
    transmute(
      fire,
      scenario = 'unburned',
      pred_sdd
    ),
  
  pred.burned %>%
    transmute(
      fire,
      scenario = 'burned',
      pred_sdd
    )
) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c('unburned', 'burned')
    )
  )


# --- plot ---

ggplot(
  pred.violin,
  aes(
    x = scenario,
    y = pred_sdd,
    fill = scenario
  )
) +
  geom_violin(
    trim = FALSE,
    alpha = 0.6
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    alpha = 0.8
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  scale_fill_manual(
    values = burn.cols,
    labels = c(
      'unburned' = 'Unburned',
      'burned' = 'Burned'
    )
  ) +
  scale_x_discrete(
    labels = c(
      'unburned' = 'Unburned',
      'burned' = 'Burned'
    )
  ) +
  labs(
    x = NULL,
    y = 'Predicted SDD',
    fill = NULL
  ) +
  theme_bw()
# ----------------------------------- ** PREDICTED VS OBSERVED MAPS ** ------------------------------------

df.500$pred_sdd <- predict(model.sdd, newdata = df.500, type = 'response')

# ----- observed vs predicted SDD maps -----

fires <- c('Creek', 'Castle', 'Caldor')


# --- Common scales across all fires ---
# common SDD range
sdd.limits <- range(
  c(df.500$sdd, df.500$pred_sdd),
  na.rm = TRUE
)

# common difference range, symmetric around zero
diff.max <- quantile(
  abs(df.500$pred_sdd - df.500$sdd),
  0.99,
  na.rm = TRUE
)

diff.limits <- c(-diff.max, diff.max)

for (fire.name in fires) {
  
  map.sdd <- df.500 %>%
    filter(fire == fire.name) %>%
    select(x, y, wy, sdd, pred_sdd) %>%
    pivot_longer(
      cols = c(sdd, pred_sdd),
      names_to = 'type',
      values_to = 'sdd_value'
    ) %>%
    mutate(
      type = factor(
        type,
        levels = c('sdd', 'pred_sdd'),
        labels = c('Observed', 'Predicted')
      )
    )
  
  # ----- observed and predicted -----
  
  p <- ggplot(
    map.sdd,
    aes(
      x = x,
      y = y,
      fill = sdd_value
    )
  ) +
    geom_tile() +
    facet_grid(
      type ~ wy
    ) +
    coord_equal() +
    scale_fill_viridis_c(
      option = 'viridis',
      limits = sdd.limits
    ) +
    labs(
      title = paste(fire.name, 'Fire'),
      x = NULL,
      y = NULL,
      fill = 'SDD'
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = 'bold',
        size = 14,
        hjust = 0.5
      ),
      strip.text = element_text(
        face = 'bold',
        size = 11
      )
    )
  
  print(p)
  
  
  # ----- predicted - observed difference -----
  
  map.diff <- map.sdd %>%
    pivot_wider(
      names_from = type,
      values_from = sdd_value
    ) %>%
    mutate(
      diff = Predicted - Observed
    )
  
  p2 <- ggplot(
    map.diff,
    aes(
      x = x,
      y = y,
      fill = diff
    )
  ) +
    geom_tile() +
    facet_wrap(
      ~ wy,
      nrow = 1
    ) +
    coord_equal() +
    scale_fill_gradient2(
      low = 'blue',
      mid = 'white',
      high = 'red',
      midpoint = 0,
      limits = diff.limits,
      oob = scales::squish
    ) +
    labs(
      title = paste(fire.name, 'Fire'),
      subtitle = 'Predicted − Observed SDD',
      x = NULL,
      y = NULL,
      fill = 'Difference\n(days)'
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = 'bold',
        size = 14,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        hjust = 0.5
      ),
      strip.text = element_text(
        face = 'bold',
        size = 11
      )
    )
  
  print(p2)
}

# ----- just predicted -----
ggplot(
  df.500 %>%
    filter(fire == 'Creek'),
  aes(
    x = x,
    y = y,
    fill = pred_sdd
  )
) +
  geom_tile() +
  facet_wrap(
    ~ wy,
    nrow = 1
  ) +
  coord_equal() +
  scale_fill_viridis_c(
    option = 'magma'
  ) +
  labs(
    title = 'Creek Fire',
    x = NULL,
    y = NULL,
    fill = 'Predicted\nSDD'
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = 'bold',
      size = 14,
      hjust = 0.5
    ),
    strip.text = element_text(
      face = 'bold',
      size = 11
    )
  )

# ----- observed - predicted -----
map.diff <- map.sdd %>%
  pivot_wider(
    names_from = type,
    values_from = sdd_value
  ) %>%
  mutate(
    diff = Predicted - Observed
  )

ggplot(
  map.diff,
  aes(
    x = x,
    y = y,
    fill = diff
  )
) +
  geom_tile() +
  facet_wrap(
    ~ wy,
    nrow = 1
  ) +
  coord_equal() +
  scale_fill_gradient2(
    low = 'blue',
    mid = 'white',
    high = 'red',
    midpoint = 0
  ) +
  labs(
    title = 'Creek Fire',
    subtitle = 'Predicted − Observed SDD',
    x = NULL,
    y = NULL,
    fill = 'Difference\n(days)'
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = 'bold',
      size = 14,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    strip.text = element_text(
      face = 'bold',
      size = 11
    )
  )

df.500 <- df.500 %>%
  mutate(
    sdd_resid = pred_sdd - sdd
  )

ggplot(
  df.500,
  aes(
    x = gap_percent,
    y = sdd_resid
  )
) +
  geom_point(
    alpha = 0.1,
    size = 0.5
  ) +
  geom_smooth(
    method = 'gam',
    formula = y ~ s(x, k = 10)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 'dashed'
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  labs(
    x = 'Gap (%)',
    y = 'Prediction residual (days)\nPredicted − Observed'
  ) +
  theme_bw()

ggplot(
  df.500 %>%
    filter(fire == 'Creek'),
  aes(
    x = x,
    y = y,
    fill = gap_percent
  )
) +
  geom_tile() +
  facet_wrap(
    ~ wy,
    nrow = 1
  ) +
  coord_equal() +
  scale_fill_viridis_c() +
  labs(
    title = 'Creek Fire',
    subtitle = 'Gap percentage',
    x = NULL,
    y = NULL,
    fill = 'Gap (%)'
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = 'bold',
      size = 14,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    strip.text = element_text(
      face = 'bold',
      size = 11
    )
  )

# ==============================================================================
# Observed Plots
# ==============================================================================
# ----- SDD vs Canopy Gap for each fire, by burned status -----
ggplot(
  df.500,
  aes(
    x = gap_percent,
    y = sdd,
    color = burned
  )
) +
  geom_point(
    alpha = 0.03
  ) +
  geom_smooth(
    method = 'gam',
    formula = y ~ s(x, k = 10),
    se = TRUE
  ) +
  facet_wrap(
    ~ fire,
    nrow = 1
  ) +
  scale_color_manual(
    values = burn.cols
  ) +
  labs(
    x = 'Canopy gap (%)',
    y = 'Snow disappearance date (day of year)',
    color = 'Burn status'
  ) +
  theme_classic()





# troubleshooting


