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


burn.cols <- c(
  'unburned' = 'turquoise4',
  'burned' = 'firebrick2'
)

fire.colors <- c(
  'Caldor' = '#009E73',
  'Castle' = '#E69F00',
  'Creek' = '#CC79A7'
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
model.sdd <- bam(sdd ~ wy * fire +
                   s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) + 
                   s(ht_zmax, by = fire, k = 20) + s(gap_percent, by = fire, k = 20) + 
                   s(swe_peak, k = 20),
                 data = df.500,
                 method = 'fREML',
                 discrete = TRUE)

model.sdd.combined <- bam(sdd ~ wy * fire + 
                   s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) + 
                   s(ht_zmax, k = 20) + s(gap_percent, k = 20) +
                   s(swe_peak, k = 20),
                 data = df.500,
                 method = 'fREML',
                 discrete = TRUE)

model.sdd.burned <- bam(sdd ~ wy * fire + burned * fire +
    s(elevation, by = wy, k = 20) + s(rad_dtm_accum, k = 20) + s(aspect_sin, k = 20) + s(tpi1200, k = 10) +
    s(ht_zmax, by = fire_burned, k = 20) + s(gap_percent, by = fire_burned, k = 20) +
    s(swe_peak, k = 20),
  data = df.500,
  method = 'fREML',
  discrete = TRUE
)





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
# ----------------------------------- ** BURN EFFECTS ** ------------------------------------
# ---------- Predicted SDD across realistic canopy structure --------------
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



