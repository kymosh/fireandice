
# ==============================================================================
#  Exploratory Random Forest
# ==============================================================================


# ------ initalize -----
library(ranger)
library(pdp)

dir <- 'data/processed/processed/rds/creek'
df.50.0 <- readRDS(file.path(dir, 'creek_long_df_50m.rds'))
out.dir <- dir <- 'data/processed/processed/rds/creek/modeling'

#rf.results <- data.frame()
#rf.var.importance <- data.frame()

# ----- create dfs -----
# full df
df.50.rf.full <- df.50 %>% 
  select(-cell) %>% # RF won't use cell
  filter(
    wy != 2020) %>% # drop 2020, since it's prefire
  mutate(
    wy = as.factor(wy)) %>% # make wy a factor 
  filter(complete.cases(.)) # drop rows with any missing values

# create dfs for each year
df.2021 <- df.50.rf.full %>% filter(wy == '2021')
df.2022 <- df.50.rf.full %>% filter(wy == '2022')
df.2023 <- df.50.rf.full %>% filter(wy == '2023')
df.2024 <- df.50.rf.full %>% filter(wy == '2024')
df.2025 <- df.50.rf.full %>% filter(wy == '2025')


# ----- rf helper function -----

rf.run.save <- function(name, df, out.dir, rf.results, rf.var.importance) {
  
  rf.mod <- ranger(
    swe_peak ~ .,
    data = df,
    num.trees = 500,
    importance = 'permutation',
    seed = 123
  )
  
  saveRDS(rf.mod, file.path(out.dir, paste0(name, '.rds')))
  
  rf.results <- rbind(
    rf.results,
    data.frame(
      model = name,
      n = nrow(df),
      num.trees = rf.mod$num.trees,
      mtry = rf.mod$mtry,
      min.node.size = rf.mod$min.node.size,
      prediction.error = rf.mod$prediction.error,
      r.squared = rf.mod$r.squared
    )
  )
  
  var.imp <- sort(rf.mod$variable.importance, decreasing = TRUE)
  
  rf.var.importance <- rbind(
    rf.var.importance,
    data.frame(
      model = name,
      variable = names(var.imp),
      importance = as.numeric(var.imp)
    )
  )
  
  saveRDS(rf.results, file.path(out.dir, 'rf_results.rds'))
  saveRDS(rf.var.importance, file.path(out.dir, 'rf_var_importance.rds'))
  
  list(
    rf.mod = rf.mod,
    rf.results = rf.results,
    rf.var.importance = rf.var.importance
  )
}

# ----- run rf models -----

# --- 2021 ---
tic('2021 rf model')
rf.2021 <- rf.run.save(name = 'rf2021', df = df.2021, out.dir, rf.results, rf.var.importance)
toc()
# 15 threads
# 27 minutes


# --- 2022 ---
tic('2022 rf model')
rf.2022 <- rf.run.save(name = 'rf2022', df = df.2022, out.dir, rf.results, rf.var.importance)
toc()



# --- 2023 ---
tic('2023 rf model')
out.2023 <- rf.run.save(name = 'rf2023', df = df.2023, out.dir, rf.results, rf.var.importance)
toc()
rf.2023 <- out.2023$rf.mod
rf.results <- out.2023$rf.results
rf.var.importance <- out.2023$rf.var.importance


# --- 2024 ---
tic('2024 rf model')
rf.2024 <- rf.run.save(name = 'rf2024', df = df.2024, out.dir, rf.results, rf.var.importance)
toc()
# can't remember when I ended up running here, may have overwritten out.2023 but kind of don't think I did because when I told it to rewrite it with doesn't actually exist...


# --- 2025 ---
tic('2025 rf model')
rf.2025 <- rf.run.save(name = 'rf2025', df = df.2025, out.dir, rf.results, rf.var.importance)
toc()



# --- full rf model ---
tic('full rf.model')
rf.full <- rf.run.save(name = 'rf_full_50_75qsnowline', df = df.50.rf.full, out.dir, rf.results, rf.var.importance)
toc()
# took 2 hours 20 minutes

