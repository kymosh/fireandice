packages <- c('tidymodels', 'dplyr', 'tidyr', 'corrplot')
lapply(packages, library, character.only = T)

dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/rds'

# get dataframe
df.50.0 <- readRDS(file.path(dir, 'creek_long_df_50m.rds'))
df.500.0 <- readRDS(file.path(dir, 'creek_long_df_500m.rds'))

# drop tpi1200, too many NAs
df.50 <- df.50.0 %>% 
  select(-topo_tpi1200) %>%
  select(-fd_fractal_dim) %>%
  select(-tmmn) # highly correlated with tmmx. NOTE: Maybe calculate mean and use that instead?
df.500 <- df.500.0 %>% 
  select(-topo_tpi1200) %>%
  select(-fd_fractal_dim) %>% # basically same as gap_pct and gap_pct has way less NAs than fractal_dim
  select(-tmmn)

saveRDS(df.50, file.path(dir, 'creek_long_df_50m_filt.rds'))
saveRDS(df.500, file.path(dir, 'creek_long_df_500m_filt.rds'))


# ----- check how many NAs (do for both 50 and 500) -----
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
  select(-cell, -wy, -swe_peak)

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
  filter(abs(Freq) > 0.8) %>%
  arrange(desc(abs(Freq)))

cor.strong




# troubleshooting

