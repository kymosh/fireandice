packages <- c('tidymodels', 'dplyr', 'tidyr')
lapply(packages, library, character.only = T)

dir <- 'J:/Fire_Snow/fireandice/data/processed/processed/rds'

# get dataframe
df.50 <- readRDS(file.path(dir, 'creek_long_df_50m.rds'))
df.500 <- readRDS(file.path(dir, 'creek_long_df_500m.rds'))

# check how many NAs (do for both 50 and 500)
na.summary <- data.frame(
  variable = names(df.500),
  prop.na = sapply(df.500, function(x) mean(is.na(x)))
)
na.summary <- na.summary[order(-na.summary$prop.na), ]
print(na.summary)

# how many rows would be removed if we drop rows with NAs?
mean(complete.cases(df.50))
mean(complete.cases(df.500))

# missingness by variable and year
df.50 %>%
  group_by(wy) %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  print(n = Inf)
df.500 %>%
  group_by(wy) %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  print(n = Inf)

# missingness by cell
cell.na <- df.50 %>%
  group_by(cell) %>%
  summarise(prop.na = mean(!complete.cases(.)))

summary(cell.na$prop.na)
table(cell.na$prop.na == 1)

cell.na <- df.500 %>%
  group_by(cell) %>%
  summarise(prop.na = mean(!complete.cases(.)))

summary(cell.na$prop.na)
table(cell.na$prop.na == 1)






# troubleshooting
sdd <- rast('J:/Fire_Snow/fireandice/data/processed/processed/tif/500m/creek/creek_sdd_500m.tif')
plot(sdd)

df.500 %>%
  group_by(wy) %>%
  summarise(
    n = n(),
    n.na.sdd = sum(is.na(sdd)),
    prop.na.sdd = mean(is.na(sdd))
  )
