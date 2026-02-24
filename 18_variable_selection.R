packages <- c('dplyr', 'vegan', 'corrplot', 'terra')
lapply(packages, library, character.only = T)


canopy.stack <- rast(file.path(dir, 'canopy_metrics_50m.tif'))
df <- as.data.frame(canopy.stack, cells = TRUE, na.rm = TRUE)

cor.mat <- cor(df)

which(abs(cor.mat) > 0.9 & abs(cor.mat) < 1, arr.ind = TRUE)

corr <- cor(df)
corrplot(corr)
names(df)

height <- df %>%
  select(starts_with('ht'))

 cover <- df %>%
   select(starts_with('cover'))

 cor.height <- cor(height) 

 corrplot(cor.height)
  