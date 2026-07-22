library('tidyverse')

fires <- c('creek', 'castle', 'caldor', 'dixie')
rds.dir <- 'data/processed/processed/rds/'

dfs <- lapply(fires, function(fire) {
  
  readRDS(paste0(rds.dir, fire, '/', fire, '_df_50m_raw.rds')
    
  ) %>%
    mutate(fire = fire)

})

df.raw <- bind_rows(dfs)
names(df.raw)

# check NAs
nas <- df.raw %>%
        group_by(fire) %>%
         summarize(
         across(everything(),
           ~mean(is.na(.)))
  )

# compare distributions
variables <- c('elevation', 'gap_percent', 'ht_zmax', 'swe_peak')

for (var in variables) {
  
  print(
    ggplot(df.raw,
           aes(x = fire,
               y = .data[[var]],
               fill = fire)) +
      geom_violin()
  )
  
}


ggplot(df.raw,
       aes(burned,
           gap_percent,
           fill = burned)) +
  geom_violin() +
  facet_wrap(~fire)
