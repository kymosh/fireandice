packages <- c( 'here', 'dplyr', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

swe.df.clustered <- readRDS(here('data', 'processed', 'processed', 'rds', 'swe_df_clustered_1524_2674.rds'))

# pivot table so swe data is in long format
swe.long <- swe.df.clustered %>%
  pivot_longer(
    cols = matches('^20\\d{2}\\.\\d{4}\\.'),
    names_to = 'date',
    values_to = 'swe'
  ) %>%
  mutate(
    date = gsub('\\.SUPERswe|\\.swe', '', date), # clean up column name
    date = as.Date(date, format = '%Y.%m%d') # and convert to date
  )

# summarize data so we have mean swe values for each cluster for each date
swe.summary <- swe.long %>%
  group_by(cluster, date, burned_status) %>%
  summarize(mean_swe = mean(swe, na.rm = TRUE), .groups = 'drop')
  
# plot time series 
ggplot(swe.summary, aes(x = date, y = mean_swe, color = burned_status)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ cluster, scales = 'free_y') +
  scale_color_manual(
    values = c('Unburned' = 'blue', 'Burned' = 'red')
  ) +
  labs(
    x = 'Date',
    y = 'Mean SWE (mm)',
    color = 'Burn Status',
    title = 'SWE Time Series by Cluster (Burned vs. Unburned)'
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_rect(fill = 'gray90', color = NA),
    legend.position = 'top'
  )



