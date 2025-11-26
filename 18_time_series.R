packages <- c( 'here', 'dplyr', 'terra', 'ggplot2', 'tidyr', 'stringr', 'lubridate')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)
# # ==============================================================================
# df.0 <- readRDS('data/processed/dataframes/swe_df_1524.rds')
# 
# 
# binned.swe.df <- df.0 %>%
#   # bin data based on HLI and elevation
#   mutate(
#     # 1. HLI group
#     hli_group = ifelse(hli < 0.65, 'low_hli', 'high_hli'),
#     # 2. Elev group
#     elev_group = case_when(
#       elev < 2000 ~ 'low_elev',
#       elev >= 2000 & elev < 2600 ~ 'mid_elev',
#       elev >= 2600 ~ 'high_elev'),
#     # 3. Combine groups
#     elev_hli = paste(hli_group, elev_group, sep = '_')) %>%
#   select(x, y, elev_hli, cbibc, starts_with("2020"), starts_with("2021"), 
#         starts_with("2022"), starts_with("2023"), starts_with("2024"), starts_with("2025"))
# 
# 
# 
# # pivot table so swe data is in long format
# swe.long <- binned.swe.df %>%
#   pivot_longer(
#     cols = matches('^20\\d{2}_\\d{4}_'),  # select SWE columns
#     names_to = 'date',
#     values_to = 'swe'
#   ) %>%
#   mutate(
#     burned_status = case_when(
#       cbibc <= 1.5 ~ 'unburned/low severity',
#       cbibc >= 1.5 ~ 'med/high severity'
#     ),
#     # Clean up column names to get just the date
#     date = gsub('_superswe$|_swe$', '', date),
#     # Convert to Date
#     date = as.Date(date, format = '%Y_%m%d')
#   ) %>%
#   select(-cbibc)  # optional: drop original cbibc column
# 
# 
# 
# # summarize data so we have mean swe values for each cluster for each date
# swe.summary <- swe.long %>%
#   group_by(elev_hli, date, burned_status) %>%
#   summarize(mean_swe = mean(swe, na.rm = TRUE), .groups = 'drop')
# 
# # plot time series 
# 
# ggplot(swe.summary, aes(x = date, y = mean_swe, color = burned_status)) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("med/high severity" = "red", "unburned/low severity" = "black")) +
#   facet_wrap(~ elev_hli, ncol = 2) +
#   theme_minimal() +
#   labs(
#     x = "Date",
#     y = "Mean SWE",
#     color = "Status",
#     title = "Time Series of SWE by Elevation & HLI",
#     subtitle = "Red = med/high severity, Black = unburned/low severity"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # ==============================================================================
# 
# library(dplyr)
# library(ggplot2)
# 
# df.0 <- readRDS('data/processed/dataframes/filtered_swe_df.rds')
# 
# df <- df.0 %>%
#   mutate(
#     # create burn status column
#     burned_status = case_when(
#       cbibc < 1.5 ~ 'unburned / low severity',
#       cbibc >= 1.5 ~ 'med / high severity',
#       TRUE ~ NA_character_
#     )) %>%
#   # keep only elevations between 1500 and 2400
#   filter(elev >= 1500 & elev <= 2400) %>%
#   # create column for high and low hli values
#   mutate(hli_group = ifelse(hli < 0.65, 'low_hli', 'high_hli')) %>%
#   # create ID column for later plotting
#   mutate(ID = row_number())
# 
# # ggplot(df, aes(x = elev, fill = burned_status)) +
# #   geom_histogram(position = 'identity', bins = 40, alpha = 0.6) +
# #   labs(
# #     x = 'Elevation (m)',
# #     y = 'Count',http://127.0.0.1:40296/graphics/plot_zoom_png?width=900&height=820
# #     fill = 'Burn Severity',
# #     title = 'Elevation Distribution by Burn Severity (1500–2500 m)'
# #   ) +
# #   theme_minimal()
# 
# 
# swe.long <- df %>%
#   pivot_longer(
#     cols = matches('^20\\d{2}_\\d{4}_'), # find all cols that are dates
#     names_to = 'date', # move them to new column called date
#     values_to = 'swe' # and move their swe values to column called swe
#   ) %>%
#   mutate(
#     date = gsub('_superswe$|_swe$', '', date), # remove suffixes so only raw date remains
#     date = as.Date(date, format = '%Y_%m%d'),
#     month = format(date, '%m')
#   ) %>%
#   group_by(hli_group, date, burned_status) %>%
#   slice_sample(n = 100)
# 
# swe.long.summary <- swe.long %>%
#   group_by(hli_group, burned_status, date) %>%
#   mutate(
#     n = n(),
#     mean_swe = mean(swe, na.rm = TRUE),
#     sd_swe = sd(swe, na.rm = TRUE),
#     sem = sd_swe / sqrt(n),
#     t_crit = qt(0.975, df = n - 1),
#     CI_lower = mean_swe - t_crit * sem,
#     CI_upper = mean_swe + t_crit * sem,
#     year = format(date, '%Y'),
#     month = format(date, '%m')
#   ) %>%
#   ungroup()
# 
# swe.mean <- swe.long %>%
#   group_by(year, burned_status, date) %>%
#   summarise(mean_peak_swe = mean(peak_swe, na.rm = T),
#             .groups = 'drop')
# 
# ggplot() +
#   # all individual lines
#   geom_line(data = swe.long,
#             aes(x = date, y = peak_swe, group = ID, color = burned_status),
#             alpha = 0.3) +   # make lines transparent
#   # bold mean lines
#   geom_line(data = swe.mean,
#             aes(x = date, y = mean_peak_swe, color = burned_status),
#             size = 1.5) +
#   # colors
#   scale_color_manual(values = c(
#     'med / high severity' = 'red',
#     'unburned / low severity' = 'blue'
#   )) +
#   # one plot per year
#   facet_wrap(~ year, ncol = 2) +
#   labs(
#     x = 'Date',
#     y = 'Peak SWE',
#     color = 'Burn Status',
#     title = 'Peak SWE by Burn Status'
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# ggplot(swe.long, aes(x = month, y = peak_swe, color = burned_status, group = ID)) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("med / high severity" = "red", "unburned / low severity" = "black")) +
#   facet_wrap(~ year, ncol = 2) +
#   theme_minimal() +
#   labs(
#     x = "Date",
#     y = "SWE",
#     color = "Status",
#     title = "Time Series of SWE by Elevation & HLI",
#     subtitle = "Red = med/high severity, Black = unburned/low severity")
# 
# ggplot(swe.long.short, aes(x = date, y = peak_swe, color = burned_status)) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("med / high severity" = "red", "unburned / low severity" = "black")) +
#   facet_wrap(burned_status ~ hli_group, ncol = 2) +
#   theme_minimal() +
#   labs(
#     x = "Date",
#     y = "SWE",
#     color = "Status",
#     title = "Time Series of SWE by Elevation & HLI",
#     subtitle = "Red = med/high severity, Black = unburned/low severity")
# 
# 
# ggplot(swe.summary, aes(x = date, y = mean_swe, color = burned_status)) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("med / high severity" = "red", "unburned / low severity" = "black")) +
#   facet_wrap(~ hli_group, ncol = 2) +
#   theme_minimal() +
#   labs(
#     x = "Date",
#     y = "Mean SWE",
#     color = "Status",
#     title = "Time Series of SWE by Elevation & HLI",
#     subtitle = "Red = med/high severity, Black = unburned/low severity"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggplot(swe.summary, aes(x = burned_status, y = mean_swe, fill = burned_status)) +
#   geom_boxplot(alpha = 0.7) +
#   theme_minimal() +
#   labs(
#     x = 'Burn Status',
#     y = 'Mean SWE',
#     title = 'Distribution of Mean SWE by Burn Status'
#   ) +
#   theme(legend.position = 'none')
# 
# swe.summary <- swe.summary %>%
#   mutate(year = format(date, '%Y'))
# 
# ggplot(swe.summary, aes(x = burned_status, y = mean_swe, fill = burned_status)) +
#   geom_boxplot(alpha = 0.8) +
#   facet_wrap(~ year) +
#   theme_minimal() +
#   labs(
#     x = 'Burn Status',
#     y = 'Mean SWE',
#     title = 'Mean SWE by Burn Status, Faceted by Year'
#   ) +
#   theme(legend.position = 'none')
# 
# # ==============================================================================
# 
# df.0 <- readRDS('data/processed/dataframes/filtered_swe_df.rds')
# 
# df <- df.0 %>%
#   mutate(
#     # create burn status column
#     burned_status = case_when(
#       cbibc < 1.5 ~ 'unburned / low severity',
#       cbibc >= 1.5 ~ 'med / high severity',
#       TRUE ~ NA_character_
#     )) %>%
#   # keep only elevations between 1500 and 2400
#   filter(elev >= 1500 & elev <= 2400) %>%
#   # create column for high and low hli values
#   mutate(hli_group = ifelse(hli < 0.65, 'low_hli', 'high_hli')) %>%
#   # create ID column for later plotting
#   mutate(ID = row_number())
# 
# df.sample <-df %>%
#   filter(year %in% c(2023, 2024)) %>%
#   group_by(burned_status, hli_group, year) %>%
#   slice_sample(n = 12) %>%
#   ungroup()
# 
# swe.cols <- grep('^202', colnames(df.sample), value = TRUE)
# 
# df.plot <- df.sample %>%
#   pivot_longer(
#     cols = all_of(swe.cols),
#     names_to = 'date',
#     values_to = 'swe'
#   ) %>%
#   mutate(
#     date = as.Date(gsub('_superswe$|_swe$', '', date), format = '%Y_%m%d'),
#     year = format(date, '%Y'),  # <- derive correct year from date
#     month = as.numeric(format(date, "%m"))
#   )
# 
# 
# swe.mean <- df.plot %>%
#   group_by(year, burned_status, month) %>%
#   summarize(mean_swe = mean(swe, na.rm = T, .groups = 'drop'))
# 
# ggplot() +
#   # individual pixel lines
#   geom_line(data = df.plot,
#             aes(x = month, y = swe, group = ID, color = burned_status),
#             alpha = 0.3) +
#   # bold mean lines
#   geom_line(data = swe.mean,
#             aes(x = month, y = mean_swe, color = burned_status),
#             size = 1.5) +
#   scale_color_manual(values = c('med / high severity' = 'red',
#                                 'unburned / low severity' = 'blue')) +
#   facet_wrap(~ year, ncol = 2) +
#   labs(x = 'Date', y = 'Peak SWE', color = 'Burn Status',
#        title = 'Peak SWE by Burn Status') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # ==============================================================================
# # 2023 and 2024
# # ==============================================================================
# 
# df.0 <- readRDS('data/processed/dataframes/filtered_swe_df.rds')
# 
# # Get original SWE columns for 2023/2024
# swe.cols <- grep('^2023|^2024', colnames(df.0), value = TRUE)
# 
# df <- df.0 %>%
#   # get rid of year col and keep only 2023 and 2024 swe cols
#   select(x, y, all_of(swe.cols), cbibc, aspect, hli,
#          elev, pr, tmmx, tmmn, forest_type) %>%
#   # keep only elevations between 1500 and 2400
#   filter(elev >= 1500 & elev <= 2400) %>%
#   filter(forest_type == 'temperate_subpolar_needleleaf_forest') %>%
#   mutate(
#     burned_status = case_when(
#       cbibc < 1.5 ~ 'unburned / low severity',
#       cbibc >= 1.5 ~ 'med / high severity',
#       TRUE ~ NA_character_
#     ),
#     hli_group = ifelse(hli < 0.65, 'low_hli', 'high_hli'),
#     ID = row_number()
#   ) %>%
#   rename_with(
#     ~ str_extract(.x, '^20\\d{2}_\\d{4}'),
#     all_of(swe.cols)
#   )
# 
# swe.cols <- grep('^2023|^2024', colnames(df), value = TRUE)
# 
# df.sample <- df %>%
#   group_by(burned_status, hli_group) %>%
#   slice_sample(n = 1000) %>%
#   ungroup()
# 
# # Select only columns for SWE in 2023 and 2024
# swe.cols <- grep('^2023|^2024', colnames(df.sample), value = TRUE)
# 
# df.long <- df.sample %>%
#   pivot_longer(
#     cols = all_of(swe.cols),        # all SWE columns for 2023 and 2024
#     names_to = 'date',              # new column with the date
#     values_to = 'swe'               # new column with the SWE value
#   ) %>%
#   mutate(
#     date = as.Date(date, format = '%Y_%m%d'),  # convert column names to Date
#     year = format(date, '%Y'),                 # extract year
#     month = as.numeric(format(date, '%m'))     # numeric month for plotting
# )
# 
# swe.mean <- df.long %>%
#   group_by(year, burned_status, month) %>%
#   summarize(
#     n = n(),
#     mean_swe = mean(swe, na.rm = TRUE),
#     sd_swe = sd(swe, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(
#     sem = sd_swe / sqrt(n),                        # standard error
#     t_crit = qt(0.975, df = n - 1),               # t critical for 95% CI
#     CI_lower = mean_swe - t_crit * sem,           # lower bound
#     CI_upper = mean_swe + t_crit * sem            # upper bound
#   )
# 
# ggplot() +
# 
#   # bold mean lines
#   geom_line(data = swe.mean,
#             aes(x = month, y = mean_swe, color = burned_status),
#             size = 1.5) +
#   
#   scale_color_manual(values = c('med / high severity' = 'red',
#                                 'unburned / low severity' = 'blue')) +
#   scale_fill_manual(values = c('med / high severity' = 'red',
#                                'unburned / low severity' = 'blue')) +
#   
#   facet_wrap(~ year, ncol = 1) +
#   labs(x = 'Month', y = 'Peak SWE', color = 'Burn Status', fill = 'Burn Status',
#        title = 'SWE Time Series',
#        subtitle = 'Faded lines = individual pixels, Bold lines = mean') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# # ==============================================================================
# # 2021 and 2022
# # ==============================================================================
# 
# df.0 <- readRDS('data/processed/dataframes/filtered_swe_df.rds')
# 
# # Get original SWE columns for 2023/2024
# swe.cols <- grep('^2021|^2022', colnames(df.0), value = TRUE)
# # code for all of swe cols
# swe.cols <- grep('^20', colnames(df.0), value = TRUE)
# 
# df <- df.0 %>%
#   # get rid of year col and keep only 2023 and 2024 swe cols
#   select(x, y, year, all_of(swe.cols), cbibc, aspect, hli,
#          elev, pr, tmmx, tmmn, forest_type) %>%
#   # keep only elevations between 1500 and 2400
#   filter(elev >= 1500 & elev <= 2400) %>%
#   filter(forest_type == 'temperate_subpolar_needleleaf_forest') %>%
#   mutate(
#     burned_status = case_when(
#       cbibc < 1.5 ~ 'unburned / low severity',
#       cbibc >= 1.5 ~ 'med / high severity',
#       TRUE ~ NA_character_
#     ),
#     hli_group = ifelse(hli < 0.65, 'low_hli', 'high_hli'),
#     # give every pixel a unique ID number
#     ID = as.integer(factor(paste(x, y, sep = "_"))) 
#   ) %>%
#   rename_with(
#     ~ str_extract(.x, '^20\\d{2}_\\d{4}'),
#     all_of(swe.cols)
#   )
# 
# swe.cols <- grep('^2021|^2022', colnames(df), value = TRUE)
# 
# df.sample <- df %>%
#   group_by(burned_status, hli_group, year) %>%
#   slice_sample(n = 1000) %>%
#   ungroup()
# 
# # Select only columns for SWE in 2023 and 2024
# swe.cols <- grep('^2021|^2022', colnames(df.sample), value = TRUE)
# 
# df.long <- df.sample %>%
#   pivot_longer(
#     cols = all_of(swe.cols),        # all SWE columns for 2023 and 2024
#     names_to = 'date',              # new column with the date
#     values_to = 'swe'               # new column with the SWE value
#   ) %>%
#   mutate(
#     date = as.Date(date, format = '%Y_%m%d'),  # convert column names to Date
#     year = format(date, '%Y'),                 # extract year
#     month = as.numeric(format(date, '%m'))     # numeric month for plotting
#   )
# 
# swe.mean <- df.long %>%
#   group_by(year, burned_status, month) %>%
#   summarize(
#     n = n(),
#     mean_swe = mean(swe, na.rm = TRUE),
#     sd_swe = sd(swe, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(
#     sem = sd_swe / sqrt(n),                        # standard error
#     t_crit = qt(0.975, df = n - 1),               # t critical for 95% CI
#     CI_lower = mean_swe - t_crit * sem,           # lower bound
#     CI_upper = mean_swe + t_crit * sem            # upper bound
#   )
# 
# ggplot() +
#   
#   # bold mean lines
#   geom_line(data = swe.mean,
#             aes(x = month, y = mean_swe, color = burned_status),
#             size = 1.5) +
#   
#   scale_color_manual(values = c('med / high severity' = 'red',
#                                 'unburned / low severity' = 'blue')) +
#   scale_fill_manual(values = c('med / high severity' = 'red',
#                                'unburned / low severity' = 'blue')) +
#   
#   facet_wrap(~ year, ncol = 1) +
#   labs(x = 'Month', y = 'Peak SWE', color = 'Burn Status', fill = 'Burn Status',
#        title = 'SWE Time Series',
#        subtitle = 'Faded lines = individual pixels, Bold lines = mean') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# test <-  df.0 %>% filter(x == 280375, y == 4133925)


# ==============================================================================
# Finally successful time series
# ==============================================================================
# this one is good but I think the one splitting hli into 3 groups is better

# df.0 <- readRDS('data/processed/dataframes/filtered_swe_df.rds')
# 
# swe.cols <- grep('^20', colnames(df.0), value = TRUE)
# 
# df <- df.0 %>%
#   # get rid of year col and keep only 2023 and 2024 swe cols
#   select(x, y, year, all_of(swe.cols), cbibc, aspect, hli,
#          elev, pr, tmmx, tmmn, forest_type) %>%
#   # keep only elevations between 1500 and 2400
#   filter(elev >= 1500 & elev <= 2400) %>%
#   filter(forest_type == 'temperate_subpolar_needleleaf_forest') %>%
#   mutate(
#     burned_status = case_when(
#       cbibc < 1.5 ~ 'unburned / low severity',
#       cbibc >= 1.5 ~ 'med / high severity',
#       TRUE ~ NA_character_
#     ),
#     hli_group = ifelse(hli < 0.65, 'low_hli', 'high_hli'),
#     # give every pixel a unique ID number
#     ID = as.integer(factor(paste(x, y, sep = "_"))) 
#   # ) %>%
#   # rename_with(
#   #   ~ str_extract(.x, '^20\\d{2}_\\d{4}'),
#   #   all_of(swe.cols)
#   )
# 
# 
# # pivot df so that there is one row for every swe observation
# df.long <- df %>%
#   pivot_longer(
#     cols = all_of(swe.cols), # take all of my swe cols
#     names_to = 'date', # make new row for each one and put the column name in a column called 'date'
#     values_to = 'swe') %>% # put the values in a new col called 'swe'
#   mutate(
#     # convert column to name to date
#     date = as.Date(date, format = '%Y_%m%d'), 
#     swe_year = as.integer(format(date, '%Y')),
# 
#     # extract julian day
#     jd = yday(date)) %>%
#     # make sure year matches date (safety check)
#     
#     filter(swe_year == year) %>% # we only want to keep the rows where the year of the sample matches the year for that row
#     select(-swe_year) # get rid of the temp date and swe_year cols
# 
# 
# # create summarized dfs for plotting    
# df.summarized.jd <- df.long %>%
#   group_by(year, burned_status, jd) %>%
#   summarise(
#     mean.swe = mean(swe, na.rm = TRUE),
#     sd.swe   = sd(swe, na.rm = TRUE),
#     n        = sum(!is.na(swe)),
#     se       = sd.swe / sqrt(n),
#     ci_lower = mean.swe - 1.96 * se,
#     ci_upper = mean.swe + 1.96 * se,
#     .groups = 'drop'
#   )
# 
# df.summarized.date <- df.long %>%
#   group_by(year, burned_status, date) %>%
#   summarise(
#     mean.swe = mean(swe, na.rm = TRUE),
#     sd.swe   = sd(swe, na.rm = TRUE),
#     n        = sum(!is.na(swe)),
#     se       = sd.swe / sqrt(n),
#     ci_lower = mean.swe - 1.96 * se,
#     ci_upper = mean.swe + 1.96 * se,
#     .groups = 'drop'
#   )
# 
# df.summarized.hli <- df.long %>%
#   group_by(burned_status, hli_group, date) %>%
#   summarise(
#     mean.swe = mean(swe, na.rm = TRUE),
#     sd.swe   = sd(swe, na.rm = TRUE),
#     n        = sum(!is.na(swe)),
#     se       = sd.swe / sqrt(n),
#     ci_lower = mean.swe - 1.96 * se,
#     ci_upper = mean.swe + 1.96 * se,
#     .groups = 'drop'
#   )

# ==============================================================================
#                                 plots
# ==============================================================================
# simple plot faceting by year
ggplot(df.summarized.jd, aes(x = jd, y = mean.swe, color = burned_status, fill = burned_status)) +
  geom_line() +
  facet_wrap(~ year, scales = 'free_y') +
  theme_minimal()

# simple plot across all years
ggplot(df.summarized.date, aes(x = date, y = mean.swe, color = burned_status, fill = burned_status)) +
  geom_line() +
  theme_minimal()

ggplot(df.summarized.hli, aes(x = date, y = mean.swe, color = burned_status, fill = burned_status)) +
  geom_line() +
  facet_wrap(~ hli_group) +
  theme_minimal()

ggplot(df, aes(x = elev, fill = burned_status)) +
  geom_density(alpha = 0.5) 
ggplot(df, aes(x = aspect, fill = burned_status)) +
  geom_density(alpha = 0.5) 
ggplot(df, aes(x = hli_group, fill = burned_status)) +
  geom_density(alpha = 0.5)

# ==============================================================================
# let's try binning hli into 3 groups 
# ==============================================================================
df.0 <- readRDS('data/processed/dataframes/filtered_swe_df.rds')

swe.cols <- grep('^20', colnames(df.0), value = TRUE)

df <- df.0 %>%

  select(x, y, year, all_of(swe.cols), cbibc, aspect, hli,
         elev, pr, tmmx, tmmn, forest_type) %>%
  # keep only elevations between 1500 and 2400
  filter(elev >= 1500 & elev <= 2400) %>%
  filter(forest_type == 'temperate_subpolar_needleleaf_forest') %>%
  mutate(
    burned_status = case_when(
      cbibc < 1.5 ~ 'unburned / low severity',
      cbibc >= 1.5 ~ 'med / high severity',
      TRUE ~ NA_character_
    ),
    # hli group based on quantiles
    hli_group = ntile(hli, 3),
    hli_group = factor(
      hli_group,
      levels = c(1, 2, 3),
      labels = c('low_hli', 'mid_hli', 'high_hli')
    ),
    # give every pixel a unique ID number
    ID = as.integer(factor(paste(x, y, sep = "_"))) 
  )


# pivot df so that there is one row for every swe observation
df.long <- df %>%
  pivot_longer(
    cols = all_of(swe.cols), # take all of my swe cols
    names_to = 'date', # make new row for each one and put the column name in a column called 'date'
    values_to = 'swe') %>% # put the values in a new col called 'swe'
  mutate(
    # convert column to name to date
    date = as.Date(date, format = '%Y_%m%d'), 
    swe_year = as.integer(format(date, '%Y')),
    
    # extract julian day
    jd = yday(date)) %>%
  # make sure year matches date (safety check)
  
  filter(swe_year == year) %>% # we only want to keep the rows where the year of the sample matches the year for that row
  select(-swe_year) # get rid of the temp date and swe_year cols


# create summarized dfs for plotting    
df.summarized.jd <- df.long %>%
  group_by(year, burned_status, jd) %>%
  summarise(
    mean.swe = mean(swe, na.rm = TRUE),
    sd.swe   = sd(swe, na.rm = TRUE),
    n        = sum(!is.na(swe)),
    se       = sd.swe / sqrt(n),
    ci_lower = mean.swe - 1.96 * se,
    ci_upper = mean.swe + 1.96 * se,
    .groups = 'drop'
  )

df.summarized.date <- df.long %>%
  group_by(year, burned_status, date) %>%
  summarise(
    mean.swe = mean(swe, na.rm = TRUE),
    sd.swe   = sd(swe, na.rm = TRUE),
    n        = sum(!is.na(swe)),
    se       = sd.swe / sqrt(n),
    ci_lower = mean.swe - 1.96 * se,
    ci_upper = mean.swe + 1.96 * se,
    .groups = 'drop'
  )

df.summarized.hli <- df.long %>%
  group_by(burned_status, hli_group, date) %>%
  summarise(
    mean.swe = mean(swe, na.rm = TRUE),
    sd.swe   = sd(swe, na.rm = TRUE),
    n        = sum(!is.na(swe)),
    se       = sd.swe / sqrt(n),
    ci_lower = mean.swe - 1.96 * se,
    ci_upper = mean.swe + 1.96 * se,
    .groups = 'drop'
  )

ggplot(df.summarized.hli, aes(x = date, y = mean.swe, color = burned_status, fill = burned_status)) +
  geom_line() +
  facet_wrap(~ hli_group) +
  theme_minimal()


###### just for plotting for poster #####

df.summarized.hli.0 <- df.long %>%
  group_by(burned_status, hli_group, date) %>%
  summarise(
    mean.swe = mean(swe, na.rm = TRUE),
    sd.swe   = sd(swe, na.rm = TRUE),
    n        = sum(!is.na(swe)),
    se       = sd.swe / sqrt(n),
    ci_lower = mean.swe - 1.96 * se,
    ci_upper = mean.swe + 1.96 * se,
    .groups = 'drop'
  )

df.summarized.hli <- df.summarized.hli.0 %>%
  mutate(
    hli_group = factor(
      hli_group,
      levels = c("low_hli", "mid_hli", "high_hli"),
      labels = c("Low HLI", "Mid HLI", "High HLI")
    ),
    burned_status = factor(
      burned_status,
      levels = c("unburned / low severity", "med / high severity"),
      labels = c("Unburned / Low Severity", "Med / High Severity")
    )
  ) %>%
  rename(
    HLI_Group = hli_group,
    Burned_Status = burned_status
  )


ggplot() +

  
  # smoothed mean line
  geom_line(
    data = df.summarized.hli,
    aes(x = date, y = mean.swe, color = Burned_Status),
    method = "loess",
    span = 0.4,
    se = FALSE,
    size = 1.3
  ) +
  
  facet_wrap(~ HLI_Group, ncol = 3, scales = 'free_y') +
  
  scale_color_manual(values = c(
    "Unburned / Low Severity" = "#22A884",
    "Med / High Severity"     = "#440154"
  )) +
  scale_fill_manual(values = c(
    "Unburned / Low Severity" = "#22A884",
    "Med / High Severity"     = "#440154"
  )) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA)
  ) +
  labs(
    x = "",
    y = "SWE (m)",
    title = "Full SWE Time Series (All Years)",
    subtitle = "Mean ± 95% CI ribbon with LOESS smoothing",
    color = "Burned Status",
    fill  = "Burned Status"
  )

  
# ==============================================================================
# let's try the subsampling
# ==============================================================================

# take an equal number of pixels per combo
df.sub <- df %>%
  filter(year %in% c('2023')) %>%
  group_by(hli_group, burned_status, year) %>%
  slice_sample(n = 100) %>%   # same n in every combo
  ungroup()

# pivot df so that there is one row for every swe observation
df.long.sub <- df.sub %>%
  pivot_longer(
    cols = all_of(swe.cols), # take all of my swe cols
    names_to = 'date', # make new row for each one and put the column name in a column called 'date'
    values_to = 'swe') %>% # put the values in a new col called 'swe'
  mutate(
    # convert column to name to date
    date = as.Date(date, format = '%Y_%m%d'), 
    swe_year = as.integer(format(date, '%Y')),
    
    # extract julian day
    jd = yday(date)) %>%
  # make sure year matches date (safety check)
  
  filter(swe_year == year) %>% # we only want to keep the rows where the year of the sample matches the year for that row
  select(-swe_year) # get rid of the temp date and swe_year cols

# ggplot(df.summarized.hli, aes(x = date, y = mean.swe, color = burned_status, fill = burned_status)) +
#   geom_line() +
#   facet_wrap(~ hli_group) +
#   theme_minimal()

df.sub.ribbon <- df.long.sub %>%
  group_by(year, HLI_Group, Burned_Status, date) %>%
  summarise(
    mean_swe = mean(swe, na.rm = TRUE),
    sd_swe   = sd(swe, na.rm = TRUE),
    n        = sum(!is.na(swe)),
    se       = sd_swe / sqrt(n),
    ci_lower = mean_swe - 1.96 * se,
    ci_upper = mean_swe + 1.96 * se,
    .groups = "drop"
  )

df.long.sub <- df.long.sub %>%
  mutate(
    hli_group = factor(
      hli_group,
      levels = c("low_hli", "mid_hli", "high_hli"),
      labels = c("Low HLI", "Mid HLI", "High HLI")
    ),
    burned_status = factor(
      burned_status,
      levels = c("unburned / low severity", "med / high severity"),
      labels = c("Unburned / Low Severity", "Med / High Severity")
    )
  ) %>%
  rename(
    Burned_Status = burned_status,
    HLI_Group = hli_group
  )

ggplot() +
  
  # raw time series
  geom_line(
    data = df.long.sub,
    aes(x = date, y = swe, group = ID, color = Burned_Status),
    alpha = 0.15
  ) +
  
  # ribbon
  geom_ribbon(
    data = df.sub.ribbon,
    aes(x = date, ymin = ci_lower, ymax = ci_upper, fill = Burned_Status),
    alpha = 0.2,
    color = NA
  ) +
  
  # mean line
  geom_line(
    data = df.sub.ribbon,
    aes(x = date, y = mean_swe, color = Burned_Status),
    size = 1.4
  ) +
  
  facet_wrap(~ HLI_Group) +
  
  # viridis colors
  scale_color_manual(values = c(
    'Unburned / Low Severity' = '#1F9E89FF',
    'Med / High Severity'     = '#440154FF'
  )) +
  scale_fill_manual(values = c(
    'Unburned / Low Severity' = '#1F9E89FF',
    'Med / High Severity'     = '#440154FF'
  )) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # 🔹 Center titles
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  ) +
  
  labs(
    x = "",
    y = "SWE (m)",
    title = "2023 SWE Time Series by Heat Load Index (HLI)",
    subtitle = "Pixel-level Snow Water Equivalent for subset of 2023 data (with mean ± 95% CI)",
    color = "Burned Status",
    fill  = "Burned Status"
  )





install.packages('colorspace')
library('colorspace')
pal <- (c('#D55E00', '#56B4E9'))
colorspace::swatchplot(pal, cvd = TRUE)

# ==============================================================================
# let's try binning aspect to make sure the distributions are even
# ==============================================================================
# didn't really make a difference 
# 1. Add aspect bins
# df_with_bins <- df %>%
#   mutate(
#     aspect_bin = cut(
#       aspect,
#       breaks = seq(0, 2*pi, length.out = 9),
#       include.lowest = TRUE,
#       labels = c('N','NE','E','SE','S','SW','W','NW')
#     ))
# 
# # 2. Compute group sizes separately
# group_sizes <- df_with_bins %>%
#   count(aspect_bin, burned_status, name = "group_n")
# 
# # 3. Join sizes back + sample evenly
# df2 <- df_with_bins %>%
#   left_join(group_sizes, by = c("aspect_bin", "burned_status")) %>%
#   group_by(aspect_bin, burned_status) %>%
#   slice_sample(n = min(group_sizes$group_n)) %>%
#   select(-group_n) %>%
#   ungroup()
# 
# # density plots for aspect and elevation
# ggplot(df2, aes(x = aspect, fill = burned_status)) +
#   geom_density(alpha = 0.5) 
# ggplot(df2, aes(x = elev, fill = burned_status)) +
#   geom_density(alpha = 0.5) 
# ggplot(df2, aes(x = hli, fill = burned_status)) +
#   geom_density(alpha = 0.5)
# 
# # pivot df so that there is one row for every swe observation
# df.long.2 <- df2 %>%
#   pivot_longer(
#     cols = all_of(swe.cols), # take all of my swe cols
#     names_to = 'date', # make new row for each one and put the column name in a column called 'date'
#     values_to = 'swe') %>% # put the values in a new col called 'swe'
#   mutate(
#     # convert column to name to date
#     date = as.Date(date, format = '%Y_%m%d'), 
#     swe_year = as.integer(format(date, '%Y')),
#     
#     # extract julian day
#     jd = yday(date)) %>%
#   # make sure year matches date (safety check)
#   
#   filter(swe_year == year) %>% # we only want to keep the rows where the year of the sample matches the year for that row
#   select(-swe_year) # get rid of the temp date and swe_year cols
# 
# df.summarized.2.hli <- df.long.2 %>%
#   group_by(hli_group, burned_status, date) %>%
#   summarise(
#     mean.swe = mean(swe, na.rm = TRUE),
#     sd.swe   = sd(swe, na.rm = TRUE),
#     n        = sum(!is.na(swe)),
#     se       = sd.swe / sqrt(n),
#     ci_lower = mean.swe - 1.96 * se,
#     ci_upper = mean.swe + 1.96 * se,
#     .groups = 'drop'
#   )
# ggplot(df.summarized.2.hli, aes(x = date, y = mean.swe, color = burned_status, fill = burned_status)) +
#   geom_line() +
#   facet_wrap(~ hli_group) +
#   theme_minimal()
