# Load packages
packages <- c( 'here', 'exactextractr', 'raster', 'sf', 'terra', 'geodata',
               'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr', 'stringr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Directory with input TIFs
tif.dir <- here('data', 'processed', 'processed', 'tif')

# List files with matching keywords
tif.files <- list.files(tif.dir, pattern = '\\.tif$', full.names = TRUE)

# Filter files by basin
creek.tifs   <- tif.files[str_detect(tif.files, 'SanJoaquin.*clipped')]

file_info <- tibble(fname = basename(creek.tifs)) %>%
  mutate(
    fire       = 'creek',
    year       = str_extract(fname, '\\d{4}'),
    month      = str_extract(fname, '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)'),
    day        = str_extract(fname, '(?<=\\d{4}[A-Za-z]{3})\\d{1,2}'),  # 1- or 2-digit day
    date       = as.Date(str_c(year, month, str_pad(day, 2, pad = '0'), sep = '-'), format = '%Y-%b-%d'),
    variable   = str_extract(fname, '(?<=_)swe|depth|melt(?=_)'),  # tweak if needed
    resolution = str_extract(fname, '\\d{2,}m')
  )

# remove report tifs/superswe tifs
file_info <- file_info[-c(1, 2, 18), ]

