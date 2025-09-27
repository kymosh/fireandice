packages <- c('tidyverse', 'terra')
install.packages(set.diff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# resample all data to SDD resolution

in.dir <- here('data', 'processed', 'processed', 'tif')

all.variables <- list.files(in.dir, pattern = )