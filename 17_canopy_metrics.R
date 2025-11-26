packages <- c('here', 'terra')
install.packages(setdiff(packages, row.names(installed.packages())))
lapply(packages, library, character.only = T)


mean.csm <- rast(here('data', 'raw', 'ALS', 'tif', 'CreekFire_2021_MeanCSM_Meters.tif'))
rm(meancsm)

summary(mean.csm)

#test