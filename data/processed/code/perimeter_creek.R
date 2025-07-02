# set working directory (PC)
setwd("C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice")

# mac
#setwd("/Users/kyliemosher/OneDrive/thesis/fireandice")

packages <- (c('exactextractr', 'raster', 'sf', 'terra', 'geodata', 'tidyverse', 'spatialEco', 'patchwork', 'knitr', 'dplyr'))
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

creek.fire.0 <- st_read('data/raw/fire_info/shp/creek_fire_perim.shp')
creek.fire.perimeter <- creek.fire.0[creek.fire.0$ALARM_DATE == as.Date('2020-09-04'), ]
creek.fire.perimeter <- creek.fire.perimeter %>% select(-Shape_Area)

plot(st_geometry(creek.fire.perimeter), col = adjustcolor('green', alpha.f = 0.5), main = 'Creek Study Extent')

st_write(creek.fire.perimeter, 'data/raw/fire_info/shp/creek_fire_perimeter.shp', delete_layer = T)
