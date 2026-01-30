packages <- c('terra', 'sf', 'mapview', 'lidR', 'dplyr', 'raster', 'future', 'future.apply', 'stringr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
#  Gap Metrics
# ==============================================================================

# ----- setup -----
# use 70% of available memory before spilling to temp files
terraOptions(memfrac = 0.7)

chm.dir <- 'data/processed/processed/tif/1m/creek_chm'
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/gap'
dir.create(out.dir, recursive = T, showWarnings = F)
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)
vrt.file <- vrt(chm.files)

# 
gap.metric.single.tile <- function()
  
  
  
  # ----- below is the gap metric code I had previously used -----
# define gap as height is less than 2m
gap.mask.1m <- ifel(!is.na(chm) & chm < 2, 1, NA)
names(gap.mask.1m) <- 'gap_mask'

# check
table(values(gap.mask.1m), useNA = "ifany")
plot(gap.mask.1m)

# create individual gaps with their own ID numbers
gap.id.1m <- patches(gap.mask.1m, directions = 8)
names(gap.id.1m) <- 'gap_id'

summary(gap.id.1m)

# add gap size
gaps <- values(gap.id.1m)
gaps <- gaps[!is.na(gaps)]

# calculate gap areas
gap.areas <- table(gaps) * prod(res(gap.id.1m))
# create df of indiv gaps and their areas
gap.df <- data.frame(gap_ID = as.integer(names(gap.areas)),
                     gap_area_m2 = as.numeric(gap.areas))
# inspect
head(gap.df)
summary(gap.df$gap_area_m2)

##### break into ecologically meaningful gap size bins
gap.df$gap_class <- cut(
  gap.df$gap_area_m2,
  breaks = c(0, 10, 100, 1000, Inf),
  labels = c('small', 'medium', 'large', 'xlarge')
)

# map gap class back to the raster
gap.lookup <- data.frame(gap_id = gap.df$gap_ID,
                         gap_class = as.integer(gap.df$gap_class))

gap.class.1m <- classify(gap.id.1m, rcl = as.matrix(gap.lookup))

# ------- aggregate to 50m -------
fact <-  50 / res(gap.class.1m)[1]

# core gap pct
gap.pct.50 <- aggregate(
  !is.na(gap.id.1m),
  fact = fact,
  fun = mean,
  na.rm = T
)
names(gap.pct.50) <- 'gap_pct'

# gap-specific percentages
gap_small <- aggregate(
  gap.class.1m == 1,
  fact = fact,
  fun = mean,
  na.rm = T
)
gap_medium <- aggregate(
  gap.class.1m == 2,
  fact = fact,
  fun = mean,
  na.rm = T
)
gap_large <- aggregate(
  gap.class.1m == 3,
  fact = fact,
  fun = mean,
  na.rm = T
)
gap_xlarge <- aggregate(
  gap.class.1m == 4,
  fact = fact,
  fun = mean,
  na.rm = T
)

names(gap_small)  <- 'gap_small_pct'
names(gap_medium) <- 'gap_medium_pct'
names(gap_large)  <- 'gap_large_pct'
names(gap_xlarge) <- 'gap_xlarge_pct'



# visualize
plot(gap_large)

ext_small <- ext(308350, 308550, 4135500, 4135700)

terra::plot(
  terra::crop(gap.class.1m, ext_small),
  col = viridisLite::viridis(100, direction = -1),
  main = 'Gap Mask: Zoom'
)

plot(gap.class.1m, col = viridisLite::viridis(100, direction = -1))

ext_chm <- ext(308000, 309000, 4135000, 4139000) 
terra::plot(
  terra::crop(swe.rast$cbibc, ext_chm),
  col = viridisLite::viridis(100),
  main = 'Gap Mask: Zoom'
)

# ------- distance to gap/canopy -----------------

# recalculate gap mask to keep NAs (necessary for dist calculation)
gap.mask.for.dist <- ifel(!is.na(chm) & chm < 2, 1, NA)
dist.to.gap.all <- distance(gap.mask.for.dist)
names(dist.to.gap.all) <- 'dist_to_gap'


canopy.mask.for.dist <- ifel(!is.na(chm) & chm >= 2, 2, NA)
dist.to.canopy.all <- distance(canopy.mask.for.dist)
names(dist.to.canopy.all) <- 'dist_to_canopy'

# mask out gap pixels
dist.to.gap <- mask(dist.to.gap.all, canopy.mask.for.dist)
# mask out canopy pixels
dist.to.canopy <- mask(dist.to.canopy.all, gap.mask.for.dist)

# ------- aggregate to 50m -----

dist.to.gap.mean <- aggregate(dist.to.gap, fact = fact, fun = mean, na.rm = T)


dist.to.canopy.mean <- aggregate(dist.to.canopy, fact = fact, fun = mean, na.rm = T)
dist.to.canopy.max <- aggregate(dist.to.canopy, fact = fact, fun = max, na.rm = T)

# rename
names(dist.to.gap.mean)    <- 'dist_to_gap_mean'

names(dist.to.canopy.mean) <- 'dist_to_canopy_mean'
names(dist.to.canopy.max)  <- 'dist_to_canopy_max'