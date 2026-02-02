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
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics'
dir.create(out.dir, recursive = T, showWarnings = F)
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

vrt.file <- 'data/processed/processed/tif/1m/creek_chm/creek_rasterize_canopy.vrt'
# vrt(chm.files, filename = vrt.file, overwrite = TRUE)
chm.vrt <- rast(vrt.file)



# ----- one-tile processing function -----
gap.metric.single.tile <- function(vrt.file,
                                   out.dir,
                                   gap.ht.m = 2, # gap threshold: chm < 2m
                                   gap.breaks = c(0, 10, 100, 1000, Inf),
                                   buffer.m = 100) {
  
  # load vrt as raster object (points to all tiles without loading all of them)
  chm.vrt <- rast(vrt.file)
  
  # return a function that processes one tile file path f
  function(f) {
  
  # ----- establish "core tile" -----
  chm.core.tile <- rast(f)
  ext.core <- ext(chm.core.tile) # get geographic extent of the tile
  
  # ----- build buffered extent around the tile core -----
  ext.buf <- ext(
    ext.core$xmin - buffer.m, ext.core$xmax + buffer.m,
    ext.core$ymin - buffer.m, ext.core$ymax + buffer.m
  )
  
  # ----- crop from the VRT so the buffer can pull neighbor tiles -----

  
  # crop vrt to buffered extent
  chm.buf <- crop(chm.vrt, ext.buf)
  
  # crop vrt to core extent, used for empty raster template
  chm.core <- crop(chm.vrt, ext.core)
  
  # ----- define aggregation factor for 50m as outputs -----
  # fact = how many 1m cells fit in 50m
  fact <- as.integer(round(50 / res(chm.buf)[1])) # should be 50
  
  # check
  if (fact < 1) {
    stop('Aggregation factor < 1. Check CHM resolution for file: ', f)
  }
  
  # clean base name for writing outputs
  base <- tools::file_path_sans_ext(basename(f))
  
  # ----- GAP MASK, PATCH IDS, GAP AREA BINS -----
  
  # make 1m gap mask
  gap.mask.1m <- ifel(!is.na(chm.buf) & chm.buf < gap.ht.m, 1, NA)
  names(gap.mask.1m) <- 'gap_mask'
  
  # label connected gap objects 
  # each connected gap gets a unique ID
  gap.id.buf <- patches(gap.mask.1m, directions = 8)
  names(gap.id.buf) <- 'gap_id'
  
  # pull gap ids(per-pixel) so we can count pixels per ID
  # pull into vector
  gaps <- values(gap.id.buf, mat = F)
  # Drop NA
  gaps <- gaps[!is.na(gaps)]
  
  # ----- handle tiles where there are no gaps -----
  
  if (length(gaps) == 0) {
    
    # Build a 50 m template aligned to the core extent
    # (aggregate() returns a raster with correct dimensions and georeferencing)
    empty50 <- aggregate(chm.core, fact = fact, fun = mean, na.rm = TRUE)
    
    # Set all values to NA (because there are no gaps / no meaningful metrics)
    values(empty50) <- NA_real_
    
    gap.dist.metrics.50 <- c(empty50, empty50, empty50, empty50, empty50,
                             empty50, empty50, empty50)
    
    names(gap.dist.metrics.50) <- c(
      'gap_pct','gap_small_pct','gap_medium_pct','gap_large_pct','gap_xlarge_pct',
      'dist_to_gap_mean','dist_to_canopy_mean','dist_to_canopy_max'
    )
    
    writeRaster(
      gap.dist.metrics.50,
      filename = file.path(out.dir, paste0(base, '_gap_dist_metrics_50m.tif')),
      overwrite = TRUE
    )
    
    return(data.frame(tile = base, n.gaps = 0))
  }
  
  # ----- area per gap ID -----
  gap.areas <- table(gaps) * prod(res(gap.id.buf))
  
  # lookup table: gap id to gap area
  gap.df <- data.frame(
    gap_id = as.integer(names(gap.areas)),
    gap_area_m2 = as.numeric(gap.areas)
  )
  
  # ----- bin into size classes -----
  # small: 0 - 10
  # medium: (10 - 100)
  # large: (100 - 1000)
  # xlarge: (1000+)
  
  gap.df$gap_class <- cut(
    gap.df$gap_area_m2,
    breaks = gap.breaks,
    labels = c('small', 'medium', 'large', 'xlarge')
  )
  
  # build reclassificaion table for classiry
  gap.lookup <- data.frame(
    from  = gap.df$gap_id,
    to = as.integer(gap.df$gap_class)
  )
  
  gap.lookup <- as.matrix(gap.lookup)
  
  # create 1m raster of gap class (1-4)
  # pixels in the same gap ID get the class corresponding to the full object size
  gap.class.buf <- classify(gap.id.buf, rcl = gap.lookup)
  names(gap.class.buf) <- 'gap_class'
  
  # crop back to core tile before 50m aggregation
  gap.id.core <- crop(gap.id.buf, ext.core)
  gap.class.core <- crop(gap.class.buf, ext.core)
  
  # ----- Aggregate gap metrics to 50m -----
  
  # overall gap fraction per 50m cell
  gap.pct.50 <- aggregate(!is.na(gap.id.core), fact = fact, fun = mean, na.rm = T)
  names(gap.pct.50) <- 'gap_pct'
  
  # class-specific gap fractions per 50m cell
  gap.small <- aggregate(gap.class.core == 1, fact = fact, fun = mean, na.rm = T)
  gap.medium <- aggregate(gap.class.core == 2, fact = fact, fun = mean, na.rm = T)
  gap.large <- aggregate(gap.class.core == 3, fact = fact, fun = mean, na.rm = T)
  gap.xlarge <- aggregate(gap.class.core == 4, fact = fact, fun = mean, na.rm = T)
  
  names(gap.small) <- 'gap_small_pct'
  names(gap.medium) <- 'gap_medium_pct'
  names(gap.large) <- 'gap_large_pct'
  names(gap.xlarge) <- 'gap_xlarge_pct'
  
  
  
  # ------------ DISTANCE METRICS --------------
  # build 'feature masks' for distance()
  gap.mask.for.dist <- ifel(!is.na(chm.buf) & chm.buf < gap.ht.m, 1, NA)
  
  # same for canopy
  canopy.mask.for.dist <- ifel(!is.na(chm.buf) & chm.buf >= gap.ht.m, 1, NA)
  
  # dist to nearest gap
  dist.to.gap.all <- distance(gap.mask.for.dist)
  
  # dist to nearest canopy
  dist.to.canopy.all <- distance(canopy.mask.for.dist)
  
  # dist_to_gap is meaningful only for canopy pixels, so mask others
  dist.to.gap.buf <- mask(dist.to.gap.all, canopy.mask.for.dist)
  
  # same but for dist_to_canopy
  dist.to.canopy.buf <- mask(dist.to.canopy.all, gap.mask.for.dist)
  
  # crop buffered distance rasters back to core tile
  dist.to.gap.core <- crop(dist.to.gap.buf, ext.core)
  dist.to.canopy.core <- crop(dist.to.canopy.buf, ext.core)
  
  # ----- aggregate distances to 50m -----
  
  dist.to.gap.mean <- aggregate(dist.to.gap.core, fact = fact, fun = mean, na.rm = T)
  names(dist.to.gap.mean) <- 'dist_to_gap_mean'
  dist.to.canopy.mean <- aggregate(dist.to.canopy.core, fact = fact, fun = mean, na.rm = T)
  names(dist.to.canopy.mean) <- 'dist_to_canopy_mean'
  dist.to.canopy.max <- aggregate(dist.to.canopy.core, fact = fact, fun = max, na.rm = T)
  names(dist.to.canopy.max) <- 'dist_to_canopy_max'
  
  dist.out <- c(dist.to.gap.mean, dist.to.canopy.mean, dist.to.canopy.max)
  
  # -------------- WRITE OUTPUTS -------------------
  
  # combine into 1 multi-layer output
  gap.dist.metrics.50 <- c(gap.pct.50, gap.small, gap.medium, gap.large, gap.xlarge,
                           dist.to.gap.mean, dist.to.canopy.mean, dist.to.canopy.max)
  
  # write metrics
  writeRaster(
    gap.dist.metrics.50,
    filename = file.path(out.dir, paste0(base, '_gap_dist_metrics_50m.tif')),
    overwrite = T)
  
  # return summary row to track progress
  data.frame(
    tile = base, 
    n.gaps = nrow(gap.df)
  )
  } 
}
  


# ==============================================================================
# Calculate metrics
# ==============================================================================

# -------------- test run on 5 tiles -------------
gap.fun <- gap.metric.single.tile(vrt.file, out.dir, buffer.m = 100)
summary.5 <- lapply(chm.files[1:5], gap.fun) #12:46
summary.5 <- do.call(rbind, summary.5)
summary.5

# visually inspect
r1 <- rast(file.path(out.dir,
                     'creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7732_norm_gap_dist_metrics_50m.tif'))
r2 <- rast(file.path(out.dir,
                           'creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7733_norm_gap_dist_metrics_50m.tif'))

plot(r1$gap_large_pct)  
plot(r2$gap_large_pct)  

summary(values(r1$gap_pct))
summary(values(r1$dist_to_gap_mean))

summary(values(1 - r1$gap_pct))


# -------------- test on 36 tiles ---------------
# ----- select contiguous 6 x 6 km block to test on -----
ctg.norm <- readLAScatalog('data/processed/ALS/normalized/creek') 
d <- ctg.norm@data
nms <- names(d)

xmn_name <- nms[grep('Min\\.X|^xmin$|Xleft',   nms, ignore.case = TRUE)][1]
xmx_name <- nms[grep('Max\\.X|^xmax$|Xright',  nms, ignore.case = TRUE)][1]
ymn_name <- nms[grep('Min\\.Y|^ymin$|Ybottom', nms, ignore.case = TRUE)][1]
ymx_name <- nms[grep('Max\\.Y|^ymax$|Ytop',    nms, ignore.case = TRUE)][1]

xmn <- d[[xmn_name]]; xmx <- d[[xmx_name]]
ymn <- d[[ymn_name]]; ymx <- d[[ymx_name]]

x0 <- 310000
y0 <- 4130000
block_m <- 6000

xmin_b <- x0 - block_m/2
xmax_b <- x0 + block_m/2
ymin_b <- y0 - block_m/2
ymax_b <- y0 + block_m/2

keep <- (xmx > xmin_b) & (xmn < xmax_b) & (ymx > ymin_b) & (ymn < ymax_b)

files.sub <- d$filename[keep]
length(files.sub)

chm.dir <- 'data/processed/processed/tif/1m/creek_chm'

tile.base <- tools::file_path_sans_ext(basename(files.sub))   # e.g. "..._norm"
tiles.36 <- file.path(chm.dir, paste0('creek_chm_', tile.base, '.tif'))

# keep only the ones that actually exist (and warn if any missing)
exists <- file.exists(tiles.36)

if (any(!exists)) {
  warning('Missing CHM files for ', sum(!exists), ' selected tiles. Example missing:\n',
          paste(head(tiles.36[!exists], 5), collapse = '\n'))
}

tiles.36 <- tiles.36[exists]
length(tiles.36)

# ----- test 36 tile block in parallel -----

plan(multisession, workers = 8)

out.dir.36 <- file.path(out.dir, 'test_36_gap_dist')
dir.create(out.dir.36, recursive = T, showWarnings = F)

start <- Sys.time()
summary.36 <- future_lapply(tiles.36, function(f){
  
  gap.fun <- gap.metric.single.tile(
    vrt.file = vrt.file,
    out.dir  = out.dir.36,
    buffer.m = 100
  )
  
  # returned function runs once per tile
    tryCatch(
      gap.fun(f),
      error = function(e) data.frame(
        tile = tools::file_path_sans_ext(basename(f)),
        n.gaps = NA_integer_,
        error = conditionMessage(e)
      )
    )
  
})


summary.36 <- do.call(rbind, summary.36)
end <- Sys.time()
message('36-tile block finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')

summary.36
# 2.07 minutes










# -------------- FINAL RUN ----------------------
plan(multisession, workers = 10)

out.dir.gap.dist <- file.path(out.dir, 'gap_dist')
dir.create(out.dir.gap.dist, recursive = TRUE, showWarnings = FALSE)

start <- Sys.time()
summary.all <- future_lapply(chm.files, function(f){
  
  gap.fun <- gap.metric.single.tile(
    vrt.file = vrt.file,
    out.dir  = out.dir.gap.dist,
    buffer.m = 100
  )
  
  # returned function runs once per tile
  tryCatch(
    gap.fun(f),
    error = function(e) data.frame(
      tile = tools::file_path_sans_ext(basename(f)),
      n.gaps = NA_integer_,
      error = conditionMessage(e)
    )
  )
  
})


summary.all <- do.call(rbind, summary.all)
end <- Sys.time()
message('Full run finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')

# save summary
saveRDS(summary.all, file.path(out.dir.gap.dist, 'summary_all.rds'))
write.csv(summary.all, file.path(out.dir.gap.dist, 'summary_all.csv'), row.names = FALSE)


# quick checks
table(is.na(summary.all$n.gaps))
subset(summary.all, !is.na(error))
summary(summary.all$n.gaps)



#13:54 2/2/26




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