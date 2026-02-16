packages <- c('sf', 'mapview', 'lidR', 'dplyr', 'raster', 'future', 'future.apply', 'stringr', 'terra')
# install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
#  Gap Metrics
# ==============================================================================


# ----- setup -----
# use 70% of available memory before spilling to temp files
terraOptions(memfrac = 0.7)

chm.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/gap_dist_32611'
dir.create(out.dir, recursive = T, showWarnings = F)
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

vrt.file <- 'data/processed/processed/tif/1m/creek_chm_32611/creek_chm_1m_32611.vrt'
chm.vrt <- rast(vrt.file)


# ----- one-tile processing function -----
gap.metric.single.tile <- function(vrt.file,
                                   out.dir,
                                   gap.ht.m = 2,
                                   gap.breaks = c(0, 10, 100, 1000, Inf),
                                   buffer.m = 100,
                                   out.res.m = 50) {
  
  chm.vrt <- rast(vrt.file)
  
  # helper: aggregate mean on 0/1 rasters but keep NA if a cell is all-NA
  agg.mean <- function(r, fact) {
    x <- aggregate(r, fact = fact, fun = mean, na.rm = TRUE)
    v <- values(x, mat = FALSE)
    v[is.nan(v)] <- NA_real_
    values(x) <- v
    x
  }
  
  function(f) {
    
    chm.core.tile <- rast(f)
    ext.core <- ext(chm.core.tile)
    
    ext.buf <- ext(
      ext.core$xmin - buffer.m, ext.core$xmax + buffer.m,
      ext.core$ymin - buffer.m, ext.core$ymax + buffer.m
    )
    
    chm.buf  <- crop(chm.vrt, ext.buf)
    chm.core <- crop(chm.vrt, ext.core)
    
    fact <- as.integer(round(out.res.m / res(chm.buf)[1]))
    if (fact < 1) stop('Aggregation factor < 1. Check CHM resolution for file: ', f)
    
    base <- tools::file_path_sans_ext(basename(f))
    
    # ---------------- GAP BOOLEAN (preserve NA) ----------------
    # logical TRUE/FALSE, but NA where CHM is NA
    gap.bool.buf  <- ifel(is.na(chm.buf),  NA, chm.buf <  gap.ht.m)
    canopy.bool.buf <- ifel(is.na(chm.buf), NA, chm.buf >= gap.ht.m)
    
    # For patches(): ONLY gaps should be non-NA
    gap.features.buf <- ifel(gap.bool.buf == 1, 1, NA)
    
    # ---------------- PATCH IDS + GAP SIZE CLASSES ----------------
    gap.id.buf <- patches(gap.features.buf, directions = 8)
    names(gap.id.buf) <- 'gap_id'
    
    gaps <- values(gap.id.buf, mat = FALSE)
    gaps <- gaps[!is.na(gaps)]
    
    # templates aligned to core at 50m
    template50 <- aggregate(chm.core, fact = fact, fun = mean, na.rm = TRUE)
    
    # handle tiles with no gaps
    if (length(gaps) == 0) {
      
      # gap % is 0 where CHM exists, NA where CHM is NA
      gap.pct.50 <- agg.mean(as.numeric(crop(gap.bool.buf, ext.core)), fact)
      gap.pct.50 <- ifel(is.na(template50), NA, gap.pct.50)
      
      zero <- gap.pct.50 * 0
      
      na50 <- template50
      values(na50) <- NA_real_
      
      gap.dist.metrics.50 <- c(
        gap.pct.50,
        zero, zero, zero, zero,
        na50, na50, na50
      )
      
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
    
    # area per gap id (m2)
    gap.areas <- table(gaps) * prod(res(gap.id.buf))
    
    gap.df <- data.frame(
      gap_id = as.integer(names(gap.areas)),
      gap_area_m2 = as.numeric(gap.areas)
    )
    
    gap.df$gap_class <- cut(
      gap.df$gap_area_m2,
      breaks = gap.breaks,
      labels = c('small', 'medium', 'large', 'xlarge')
    )
    
    gap.lookup <- as.matrix(data.frame(
      from = gap.df$gap_id,
      to   = as.integer(gap.df$gap_class)   # 1..4
    ))
    
    gap.class.buf <- classify(gap.id.buf, rcl = gap.lookup)
    names(gap.class.buf) <- 'gap_class'
    
    # crop to core for aggregation
    gap.id.core    <- crop(gap.id.buf, ext.core)
    gap.class.core <- crop(gap.class.buf, ext.core)
    
    gap.bool.core <- crop(gap.bool.buf, ext.core)
    
    # helper: 1 where condition TRUE, 0 where FALSE, NA where CHM is NA
    bin01_na <- function(cond, chm) {
      ifel(is.na(chm), NA_real_, ifel(cond, 1, 0))
    }
    
    
    # ---------------- GAP % (0/1 with NA preserved) ----------------
    gap.pct.50 <- agg.mean(as.numeric(gap.bool.core), fact)
    gap.pct.50 <- mask(gap.pct.50, template50)
    names(gap.pct.50) <- 'gap_pct'
    
    # IMPORTANT: class % should be relative to ALL valid pixels, not just gap pixels
    gap.small  <- agg.mean(bin01_na(gap.class.core == 1, chm.core), fact)
    gap.medium <- agg.mean(bin01_na(gap.class.core == 2, chm.core), fact)
    gap.large  <- agg.mean(bin01_na(gap.class.core == 3, chm.core), fact)
    gap.xlarge <- agg.mean(bin01_na(gap.class.core == 4, chm.core), fact)
    
    gap.small  <- mask(gap.small,  template50)
    gap.medium <- mask(gap.medium, template50)
    gap.large  <- mask(gap.large,  template50)
    gap.xlarge <- mask(gap.xlarge, template50)
    
    names(gap.small)  <- 'gap_small_pct'
    names(gap.medium) <- 'gap_medium_pct'
    names(gap.large)  <- 'gap_large_pct'
    names(gap.xlarge) <- 'gap_xlarge_pct'
    
    
    # ---------------- DISTANCE METRICS ----------------
    # distance() computes distance to nearest non-NA cell
    gap.feature.dist   <- ifel(gap.bool.buf == 1,    1, NA)
    canopy.feature.dist <- ifel(canopy.bool.buf == 1, 1, NA)
    
    dist.to.gap.all    <- distance(gap.feature.dist)
    dist.to.canopy.all <- distance(canopy.feature.dist)
    
    # dist to gap is meaningful only for canopy pixels
    dist.to.gap.buf <- mask(dist.to.gap.all, canopy.feature.dist)
    
    # dist to canopy is meaningful only for gap pixels
    dist.to.canopy.buf <- mask(dist.to.canopy.all, gap.feature.dist)
    
    dist.to.gap.core    <- crop(dist.to.gap.buf, ext.core)
    dist.to.canopy.core <- crop(dist.to.canopy.buf, ext.core)
    
    dist.to.gap.mean <- aggregate(dist.to.gap.core, fact = fact, fun = mean, na.rm = TRUE)
    dist.to.canopy.mean <- aggregate(dist.to.canopy.core, fact = fact, fun = mean, na.rm = TRUE)
    dist.to.canopy.max  <- aggregate(dist.to.canopy.core, fact = fact, fun = max,  na.rm = TRUE)
    
    # NaN -> NA safety
    fix.nan <- function(r) {
      v <- values(r, mat = FALSE)
      v[is.nan(v)] <- NA_real_
      values(r) <- v
      r
    }
    
    dist.to.gap.mean    <- fix.nan(dist.to.gap.mean)
    dist.to.canopy.mean <- fix.nan(dist.to.canopy.mean)
    dist.to.canopy.max  <- fix.nan(dist.to.canopy.max)
    
    dist.to.gap.mean    <- mask(dist.to.gap.mean,    template50)
    dist.to.canopy.mean <- mask(dist.to.canopy.mean, template50)
    dist.to.canopy.max  <- mask(dist.to.canopy.max,  template50)
    
    
    names(dist.to.gap.mean)    <- 'dist_to_gap_mean'
    names(dist.to.canopy.mean) <- 'dist_to_canopy_mean'
    names(dist.to.canopy.max)  <- 'dist_to_canopy_max'
    
    # ---------------- WRITE OUTPUT ----------------
    gap.dist.metrics.50 <- c(
      gap.pct.50, gap.small, gap.medium, gap.large, gap.xlarge,
      dist.to.gap.mean, dist.to.canopy.mean, dist.to.canopy.max
    )
    
    gap.dist.metrics.50 <- mask(gap.dist.metrics.50, template50)
    
    writeRaster(
      gap.dist.metrics.50,
      filename = file.path(out.dir, paste0(base, '_gap_dist_metrics_50m.tif')),
      overwrite = TRUE
    )
    
    data.frame(tile = base, n.gaps = nrow(gap.df))
  }
}

  


# ==============================================================================
# Calculate metrics
# ==============================================================================

# -------------- test run on 5 tiles -------------
gap.fun <- gap.metric.single.tile(vrt.file, out.dir, buffer.m = 100)
summary.5 <- lapply(chm.files[1:5], gap.fun) 
summary.5 <- do.call(rbind, summary.5)
summary.5

# visually inspect
r1 <- rast(file.path(out.dir,
                     'chm_USGS_LPC_CA_SierraNevada_B22_11SLB0727_norm_gap_dist_metrics_50m.tif'))
r2 <- rast(file.path(out.dir,
                           'chm_USGS_LPC_CA_SierraNevada_B22_11SLB0728_norm_gap_dist_metrics_50m.tif'))

plot(r1)  
plot(r2$gap_large_pct)  

summary(values(r1$gap_pct))
summary(values(r1$dist_to_gap_mean))

summary(values(1 - r1$gap_pct))

gap.df <- as.data.frame(r1)

print(summary(gap.df$gap_area_m2))
print(table(gap.df$gap_class, useNA = 'ifany'))
print(quantile(gap.df$gap_area_m2, c(0.5, 0.9, 0.95, 0.99)))



# -------------- test on 36 tiles ---------------
# ----- select contiguous 6 x 6 km block to test on -----

# inputs
chm.dir <- 'data/processed/processed/tif/1m/creek_chm_test_36'
test.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)
vrt.file <- 'data/processed/processed/tif/1m/creek_chm_32611/creek_chm_1m_32611.vrt'


# outputs
out.dir <- 'data/processed/processed/tif/50m/gap_test'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

# ----- test 36 tile block in parallel -----

plan(multisession, workers = 10)

start <- Sys.time()
summary.36 <- future_lapply(test.files, function(f){
  
  gap.fun <- gap.metric.single.tile(
    vrt.file = vrt.file,
    out.dir  = out.dir,
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

out.dir.gap.dist <- file.path(out.dir)
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
# ran on 2/2/26 and finished in 126 minutes

# save summary
saveRDS(summary.all, file.path(out.dir.gap.dist, 'summary_all.rds'))
write.csv(summary.all, file.path(out.dir.gap.dist, 'summary_all.csv'), row.names = FALSE)


# ==============================================================================
# Checks and Validation
# ==============================================================================

table(is.na(summary.all$n.gaps))
subset(summary.all, !is.na(error))
summary(summary.all$n.gaps)

out.files <- list.files(out.dir.gap.dist,
                        pattern = '_gap_dist_metrics_50m\\.tif$',
                        full.names = TRUE)

set.seed(1)
samp <- sample(out.files, 5)

for (p in samp) {
  r <- rast(p)
  
  cat('\n---', basename(p), '---\n')
  cat('nlyr:', nlyr(r), '\n')
  print(names(r))
  
  # range checks
  print(global(r$gap_pct, range, na.rm = TRUE))
  print(global(r$dist_to_canopy_max, range, na.rm = TRUE))
}

r1 <- rast(out.files[1])
r5 <- rast(out.files[5])
plot(r1$gap_pct, main = 'gap_pct (50m)')
plot(r5$gap_pct, main = 'gap_pct (50m)')
plot(r1$dist_to_gap_mean, main = 'dist_to_gap_mean (50m)')
plot(r5$dist_to_gap_mean, main = 'dist_to_gap_mean (50m)')

# look for if "we are hitting the buffer"
mx <- sapply(out.files, function(p) {
  r <- rast(p)
  global(r$dist_to_canopy_max, max, na.rm = TRUE)[1,1]
})

summary(mx)
quantile(mx, c(0.5, 0.9, 0.95, 0.99), na.rm = TRUE)

hist(mx, breaks = 50, main = 'Max distance to canopy per tile')

# dominant canopy-gap length is ~30-40m



# ==============================================================================
#  Mosaic into single raster
# ==============================================================================
library(terra)
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/gap_dist_32611'
files <- list.files(out.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)
raster.list <- lapply(files, rast)
raster.collection <- sprc(raster.list)

m <- mosaic(raster.collection)
plot(m)
plot(m$dist_to_canopy_max)

write.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics'
out.m <- file.path(write.dir, 'creek_fractal_dim_50m_32611.tif')
writeRaster(m, out.m, overwrite = T, 
            wopt = list(gdal = c('COMPRESS=LZW', 'TILED=YES', 'BIGTIFF=YES')))



r <- rast(file.path(out.dir, 'creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB9567_norm_gap_dist_metrics_50m.tif'))
plot(r)
# 11SKB9567




# ------ trouble shooting --------

gap.ht.m = 2
gap.breaks = c(0, 10, 100, 1000, Inf)
buffer.m = 100
out.res.m = 50

chm.core.tile <- rast('data/processed/processed/tif/1m/creek_chm_32611/creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7835_norm.tif')
ext.core <- ext(chm.core.tile)

vrt.file <- 'data/processed/processed/tif/1m/creek_chm_32611/creek_chm_1m_32611.vrt'
chm.vrt <- rast(vrt.file)

ext.buf <- ext(
  ext.core$xmin - buffer.m, ext.core$xmax + buffer.m,
  ext.core$ymin - buffer.m, ext.core$ymax + buffer.m
)

chm.buf  <- crop(chm.vrt, ext.buf)
chm.core <- crop(chm.vrt, ext.core)

fact <- as.integer(round(out.res.m / res(chm.buf)[1]))
if (fact < 1) stop('Aggregation factor < 1. Check CHM resolution for file: ', f)


# ---------------- GAP BOOLEAN (preserve NA) ----------------
# logical TRUE/FALSE, but NA where CHM is NA
gap.bool.buf  <- ifel(is.na(chm.buf),  NA, chm.buf <  gap.ht.m)
canopy.bool.buf <- ifel(is.na(chm.buf), NA, chm.buf >= gap.ht.m)

# For patches(): ONLY gaps should be non-NA
gap.features.buf <- ifel(gap.bool.buf == 1, 1, NA)

# --- convert to numeric 0/1 ---
gap01 <- ifel(is.na(chm.buf), NA_real_, as.numeric(chm.buf < gap.ht.m))

# 3x3 window
w <- matrix(1, 5, 5)

# --- erode: remove thin connections ---
gap.erode <- focal(gap01, w = w, fun = min, na.rm = TRUE, expand = FALSE)

# --- dilate: regrow main blobs ---
gap.open <- focal(gap.erode, w = w, fun = max, na.rm = TRUE, expand = FALSE)

# restore NA footprint
gap.bool.buf <- ifel(is.na(gap01), NA, gap.open == 1)


# ---------------- PATCH IDS + GAP SIZE CLASSES ----------------
gap.id.buf <- patches(gap.features.buf, directions = 8)
names(gap.id.buf) <- 'gap_id'

gaps <- values(gap.id.buf, mat = FALSE)
gaps <- gaps[!is.na(gaps)]

# templates aligned to core at 50m
template50 <- aggregate(chm.core, fact = fact, fun = mean, na.rm = TRUE)

# handle tiles with no gaps
if (length(gaps) == 0) {
  
  # gap % is 0 where CHM exists, NA where CHM is NA
  gap.pct.50 <- agg.mean(as.numeric(crop(gap.bool.buf, ext.core)), fact)
  gap.pct.50 <- ifel(is.na(template50), NA, gap.pct.50)
  
  zero <- gap.pct.50 * 0
  
  na50 <- template50
  values(na50) <- NA_real_
  
  gap.dist.metrics.50 <- c(
    gap.pct.50,
    zero, zero, zero, zero,
    na50, na50, na50
  )
  
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

# area per gap id (m2)
gap.areas <- table(gaps) * prod(res(gap.id.buf))

gap.df <- data.frame(
  gap_id = as.integer(names(gap.areas)),
  gap_area_m2 = as.numeric(gap.areas)
)

gap.df$gap_class <- cut(
  gap.df$gap_area_m2,
  breaks = gap.breaks,
  labels = c('small', 'medium', 'large', 'xlarge')
)

gap.lookup <- as.matrix(data.frame(
  from = gap.df$gap_id,
  to   = as.integer(gap.df$gap_class)   # 1..4
))

gap.class.buf <- classify(gap.id.buf, rcl = gap.lookup)
names(gap.class.buf) <- 'gap_class'

# crop to core for aggregation
gap.id.core    <- crop(gap.id.buf, ext.core)
gap.class.core <- crop(gap.class.buf, ext.core)

gap.bool.core <- crop(gap.bool.buf, ext.core)

# check
freq(gap.class.core)
gap.area.total <- sum(values(gap.bool.core == 1, mat = FALSE), na.rm = TRUE) * prod(res(chm.core))

# area by class (m2)
tab <- freq(gap.class.core)
tab$area_m2 <- tab$count * prod(res(chm.core))
tab$area_frac_of_gap <- tab$area_m2 / sum(tab$area_m2)

gap.area.total
tab

plot(gap.bool.core,
     main = '1m gap mask (core)',
     col = c('darkgreen', 'yellow'),
     legend = FALSE)


gap.id.core <- crop(gap.id.buf, ext.core)

plot(gap.id.core,
     main = '8 directions',
     col = rainbow(50))



# helper: 1 where condition TRUE, 0 where FALSE, NA where CHM is NA
bin01_na <- function(cond, chm) {
  ifel(is.na(chm), NA_real_, ifel(cond, 1, 0))
}


# ---------------- GAP % (0/1 with NA preserved) ----------------
gap.pct.50 <- agg.mean(as.numeric(gap.bool.core), fact)
gap.pct.50 <- mask(gap.pct.50, template50)
names(gap.pct.50) <- 'gap_pct'

# IMPORTANT: class % should be relative to ALL valid pixels, not just gap pixels
gap.small  <- agg.mean(bin01_na(gap.class.core == 1, chm.core), fact)
gap.medium <- agg.mean(bin01_na(gap.class.core == 2, chm.core), fact)
gap.large  <- agg.mean(bin01_na(gap.class.core == 3, chm.core), fact)
gap.xlarge <- agg.mean(bin01_na(gap.class.core == 4, chm.core), fact)

gap.small  <- mask(gap.small,  template50)
gap.medium <- mask(gap.medium, template50)
gap.large  <- mask(gap.large,  template50)
gap.xlarge <- mask(gap.xlarge, template50)

names(gap.small)  <- 'gap_small_pct'
names(gap.medium) <- 'gap_medium_pct'
names(gap.large)  <- 'gap_large_pct'
names(gap.xlarge) <- 'gap_xlarge_pct'
