packages <- c('sf', 'mapview', 'lidR', 'dplyr', 'raster', 'future', 'future.apply', 'stringr', 'nhdplusTools', 'terra')
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

water.path <- 'data/processed/processed/shp/nhd_water_creek_32611.shp'


# ----- one-tile processing function -----
gap.metric.single.tile <- function(vrt.file,
                                   out.dir,
                                   water.path,
                                   gap.ht.m = 2,
                                   buffer.m = 100,
                                   out.res.m = 50) {
  
  chm.vrt <- rast(vrt.file)
  
  # helper: aggregate with NA-safe (NaN -> NA)
  agg.clean <- function(r, fact, fun) {
    x <- aggregate(r, fact = fact, fun = fun, na.rm = TRUE)
    v <- values(x, mat = FALSE)
    v[is.nan(v)] <- NA_real_
    values(x) <- v
    x
  }
  
  function(f) {
    
    ext.core <- ext(rast(f))
    ext.buf <- ext(
      ext.core$xmin - buffer.m, ext.core$xmax + buffer.m,
      ext.core$ymin - buffer.m, ext.core$ymax + buffer.m
    )
    
    chm.buf  <- crop(chm.vrt, ext.buf)
    chm.core <- crop(chm.vrt, ext.core)
    
    # ------ water mask -----
    water <- vect(water.path)
    water <- crop(water, ext.buf)
    chm.buf <- mask(chm.buf, water, inverse = TRUE)
    chm.core <- mask(chm.core, water, inverse = TRUE)
    
    fact <- as.integer(round(out.res.m / res(chm.buf)[1]))
    if (fact < 1) stop('Aggregation factor < 1. Check CHM resolution for file: ', f)
    
    base <- tools::file_path_sans_ext(basename(f))
    
    # template aligned to core at 50m
    template50 <- agg.clean(chm.core, fact, mean)
    
    # ---------------- GAP BOOLEAN (preserve NA) ----------------
    gap.bool.buf    <- ifel(is.na(chm.buf),  NA, chm.buf <  gap.ht.m)
    canopy.bool.buf <- ifel(is.na(chm.buf), NA, chm.buf >= gap.ht.m)
    
    gap.bool.core <- crop(gap.bool.buf, ext.core)
    
    # ---------------- GAP % ----------------
    gap.pct.50 <- agg.clean(as.numeric(gap.bool.core), fact, mean)
    gap.pct.50 <- mask(gap.pct.50, template50)
    names(gap.pct.50) <- 'gap_pct'
    
    # ---------------- DISTANCE METRICS ----------------
    # distance() computes distance to nearest non-NA cell
    gap.feature.dist     <- ifel(gap.bool.buf == 1,    1, NA)
    canopy.feature.dist  <- ifel(canopy.bool.buf == 1, 1, NA)
    
    dist.to.gap.all      <- distance(gap.feature.dist)
    dist.to.canopy.all   <- distance(canopy.feature.dist)
    
    # dist to gap meaningful only for canopy pixels
    dist.to.gap.buf      <- mask(dist.to.gap.all, canopy.feature.dist)
    
    # dist to canopy meaningful only for gap pixels
    dist.to.canopy.buf   <- mask(dist.to.canopy.all, gap.feature.dist)
    
    dist.to.gap.core     <- crop(dist.to.gap.buf, ext.core)
    dist.to.canopy.core  <- crop(dist.to.canopy.buf, ext.core)
    
    dist.to.gap.mean     <- agg.clean(dist.to.gap.core,    fact, mean)
    dist.to.canopy.mean  <- agg.clean(dist.to.canopy.core, fact, mean)
    dist.to.canopy.max   <- agg.clean(dist.to.canopy.core, fact, max)
    
    dist.to.gap.mean     <- mask(dist.to.gap.mean,    template50)
    dist.to.canopy.mean  <- mask(dist.to.canopy.mean, template50)
    dist.to.canopy.max   <- mask(dist.to.canopy.max,  template50)
    
    names(dist.to.gap.mean)    <- 'dist_to_gap_mean'
    names(dist.to.canopy.mean) <- 'dist_to_canopy_mean'
    names(dist.to.canopy.max)  <- 'dist_to_canopy_max'
    
    # ---------------- WRITE OUTPUT ----------------
    out <- c(gap.pct.50, dist.to.gap.mean, dist.to.canopy.mean, dist.to.canopy.max)
    out <- mask(out, template50)
    
    writeRaster(
      out,
      filename = file.path(out.dir, paste0(base, '_gap_dist_metrics_50m.tif')),
      overwrite = TRUE
    )
    
    data.frame(tile = base)
  }
}

  


# ==============================================================================
# Calculate metrics
# ==============================================================================


# -------------- test run on 5 tiles -------------
out.dir <- "data/processed/processed/tif/50m/creek/canopy_metrics/gap_dist_32611_test"
gap.fun <- gap.metric.single.tile(vrt.file, out.dir, water.path, buffer.m = 100)
summary.5 <- lapply(chm.files[1:5], gap.fun) 
summary.5 <- do.call(rbind, summary.5)
summary.5

# visually inspect
r1 <- rast(file.path(out.dir,
                     'creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7732_norm_gap_dist_metrics_50m.tif'))
r2 <- rast(file.path(out.dir,
                           'creek_chm_USGS_LPC_CA_SierraNevada_B22_11SKB7733_norm_gap_dist_metrics_50m.tif'))

plot(r1)  
plot(r2$dist_to_gap_mean)  

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
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/gap_dist_32611_test'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

# run
plan(multisession, workers = 10)

start <- Sys.time()
summary.36 <- future_lapply(test.files, function(f){
  
  gap.fun <- gap.metric.single.tile(
    vrt.file = vrt.file,
    out.dir  = out.dir,
    water = water.path,
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
  
}, future.seed = TRUE)


summary.36 <- do.call(rbind, summary.36)
end <- Sys.time()
message('36-tile block finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')

summary.36
# 2.07 minutes


r1 <- rast(file.path(out.dir,
                     'chm_USGS_LPC_CA_SierraNevada_B22_11SLB0727_norm_gap_dist_metrics_50m.tif'))
r2 <- rast(file.path(out.dir,
                     'chm_USGS_LPC_CA_SierraNevada_B22_11SLB0728_norm_gap_dist_metrics_50m.tif'))

plot(r1)  
plot(r2$dist_to_gap_mean)  



# -------------- FINAL RUN ----------------------
plan(multisession, workers = 10)
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/gap_dist_32611'

start <- Sys.time()
summary.all <- future_lapply(chm.files, function(f){
  
  gap.fun <- gap.metric.single.tile(
    vrt.file = vrt.file,
    out.dir  = out.dir,
    water = water.path,
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
  
}, future.seed = TRUE)


summary.all <- do.call(rbind, summary.all)
end <- Sys.time()
message('Full run finished in ', round(difftime(end, start, units = 'mins'), 2), ' minutes')
# ran on 2/2/26 and finished in 126 minutes - that was with % gap metrics
# rerun on 2/16 w/out % gap metrics and finished in 19 minutes


# ==============================================================================
# Checks and Validation
# ==============================================================================

table(is.na(summary.all$n.gaps))
subset(summary.all, !is.na(error))
summary(summary.all$n.gaps)

out.files <- list.files(out.dir,
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
out.m <- file.path(write.dir, 'creek_gap_50m_32611.tif')
writeRaster(m, out.m, overwrite = T)


# ------ trouble shooting --------
mx <- m$dist_to_canopy_mean
hi <- which(values(mx) > 200)
length(hi)
xy <- xyFromCell(mx, hi)

plot(mx)
points(xy, pch = 20, cex = 0.5, col = 'red')

worst.cell <- which.max(values(mx))
worst.xy <- xyFromCell(mx, worst.cell)
worst.xy

e <- ext(worst.xy[1] - 5000, worst.xy[1] + 5000,
         worst.xy[2] - 5000, worst.xy[2] + 5000)

plot(crop(mx, e), main = 'dist_to_gap_mean (zoom)')
plot(st_geometry(nhd), add = T, border = 'blue', lwd = 2)

cover <- rast('data/processed/processed/tif/50m/creek/canopy_metrics/creek_cover_metrics_50m_32611.tif')
height <- rast('data/processed/processed/tif/50m/creek/canopy_metrics/creek_height_metrics_50m_32611.tif')
plot(crop(cover$cover_2m, e))
plot(crop(height$zmean, e))
plot(nhd, add = T)

creek <- read_sf('data/processed/processed/shp/mosher_creek_studyarea/study_extent_creek_32611.shp')

# ----- download water polygons -----
# read in shape file of study area
creek <- read_sf('data/processed/processed/shp/mosher_creek_studyarea/study_extent_creek_32611.shp')
# download nhd water data
water <- get_nhdphr(AOI = creek, type = 'nhdwaterbody')


plot(st_geometry(creek))
plot(st_geometry(nhd), add = T, border = 'blue', lwd = 1)
unique(nhd$ftype)
