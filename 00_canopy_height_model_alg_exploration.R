packages <- c('terra', 'sf', 'mapview', 'lidR', 'dplyr', 'ForestGapR', 'raster', 'future', 'future.apply', 'stringr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


# =================================================================================
# test on few tiles 
# =================================================================================

# inputs
norm.dir <- 'data/processed/ALS/normalized/creek'
out.base <- 'data/processed/ALS/tests'

# chm resolution
res.m <- 1

# how many tiles to test
n.test <- 8

# normalized catalog
ctg.norm <- readLAScatalog('data/processed/ALS/normalized/creek')

# keep simple for test
opt_progress(ctg.norm) <- TRUE
opt_chunk_size(ctg.norm) <- 0
opt_chunk_buffer(ctg.norm) <- 0
opt_filter(ctg.norm) <- '-drop_withheld'

# --- Select tiles to test ---
# ctg@data has one row per file; take a sample for now
set.seed(1)
idx <- sample(seq_len(nrow(ctg.norm@data)), size = min(n.test, nrow(ctg.norm@data)))
ctg.test <- ctg.norm[idx, ]

dir.create(file.path(out.base, 'p2r'), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out.base, 'pitfree'), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out.base, 'diff_p2r_minus_pitfree'), recursive = TRUE, showWarnings = FALSE)

opt_output_files(ctg.test) <- file.path(out.base, 'p2r', '{ORIGINALFILENAME}_chm_p2r')

# --- test ---
chm.p2r <- rasterize_canopy(ctg.test,
                            res.m,
                            algorithm = p2r())
# 5-6 minutes
opt_output_files(ctg.test) <- file.path(out.base, 'pitfree', '{ORIGINALFILENAME}_chm_pit')
chm.pit <- rasterize_canopy(ctg.test,
                            res.m,
                            algorithm = pitfree())
# 2:41 -

# --- difference rasters (p2r - pitfree) ---
p2r.files <- list.files(file.path(out.base, 'p2r'), pattern = '\\.tif$', full.names = T)
pit.files <- list.files(file.path(out.base, 'pitfree'), pattern = '\\.tif$', full.names = T)

# match by original filename prefix
get.key <- function(x) sub('_chm_.*$', '', basename(x))
p2r.key <- get.key(p2r.files)
pit.key <- get.key(pit.files)

common <- intersect(p2r.key, pit.key)

for(k in common) {
  f1 <- p2r.files[p2r.key == k][1]
  f2 <- pit.files[pit.key == k][1]
  
  r1 <- rast(f1)
  r2 <- rast(f2)
  
  d <- r1- r2
  names(d) <- 'p2r_minus_pitfree'
  
  out.diff <- file.path(out.base, 'diff_p2r_minus_pitfree', paste0(k, 'diff_p2r_minus_pitfree.tif'))
  writeRaster(d, out.diff, overwrite = T)
}

# --- Quick summary stats for sanity ---
# min/median/max differences per tile)
diff.files <- list.files(
  file.path(out.base, 'diff_p2r_minus_pitfree'),
  pattern = '\\.tif$',
  full.names = TRUE
)

stats <- lapply(diff.files, function(f) {
  
  r <- rast(f)
  
  # min/mean/max via global
  g <- terra::global(r, fun = c('min', 'mean', 'max'), na.rm = TRUE)
  
  # median robustly
  v <- values(r, mat = FALSE)
  v <- v[is.finite(v)]
  med <- if (length(v)) median(v) else NA_real_
  
  data.frame(
    tile = basename(f),
    min = g$min,
    mean = g$mean,
    median = med,
    max = g$max
  )
})

stats <- do.call(rbind, stats)
print(stats)


thresholds <- c(1, 5, 10, 20)  # meters

qc <- lapply(diff.files, function(f) {
  r <- rast(f)
  v <- values(r, mat = FALSE)
  v <- v[is.finite(v)]
  n <- length(v)
  
  out <- data.frame(
    tile = basename(f),
    n = n,
    pct_abs_gt_1  = mean(abs(v) > 1)  * 100,
    pct_abs_gt_5  = mean(abs(v) > 5)  * 100,
    pct_abs_gt_10 = mean(abs(v) > 10) * 100,
    pct_abs_gt_20 = mean(abs(v) > 20) * 100
  )
  out
})

qc <- do.call(rbind, qc)
print(qc)


# ----- visually compare -----

# pick a tile 
tile <- 'USGS_LPC_CA_SierraNevada_B22_11SKB8723_norm'

p2r.file  <- file.path(out.base, 'p2r', paste0(tile, '_chm_p2r.tif'))
pit.file  <- file.path(out.base, 'pitfree', paste0(tile, '_chm_p2r.tif'))
diff.file <- file.path(out.base, 'diff_p2r_minus_pitfree', paste0(tile, '_diff_p2r_minus_pitfree.tif'))

r.p2r  <- rast(p2r.file)
r.pit  <- rast(pit.file)
r.diff <- rast(diff.file)

par(mfrow = c(1, 3))
plot(r.p2r,  main = 'CHM p2r')
plot(r.pit,  main = 'CHM pitfree')
plot(r.diff, main = 'Difference (p2r - pitfree)')
par(mfrow = c(1, 1))

# draw an extent manually after plotting once
ext.zoom <- ext(287600, 287850 , 4123500, 4123750)  # <-- change coords to somewhere interesting

p2r.z  <- crop(r.p2r, ext.zoom)
pit.z  <- crop(r.pit, ext.zoom)
diff.z <- crop(r.diff, ext.zoom)

par(mfrow = c(1, 3))
plot(p2r.z,  main = 'p2r (zoom)')
plot(pit.z,  main = 'pitfree (zoom)')
plot(diff.z, main = 'diff (zoom)')
par(mfrow = c(1, 1))


