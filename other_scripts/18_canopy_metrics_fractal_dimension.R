packages <- c('mapview', 'lidR', 'dplyr', 'future', 'future.apply', 'tools', 'terra')
#install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


# ---------- setup ----------

# inputs
chm.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
# 'data/processed/processed/tif/1m/creek_chm' for whole run
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

# ==============================================================================
#  Fractal Dimension 
# ==============================================================================
water.path <- 'data/processed/processed/shp/nhd_water_creek_32611.shp'
box.sizes <- c(1, 2, 5, 10, 25)

boxcount.fractal.dim <- function(mat, box.sizes) {
  
  # coverage gate: require enough valid pixels
  cov <- mean(!is.na(mat))
  if (cov < 0.9) return(NA_real_)  # adjust 0.9 -> 0.8 if needed
  
  # structure gate: avoid almost-all-0 or almost-all-1
  v <- mat[!is.na(mat)]
  p1 <- mean(v == 1)
  if (p1 < 0.05 || p1 > 0.95) return(NA_real_)
  
  # treat missing as background after gating
  mat[is.na(mat)] <- 0
  
  n.box <- numeric(length(box.sizes))
  
  for (i in seq_along(box.sizes)) {
    bs <- box.sizes[i]
    
    nr <- nrow(mat) - (nrow(mat) %% bs)
    nc <- ncol(mat) - (ncol(mat) %% bs)
    if (nr < bs || nc < bs) { n.box[i] <- 0; next }
    
    m <- mat[1:nr, 1:nc, drop = FALSE]
    m <- array(m, dim = c(bs, nr / bs, bs, nc / bs))
    block.sum <- apply(m, c(2, 4), sum)
    n.box[i] <- sum(block.sum > 0)
  }
  
  keep <- n.box > 0
  if (sum(keep) < 2) return(NA_real_)
  
  fit <- lm(log(n.box[keep]) ~ log(1 / box.sizes[keep]))
  unname(coef(fit)[2])
}



fractal.dim.fun <- function(v, ...) {
  
  if (all(is.na(v))) return(NA_real_)
  
  n <- sqrt(length(v))
  
  if (!isTRUE(all.equal(n, round(n)))) return(NA_real_)
  
  n <- as.integer(round(n))
  
  mat <- matrix(v, nrow = n, ncol = n, byrow = TRUE)
  
  boxcount.fractal.dim(mat = mat, box.sizes = box.sizes)
}

gap.ht <- 2

fractal.dim.one.tiles <- function(f, water.path, out.dir, gap.ht = 2) {
  
  library(terra)
  
  r <- rast(f)
  
  t0 <- Sys.time()
  message('\n--- Fractal dim (from CHM): ', basename(f), ' ---')
  
  # ---- mask water ----
  water <- vect(water.path)
  water <- crop(water, ext(r))
  r <- mask(r, water, inverse = TRUE)
  
  # gap mask: 1 = gap, 0 = not gap, NA = missing data and water
  gap.mask <- ifel(is.na(r), NA, ifel(r < gap.ht, 1, 0))
  
  fd <- aggregate(gap.mask, fact = 50, fun = fractal.dim.fun,
                  expand = FALSE,
                  na.rm = FALSE)
  
  names(fd) <- 'fractal_dim'
  
  out.file <- file.path(out.dir, 
                        paste0(file_path_sans_ext(basename(f)), '_fractal_dim_50m.tif'))
  writeRaster(fd, out.file, overwrite = TRUE, 
              wopt = list(gdal = c('COMPRESS=LZW', 'TILED=YES'))
  )
  t1 <- Sys.time()
  message('Finished: ', format(t1, '%Y-%m-%d %H:%M:%S'),
          ' | Elapsed min: ', round(as.numeric(difftime(t1, t0, units = 'mins')), 2))
  
  out.file
}


# --------------- test on 5 tiles---------------

# inputs
chm.dir <- 'data/processed/processed/tif/1m/creek_chm_test_36' # for test
#chm.dir <- 'data/processed/processed/tif/1m/creek_chm_32611' # for whole run
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

# test on 5 tiles
test.files <- chm.files[1:5]

# outputs
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/fractal_dim_32611_test'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

start.time <- Sys.time()
out.files <- lapply(test.files, fractal.dim.one.tiles)
end.time <- Sys.time()

message('\nAll 5 test tiles finished at: ', format(end.time, '%Y-%m-%d %H:%M:%S'))
message('Total elapsed min: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))

out.files

# --------- check -----------

fd1 <- rast(file.path(out.dir, 'creek_chm_USGS_LPC_CA_SierraNevada_B22_11SLB0727_norm_fractal_dim_50m.tif'))
fd1
global(fd1, range, na.rm = TRUE)
hist(values(fd1))
plot(fd1)

raster.list <- list.files(out.dir, full.names = TRUE)
test.files.col <- sprc(raster.list)

m <- mosaic(test.files.col)
plot(m)

summary(values(m))
# --------------- test on 36 tiles---------------

# inputs
chm.dir <- 'data/processed/processed/tif/1m/creek_chm_test_36'
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

# test on 36 tiles
test.files <- chm.files

# outputs
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/fractal_dim_32611_test'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

# --- parallel setup ---
plan(multisession, workers = 12)

start.time <- Sys.time()
out.files <- future_lapply(test.files, function(f){
  fractal.dim.one.tiles(
    f,
    water.path = water.path,
    out.dir  = out.dir,
    gap.ht = 2)
})
  
end.time <- Sys.time()

message('\nAll 36 test tiles finished at: ', format(end.time, '%Y-%m-%d %H:%M:%S'))
message('Total elapsed min: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))

fd1 <- rast(out.files[[1]])
fd2 <- rast(out.files[[2]])

plot(fd1)
plot(fd2)

hist(values(fd1), breaks = 30)
hist(values(fd2), breaks = 30)

summary(values(fd1))


# read the same CHM tile you used for fd1
chm <- rast('data/processed/ALS/chm_test_tiles/chm_USGS_LPC_CA_SierraNevada_B22_11SLB0727_norm.tif')

# binary gap (TRUE/FALSE)
gap.bool <- chm < gap.ht

# gap fraction at 50 m
gap.frac.50m <- aggregate(
  gap.bool,
  fact = 50,
  fun = mean,
  na.rm = TRUE
)

names(gap.frac.50m) <- 'gap_frac'
compareGeom(fd1, gap.frac.50m, stopOnError = TRUE)

x <- values(gap.frac.50m)
y <- values(fd1)

keep <- is.finite(x) & is.finite(y)

plot(
  x[keep], y[keep],
  pch = 16, cex = 0.6,
  xlab = 'Gap fraction (CHM < 2 m)',
  ylab = 'Fractal dimension',
  main = 'FD vs gap fraction (50 m)'
)

smoothScatter(
  x[keep], y[keep],
  xlab = 'Gap fraction',
  ylab = 'Fractal dimension'
)

# -------------- final run ----------------------

plan(multisession, workers = 12)
chm.dir <- 'data/processed/processed/tif/1m/creek_chm_32611'
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/fractal_dim_32611'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

length(chm.files)  # should be 2860

start.time <- Sys.time()
out.files <- future_lapply(chm.files, function(f){
  fractal.dim.one.tiles(
    f,
    water.path = water.path,
    out.dir  = out.dir,
    gap.ht = 2)
})

end.time <- Sys.time()

message('\nAll tiles finished at: ', format(end.time, '%Y-%m-%d %H:%M:%S'))
message('Total elapsed hours: ',
        round(as.numeric(difftime(end.time, start.time, units = 'hours')), 2))

out.files <- unlist(out.files, use.names = FALSE)
# 14:37 2/5/26 elapsed time: 0.31 hours


# ==============================================================================
#  Mosaic into single raster
# ==============================================================================
library(terra)
out.dir <- 'data/processed/processed/tif/50m/creek/canopy_metrics/fractal_dim_32611'
files <- list.files(out.dir, pattern = '\\.tif$', full.names = TRUE)
length(files)
raster.list <- lapply(files, rast)
raster.collection <- sprc(raster.list)

m <- mosaic(raster.collection)
plot(m)

out.m <- file.path(out.dir, 'creek_fractal_dim_50m_32611_masked.tif')
writeRaster(m, out.m, overwrite = T)







