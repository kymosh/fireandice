packages <- c('mapview', 'lidR', 'dplyr', 'future', 'future.apply', 'tools', 'terra')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)


# ---------- setup ----------

# inputs
chm.dir <- 'data/processed/ALS/chm_test_tiles'
# 'data/processed/processed/tif/1m/creek_chm' for whole run
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

# test on 5 tiles
test.files <- chm.files[1:5]

# outputs
out.dir <- 'data/processed/ALS/tif/fractal_dimension_test'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
#  Fractal Dimension 
# ==============================================================================

boxcount.fractal.dim <- function(mat, box.sizes) {
  
  # mat: matrix with 1 = gap, NA = no gap
  # box sizes: vector of box sizes (in pixels)
  
  mat[is.na(mat)] <- 0
  
  n.box <- numeric(length(box.sizes))
  
  for (i in seq_along(box.sizes)) {
    
    bs <- box.sizes[i]
    
    # trim matrix so dimensions divisible by box size
    nr <- nrow(mat) - (nrow(mat) %% bs)
    nc <- ncol(mat) - (ncol(mat) %% bs)
    
    if (nr < bs || nc < bs) {
      n.box[i] <- 0
      next
    }
    
    m <- mat[1:nr, 1:nc, drop = FALSE]
    
    # reshape into blocks
    m <- array(
      m, dim = c(bs, nr / bs, bs, nc / bs)
    )
    
    # sum within each block
    block.sum <- apply(m, c(2, 4), sum)
    
    # count occupied boxes
    n.box[i] <- sum(block.sum > 0)
  }
  
  # remove zero-count scales
  keep <- n.box > 0 
  
  if(sum(keep) < 2) return(NA_real_)
  
  fit <- lm(log(n.box[keep]) ~ log(1 / box.sizes[keep]))
  
  unname(coef(fit)[2])
  
}


box.sizes <- c(1, 2, 5, 10, 25)


fractal.dim.fun <- function(v, ...) {
  
  if (all(is.na(v))) return(NA_real_)
  
  n <- sqrt(length(v))
  if (!isTRUE(all.equal(n, round(n)))) return(NA_real_)
  
  n <- as.integer(round(n))
  mat <- matrix(v, nrow = n, ncol = n, byrow = TRUE)
  
  boxcount.fractal.dim(mat = mat, box.sizes = box.sizes)
}

gap.ht <- 2

fractal.dim.one.tiles <- function(f) {
  
  r <- rast(f)
  
  t0 <- Sys.time()
  message('\n--- Fractal dim (from CHM): ', basename(f), ' ---')
  
  # gap mask: 1 = gap, NA = not gap
  gap.mask <- ifel(r < gap.ht, 1, NA)
  
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
chm.dir <- 'data/processed/ALS/chm_test_tiles'
# 'data/processed/processed/tif/1m/creek_chm' for whole run
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

# test on 5 tiles
test.files <- chm.files[1:5]

# outputs
out.dir <- 'data/processed/ALS/tif/fractal_dimension_test'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

start.time <- Sys.time()
out.files <- lapply(test.files, fractal.dim.one.tiles)
end.time <- Sys.time()

message('\nAll 5 test tiles finished at: ', format(end.time, '%Y-%m-%d %H:%M:%S'))
message('Total elapsed min: ', round(as.numeric(difftime(end.time, start.time, units = 'mins')), 2))

out.files

# --------- check -----------

fd1 <- rast(out.files[1])
fd1
global(fd1, range, na.rm = TRUE)
hist(values(fd1))
plot(fd1)

# --------------- test on 36 tiles---------------

# inputs
chm.dir <- 'data/processed/ALS/chm_test_tiles'
chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

# test on 36 tiles
test.files <- chm.files

# outputs
out.dir <- 'data/processed/ALS/tif/fractal_dimension_test'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

# --- parallel setup ---
plan(multisession, workers = 12)
terraOptions(progress = 1)

start.time <- Sys.time()
out.files <- future_lapply(test.files, fractal.dim.one.tiles, future.seed = TRUE)
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

# note: should be all ready to run on processing computer

plan(multisession, workers = 12)
chm.dir <- 'data/processed/processed/tif/1m/creek_chm'

chm.files <- list.files(chm.dir, pattern = '\\.tif$', full.names = TRUE)

length(chm.files)  # should be 2889

start.time <- Sys.time()
out.files <- future_lapply(
  chm.files,
  process.one.fractal,
  future.seed = TRUE
)
end.time <- Sys.time()

message('\nAll tiles finished at: ', format(end.time, '%Y-%m-%d %H:%M:%S'))
message('Total elapsed hours: ',
        round(as.numeric(difftime(end.time, start.time, units = 'hours')), 2))

out.files <- unlist(out.files, use.names = FALSE)
