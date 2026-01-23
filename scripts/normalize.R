# scripts/normalize_creek_dtm.R

suppressPackageStartupMessages({
  library(terra)
  library(lidR)
  library(dplyr)
  library(stringr)
  library(future)
  library(future.apply)
})

# -----------------------
# USER SETTINGS
# -----------------------
workers <- 10
buffer <- 20

# test block settings 
x0 <- 310000
y0 <- 4130000
block.m <- 6000

run.test.block <- FALSE  # set FALSE to run all tiles

las.dir <- 'data/raw/ALS/laz_creek'
dtm.dir <- 'data/raw/DEM/creek'
out.dir <- 'data/processed/ALS/normalized/creek'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)


log.file <- file.path(out.dir, 'normalize_run.log')


log.msg <- function(...) {
  msg <- paste0(format(Sys.time(), '%Y-%m-%d %H:%M:%S'), ' | ', paste(..., collapse = ' '))
  cat(msg, '\n')
  cat(msg, '\n', file = log.file, append = TRUE)
}

# -----------------------
# SAFETY: run from project root
# -----------------------
log.msg('Working directory:', getwd())
log.msg('las.dir:', las.dir)
log.msg('dtm.dir:', dtm.dir)
log.msg('out.dir:', out.dir)

# -----------------------
# Build LAScatalog (so ctg.full exists in script)
# -----------------------
ctg.full <- readLAScatalog(las.dir)

d <- ctg.full@data
nms <- names(d)

# --- extent columns (lidR version differences) ---
xmn.name <- nms[grep('Min\\.X|^xmin$|Xleft',   nms, ignore.case = TRUE)][1]
xmx.name <- nms[grep('Max\\.X|^xmax$|Xright',  nms, ignore.case = TRUE)][1]
ymn.name <- nms[grep('Min\\.Y|^ymin$|Ybottom', nms, ignore.case = TRUE)][1]
ymx.name <- nms[grep('Max\\.Y|^ymax$|Ytop',    nms, ignore.case = TRUE)][1]

xmn <- d[[xmn.name]]; xmx <- d[[xmx.name]]
ymn <- d[[ymn.name]]; ymx <- d[[ymx.name]]

# --- bbox for ~36 tiles ---
xmin.b <- x0 - block.m / 2
xmax.b <- x0 + block.m / 2
ymin.b <- y0 - block.m / 2
ymax.b <- y0 + block.m / 2

keep <- (xmx > xmin.b) & (xmn < xmax.b) &
  (ymx > ymin.b) & (ymn < ymax.b)

# filename column
file.col <- nms[grep('filename$|^file$|^files?$|fullpath', nms, ignore.case = TRUE)][1]
files.sub.test <- d[[file.col]][keep]

log.msg('Catalog tiles total:', nrow(d))
log.msg('Test block tiles:', length(files.sub.test))

# -----------------------
# Pair LAS <-> DTM by tile code
# -----------------------
las.files <- list.files(las.dir, pattern = '\\.la[sz]$', full.names = TRUE, ignore.case = TRUE)
dtm.files <- list.files(dtm.dir, pattern = '\\.(tif|tiff|img)$', full.names = TRUE, ignore.case = TRUE)

stopifnot(length(las.files) > 0)
stopifnot(length(dtm.files) > 0)

get.tile.code <- function(x) {
  x <- toupper(basename(x))
  m <- str_match(x, '([0-9]{2}[A-Z]{3}[0-9]{4})')
  m[, 2]
}

las.df <- tibble(
  las.file = las.files,
  tile.code = vapply(las.files, get.tile.code, character(1))
)

dtm.df <- tibble(
  dtm.file = dtm.files,
  tile.code = vapply(dtm.files, get.tile.code, character(1))
)

# Fail fast if extraction failed
if (any(is.na(las.df$tile.code))) stop('Could not extract tile.code from some LAS files.')
if (any(is.na(dtm.df$tile.code))) stop('Could not extract tile.code from some DTM files.')

pairs <- left_join(las.df, dtm.df, by = 'tile.code')

# Fail fast if any DTMs missing
missing.dtm <- pairs %>% filter(is.na(dtm.file))
if (nrow(missing.dtm) > 0) {
  log.msg('Missing DTM examples:', paste(head(missing.dtm$las.file, 5), collapse = ' | '))
  stop('Some LAS tiles did not find a matching DTM file.')
}

# Duplicates check (not always fatal, but you should know)
dups <- pairs %>% count(tile.code) %>% filter(n > 1)
if (nrow(dups) > 0) {
  log.msg('WARNING: duplicate tile.code entries found. Example codes:',
          paste(head(dups$tile.code, 5), collapse = ' | '))
}

# Subset to 36-tile block if requested
if (isTRUE(run.test.block)) {
  pairs.run <- pairs[toupper(basename(pairs$las.file)) %in% toupper(basename(files.sub.test)), , drop = FALSE]
  log.msg('Pairs in test block:', nrow(pairs.run))
  stopifnot(nrow(pairs.run) == length(files.sub.test))
} else {
  pairs.run <- pairs
  log.msg('Pairs in full run:', nrow(pairs.run))
}

# -----------------------
# Normalize function (resume-safe)
# -----------------------
normalize <- function(las.file, dtm.file, out.dir, buffer = 20) {
  
  out.file <- file.path(
    out.dir,
    paste0(tools::file_path_sans_ext(basename(las.file)), '_norm.laz')
  )
  
  if (file.exists(out.file) && file.info(out.file)$size > 5 * 1024^2) {
    return(out.file)
  }
  
  out <- tryCatch({
    
    las <- readLAS(las.file, filter = '-drop_withheld -drop_class 7 18')
    if (is.empty(las)) return(NA_character_)
    
    dtm <- rast(dtm.file)
    
    e <- ext(las)
    e.buf <- ext(e$xmin - buffer, e$xmax + buffer,
                 e$ymin - buffer, e$ymax + buffer)
    dtm <- crop(dtm, e.buf)
    
    las.norm <- normalize_height(las, dtm)
    writeLAS(las.norm, out.file)
    
    rm(las, dtm, las.norm)
    gc()
    
    out.file
    
  }, error = function(e) {
    # write a tiny per-tile error marker so you can review later
    errfile <- file.path(out.dir, paste0(basename(las.file), '_ERROR.txt'))
    writeLines(conditionMessage(e), errfile)
    NA_character_
  })
  
  out
}


# -----------------------
# Run in parallel (batched) + log time
# -----------------------
set_lidr_threads(1)
plan(multisession, workers = workers)
options(future.globals.maxSize = 8 * 1024^3)

log.msg('Starting run. workers=', workers, ' buffer=', buffer)
log.msg('Already on disk:', length(list.files(out.dir, pattern = '_norm\\.laz$', full.names = TRUE)))

t0 <- Sys.time()

n <- nrow(pairs.run)
batch.size <- 120   # good starting point (adjust if needed)
starts <- seq(1, n, by = batch.size)

out.files <- character(n)

for (b in seq_along(starts)) {
  
  i1 <- starts[b]
  i2 <- min(i1 + batch.size - 1, n)
  
  log.msg('Batch ', b, '/', length(starts), ' | tiles ', i1, '-', i2)
  
  out.files[i1:i2] <- future_mapply(
    FUN = normalize,
    las.file = pairs.run$las.file[i1:i2],
    dtm.file = pairs.run$dtm.file[i1:i2],
    MoreArgs = list(out.dir = out.dir, buffer = buffer),
    SIMPLIFY = TRUE,
    future.seed = TRUE
  )
  
  terra::tmpFiles(remove = TRUE)
  
  done <- sum(file.exists(out.files[1:i2]) & !is.na(out.files[1:i2]))
  elapsed.min <- as.numeric(difftime(Sys.time(), t0, units = 'mins'))
  log.msg('Progress: ', done, '/', n, ' | elapsed min: ', round(elapsed.min, 2))
}

t1 <- Sys.time()
plan(sequential)

elapsed.min <- as.numeric(difftime(t1, t0, units = 'mins'))
log.msg('Finished. Elapsed minutes:', round(elapsed.min, 2))
log.msg('Outputs that exist:', length(list.files(out.dir, pattern = '_norm\\.laz$', full.names = TRUE)), ' total')

