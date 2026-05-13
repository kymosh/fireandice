# scripts/normalize_creek_dtm.R



# run in command prompt with this code:
# "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts\normalize.R

# ----- runtime notes -----
# 2889 tiles ran in 413 minutes

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
workers <- 10 # failed at 16
buffer <- 20

run.test.block <- TRUE  # set FALSE to run all tiles

fire <- 'caldor'
acq <- 'CA_SierraNevada_8_2022'
j.dir <- 'J:/Fire_Snow/fireandice'

las.dir <- file.path(j.dir, paste0('data/raw/ALS/laz_', fire), acq)
dtm.dir <-  file.path(j.dir, 'data/raw/DEM', fire)
out.dir <-  file.path(j.dir, 'data/processed/ALS/normalized', fire)
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
# Build LAScatalog 
# -----------------------
ctg.full <- readLAScatalog(las.dir)

d <- ctg.full@data
nms <- names(d)

# tiles to test
test.tiles <- c('11SKD4406', '11SKD4407', '11SKD4306', '11SKD4307')

# filename column
file.col <- nms[grep('filename$|^file$|^files?$|fullpath', nms, ignore.case = TRUE)][1]

# keep files whose names contain one of the test tile IDs
tile.pattern <- paste(test.tiles, collapse = '|')

keep <- grepl(tile.pattern, basename(d[[file.col]]))

files.sub.test <- d[[file.col]][keep]

log.msg('Catalog tiles total:', nrow(d))
log.msg('Test tiles:', paste(test.tiles, collapse = ', '))
log.msg('Test files found:', length(files.sub.test))

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

# Duplicates check
dups <- pairs %>% count(tile.code) %>% filter(n > 1)
if (nrow(dups) > 0) {
  log.msg('WARNING: duplicate tile.code entries found. Example codes:',
          paste(head(dups$tile.code, 5), collapse = ' | '))
}

# Subset to 36-tile block if requested
if (isTRUE(run.test.block)) {
  pairs.run <- pairs[pairs$las.file %in% files.sub.test, , drop = FALSE]
  log.msg('Pairs in test block:', nrow(pairs.run))
  stopifnot(nrow(pairs.run) == length(files.sub.test))
} else {
  pairs.run <- pairs
  log.msg('Pairs in full run:', nrow(pairs.run))
}

# -----------------------
# Normalize function (resume-safe)
# -----------------------

log.msg('LAS files found:', length(las.files))
log.msg('DTM files found:', length(dtm.files))

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
    las.norm <- filter_poi(las.norm, Z >= -5 & Z <= 150)
    writeLAS(las.norm, out.file)
    
    rm(las, dtm, las.norm)
    gc()
    
    out.file
    
  }, error = function(e) {
    # write a tiny per-tile error marker so you can review later
    errfile <- file.path(out.dir, paste0(basename(las.file), '_ERROR.txt'))
    writeLines(conditionMessage(e), errfile)
    log.msg('ERROR on ', basename(las.file), ': ', conditionMessage(e))
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
  
  done <- length(list.files(out.dir, pattern = '_norm\\.laz$', full.names = TRUE))
  elapsed.min <- as.numeric(difftime(Sys.time(), t0, units = 'mins'))
  log.msg('Progress: ', done, '/', n, ' | elapsed min: ', round(elapsed.min, 2))
}

t1 <- Sys.time()
plan(sequential)

elapsed.min <- as.numeric(difftime(t1, t0, units = 'mins'))
log.msg('Finished. Elapsed minutes:', round(elapsed.min, 2))
log.msg('Outputs that exist:', length(list.files(out.dir, pattern = '_norm\\.laz$', full.names = TRUE)), ' total')

