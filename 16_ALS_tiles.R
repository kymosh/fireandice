packages <- c('terra', 'sf', 'dplyr', 'future', 'future.apply', 'progressr', 'tictoc', 'httr')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# code for downloading lidar tiles from USGS Rockyweb in bulk
# ==============================================================================

# read in shp file of file index
# change fire name
# index <- read_sf('data/processed/processed/shp/tile_index_1524_castle.shp')
index <- read_sf('data/processed/processed/shp/tile_index_dixie_6_low.shp')

# chose out.dir depending on which computer you're on
#out.dir <- 'data/raw/ALS/laz_dixie/CA_SierraNevada_4_2022' # processing computer
#out.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_dixie' # km computer
out.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_dixie/CA_SierraNevada_7_2022_low' # km computer
dir.create(out.dir, showWarnings = F, recursive = T)

# make sure to check shape files so you're using the correct tile_ID col
tile.ids <- index$Tile_ID
acquisition <- index$WU_NAME

# ----- build RockyWeb download URLs -----

base.url <- paste0(
  'https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/',
  'CA_SierraNevada_B22/'
)

urls <- paste0(
  base.url, 
  acquisition, '/LAZ/USGS_LPC_CA_SierraNevada_B22_',
  tile.ids,
  '.laz'
)



# ----- set up  -----

# 2 workers works best for rockyweb downloads
plan(multisession, workers = 2)

dest.files <- file.path(out.dir, basename(urls))

min.size <- 50 * 1024^2 # 50 MB

download.one <- function(url, dest, min.size) {
  
  options(timeout = 3000)
  
  # if the file already exists locally AND it's at least the min size, do nothing
  if (file.exists(dest) && file.info(dest)$size >= min.size) return(TRUE)
  # if the file already exists locally but it's too small, remove it
  if (file.exists(dest)) file.remove(dest)
  
  # download the file
  ok <- tryCatch(
    identical(download.file(url, dest, mode = 'wb', quiet = TRUE), 0L),
    error = function(e) FALSE
  )
  
  # if download was not successful, remove it
  if (!ok) {
    if (file.exists(dest)) file.remove(dest)
    return(FALSE)
  }
  
  # after downloading, check again for files that are too small
  if (!file.exists(dest) || file.info(dest)$size < min.size) {
    if (file.exists(dest)) file.remove(dest)
    return(FALSE)
  }
  
  TRUE
}

handlers(global = TRUE)
handlers('txtprogressbar')


# ----- test -----
test.urls <- urls[1:5]
test.dest <- dest.files[1:5]

results <- future_mapply(
  FUN = download.one,
  url = test.urls,
  dest = test.dest,
  SIMPLIFY = T,
  min.size = min.size
)

# ----- run on all -----
tic('Downloading ALS Tiles')

with_progress({
  
  p <- progressor(along = urls)
  
  results <- future_mapply(
    FUN = function(url, dest) {
      out <- download.one(url, dest, min.size)
      p()
      out
    },
    url = urls,
    dest = dest.files,
    SIMPLIFY = TRUE
  )
})

toc()



# ----- check for missing tiles -----
downloaded.files <- list.files(out.dir, pattern = '\\.laz$', full.names = F)

# extract tile ID from filename
downloaded.ids <- downloaded.files %>%
  tools::file_path_sans_ext() %>%
  sub('USGS_LPC_CA_SierraNevada_B22_', '', .)

# tile IDs that should have downloaded but are missing
missing.ids <- setdiff(tile.ids, downloaded.ids)

length(missing.ids)
missing.ids

# rerun just on missing ids
missing.urls <- urls[tile.ids %in% missing.ids]
missing.dest <- dest.files[tile.ids %in% missing.ids]

options(timeout = 3000)

results.missing <- logical(length(missing.urls))

for (i in seq_along(missing.urls)) {
  
  cat('\n', i, '/', length(missing.urls), ': ', basename(missing.dest[i]), '\n')
  
  if (file.exists(missing.dest[i])) file.remove(missing.dest[i])
  
  ok <- tryCatch(
    identical(
      download.file(
        url = missing.urls[i],
        destfile = missing.dest[i],
        mode = 'wb',
        quiet = FALSE,
        method = 'libcurl'
      ),
      0L
    ),
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  
  results.missing[i] <- ok && file.exists(missing.dest[i])
  
  cat('success:', results.missing[i], '\n')
}


# ----- check for partial downloads -----
# create dataframe
check.downloads <- data.frame(
  tile = tile.ids,
  url = urls,
  dest = dest.files
)

# local file sizes
check.downloads$local.exists <- file.exists(check.downloads$dest)

check.downloads$local.bytes <- NA_real_
has.local <- check.downloads$local.exists

check.downloads$local.bytes[has.local] <- file.info(check.downloads$dest[has.local])$size

# get size that download should be (remote.bytes)
get.remote.size <- function(u) {
  
  res <- tryCatch(
    httr::GET(
      u,
      httr::add_headers(Range = 'bytes=0-0'),
      httr::timeout(30)
    ),
    error = function(e) NULL
  )
  
  if (is.null(res) || !httr::status_code(res) %in% c(200, 206)) {
    return(NA_real_)
  }
  
  cr <- httr::headers(res)[['content-range']]
  
  if (!is.null(cr)) {
    return(as.numeric(sub('.*/', '', cr)))
  }
  
  len <- httr::headers(res)[['content-length']]
  
  if (!is.null(len)) {
    return(as.numeric(len))
  }
  
  NA_real_
}
check.downloads$remote.bytes <- sapply(check.downloads$url, get.remote.size)
table(is.na(check.downloads$remote.bytes), useNA = 'ifany')


# compare
check.downloads <- check.downloads %>%
  mutate(
    local.mb = round(local.bytes / 1024^2, 2),
    remote.mb = round(remote.bytes / 1024^2, 2),
    complete = local.exists & !is.na(remote.bytes) & local.bytes == remote.bytes,
    partial = local.exists & !is.na(remote.bytes) & local.bytes < remote.bytes,
    too.large = local.exists & !is.na(remote.bytes) & local.bytes > remote.bytes,
    missing = !local.exists
  )

# summary
table(check.downloads$complete, useNA = 'ifany') # want all to be TRUE
table(check.downloads$partial, useNA = 'ifany') # want all to be FALSE
table(check.downloads$missing, useNA = 'ifany') # want all to be FALSE

# if partial downloads, do this next:
bad.downloads <- check.downloads %>%
  filter(!complete) %>%
  arrange(desc(partial), tile)

bad.downloads %>%
  select(tile, local.mb, remote.mb, complete, partial, too.large, missing, url, dest)

# delete bad files
file.remove(bad.downloads$dest)

# ==============================================================================
# code for downloading DEM tifs from USGS Rockyweb in bulk
# ==============================================================================
check_missing_dems <- function(acq.check, out.dir, dem.tile.ids, dem.acquisition) {
  
  out.dir.check <- file.path(out.dir, acq.check)
  
  downloaded.dems <- list.files(
    out.dir.check,
    pattern = '\\.tif$',
    full.names = FALSE
  )
  
  downloaded.demids <- downloaded.dems %>%
    tools::file_path_sans_ext() %>%
    sub('USGS_OPR_CA_SierraNevada_B22_', '', .)
  
  expected.idx <- dem.acquisition == acq.check
  
  expected.ids <- dem.tile.ids[expected.idx]
  
  missing.dems <- setdiff(expected.ids, downloaded.demids)
  
  cat('Missing DEMs:', length(missing.dems), '\n')
  
  invisible(missing.dems)
}
# ----- creek -----
# different areas have slightly different naming conventions, so these codes are not interchangeable between areas
# extract tile names from creek batch
tile.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_creek'

files <- list.files(tile.dir, pattern = '\\.laz$', full.names = FALSE)

tile.ids <- sub('.*_', '', tools::file_path_sans_ext(files))
tile.ids[1:5]  # sanity check

# basenames like: USGS_LPC_CA_SierraNevada_B22_11SKB7732
laz.files <- list.files(tile.dir, pattern = '\\.laz$', full.names = FALSE)
tile.basenames <- tools::file_path_sans_ext(laz.files)
tile.basenames[1:5]

tile.ids <- sub('.*_', '', tile.basenames)
tile.ids[1:5]

work.units <- c(
  'CA_SierraNevada_11_B22',
  'CA_SierraNevada_13_B22',
  'CA_SierraNevada_14_B22'
)

# Base URL for DEM products (OPR)
base <- 'https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/OPR/Projects/CA_SierraNevada_B22'


get_unit_links <- function(unit) {
  url <- paste0(base, '/', unit, '/0_file_download_links.txt')
  raw <- tryCatch(readLines(url, warn = FALSE), error = function(e) character(0))
  if (!length(raw)) return(character(0))
  
  # Some of these "txt" files behave like one long line: split into tokens (URLs)
  tokens <- unlist(strsplit(raw, '\\s+'))
  tokens[nzchar(tokens)]
}

# Pull all links once per work unit
unit.links <- setNames(lapply(work.units, get_unit_links), work.units)

# sanity check: should be BIG, non-zero
sapply(unit.links, length)

# match using tile id, keep only .tif
dem.urls <- unique(unlist(lapply(tile.ids, function(id) {
  hits <- unlist(lapply(unit.links, function(x) x[grepl(id, x, fixed = TRUE)]))
  hits
}), use.names = FALSE))

dem.urls <- dem.urls[grepl('\\.tif$', dem.urls, ignore.case = TRUE)]
length(dem.urls)
dem.urls[1:10]

# --- download ---
out.dir <- 'J:/Structure_Data/Fire_Snow/fireandice/data/raw/DEM'

dest <- file.path(out.dir, basename(dem.urls))

mapply(function(u, d) download.file(u, d, mode = 'wb', quiet = TRUE),
       u = dem.urls, d = dest)


# ----- caldor ------
# determine tile names
index.a <- read_sf('data/processed/processed/shp/tile_index_caldor_8.shp')
index.b <- read_sf('data/processed/processed/shp/tile_index_caldor_5.shp')

# standardize col names
dem.index <- bind_rows(
  st_drop_geometry(index.a) %>%
    select(Tile, WU_NAME),
  
  st_drop_geometry(index.b) %>%
    rename(Tile = Tile_ID, WU_NAME = WU_Name) %>%
    select(Tile, WU_NAME)
) %>%
  distinct(Tile, WU_NAME, .keep_all = TRUE)

# base output directory
out.dir <- 'data/raw/DEM/caldor' # processing comp

dem.tile.ids <- dem.index$Tile
dem.acquisition <- dem.index$WU_NAME

# build urls
base.url <- paste0(
  'https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/OPR/Projects/',
  'CA_SierraNevada_B22/'
)

file.names <- ifelse(
  grepl('_5_', dem.acquisition),
  paste0('USGS_OPR_CA_SierraNevada_B22_bh_', dem.tile.ids, '.tif'),
  paste0('USGS_OPR_CA_SierraNevada_B22_', dem.tile.ids, '.tif')
)

urls <- paste0(
  base.url,
  dem.acquisition,
  '/TIFF/',
  file.names
)

# save into acquisition folders, but remove _bh_ from local filename
dest.names <- gsub('_bh_', '_', basename(urls))

dest.files <- file.path(
  out.dir,
  dem.acquisition,
  dest.names
)

# create acquisition folders
invisible(lapply(
  unique(dirname(dest.files)),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# settings
min.size <- 5 * 1024^2 # 5 MB
options(timeout = 1000)
plan(multisession, workers = 2)
handlers(global = TRUE)
handlers('txtprogressbar')

# test 
test.urls <- urls[1:5]
test.dest <- dest.files[1:5]

results <- future_mapply(
  FUN = download.one,
  url = test.urls,
  dest = test.dest,
  SIMPLIFY = T,
  min.size = min.size
)

# download
tic('Downloading DEM tiles')
with_progress({
  
  p <- progressor(along = urls)
  
  results <- future_mapply(
    FUN = function(url, dest) {
      out <- download.one(url, dest, min.size)
      p()
      out
    },
    url = urls,
    dest = dest.files,
    SIMPLIFY = TRUE
  )
})
toc()

# --- check for missing tiles ---
missing.dems <- check_missing_dems(
  acq.check = 'CA_SierraNevada_8_2022',
  out.dir = out.dir,
  dem.tile.ids = dem.tile.ids,
  dem.acquisition = dem.acquisition
)

expected.idx <- dem.acquisition == 'CA_SierraNevada_8_2022'
missing.idx <- which(expected.idx & dem.tile.ids %in% missing.dems)
missing.urls <- urls[missing.idx]
missing.dest <- dest.files[missing.idx]

# download missing DEMS
for (i in seq_along(missing.urls)) {
  
  cat('\n', i, '/', length(missing.urls), ': ', basename(missing.dest[i]), '\n')
  
  if (file.exists(missing.dest[i])) file.remove(missing.dest[i])
  
  ok <- tryCatch(
    identical(
      download.file(
        url = missing.urls[i],
        destfile = missing.dest[i],
        mode = 'wb',
        quiet = FALSE,
        method = 'libcurl'
      ),
      0L
    ),
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  
  results.missing[i] <- ok && file.exists(missing.dest[i])
  
  cat('success:', results.missing[i], '\n')
}


# ----- castle ------
# determine tile names
dem.index <- read_sf('data/processed/processed/shp/tile_index_1524_castle.shp')

# chose out.dir depending on which computer you're on
out.dir <- 'data/raw/DEM/castle' # processing computer
out.dir <- 'J:/Fire_Snow/fireandice/data/raw/DEM/castle' # km computer

dem.tile.ids <- dem.index$Tile
dem.acquisition <- dem.index$WU_NAME

# build urls
base.url <- paste0(
  'https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/OPR/Projects/',
  'CA_SierraNevada_B22/'
)

file.names <- ifelse(
  grepl('_9_', dem.acquisition),
  paste0('USGS_OPR_CA_SierraNevada_B22_bh_', dem.tile.ids, '.tif'),
  paste0('USGS_OPR_CA_SierraNevada_B22_be_', dem.tile.ids, '.tif')
)

urls <- paste0(
  base.url,
  dem.acquisition,
  '/TIFF/',
  file.names
)

# build destination file paths
dest.names <- gsub('_(bh|be)_', '_', basename(urls))
dest.files <- file.path(out.dir, dest.names)

# settings
min.size <- 5 * 1024^2 # 5 MB
options(timeout = 1000)
plan(multisession, workers = 2)
handlers(global = TRUE)
handlers('txtprogressbar')

# test 
test.urls <- urls[1:5]
test.dest <- dest.files[1:5]

results <- future_mapply(
  FUN = download.one,
  url = test.urls,
  dest = test.dest,
  SIMPLIFY = T,
  min.size = min.size
)

# download
tic('Downloading DEM tiles')
with_progress({
  
  p <- progressor(along = urls)
  
  results <- future_mapply(
    FUN = function(url, dest) {
      out <- download.one(url, dest, min.size)
      p()
      out
    },
    url = urls,
    dest = dest.files,
    SIMPLIFY = TRUE
  )
})
toc()

# check for missing tiles 
downloaded.dems <- list.files(out.dir, pattern = '\\.tif$', full.names = F)

# extract tile ID from filename
downloaded.demids <- downloaded.dems %>%
  tools::file_path_sans_ext() %>%
  sub('USGS_OPR_CA_SierraNevada_B22_', '', .)

# tile IDs that should have downloaded but are missing
missing.dems <- setdiff(dem.tile.ids, downloaded.demids)

length(missing.dems)
missing.dems

missing.urls <- urls[tile.ids %in% missing.dems]
missing.dest <- dest.files[tile.ids %in% missing.dems]

results.missing <- future_mapply(
  FUN = download.one,
  url = missing.urls,
  dest = missing.dest,
  MoreArgs = list(min.size = min.size),
  SIMPLIFY = TRUE
)

# ----- dixie ------
# determine tile names
index.a <- read_sf('data/processed/processed/shp/tile_index_dixie_4_low.shp')
index.b <- read_sf('data/processed/processed/shp/tile_index_dixie_7_low.shp')
index.c <- read_sf('data/processed/processed/shp/tile_index_dixie_6_low.shp')

dem.index <- bind_rows(
  
  st_drop_geometry(index.a) %>%
    rename(Tile = Tile_ID) %>%
    select(Tile, WU_NAME),
  
  st_drop_geometry(index.b) %>%
    select(Tile, WU_NAME),
  
  st_drop_geometry(index.c) %>%
    rename(Tile = Tile_ID) %>%
    select(Tile, WU_NAME)
  
) %>%
  distinct(Tile, WU_NAME, .keep_all = TRUE)

out.dir <- 'data/raw/DEM/dixie'
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

dem.tile.ids <- dem.index$Tile
dem.acquisition <- dem.index$WU_NAME

base.url <- paste0(
  'https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/OPR/Projects/',
  'CA_SierraNevada_B22/'
)

file.names <- paste0(
  'USGS_OPR_CA_SierraNevada_B22_bh_',
  dem.tile.ids,
  '.tif'
)

urls <- paste0(
  base.url,
  dem.acquisition,
  '/TIFF/',
  file.names
)

dest.names <- gsub('_bh_', '_', basename(urls))

dest.files <- file.path(
  out.dir,
  dem.acquisition,
  dest.names
)

invisible(lapply(
  unique(dirname(dest.files)),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# settings
min.size <- 5 * 1024^2 # 5 MB
options(timeout = 1000)
plan(multisession, workers = 2)
handlers(global = TRUE)
handlers('txtprogressbar')

# test 
test.urls <- urls[1:5]
test.dest <- dest.files[1:5]

results <- future_mapply(
  FUN = download.one,
  url = test.urls,
  dest = test.dest,
  SIMPLIFY = T,
  min.size = min.size
)

# download
tic('Downloading DEM tiles')
with_progress({
  
  p <- progressor(along = urls)
  
  results <- future_mapply(
    FUN = function(url, dest) {
      out <- download.one(url, dest, min.size)
      p()
      out
    },
    url = urls,
    dest = dest.files,
    SIMPLIFY = TRUE
  )
})
toc()

# ----- check for missing tiles -----
acq.check <- 'CA_SierraNevada_4_2022'

missing.dems <- check_missing_dems(
  acq.check = acq.check,
  out.dir = out.dir,
  dem.tile.ids = dem.tile.ids,
  dem.acquisition = dem.acquisition
)

expected.idx <- dem.acquisition == acq.check
missing.idx <- which(expected.idx & dem.tile.ids %in% missing.dems)
missing.urls <- urls[missing.idx]
missing.dest <- dest.files[missing.idx]
options(timeout = 3000)
results.missing <- logical(length(missing.urls))
for (i in seq_along(missing.urls)) {
  
  cat('\n', i, '/', length(missing.urls), ': ', basename(missing.dest[i]), '\n')
  
  if (file.exists(missing.dest[i])) file.remove(missing.dest[i])
  
  ok <- tryCatch(
    identical(
      download.file(
        url = missing.urls[i],
        destfile = missing.dest[i],
        mode = 'wb',
        quiet = FALSE,
        method = 'libcurl'
      ),
      0L
    ),
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  
  results.missing[i] <- ok && file.exists(missing.dest[i])
  
  cat('success:', results.missing[i], '\n')
}


# ==============================================================================
# Divide into acquisitions for overlapping areas
# ==============================================================================
# only caldor and dixie have overlapping tiles (I think)

fire <- 'dixie'

index <- read_sf(paste0('data/processed/processed/shp/tile_index_1524_', fire, '.shp'))

als.dir <- file.path('J:/Fire_Snow/fireandice/data/raw/ALS', paste0('laz_', fire))

# all LAZ files currently mixed together
files <- list.files(als.dir, pattern = '\\.laz$', full.names = TRUE)

# make lookup table from index
tile.lookup <- index %>%
  st_drop_geometry() %>%
  select(Tile, WU_NAME) %>%
  distinct()

# extract tile ID from filename and create table with old and new file paths
file.info <- data.frame(
  file = files,
  filename = basename(files)
) %>%
  mutate(
    Tile = str_remove(filename, '^USGS_LPC_CA_SierraNevada_B22_'),
    Tile = str_remove(Tile, '\\.laz$')
  ) %>%
  left_join(tile.lookup, by = 'Tile') %>%
  mutate(
    new.dir = file.path(als.dir, WU_NAME),
    new.file = file.path(new.dir, filename)
  )

# check everything
table(file.info$WU_NAME, useNA = 'ifany')

missing.acq <- filter(file.info, is.na(WU_NAME))
missing.acq
# shouldn't be missing anything

# create new folders
dir.create(als.dir, recursive = TRUE, showWarnings = FALSE)

unique.dirs <- unique(file.info$new.dir[!is.na(file.info$new.dir)])

invisible(lapply(unique.dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

ok <- file.rename(
  file.info$file[!is.na(file.info$WU_NAME)],
  file.info$new.file[!is.na(file.info$WU_NAME)]
)

table(ok)













# ----------- OLD -------------
# ==============================================================================
# select only needed tiles from Liz's harddrive
# ==============================================================================

index.13 <- read_sf('data/raw/ALS/tile_index_13.shp')
index.14 <- read_sf('data/raw/ALS/tile_index_14.shp')

# ----- create list of file names -----
prefix <- 'USGS_LPC_CA_SierraNevada_B22_'
basenames <- paste0(prefix, index.14$Tile)
tiles.laz <- paste0(basenames, '.laz')

writeLines(tiles.laz, 'J:/Fire_Snow/fireandice/data/raw/ALS/tiles_14.txt')

# ----- copy only needed tiles -----
source.dir <- 'D:/Siera_2022_LPC_Unit13'
dest.dir <- "J:/Fire_Snow/fireandice/data/raw/ALS/laz"

# full paths
from <- file.path(source.dir, tiles.laz)
to <- file.path(dest.dir, tiles.laz)

# check to makes sure all files exists 
exists <- file.exists(from)
unique(exists)

# the actual copying function is completed directly in powershell

# ==============================================================================
# remove tiles that are too low in elevation
# ==============================================================================

# ----- CREEK -----

# shapefile that contains all tile that are too low in elevation
tiles.to.remove <- read_sf('data/raw/ALS/tiles_to_remove.shp')
# *** add in code to also remove files that are not complete! ***

# ----- create list of file names -----
prefix <- 'USGS_LPC_CA_SierraNevada_B22_'
basenames <- paste0(prefix, tiles.to.remove$Tile)
to.remove <- paste0(basenames, '.laz')

# name dirs
source.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_creek'
dest.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_creek_unused'

# get full paths of files 
files.to.move <- file.path(source.dir, to.remove)
files.to.move <- files.to.move[file.exists(files.to.move)]

head(files.to.move)
length(files.to.move)


# move files
file.rename(from = files.to.move, 
            to = file.path(dest.dir, basename(files.to.move)))

# ----- DIXIE -----

# shapefile that contains all tiles that we wamt
tiles.dixie <- read_sf('data/processed/processed/shp/tile_index_1524_dixie.shp')

# *** add in code to also remove tiles that are not complete! ***

# ----- create list of file names -----
prefix <- 'USGS_LPC_CA_SierraNevada_B22_'
basenames <- paste0(prefix, tiles.to.remove$Tile)
to.remove <- paste0(basenames, '.laz')

# name dirs
source.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_creek'
dest.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_creek_unused'

# get full paths of files 
files.to.move <- file.path(source.dir, to.remove)
files.to.move <- files.to.move[file.exists(files.to.move)]

head(files.to.move)
length(files.to.move)


# move files
file.rename(from = files.to.move, 
            to = file.path(dest.dir, basename(files.to.move)))




extra.ids <- setdiff(downloaded.ids, tile.ids)

length(extra.ids)
extra.ids

files <- list.files(out.dir, pattern = '_bh_', full.names = TRUE)

new.files <- file.path(
  out.dir,
  gsub('_bh_', '_', basename(files))
)

file.rename(files, new.files)


library(sf)
library(dplyr)
library(stringr)

fire <- 'dixie'

dem.dir <- file.path('data/raw/DEM', fire)

# combine both acquisition indexes
index.b <-  index.b %>%
  mutate(Tile_ID = Tile)

index.all <- bind_rows(
  st_drop_geometry(index.a),
  st_drop_geometry(index.b),
  st_drop_geometry(index.c)
) %>%
  select(Tile_ID, WU_NAME) %>%
  distinct()

# list DEMs currently in main fire folder
dem.files <- list.files(
  dem.dir,
  pattern = '\\.tif$',
  full.names = TRUE,
  recursive = FALSE
)

# build move table
dem.move <- data.frame(
  file = dem.files,
  filename = basename(dem.files)
) %>%
  mutate(
    Tile_ID = str_match(filename, '([0-9]{2}[A-Z]{3}[0-9]{4})')[, 2]
  ) %>%
  left_join(index.all, by = 'Tile_ID') %>%
  mutate(
    new.dir = file.path(dem.dir, WU_NAME),
    new.file = file.path(new.dir, filename)
  )

# check for files that did not match either index
dem.move %>%
  filter(is.na(WU_NAME)) %>%
  select(filename, Tile_ID)

# create acquisition folders
invisible(lapply(
  unique(dem.move$new.dir),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# move files
ok <- file.rename(
  from = dem.move$file,
  to = dem.move$new.file
)

table(ok)
