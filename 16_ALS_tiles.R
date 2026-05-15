packages <- c('terra', 'sf', 'dplyr', 'future', 'future.apply', 'progressr', 'tictoc')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# code for downloading lidar tiles from USGS Rockyweb in bulk
# ==============================================================================

# read in shp file of file index
# change fire name
index <- read_sf('data/processed/processed/shp/tile_index_1524_dixie.shp')
index <- read_sf('data/processed/processed/shp/tile_index_dixie_7.shp')

# chose out.dir depending on which computer you're on
out.dir <- 'data/raw/ALS/laz_castle' # processing computer
out.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_dixie' # km computer
out.dir <- 'J:/Fire_Snow/fireandice/data/raw/ALS/laz_dixie/CA_SierraNevada_7_2022' # km computer

# make sure to check shape files so you're using the correct tile_ID col

tile.ids <- index$Tile
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



# ----- set up parallelism -----

# 2 workers works best for rockyweb downloads
plan(multisession, workers = 2)

dest.files <- file.path(out.dir, basename(urls))

# ----- test -----
test.urls <- urls[1:5]
test.dest <- dest.files[1:5]


# ----- download function -----

min.size <- 50 * 1024^2 # 50 MB

download.one <- function(url, dest, min.size) {
  
  options(timeout = 1000)
  
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

# run on test first
results <- future_mapply(
  FUN = download.one,
  url = test.urls,
  dest = test.dest,
  SIMPLIFY = T,
  min.size = min.size
)

# run on all 
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

results.missing <- future_mapply(
  FUN = download.one,
  url = missing.urls,
  dest = missing.dest,
  MoreArgs = list(min.size = min.size),
  SIMPLIFY = TRUE
)

download.check <- data.frame(
  tile = tile.ids,
  acquisition = acquisition,
  url = urls,
  dest = dest.files,
  downloaded = results
)

failed <- subset(download.check, !downloaded)
failed

# troubleshooting
missing.check <- data.frame(
  tile = tile.ids,
  url = urls,
  dest = dest.files,
  downloaded = tile.ids %in% downloaded.ids,
  exists = file.exists(dest.files),
  size.mb = ifelse(file.exists(dest.files),
                   round(file.info(dest.files)$size / 1024^2, 2),
                   NA)
)

missing.check <- missing.check[!missing.check$downloaded, ]

missing.check[, c('tile', 'exists', 'size.mb', 'url')]

library(httr)

missing.check$status <- sapply(missing.check$url, function(u) {
  res <- tryCatch(HEAD(u, timeout(30)), error = function(e) NULL)
  if (is.null(res)) NA_integer_ else status_code(res)
})

table(missing.check$status, useNA = 'ifany')
missing.check[missing.check$status != 200 | is.na(missing.check$status), ]

options(timeout = 3000)
for (i in seq_along(missing.urls)) {
  
  cat(i, '/', length(missing.urls), ': ',
      basename(missing.dest[i]), '\n')
  
  try(
    download.file(
      url = missing.urls[i],
      destfile = missing.dest[i],
      mode = 'wb',
      quiet = TRUE
    ),
    silent = TRUE
  )
}
# ==============================================================================
# code for downloading DEM tifs from USGS Rockyweb in bulk
# ==============================================================================

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
index <- read_sf('data/processed/processed/shp/tile_index_1524_caldor.shp')
dem.index <- subset(index, !Tile %in% missing.ids) # take out the ones that were too small

# chose out.dir depending on which computer you're on
out.dir <- 'data/raw/DEM/caldor' # processing computer
out.dir <- 'J:/Fire_Snow/fireandice/data/raw/DEM/caldor' # km computer

dem.tile.ids <- dem.index$Tile
dem.acquisition <- dem.index$WU_NAME

# build urls
base.url <- paste0(
  'https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/OPR/Projects/',
  'CA_SierraNevada_B22/'
)

file.names <- ifelse(
  grepl('_5_', acquisition),
  paste0('USGS_OPR_CA_SierraNevada_B22_bh_', dem.tile.ids, '.tif'),
  paste0('USGS_OPR_CA_SierraNevada_B22_', dem.tile.ids, '.tif')
)

urls <- paste0(
  base.url,
  acquisition,
  '/TIFF/',
  file.names
)

# build destination file paths
dest.names <- gsub('_bh_', '_', basename(urls))
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
missing.dems <- setdiff(tile.ids, downloaded.demids)

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
dem.index <- read_sf('data/processed/processed/shp/tile_index_1524_dixie.shp')

# chose out.dir depending on which computer you're on
#out.dir <- 'data/raw/DEM/dixie' # processing computer
out.dir <- 'J:/Fire_Snow/fireandice/data/raw/DEM/dixie' # km computer

dem.tile.ids <- dem.index$Tile
dem.acquisition <- dem.index$WU_NAME

# build urls
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

# build destination file paths
dest.names <- gsub('_bh_', '_', basename(urls))
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

missing.urls <- urls[dem.tile.ids %in% missing.dems]
missing.dest <- dest.files[dem.tile.ids %in% missing.dems]

results.missing <- future_mapply(
  FUN = download.one,
  url = missing.urls,
  dest = missing.dest,
  MoreArgs = list(min.size = min.size),
  SIMPLIFY = TRUE
)


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
