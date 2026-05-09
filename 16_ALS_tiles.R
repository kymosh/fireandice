packages <- c('terra', 'sf', 'dplyr', 'future', 'future.apply', 'progressr', 'tictoc')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# code for downloading lidar tiles from USGS Rockyweb in bulk
# ==============================================================================

# read in shp file of file index
# change fire name
index <- read_sf('data/processed/processed/shp/tile_index_1524_castle.shp')
out.dir <- 'data/raw/ALS/laz_castle'

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

options(timeout = 1000)

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



results <- future_mapply(
  FUN = download.one,
  url = urls,
  dest = dest.files,
  SIMPLIFY = T,
  min.size = min.size
)






# ==============================================================================
# code for downloading DEM tifs from USGS Rockyweb in bulk
# ==============================================================================

# extract tile names from creek batch

tile.dir <- 'J:/Structure_Data/Fire_Snow/fireandice/data/raw/ALS/laz_creek'

files <- list.files(tile.dir, pattern = '\\.laz$', full.names = FALSE)

tile.ids <- sub('.*_', '', tools::file_path_sans_ext(files))

tile.ids[1:10]  # sanity check

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

# ------ download ------
out.dir <- 'J:/Structure_Data/Fire_Snow/fireandice/data/raw/DEM'

dest <- file.path(out.dir, basename(dem.urls))

mapply(function(u, d) download.file(u, d, mode = 'wb', quiet = TRUE),
       u = dem.urls, d = dest)



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





