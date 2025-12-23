packages <- c('terra', 'sf', 'dplyr', 'future', 'future.apply')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

# ==============================================================================
# code for downloading lidar tiles from USGS Rockyweb in bulk
# ==============================================================================

# read in shp file of file index
index.11 <- read_sf('data/raw/ALS/tile_index_11.shp')

tile.ids <- index.11$Tile



# ----- build RockyWeb download URLs -----

base.url <- paste0(
  'https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/',
  'CA_SierraNevada_B22/',
  'CA_SierraNevada_11_B22/',  
  'LAZ/'
)

urls <- paste0(
  base.url,
  'USGS_LPC_CA_SierraNevada_B22_',
  tile.ids,
  '.laz'
)

# make this where we want the files to go
out.dir <- normalizePath('data/raw/ALS/creek_fire', mustWork = FALSE)
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

# ----- set up parallelism -----

# 2 workers works best for rockyweb downloads
plan(multisession, workers = 2)

dest.files <- file.path(out.dir, basename(urls))

# ----- test -----
test.urls <- urls[1:5]
test.dest <- dest.files[1:5]


# ----- download function -----

min.size <- 100*1024^2

download.one <- function(url, dest) {
  
  options(timeout = 600)
  
  if (file.exists(dest)) {
    if (file.info(dest)$size > min.size) {
      return(TRUE)
    } else {
      file.remove(dest) # delete corrupted partial
    }
  }
  
  tryCatch(
    {
      message('Downloading: ', basename(dest))
      download.file(url, dest, mode = 'wb', quiet = TRUE)
      TRUE
    },
    error = function(e) {
      message('FAILED: ', basename(dest))
      FALSE
    }
  )
}
    


results <- future_mapply(
  FUN = download.one,
  url = test.urls,
  dest = test.dest,
  SIMPLIFY = T
)
