packages <- c('terra', 'lidR', 'sf', 'future', 'lidRviewer')
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)

library('lidRviewer')

install.packages('lidRviewer', repos='https://r-lidar.r-universe.dev')
las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud-reduced.laz')

plot(las) 
view(las) # opens in new window
# can toggle between different views
  # c - classification
  # z - height
  # l - loggles idome lighting
  # i - intensity

las <- classify_noise(las, ivf(res = 3, n = 10)) # ivf algorithm
table(las$Classification)

# deletes points classified as noise (class 18)
las <- filter_poi(las, Classification != LASNOISE) # could also put 18 here
table(las$Classification)

plot(las)
view(las)
max(las$Z)

# can also filter using height
# las <- filter_poi(las, Z < 400)

colnames(las@data)

#--------- always use tin()?

las <- classify_ground(las, algorithm = csf())

# make dem
dem <- rasterize_terrain(las, res = 1, algorithm = tin())
plot(dem)

# calc slope and aspect
slope = terrain(dem, v = 'slope', unit = 'degrees')
aspect = terrain(dem, v = 'aspect', unit = 'degrees')

# plot
par(mfrow=c(1,2)) # make a 1x2 panel plot window
plot(slope)
plot(aspect)
# writeRaster(dem, 'dem_lidar_workshop.tiff') # write GEOTiff to file

# ------- canopy surface model
csm <- rasterize_canopy(las, algorithm = pitfree())

# ------- canopy height model
chm <- csm - dem # normalization
par(mfrow = c(2,2), mar=c(1,1,3,2))
plot(dem)
plot(csm)
plot(chm)

# normalize height
las.norm2 <- normalize_height(las, algorithm = tin())

# quantify different structure thresholds
understory_points <- filter_poi(las.norm2, Classification != LASGROUND & Z < 2) # keep only points that are not ground and are < 2m
understory_only <- rasterize_density(understory_points, res = 10)
view(understory_points)

# total density
total_density <- rasterize_density(las.norm2, res = 10)

# percent 
percent_understory <- understory_only / total_density
par(mfrow = c(1,1))
plot(percent_understory)

# make custom functions
my_custom_function <- function(z) {sd(z) / mean(z)}

complexity <- pixel_metrics(las, my_custom_function(Z), res = 10)

# ==============================================================================
# stem mapping
# ==============================================================================

tree.tops <- locate_trees(chm, lmf(ws = 6, hmin = 10)) # lmf = local maximum function, hmin = min tree height
plot(chm)
plot(tree.tops$geometry, add = T, pch = 16, cex = 0.2)

# define algorithm
crown.delim.alg <- dalponte2016(chm, tree.tops, th_tree = 2) # threshold below which a pixel cannot be a tree
# can use different algorithms, silva2016 is another option or li2012

crown.raster <- crown.delim.alg()

# convert to polygons
crowns <- as.polygons(crown.raster)


plot(chm)
plot(crowns, col = pastel.colors(8000), add = T)

# can convert polygons into vect objects and write
#writeVector(vect(tree.tops), 'tree_tops.shp')
#writeVector(crowns, 'crowns.shp')

# ==============================================================================
# stand characteristics
# ==============================================================================

n.trees <- nrow(crowns)
n.trees

# calculate area of scene
scene.area <- prod(res(chm)) * prod(dim(chm))
scene.area.hect <- scene.area / 10000

# calc tree density
print(n.trees / scene.area.hect)

# convert from terra to sf
crowns.sf <- st_as_sf(crowns)

# convert to sf object
crowns.sf$crown_area = st_area(crowns.sf)
crowns.sf$crown_radius = sqrt(crowns.sf$crown_area/pi)

hist(crowns.sf$crown_radius)
hist(tree.tops$Z)
plot(crowns.sf$crown_radius ~ tree.tops$Z)



# ============
# efficiency
# ==========
library(rlas)

# indicate the path to the file name that you want to index
writelax('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud.laz')

# ----------- subset data as you read it in to save on processing time/space ----------

# --- crop ---
# Subset within the bounding box specified by the UTM16 coordinates 740400 3459500 740500 3459600
las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud.laz', filter = '-keep_xy 740400 3459500 740500 3459600') #xmin ymin xmax ymax

# Subset a 30 m circular area around the coordinate 740400 3459500 
las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud.laz', filter = '-keep_circle 740400 3459500 30')
#xcenter ycenter radius

# --- thin ---
# Thin the LiDAR scene randomly to 10% of its original density
las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud.laz', filter = '-keep_random_fraction 0.1') 
las = filter_poi(las, Z < 300 & Z > 30)
view(las)

# Load one point every 2 m. Useful for a coarse DEM or CHM (randomly!)
las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud.laz', filter = '-thin_with_grid 2')
las = filter_poi(las, Z < 300 & Z > 30)
view(las) # Load one point in every 0.5 x 0.5 x 0.5 m voxel.

# controls for specific density. keeps 1 random point within voxel. essentially normalizes point density
las = readLAS('NEON_D03_JERC_DP1_740000_3459000_classified_point_cloud.laz', filter = '-thin_with_voxel 0.5 -drop_z_above 90 -drop_z_below 20')
#las = filter_poi(las, Z < 300)
#noise filter not needed since drop z took care of it.
view(las) 

# list of all ways to filter ****** good resource! ******
readLAS(filter = '-help')


# example code (won't work)
# Path to your lidar directory
dir = 'C:/JERC/ClassifiedPointCloud/'

# list of all the .las or .laz files in that directory
lazfiles = list.files(dir, pattern = '.las|.laz', full.names=TRUE)

# Loop through all files and writelax for each one.
library(rlas)

for(f in lazfiles) writelax(f) # writes .lax file for each file


#### trying to do this with my own data
ctg.t <- readLAScatalog('data/raw/ALS/laz/random_tiles')

dir <- 'las-20251202T171856Z-1-001/las'

ctg.t <- readLAScatalog(dir)
plot(ctg.t)

### set up parallelization (this way is specific to lidR package)
# Let's make the chunks bigger! 
opt_chunk_size(ctg.t) = 2000                   # play with number of workers and chunk size to optimaze based on our storeage and processing needs
# 2000 would be larger chunk
# 250 breaks each of my 1 tiles into 16
# in this case the chunks are actually smaller

#give each chunk a 10 m buffer 
opt_chunk_buffer(ctg.t) = 10

# Choose the origin of where your chunks are drawn # i like them to snap to round numbers, so start at (0,0)
opt_chunk_alignment(ctg.t) = c(0,0)

# let's thin data down to 0.5 m voxel 
opt_filter(ctg.t) = '-thin_with_voxel 0.5'

# Now view your chunk pattern 
plot(ctg.t, chunk_pattern = TRUE) 

# create function to run on each chunk to create chm
chm.function <- function(las) {
  las = classify_noise(las, ivf(res = 3, n = 10))
  las = filter_poi(las, Classification != LASNOISE)
  las = classify_ground(las, algorithm = csf())
  dem = rasterize_terrain(las, res = 1, algorithm = tin())
  csm = rasterize_canopy(las, algorithm = pitfree())
  chm = csm - dem
  return(chm)
}

chm <- chm.function(las)
plot(chm)

# Set the number of cores to use using the `plan` and `multisession` functions
plan(multisession, workers = 4)
output <- catalog_map(ctg.t, chm.function)
# catalog_map reads in all the files in your catalog, clip them to your chunks, use the buffer, and process all in one

plot(output)










