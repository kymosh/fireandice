# Parameters for running r.sun in GRASS GIS. These are imported into rsun.py, which is the main script for running r.sun and processing outputs.


# Specify path where GRASS Python is located and add it to sys.path
grass_bin = r'C:\Program Files\GRASS GIS 8.4\grass84.bat'


gisdb = r"J:\Structure_Data\Fire_Snow\fireandice\grassdata" # path to GRASS GIS database (GISDBASE)
location = "rsun_full_location" # name of GRASS location (should match the name of the location you created in GRASS GIS)
mapset = "PERMANENT" 

surface = 'dtm' # either 'dtm' or 'dsm' - this is used to specify which DEM to use for the r.sun runs, and it is also included in the output file names for clarity. Make sure the input DEM file name in dem_in matches this surface name.
dem_in = rf'J:\Structure_Data\Fire_Snow\fireandice\data\processed\processed\tif\1m\creek_dtm_1m.tif'
out_dir = r'J:\Structure_Data\Fire_Snow\fireandice\data\processed\processed\tif\1m\creek_rad'

dem_name = surface # name of the imported DEM raster
slope_name = f'{surface}_slope' # name of the slope raster
aspect_name = f'{surface}_aspect' # name of the aspect raster
horizon_basename = f'{surface}_horizon' # base name for horizon rasters, which will have the format {horizon_basename}_{azimuth}
horizon_step = 30 # step size for horizon rasters in degrees (e.g., 15 means rasters will be generated for azimuths 0, 30, 60, ..., 360)

days = [15] # start with just one day for testing, then add more to the list. 
step = 0.5
linke_value = 1.8
albedo_value = 0.6

nprocs = 35 # number of processors for parallel processing