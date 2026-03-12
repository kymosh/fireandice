# Parameters for running r.sun in GRASS GIS. These are imported into rsun.py, which is the main script for running r.sun and processing outputs.

grass_bin = r'C:\Program Files\GRASS GIS 8.4\grass84.bat'

gisdb = r"J:\Structure_Data\Fire_Snow\fireandice\grassdata"
location = "rsun_test_location"
mapset = "PERMANENT"

dem_in = r'J:\Structure_Data\Fire_Snow\fireandice\data\processed\processed\tif\1m\creek_dem_test_9.tif'
out_dir = r'J:\Structure_Data\Fire_Snow\fireandice\data\processed\processed\tif\rsun_test_outputs'

dem_name = 'dem'
slope_name = 'slope'
aspect_name = 'aspect'

days = [15, 45]
step = 0.5
linke_value = 1.8
albedo_value = 0.2

nprocs = 1