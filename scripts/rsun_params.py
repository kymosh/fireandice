# Parameters for running r.sun in GRASS GIS. These are imported into rsun.py, which is the main script for running r.sun and processing outputs.

# set these each run!
fire = 'dixie'
mode = 'full'   # 'test' or 'full'
surface = 'dsm' # either 'dtm' or 'dsm' - this is used to specify which DEM to use for the r.sun runs
in_res = 5

# --- shouldn't need to change anything else below here ---

epsg = {
    'castle': 32611,
    'caldor': 32610,
    'dixie': 32610
}[fire]

# Specify path where GRASS Python is located and add it to sys.path
grass_bin = r'C:\Program Files\GRASS GIS 8.4\grass84.bat'

gisdb = r"J:\Structure_Data\Fire_Snow\fireandice\grassdata" # path to GRASS GIS database (GISDBASE)

mapset = "PERMANENT" 



# ----- Mode specific settings -----
if mode == 'test':
    location = f'rsun_test_{epsg}' # name of GRASS location 
    dem_in = rf'J:\Structure_Data\Fire_Snow\fireandice\data\processed\processed\tif\{in_res}m\{fire}\{fire}_{surface}_test_9.tif'
    out_dir = r'J:\Structure_Data\Fire_Snow\fireandice\data\processed\processed\tif\rsun_test_outputs'

elif mode == 'full':
    location = f'rsun_full_{epsg}' # name of GRASS location 
    dem_in = rf'J:\Structure_Data\Fire_Snow\fireandice\data\processed\processed\tif\{in_res}m\{fire}\{fire}_{surface}_{in_res}m.tif'
    out_dir = rf'J:\Structure_Data\Fire_Snow\fireandice\data\processed\processed\tif\5m\{fire}\rad'

res = 5
days = [349, 46, 74, 105, 135, 166] # start with just one day for testing, then add more to the list
step = 0.5
linke_value = 1.8
albedo_value = 0.6

dem_name = f'{fire}_{surface}_{res}m'
slope_name = f'{fire}_{surface}_{res}m_slope'
aspect_name = f'{fire}_{surface}_{res}m_aspect'



nprocs = 30 # number of processors for parallel processing