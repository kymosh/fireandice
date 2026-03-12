import os
import subprocess
import sys

# --------------------------------------------------
# USER INPUTS
# --------------------------------------------------

grass_bin = r"C:\Program Files\GRASS GIS 8.4\grass84.bat"

gisdb = r"C:\grassdata"
location = "rsun_test_location"
mapset = "PERMANENT"

dem = r"J:\Fire_Snow\fireandice\data\processed\processed\tif\1m\creek_dem_test_9.tif"

out_dir = r"J:\Fire_Snow\fireandice\data\processed\processed\tif\rsun_test_outputs"

os.makedirs(out_dir, exist_ok=True)

# --------------------------------------------------
# CREATE LOCATION FROM DEM
# --------------------------------------------------

subprocess.run([
    grass_bin,
    "-c", dem,
    os.path.join(gisdb, location),
    "--exec",
    "g.region",
    "raster=" + dem
])

# --------------------------------------------------
# RUN r.sun INSIDE GRASS
# --------------------------------------------------

cmd = [
    grass_bin,
    os.path.join(gisdb, location, mapset),
    "--exec",

    "bash", "-c",

    f"""
    r.in.gdal input="{dem}" output=dem_test --overwrite
    g.region raster=dem_test

    r.slope.aspect elevation=dem_test slope=slope aspect=aspect --overwrite

    r.sun elevation=dem_test slope=slope aspect=aspect day=172 \
    beam_rad=beam diff_rad=diff glob_rad=glob insol_time=insol \
    --overwrite

    r.out.gdal input=glob output="{out_dir}/rsun_global_test.tif" format=GTiff --overwrite
    """
]

subprocess.run(cmd)

print("Finished r.sun test")