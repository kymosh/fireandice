import os
import sys
import subprocess
import rsun_params as p
import datetime

# This script prepares the terrain data for running r.sun. It imports the DEM into GRASS GIS, calculates slope, aspect, and horizon rasters, and prints out info about the generated maps. The outputs are used as inputs for the r.sun runs in rsun.py.

# Specify path where GRASS Python is located and add it to sys.path
grass_python = subprocess.check_output(
    [p.grass_bin, '--config', 'python_path'],
    text=True
).strip()

if grass_python not in sys.path:
    sys.path.append(grass_python)

# Now we can import grass.script after ensuring the path is set
import grass.script as gs


def main():

    start_time = datetime.datetime.now()
    print(f'Start time: {start_time}')

    os.makedirs(p.out_dir, exist_ok=True) # ensure output directory exists

    print('Starting terrain prep')
    print(f'GISDB: {p.gisdb}')
    print(f'Location: {p.location}')
    print(f'Mapset: {p.mapset}')
    print(f'Input DEM: {p.dem_in}')

    with gs.setup.init(p.gisdb, p.location, p.mapset): # start GRASS session
        print('GRASS session started')

        # import DEM into GRASS
        gs.run_command(
            'r.in.gdal',
            input=p.dem_in,
            output=p.dem_name,
            overwrite=True
        )
        # set region to match DEM
        gs.run_command(
            'g.region',
            raster=p.dem_name
        )
        # calculate slope and aspect
        gs.run_command(
            'r.slope.aspect',
            elevation=p.dem_name,
            slope=p.slope_name,
            aspect=p.aspect_name,
            format='degrees',
            nprocs=p.nprocs,
            overwrite=True
        )

        print('\nDEM info:')
        print(gs.read_command('r.info', map=p.dem_name))

        print('\nSlope info:')
        print(gs.read_command('r.info', map=p.slope_name))

        print('\nAspect info:')
        print(gs.read_command('r.info', map=p.aspect_name))
        
    print('\nTerrain prep finished successfully')

    end_time = datetime.datetime.now()
    print(f'End time: {end_time}')
    print(f'Total runtime: {end_time - start_time}')


if __name__ == '__main__':
    main()