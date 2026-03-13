import os
import sys
import subprocess
import rsun_params as p
import datetime

# This script runs r.sun in GRASS GIS for the specified days and processes the outputs. It exports the global radiation maps as GeoTIFFs to the specified output directory.

grass_python = subprocess.check_output( # get the path to GRASS Python
    [p.grass_bin, '--config', 'python_path'],
    text=True
).strip()

# Ensure the GRASS Python path is in sys.path so we can import grass.script
if grass_python not in sys.path:
    sys.path.append(grass_python)

# Now we can import grass.script after ensuring the path is set
import grass.script as gs


def main():

    start_time = datetime.datetime.now()
    print(f'Start time: {start_time}')

    os.makedirs(p.out_dir, exist_ok=True) # ensure output directory exists

    print('Starting r.sun run')
    print(f'GISDB: {p.gisdb}')
    print(f'Location: {p.location}')
    print(f'Mapset: {p.mapset}')
    print(f'Days: {p.days}')

    with gs.setup.init(p.gisdb, p.location, p.mapset): # start GRASS session
        print('GRASS session started')

        # make sure region matches DEM
        gs.run_command(
            'g.region',
            raster=p.dem_name
        )
        # loop through specified days and run r.sun
        for day in p.days:
            print(f'\nRunning r.sun for day {day}')

            day_str = str(day).zfill(3)
            beam_name = f'beam_{day_str}'
            diff_name = f'diff_{day_str}'
            glob_name = f'glob_{day_str}'
            # run r.sun with specified parameters
            gs.run_command( 
                'r.sun',
                elevation=p.dem_name, # these were generated in the terrain prep script
                slope=p.slope_name,
                aspect=p.aspect_name,
                day=day, # these were defined in the parameters script
                step=p.step,
                linke_value=p.linke_value,
                albedo_value=p.albedo_value,
                beam_rad=beam_name, # output names for the radiation components
                diff_rad=diff_name,
                glob_rad=glob_name,
                nprocs=p.nprocs,
                overwrite=True
            )
            # define output file paths for the radiation maps
            glob_file = os.path.join(p.out_dir, f'rad_global_{p.surface}_day{day}.tif') 
            beam_file = os.path.join(p.out_dir, f'rad_beam_{p.surface}_day{day}.tif')
            diff_file = os.path.join(p.out_dir, f'rad_diff_{p.surface}_day{day}.tif')

            # export the radiation maps to GeoTIFFs
            gs.run_command(
                'r.out.gdal',
                input=glob_name,
                output=glob_file,
                format='GTiff',
                overwrite=True
            )

            gs.run_command(
                'r.out.gdal',
                input=beam_name,
                output=beam_file,
                format='GTiff',
                overwrite=True
            )

            gs.run_command(
                'r.out.gdal',
                input=diff_name,
                output=diff_file,
                format='GTiff',
                overwrite=True
            )

            print(f'Exported: {glob_file}')
            print(f'Exported: {beam_file}')
            print(f'Exported: {diff_file}')

    print('\nr.sun run finished successfully')

    end_time = datetime.datetime.now()
    print(f'End time: {end_time}')
    print(f'Total runtime: {end_time - start_time}')

if __name__ == '__main__':
    main()