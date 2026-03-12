import os
import sys
import subprocess
import rsun_params as p

grass_python = subprocess.check_output(
    [p.grass_bin, '--config', 'python_path'],
    text=True
).strip()

if grass_python not in sys.path:
    sys.path.append(grass_python)

import grass.script as gs


def main():
    os.makedirs(p.out_dir, exist_ok=True)

    print('Starting terrain prep')
    print(f'GISDB: {p.gisdb}')
    print(f'Location: {p.location}')
    print(f'Mapset: {p.mapset}')
    print(f'Input DEM: {p.dem_in}')

    with gs.setup.init(p.gisdb, p.location, p.mapset):
        print('GRASS session started')

        gs.run_command(
            'r.in.gdal',
            input=p.dem_in,
            output=p.dem_name,
            overwrite=True
        )

        gs.run_command(
            'g.region',
            raster=p.dem_name
        )

        gs.run_command(
            'r.slope.aspect',
            elevation=p.dem_name,
            slope=p.slope_name,
            aspect=p.aspect_name,
            format='degrees',
            overwrite=True
        )

        print('\nDEM info:')
        print(gs.read_command('r.info', map=p.dem_name))

        print('\nSlope info:')
        print(gs.read_command('r.info', map=p.slope_name))

        print('\nAspect info:')
        print(gs.read_command('r.info', map=p.aspect_name))

    print('\nTerrain prep finished successfully')


if __name__ == '__main__':
    main()