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

    print('Starting r.sun run')
    print(f'GISDB: {p.gisdb}')
    print(f'Location: {p.location}')
    print(f'Mapset: {p.mapset}')
    print(f'Days: {p.days}')

    with gs.setup.init(p.gisdb, p.location, p.mapset):
        print('GRASS session started')

        # make sure region matches DEM
        gs.run_command(
            'g.region',
            raster=p.dem_name
        )

        for day in p.days:
            print(f'\nRunning r.sun for day {day}')

            beam_name = f'beam_{day}'
            diff_name = f'diff_{day}'
            glob_name = f'glob_{day}'

            gs.run_command(
                'r.sun',
                elevation=p.dem_name,
                slope=p.slope_name,
                aspect=p.aspect_name,
                day=day,
                step=p.step,
                linke_value=p.linke_value,
                albedo_value=p.albedo_value,
                beam_rad=beam_name,
                diff_rad=diff_name,
                glob_rad=glob_name,
                overwrite=True
            )

            out_file = os.path.join(p.out_dir, f'rsun_global_day{day}.tif')

            gs.run_command(
                'r.out.gdal',
                input=glob_name,
                output=out_file,
                format='GTiff',
                overwrite=True
            )

            print(f'Exported: {out_file}')

    print('\nr.sun run finished successfully')


if __name__ == '__main__':
    main()