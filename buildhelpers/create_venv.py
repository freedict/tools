"""Create the FreeDict environment with the selected Python interpreter."""

import argparse
import os
from pathlib import Path
import subprocess
import sys


def create_environment(destination, tools):
    if sys.version_info < (3, 12):
        raise RuntimeError('The complete FreeDict toolchain requires Python 3.12 or newer.')
    destination = destination.expanduser().resolve()
    if destination.exists() and any(destination.iterdir()) and not (destination / 'pyvenv.cfg').is_file():
        raise RuntimeError(f'Refusing to reuse a non-venv directory: {destination}')
    subprocess.run([sys.executable, '-m', 'venv', str(destination)], check=True)
    python = destination / ('Scripts/python.exe' if os.name == 'nt' else 'bin/python')
    installed = subprocess.run([str(python), '-c', 'import icu'], capture_output=True).returncode == 0
    if not installed and not os.environ.get('ICU_VERSION'):
        try:
            subprocess.run(['pkg-config', '--exists', 'icu-i18n'], check=True)
        except (OSError, subprocess.CalledProcessError) as error:
            raise RuntimeError(
                'PyICU needs ICU development files and pkg-config. On Debian/Ubuntu, '
                'install libicu-dev, pkg-config, build-essential, python3-dev and python3-venv. '
                'Custom ICU builds may supply ICU_VERSION and PYICU_CFLAGS/PYICU_LFLAGS.'
            ) from error
    # Local requirements are relative to the tools checkout, not the caller's cwd.
    subprocess.run([str(python), '-m', 'pip', 'install', '--upgrade',
                    '-r', str(tools / 'requirements.txt')], cwd=tools, check=True)
    subprocess.run([str(python), '-m', 'pip', 'check'], check=True)
    return destination


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('destination', type=Path)
    args = parser.parse_args()
    try:
        create_environment(args.destination, Path(__file__).resolve().parents[1])
    except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
        parser.exit(1, f'{error}\n')


if __name__ == '__main__':
    main()
