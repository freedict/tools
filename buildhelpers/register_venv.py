"""Register a FreeDict virtual environment without damaging other settings."""

import argparse
import configparser
from pathlib import Path
import sys


def register(config_path, environment, choice='auto'):
    config_path = Path(config_path).expanduser()
    if choice == 'no' or (choice == 'auto' and not sys.stdin.isatty()):
        return False
    if choice == 'auto' and input('Add this virtual environment to FreeDict configuration? [y|n] ').lower() != 'y':
        return False
    parser = configparser.ConfigParser()
    if config_path.exists():
        parser.read(config_path)
    parser['DEFAULT']['virtual_env'] = str(Path(environment).expanduser().resolve())
    config_path.parent.mkdir(parents=True, exist_ok=True)
    with config_path.open('w', encoding='utf-8') as config_file:
        parser.write(config_file)
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('config', type=Path)
    parser.add_argument('environment', type=Path)
    parser.add_argument('--choice', choices=['auto', 'yes', 'no'], default='auto')
    args = parser.parse_args()
    register(args.config, args.environment, args.choice)


if __name__ == '__main__':
    main()
