"""Register a FreeDict virtual environment without damaging other settings."""

import argparse
from pathlib import Path
import re
import sys


def register(config_path, environment, choice='auto'):
    config_path = Path(config_path).expanduser()
    if choice == 'no' or (choice == 'auto' and not sys.stdin.isatty()):
        return False
    if choice == 'auto' and input('Add this virtual environment to FreeDict configuration? [y|n] ').lower() != 'y':
        return False
    original = config_path.read_text(encoding='utf-8') if config_path.exists() else ''
    value = str(Path(environment).expanduser().resolve())
    lines = original.splitlines(keepends=True)
    default_start = next((i for i, line in enumerate(lines)
                          if re.match(r'\s*\[DEFAULT\]\s*(?:[#;].*)?$', line)), None)
    if default_start is None:
        lines = ['[DEFAULT]\n', f'virtual_env = {value}\n'] + lines
    else:
        default_end = next((i for i in range(default_start + 1, len(lines))
                            if re.match(r'\s*\[[^]]+\]', lines[i])), len(lines))
        replacement = f'virtual_env = {value}\n'
        existing = next((i for i in range(default_start + 1, default_end)
                         if re.match(r'\s*virtual_env\s*=', lines[i], re.I)), None)
        if existing is None:
            lines.insert(default_start + 1, replacement)
        else:
            lines[existing] = replacement
    config_path.parent.mkdir(parents=True, exist_ok=True)
    with config_path.open('w', encoding='utf-8') as config_file:
        config_file.write(''.join(lines))
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
