"""Export an existing tools tag without working-tree or untracked files."""

import argparse
import lzma
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


def create_release(tools, build_directory, tag):
    if not tag:
        raise ValueError('TAG is required; use make release TAG=<existing-tag>')
    reference = f'refs/tags/{tag}'
    tagged_commit = subprocess.check_output(
        ['git', '-C', str(tools), 'rev-parse', '--verify', '--end-of-options',
         reference + '^{commit}'], text=True).strip()
    head = subprocess.check_output(
        ['git', '-C', str(tools), 'rev-parse', 'HEAD'], text=True).strip()
    if head != tagged_commit:
        print(f'Warning: HEAD differs from TAG={tag}; archiving the tagged tree.',
              file=sys.stderr)
    build_directory.mkdir(parents=True, exist_ok=True)
    # Hierarchical Git tag names must still produce a single archive filename.
    filename = f"freedict-tools-{tag.replace('/', '-')}.tar.xz"
    destination = build_directory / filename
    temporary_path = None
    try:
        with tempfile.NamedTemporaryFile(dir=build_directory, prefix='.release-',
                                         delete=False) as temporary:
            temporary_path = Path(temporary.name)
            command = ['git', '-C', str(tools), 'archive', '--format=tar',
                       '--prefix=tools/', reference]
            with subprocess.Popen(command, stdout=subprocess.PIPE) as process:
                try:
                    with lzma.LZMAFile(temporary, 'w') as compressed:
                        shutil.copyfileobj(process.stdout, compressed)
                finally:
                    process.stdout.close()
                status = process.wait()
                if status:
                    raise subprocess.CalledProcessError(status, command)
        os.replace(temporary_path, destination)
    finally:
        if temporary_path is not None:
            temporary_path.unlink(missing_ok=True)
    return destination


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('tools', type=Path)
    parser.add_argument('build_directory', type=Path)
    args = parser.parse_args()
    try:
        destination = create_release(args.tools, args.build_directory, os.environ.get('TAG'))
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        parser.exit(1, f'{error}\n')
    print(destination)


if __name__ == '__main__':
    main()
