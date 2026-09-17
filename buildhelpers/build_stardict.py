"""Build a complete StarDict file set before publishing its completion marker."""

import argparse
import gzip
import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess
import tempfile


def fingerprint(path):
    stat = path.stat()
    return [stat.st_size, stat.st_mtime_ns]


def build(source, destination, name, converter):
    source = source.resolve()
    marker = destination / '.complete'
    inputs = [str(source), hashlib.sha256(source.read_bytes()).hexdigest(), converter]
    try:
        previous = json.loads(marker.read_text())
        if previous['inputs'] == inputs and all(
                fingerprint(destination / filename) == stat
                for filename, stat in previous['outputs'].items()):
            return
    except (OSError, ValueError, KeyError):
        pass

    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='.stardict-', dir=destination.parent) as tmp:
        staging = Path(tmp)
        output = staging / f'{name}.ifo'
        command = shlex.split(converter) + [
            '--ui=cmd', '--read-format=FreeDict', '--write-format=Stardict',
            '--write-options=dictzip=false;dictzip_syn=false', str(source), str(output)]
        result = subprocess.run(command, capture_output=True, text=True)
        log = result.stdout + result.stderr
        if result.returncode:
            raise RuntimeError(f'PyGlossary failed ({result.returncode}):\n{log}')
        required = [output, staging / f'{name}.idx', staging / f'{name}.dict']
        info = dict(line.split('=', 1) for line in output.read_text().splitlines() if '=' in line)
        if int(info.get('synwordcount', '0')):
            required.append(staging / f'{name}.syn')
        for path in required:
            if not path.is_file():
                raise RuntimeError(f'PyGlossary did not produce {path.name}')
        index = staging / f'{name}.idx'
        # A fixed timestamp makes the compressed index reproducible.
        index.with_suffix('.idx.gz').write_bytes(gzip.compress(index.read_bytes(), mtime=0))
        index.unlink()
        (staging / 'pyglossary-stardict.out').write_text(log)
        outputs = [path for path in staging.iterdir() if path.is_file()]
        destination.mkdir(exist_ok=True)
        # Failed publication must not leave a marker claiming the set is complete.
        marker.unlink(missing_ok=True)
        for suffix in ['syn', 'syn.dz', 'dict.dz']:
            (destination / f'{name}.{suffix}').unlink(missing_ok=True)
        for path in outputs:
            os.replace(path, destination / path.name)
        state = {'inputs': inputs, 'outputs': {
            path.name: fingerprint(destination / path.name) for path in outputs}}
        temporary_marker = staging / '.complete'
        temporary_marker.write_text(json.dumps(state))
        os.replace(temporary_marker, marker)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    parser.add_argument('destination', type=Path)
    parser.add_argument('name')
    parser.add_argument('--pyglossary', default='pyglossary')
    args = parser.parse_args()
    try:
        build(args.source, args.destination, args.name, args.pyglossary)
    except (OSError, ValueError, RuntimeError) as error:
        parser.exit(1, f'{error}\n')


if __name__ == '__main__':
    main()
