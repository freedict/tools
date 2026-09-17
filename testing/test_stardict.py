"""Make-level tests use a controlled converter; real conversion is opt-in."""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

REPO = Path(__file__).resolve().parents[1]


class StarDictTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.dictionary = self.root / 'eng-deu'
        self.dictionary.mkdir()
        shutil.copy2(REPO / 'testing/data/eng-deu.tei', self.dictionary)
        (self.dictionary / 'Makefile').write_text(
            f'FREEDICT_TOOLS := {REPO}\nsupported_phonetics_lang := 2\n'
            'include $(FREEDICT_TOOLS)/mk/dicts.mk\n')
        self.converter = self.root / 'converter'
        self.converter.write_text(f'''#!{sys.executable}
import os, sys, time
from pathlib import Path
with open(os.environ['CONVERSION_LOG'], 'a') as log:
    log.write('conversion\\n')
time.sleep(0.05)
output = Path(sys.argv[-1])
output.write_text("StarDict's dict ifo file\\nversion=3.0.0\\nwordcount=1\\nsynwordcount=1\\n")
output.with_suffix('.idx').write_bytes(b'house\\0' + bytes(8))
output.with_suffix('.dict').write_bytes(b'Haus')
output.with_suffix('.syn').write_bytes(b'home\\0' + bytes(4))
sys.exit(int(os.environ.get('CONVERSION_STATUS', '0')))
''')
        self.converter.chmod(0o755)
        self.output = self.dictionary / 'build/stardict'

    def make(self, *args, **env):
        return subprocess.run(
            ['make', '--no-print-directory', f'PYGLOSSARY={self.converter}',
             f'FREEDICTRC={self.root}/absent', *args], cwd=self.dictionary,
            env=dict(os.environ, CONVERSION_LOG=str(self.root / 'calls'), **env),
            capture_output=True, text=True)

    def successful(self, *args):
        result = self.make(*args)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_parallel_and_repeated_build(self):
        self.successful('-j4', 'build-stardict')
        self.successful('build-stardict')
        self.assertEqual((self.root / 'calls').read_text().splitlines(), ['conversion'])

    def test_missing_optional_output_rebuilds(self):
        self.successful('build-stardict')
        (self.output / 'eng-deu.syn').unlink()
        self.successful('-j4', 'build-stardict')
        self.assertTrue((self.output / 'eng-deu.syn').exists())
        self.assertEqual(len((self.root / 'calls').read_text().splitlines()), 2)

    def test_failure_never_publishes_partial_files(self):
        result = self.make('build-stardict', CONVERSION_STATUS='7')
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.output / '.complete').exists())
        self.assertFalse((self.output / 'eng-deu.ifo').exists())

    def test_configured_environment(self):
        venv = self.root / 'venv'
        (venv / 'bin').mkdir(parents=True)
        shutil.copy2(self.converter, venv / 'bin/pyglossary')
        (venv / 'bin/activate').write_text(f'export PATH="{venv}/bin:$PATH"\n')
        config = self.root / 'freedictrc'
        config.write_text(f'[DEFAULT]\nvirtual_env = {venv}\n')
        self.successful('build-stardict', 'PYGLOSSARY=pyglossary', f'FREEDICTRC={config}')
