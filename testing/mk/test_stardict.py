"""Exercise StarDict Make rules with a controlled converter."""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import tarfile
import unittest

REPO = Path(__file__).resolve().parents[2]


class StarDictTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.dictionary = self.root / 'eng-deu'
        self.dictionary.mkdir()
        shutil.copy2(REPO / 'testing/mk/data/eng-deu.tei', self.dictionary)
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
missing = os.environ.get('OMIT_OUTPUT')
if missing:
    output.with_suffix('.' + missing).unlink()
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
        self.successful('-j4', 'build-stardict', 'build/stardict/eng-deu.ifo',
                        'build/stardict/eng-deu.idx.gz', 'build/stardict/eng-deu.dict')
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
        self.assertFalse((self.output / 'build.stamp').exists())
        self.assertFalse(list(self.output.glob('eng-deu.*')))

    def test_missing_required_output_rebuilds(self):
        self.successful('build-stardict')
        for suffix in ['ifo', 'idx.gz', 'dict']:
            with self.subTest(suffix=suffix):
                path = self.output / f'eng-deu.{suffix}'
                path.unlink()
                self.successful('-j4', 'build-stardict')
                self.assertTrue(path.is_file())
        self.assertEqual(len((self.root / 'calls').read_text().splitlines()), 4)

    def test_changed_source_rebuilds(self):
        self.successful('build-stardict')
        with (self.dictionary / 'eng-deu.tei').open('a') as source:
            source.write('\n')
        source = self.dictionary / 'eng-deu.tei'
        marker = self.output / 'build.stamp'
        os.utime(source, ns=(source.stat().st_atime_ns, marker.stat().st_mtime_ns + 1))
        self.successful('-j4', 'build-stardict')
        self.assertEqual(len((self.root / 'calls').read_text().splitlines()), 2)

    def test_incomplete_conversion_is_not_published(self):
        for suffix in ['ifo', 'idx', 'dict', 'syn']:
            with self.subTest(suffix=suffix):
                result = self.make('build-stardict', OMIT_OUTPUT=suffix)
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse((self.output / 'build.stamp').exists())
                self.assertFalse((self.output / 'eng-deu.ifo').exists())
        self.successful('build-stardict')

    def test_failed_rebuild_preserves_previous_outputs(self):
        self.successful('build-stardict')
        before = {path.name: path.read_bytes() for path in self.output.iterdir()
                  if path.is_file()}
        with (self.dictionary / 'eng-deu.tei').open('a') as source:
            source.write('\n')
        source = self.dictionary / 'eng-deu.tei'
        marker = self.output / 'build.stamp'
        os.utime(source, ns=(source.stat().st_atime_ns, marker.stat().st_mtime_ns + 1))
        result = self.make('build-stardict', CONVERSION_STATUS='7')
        self.assertNotEqual(result.returncode, 0)
        self.assertTrue((self.output / 'tmp/eng-deu.ifo').is_file())
        after = {path.name: path.read_bytes() for path in self.output.iterdir()
                 if path.is_file()}
        self.assertEqual(before, after)
        self.successful('build-stardict')
        self.assertEqual(len((self.root / 'calls').read_text().splitlines()), 3)

    def test_configured_environment(self):
        venv = self.root / 'venv'
        (venv / 'bin').mkdir(parents=True)
        shutil.copy2(self.converter, venv / 'bin/pyglossary')
        (venv / 'bin/activate').write_text(f'export PATH="{venv}/bin:$PATH"\n')
        config = self.root / 'freedictrc'
        config.write_text(f'[DEFAULT]\nvirtual_env = {venv}\n')
        self.successful('build-stardict', 'PYGLOSSARY=pyglossary', f'FREEDICTRC={config}')

    def test_archive_and_install_include_synonyms(self):
        self.successful('-j4', 'release-stardict')
        archive = next((self.dictionary / 'build/release').glob('*.tar.xz'))
        with tarfile.open(archive) as tar:
            self.assertIn('eng-deu/eng-deu.syn', tar.getnames())
        timestamp = archive.stat().st_mtime_ns
        self.successful('release-stardict')
        self.assertEqual(archive.stat().st_mtime_ns, timestamp)
        stage = self.root / 'stage'
        self.successful('install-stardict', f'DESTDIR={stage}')
        self.assertTrue((stage / 'usr/local/share/stardict/dic/eng-deu.syn').exists())

    def test_stale_synonyms_are_removed(self):
        stage = self.root / 'stage'
        self.successful('install-stardict', f'DESTDIR={stage}')
        script = self.converter.read_text().replace('synwordcount=1', 'synwordcount=0')
        script = '\n'.join(line for line in script.splitlines()
                           if "with_suffix('.syn')" not in line) + '\n'
        self.converter.write_text(script)
        with (self.dictionary / 'eng-deu.tei').open('a') as source:
            source.write('\n')
        self.successful('install-stardict', f'DESTDIR={stage}')
        self.assertFalse((self.output / 'eng-deu.syn').exists())
        self.assertFalse((stage / 'usr/local/share/stardict/dic/eng-deu.syn').exists())

    def test_install_and_uninstall_all_formats(self):
        for relative in ['build/dictd/eng-deu.c5', 'build/dictd/eng-deu.dict',
                         'build/dictd/eng-deu.dict.dz', 'build/dictd/eng-deu.index',
                         'build/slob/eng-deu-1.0.0.slob']:
            path = self.dictionary / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(b'prebuilt fixture')
        stage = self.root / 'stage with spaces'
        restart = self.root / 'restart'
        restart.write_text(f'#!/bin/sh\ntouch "{self.root}/restarted"\n')
        restart.chmod(0o755)
        overrides = [f'DESTDIR={stage}', 'PREFIX=/opt/freedict',
                     f'DICTD_RESTART_SCRIPT={restart}',
                     '--old-file=build/dictd/eng-deu.dict.dz',
                     '--old-file=build/dictd/eng-deu.index']
        self.successful('install-restart', *overrides)
        expected = ['dictd/eng-deu.dict.dz', 'dictd/eng-deu.index',
                    'stardict/dic/eng-deu.ifo', 'stardict/dic/eng-deu.idx.gz',
                    'stardict/dic/eng-deu.dict', 'stardict/dic/eng-deu.syn', 'slob/eng-deu.slob']
        for name in expected:
            self.assertTrue((stage / 'opt/freedict/share' / name).is_file(), name)
        self.successful('uninstall', *overrides)
        self.assertFalse(any(path.is_file() for path in stage.rglob('*')))
        self.assertFalse((self.root / 'restarted').exists())


    def test_unsupported_stardict_is_rejected_before_conversion(self):
        for target in ['build-stardict', 'release-stardict', 'install-stardict', 'deploy-stardict']:
            with self.subTest(target=target):
                result = self.make('-j4', target, 'UNSUPPORTED_PLATFORMS=stardict')
                self.assertNotEqual(result.returncode, 0)
                self.assertIn('does not support', result.stdout)
                self.assertFalse((self.root / 'calls').exists())
        self.successful('build-stardict')
        result = self.make('release-stardict', 'UNSUPPORTED_PLATFORMS=stardict')
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len((self.root / 'calls').read_text().splitlines()), 1)

    def test_aggregate_operations_skip_unsupported_formats(self):
        for target in ['build', 'release', 'deploy', 'install']:
            with self.subTest(target=target):
                result = self.make('-n', target, 'UNSUPPORTED_PLATFORMS=dictd slob stardict',
                                   'DISTFILES=eng-deu.tei')
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertNotIn('--write-format=Stardict', result.stdout)
                self.assertNotIn('install-stardict', result.stdout)
        result = self.make('list-platforms', 'UNSUPPORTED_PLATFORMS=stardict')
        self.assertEqual(result.stdout.strip(), 'src dictd slob')
        result = self.make('query-star')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('Unknown platform', result.stdout)

    def test_archive_layout_with_custom_build_directory(self):
        (self.dictionary / 'README').write_text('fixture documentation')
        for build in ['output', str(self.root / 'absolute-output')]:
            with self.subTest(build=build):
                self.successful('-j4', 'release-stardict', f'BUILD_DIR={build}')
                directory = self.dictionary / build
                archive = next((directory / 'release').glob('*.tar.xz'))
                with tarfile.open(archive) as tar:
                    self.assertEqual(set(tar.getnames()),
                                     {'eng-deu/eng-deu.' + suffix for suffix in ['ifo', 'idx.gz', 'dict', 'syn']}
                                     | {'eng-deu/README'})
                    self.assertEqual(tar.extractfile('eng-deu/README').read(), b'fixture documentation')
                self.successful('install-stardict', f'BUILD_DIR={build}', f'DESTDIR={self.root}/stage')
                self.assertTrue((self.root / 'stage/usr/local/share/stardict/dic/eng-deu.syn').exists())

    def test_uninstall_removes_formats_even_when_marked_unsupported(self):
        stage = self.root / 'stage'
        self.successful('install-stardict', f'DESTDIR={stage}')
        self.successful('uninstall', 'UNSUPPORTED_PLATFORMS=stardict', f'DESTDIR={stage}')
        self.assertFalse(any(path.is_file() for path in stage.rglob('*')))
