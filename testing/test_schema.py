"""Keep schema and runtime version compatibility aligned."""

from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

from fd_tool.api.dictionary import normalize_version

try:
    from lxml import etree
except ImportError:
    etree = None

REPO = Path(__file__).resolve().parents[1]


def api_xml(version):
    return f'''<FreeDictDatabase>
      <dictionary name="eng-deu" headwords="1" edition="{version}"
          date="2026-01-01" maintainerName="Test">
        <release platform="stardict" version="{version}" size="1"
          date="2026-01-01" URL="https://example.org/dict" checksum="test"/>
      </dictionary>
      <software><tools date="2026-01-01" URL="https://example.org/tools"
          checksum="test" version="0.7.0"/></software>
    </FreeDictDatabase>'''


@unittest.skipIf(etree is None, 'lxml is required for schema tests')
class SchemaTests(unittest.TestCase):
    def test_runtime_parity(self):
        schema = etree.RelaxNG(etree.parse(str(REPO / 'freedict-database.rng')))
        cases = ['1', '0.1', '1.00.00', '1.9-fd1', '1.8.1-fd0.2.1',
                 '1.0-fd01.02', '2024.10.06+fd1', '1.2.3-alpha.1',
                 '1.0+foo-bar', '1.0-alpha+build.1', '', '1..0', '1.2.3.4',
                 '1.0-01', '1.0+', '1.0+foo..bar', ' 1.0']
        for version in cases:
            with self.subTest(version=version):
                try:
                    normalize_version(version)
                    expected = True
                except ValueError:
                    expected = False
                self.assertEqual(schema.validate(etree.fromstring(api_xml(version))), expected)

    def test_installed_validators(self):
        with tempfile.TemporaryDirectory() as tmp:
            xml = Path(tmp) / 'api.xml'
            for tool, flags in [('xmllint', ['--noout', '--relaxng']), ('jing', [])]:
                if not shutil.which(tool):
                    continue
                for version, valid in [('1.8.1-fd0.2.1', True), ('1.0-01', False)]:
                    with self.subTest(tool=tool, version=version):
                        xml.write_text(api_xml(version))
                        result = subprocess.run(
                            [tool, *flags, str(REPO / 'freedict-database.rng'), str(xml)],
                            capture_output=True, text=True)
                        self.assertEqual(result.returncode == 0, valid, result.stderr)
