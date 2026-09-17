"""Version compatibility and release selection regressions."""

import unittest

from fd_tool.api.dictionary import Dictionary, Link, normalize_version, version_key
from fd_tool.api.releases import ReleaseError, get_latest_version
from fd_tool.scripts.fd_api import find_outdated_releases


class VersionTests(unittest.TestCase):
    def test_normalization(self):
        for source, expected in {
            '1': '1.0.0', '0.5': '0.5.0', '1.00.00': '1.0.0',
            '2024.10.06+fd1': '2024.10.6+fd1',
            '1.8.1-fd0.2.1': '1.8.1+fd0.2.1',
            '1.2.3-alpha.1': '1.2.3-alpha.1',
            '1.2.3+foo-bar': '1.2.3+foo-bar',
        }.items():
            with self.subTest(source=source):
                self.assertEqual(str(normalize_version(source)), expected)

    def test_ordering(self):
        versions = ['1.0-alpha.1', '1.0', '1.0+fd1', '1.0-fd2', '1.0+fd10', '1.1']
        self.assertEqual(sorted(reversed(versions), key=version_key), versions)
        self.assertEqual(version_key('1.0-fd1.0'), version_key('1.0+fd1'))
        self.assertEqual(version_key('1.0+foo-bar'), version_key('1.0'))
        self.assertLess(version_key('1.8.1-fd0.2.1'), version_key('1.8.1-fd0.10'))

    def test_invalid_versions(self):
        for value in ['', '1..0', '1.2.3.4', '1.2.3-01', '1.2.3+']:
            with self.subTest(value=value), self.assertRaises(ValueError):
                normalize_version(value)

    def test_latest_version(self):
        for versions in [['1.2.3-alpha', '1.2.3'], ['1.0+fd1', '1.0+fd2']]:
            self.assertEqual(get_latest_version(versions), versions[-1])
            self.assertEqual(get_latest_version(reversed(versions)), versions[-1])
        with self.assertRaises(ReleaseError):
            get_latest_version([])

    def test_outdated_revision(self):
        dictionary = Dictionary('eng-deu')
        dictionary['edition'] = '1.0+fd2'
        dictionary.add_download(Link('unused', None, '1.0-fd1', 'unused'))
        self.assertEqual(find_outdated_releases([dictionary]),
                         [('eng-deu', '1.0+fd2', '1.0-fd1')])
