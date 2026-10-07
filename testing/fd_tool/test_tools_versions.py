"""Tools release tags use semantic precedence and the selected tag's date."""

import io
import os
import unittest
from unittest.mock import patch

from fd_tool.api import releases


class ToolsVersionTests(unittest.TestCase):
    def test_semantic_tools_tag_order(self):
        for tags, expected in [
            (['0.9.0', '0.10.0', 'unrelated'], '0.10.0'),
            (['0.8.0-rc.2', '0.8.0', '0.8.0-rc.10'], '0.8.0'),
            (['0.8.0-rc.2', '0.8.0-rc.10'], '0.8.0-rc.10'),
            (['0.8.0', '0.9.0-alpha.1'], '0.9.0-alpha.1'),
        ]:
            with self.subTest(tags=tags):
                self.assertEqual(releases.latest_tools_tag(tags), expected)
                self.assertEqual(releases.latest_tools_tag(reversed(tags)), expected)

    def test_no_semantic_tags_has_clear_error(self):
        with self.assertRaises(releases.ReleaseError):
            releases.latest_tools_tag(['unrelated', 'vague-version'])

    def test_api_selects_semantic_tag_and_matching_commit(self):
        tags = [{'name': tag, 'commit': {'url': 'https://api.github.com/commits/' + tag}}
                for tag in ['0.9.0', '0.10.0', 'unrelated']]
        metadata = {'commit': {'committer': {'date': '2026-01-02T12:00:00Z'}}}
        with patch.object(releases, 'github_request', side_effect=[tags, metadata]) as request, \
                patch.object(releases.urllib.request, 'urlopen',
                             return_value=io.BytesIO(b'archive')):
            result = releases.get_latest_tools_release()
        self.assertEqual(result['version'], '0.10.0')
        self.assertEqual(result['date'], '2026-01-02')
        self.assertEqual(request.call_args.args, ('commits/0.10.0',))
        self.assertTrue(result['URL'].endswith('/0.10.0.tar.gz'))

    def test_local_metadata_uses_tag_commit_not_head(self):
        with patch.dict(os.environ, {'FREEDICT_TOOLS': '/unused'}), \
                patch.object(releases.shutil, 'which', return_value='/usr/bin/git'), \
                patch.object(releases, 'git',
                             side_effect=['0.9.0\n0.10.0\nunrelated', '2026-01-02']) as git:
            version, date, url = releases.get_tools_release()
        self.assertEqual((version, date), ('0.10.0', '2026-01-02'))
        self.assertEqual(git.call_args.args[0],
                         ['show', '-s', '--format=%cs', 'refs/tags/0.10.0^{commit}'])
        self.assertTrue(url.endswith('/freedict-tools-0.10.0.tar.xz'))
