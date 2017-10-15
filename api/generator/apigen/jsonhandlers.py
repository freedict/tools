"""JSON output writer."""

import json

def write_freedict_database(path, dicts):
    """Write a freedict database to ''path`` in JSON format."""
    serialized = []
    for dictionary in dicts:
        if not dictionary.is_complete():
            raise ValueError("Not all mandatory keys set.")
        essence = {k: v  for k, v in dictionary.get_attributes().items()
                if not v is None}
        essence['name'] = dictionary.get_name()
        # set maintainer if none present
        if 'maintainerName' not in dictionary or not dictionary['maintainerName']:
            essence['maintainerName'] = 'FreeDict - no maintainer assigned'
        essence['releases'] = []
        for release in dictionary.get_downloads():
            essence['releases'].append({'platform': str(release.format),
                'size': str(release.size),
                'date': release.last_modification_date,
                'URL': str(release), 'version': str(release.version)
            })
        serialized.append(essence)
    with open(path, 'w', encoding='utf-8') as fhandle:
        json.dump(serialized, fhandle, indent=2, sort_keys=True)

