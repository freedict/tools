"""This file offers common configuration parsing facilities required for the
Freedict build system."""

import configparser
import os
import re

class ConfigurationError(Exception):
    def __init__(self, msg, path=None):
        super().__init__()
        self.path = path
        self.msg = msg
    def __repr__(self):
        if self.path:
            return 'error in configuration "%s": %s' % (self.path, self.msg)
        else:
            return self.msg

    def __str__(self):
        return repr(self)



def load_configuration(conffile):
    """Load given `config` from given path. Default values are provided and
    missing mandatory options will raise a ConfigurationError."""
    config = configparser.ConfigParser()
    config['DEFAULT'] = {
            'file_access_via': 'sshfs', # unison or sshfs possible
            'api_output_path': '.'}
    config['crafted'] = {}
    config['crafted']['local_path'] = ''
    config['generated'] = {}
    config['generated']['server'] = 'freedict.org'
    config['generated']['remote_path'] = '/var/www/download/generated'
    config['generated']['local_path'] = ''
    config['generated']['user'] = 'anonymous'
    config['generated']['skip'] = 'no'
    config['release'] = {}
    config['release']['server'] = 'freedict.org'
    config['release']['remote_path'] = '/var/www/download/dictionaries'
    config['release']['local_path'] = ''
    config['release']['user'] = 'anonymous'
    config['release']['skip'] = 'no'

    # overwrite defaults with user settings
    with open(conffile) as configfile:
        config.read_file(configfile)

    if config['DEFAULT']['file_access_via'] not in ['sshfs', 'unison']:
        raise ConfigurationError(('section=DEFAULT, file_access_via="%s": '
            'invalid value, possible values are sshfs and unison') \
                % config['DEFAULT']['file_access_via'], conffile)
    api_path = config['DEFAULT']['api_output_path']
    if api_path and os.path.exists(api_path) and os.path.isfile(api_path):
        raise ConfigurationError("expected directory, found file", path=api_path)

    for section in ('generated', 'crafted', 'release'):
        if not config[section]['local_path']:
            raise ConfigurationError("error, local_path not set for section [%s]" \
                    % section, conffile)
        path = get_path(config[section])
        if not os.path.exists(path):
            raise ConfigurationError("path \"%s\" configured in section [%s] doesn't exist" \
                    % (path, section), conffile)

    return config

def discover_and_load():
    """This file attempts to discover and load a FreeDict configuration file at
    the usual places. If no configuration was found, a ConfigurationError is
    raised."""
    paths = [os.path.join(os.path.expanduser("~"), '.config/freedict/freedictrc')]
    if os.environ.get('LOCALAPPDATA'):
        paths.append(os.path.join(os.environ['LOCALAPPDATA'], 'freedict/freedict.ini'))
    conffile = [path for path in paths if os.path.exists(path)]
    if not conffile:
        phrase = ('one of the following directories' if len(paths) > 1 else 'the following directory')
        raise ConfigurationError(("no configuration found. Please initialize "
            "one in " + phrase + ' ' + ', '.join(paths)))
    return load_configuration(conffile[0])


def get_path(section, key='local_path'):
    """Return a local_path from a section with $HOME, ~/ or %HOME% replaced."""
    home = os.path.expanduser('~')
    path = section[key]
    for pattern in (r'\$HOME', '~', '%HOME%'):
        path = re.sub('^' + pattern, home, path)
    return path


