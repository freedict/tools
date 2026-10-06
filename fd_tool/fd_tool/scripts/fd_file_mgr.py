"""This script makes remote files available for local processing. Remote files
are e.g. the released files hosted on a server as downloads or the
auto-generated dictionaries, kept outside the git repository.
This script requires a configuration. Please see the README for more details.
Running this script with the `-h` option will give an overview about its usage."""

import argparse
import os
import subprocess
import sys

from .. import config


class FileAccessError(OSError):
    """A remote-access command failed with a process exit status."""

    def __init__(self, message, returncode=1):
        super().__init__(message)
        self.returncode = returncode if returncode > 0 else 128 - returncode


def execute(command):
    """Run a remote-access command without shell interpolation."""
    result = subprocess.run(command, capture_output=True, text=True, check=False)
    if result.returncode:
        text = (result.stdout + result.stderr).strip()
        if command[0] == 'fusermount' and 'not found in /etc/mtab' in text:
            return
        raise FileAccessError(
            f'Subcommand failed: {command!r}\n{text}', result.returncode)


class UnisonFileAccess:
    """This class is one of two classes to allow access to remote files using
    unison. The drawback with unison is that before the usage by other scripts,
    all files have to be downloaded. On the other hand, this might speed up
    subsequent runs and allows offline work. On Windows, it might be desirable
    to use unison, because sshfs is not officially ported to Windows."""
    def __init__(self):
        # save make_available arguments for make_unavailable
        self.args = tuple()

    def name(self):
        return "unison"

    def make_available(self, user, server, remote_path, path):
        """Synchronize files to have them available locally."""
        self.args = (user, server, remote_path, path)
        # Use a child-specific UNISON directory instead of $HOME/.unison.
        result = subprocess.run([
            'unison', '-terse', '-auto', '-batch', '-log', '-times', '-contactquietly',
            '-ignore', 'Regex .*.swp', '-ignore', 'Regex .*.swo',
            '-ignore', 'Regex .*/build', '-ignore', 'Regex .*~',
            '-ignore', 'Regex .unison.*',
            f'ssh://{user}@{server}/{remote_path}/', path],
            env={**os.environ, 'UNISON': os.path.join(path, '.unison')}, check=False)
        if result.returncode:
            raise FileAccessError(f"Unison failed with exit code {result.returncode}",
                                  result.returncode)

    #pylint: disable=unused-argument
    def make_unavailable(self, path):
        """Synchronise in case files were created."""
        if not self.args:
            return
        self.make_available(*self.args)

class SshfsAccess:
    """This class mounts and umounts the remote files using sshfs. This will
    work on any system that fuse runs on, namely GNU/Linux, FreeBSD and Mac."""
    def name(self):
        return 'sshfs'

    def make_available(self, user, server, remote_path, path):
        """Mount remote file system using sshfs."""
        # is mounted?
        if os.path.ismount(path): # mounted, -m help says we need to return 201
            return 201 # and no action
        if len(os.listdir(path)) > 0:
            raise FileAccessError(f'{path} has to be empty before mounting', 41)
        execute(['sshfs', f'{user}@{server}:{remote_path}', path])

    def make_unavailable(self, path):
        execute(['fusermount', '-u', path])



def create_access_sessions(conf):
    """Create independent access objects for the configured remote sections."""
    sessions = []
    for section in ('release', 'generated'):
        options = conf[section]
        if options.getboolean('skip'):
            continue
        arguments = (options['user'], options['server'], options['remote_path'],
                     config.get_path(options))
        if conf['DEFAULT']['file_access_via'] == 'sshfs':
            access = SshfsAccess()
        else:
            access = UnisonFileAccess()
            # Standalone -u has no earlier process's in-memory state.
            access.args = arguments
        sessions.append((access, arguments))
    return sessions


def cleanup(sessions):
    """Clean up every session, retaining the first failure status."""
    status = 0
    for access, arguments in reversed(sessions):
        try:
            access.make_unavailable(arguments[-1])
        except OSError as error:
            print(error, file=sys.stderr)
            status = status or getattr(error, 'returncode', 1)
    return status


def acquire(sessions):
    """Acquire access, rolling back newly mounted sections on failure."""
    owned = []
    try:
        for access, arguments in sessions:
            if access.make_available(*arguments) != 201:
                owned.append((access, arguments))
    except BaseException:
        # Unison synchronization owns no mount to roll back.
        cleanup([session for session in owned if isinstance(session[0], SshfsAccess)])
        raise
    return owned


def run_with_files(conf, command):
    """Run a command with remote access and preserve its failure status."""
    owned = acquire(create_access_sessions(conf))
    status = 0
    try:
        try:
            status = subprocess.run(command, check=False).returncode
            if status < 0:
                status = 128 - status
        except OSError as error:
            print(error, file=sys.stderr)
            status = 1
    finally:
        cleanup_status = cleanup(owned)
    return status or cleanup_status


def setup():
    """Parse a single remote-access operation."""
    parser = argparse.ArgumentParser(description='FreeDict build setup utility')
    actions = parser.add_mutually_exclusive_group(required=True)
    actions.add_argument('-a', dest='print_api_path', action='store_true',
                         help='print the configured API output directory')
    actions.add_argument('-r', dest='print_release_path', action='store_true',
                         help='print the configured release directory')
    actions.add_argument('-m', dest='make_available', action='store_true',
                         help='make files available; exit 201 if all mounts already exist')
    actions.add_argument('-u', dest='umount', action='store_true',
                         help='unmount or synchronize configured remote sections')
    actions.add_argument('--run', nargs=argparse.REMAINDER, metavar='COMMAND',
                         help='run a command with remote access and cleanup')
    args = parser.parse_args()
    if args.run == []:
        parser.error('--run requires a command')
    return args


def main():
    args = setup()
    try:
        conf = config.discover_and_load()
        if args.print_api_path:
            print(config.get_path(conf['DEFAULT'], key='api_output_path'))
            status = 0
        elif args.print_release_path:
            print(config.get_path(conf['release']))
            status = 0
        elif args.run:
            status = run_with_files(conf, args.run)
        elif args.make_available:
            sessions = create_access_sessions(conf)
            owned = acquire(sessions)
            status = 201 if sessions and not owned else 0
        else:
            status = cleanup(create_access_sessions(conf))
    except config.ConfigurationError as error:
        print(error, file=sys.stderr)
        status = 42
    except OSError as error:
        print(error, file=sys.stderr)
        status = getattr(error, 'returncode', 1)
    except KeyboardInterrupt:
        status = 130
    sys.exit(status)


if __name__ == '__main__':
    main()
