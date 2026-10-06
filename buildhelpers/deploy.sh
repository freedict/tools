#!/bin/sh
# Invoked by Make through fd_file_mgr --run, after remote access is available.
set -eu

dictname=$1
version=$2
force=$3
shift 3

release_root=$(fd_file_mgr -r)
if [ -z "$release_root" ] || [ ! -d "$release_root" ]; then
    echo "Invalid release output directory: $release_root" >&2
    exit 1
fi
destination="$release_root/$dictname/$version"
# Refuse overwrites before copying any of the requested formats.
for archive in "$@"; do
    if [ -e "$destination/${archive##*/}" ] && [ "$force" != y ]; then
        echo "Release already exists: ${archive##*/}; use FORCE=y to replace it." >&2
        exit 2
    fi
    test -f "$archive"
    test -f "$archive.sha512"
done
mkdir -p "$destination"
staging=$(mktemp -d "$destination/.deploy.XXXXXX")
trap 'rm -rf -- "$staging"' 0
for archive in "$@"; do
    echo "Deploying ${archive##*/}"
    cp -- "$archive" "$staging/"
    cp -- "$archive.sha512" "$staging/"
done
chmod a+r -- "$staging/"*
mv -- "$staging/"* "$destination/"
