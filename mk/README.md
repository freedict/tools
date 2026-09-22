# Build System

Explanation
-----------

This directory contains make include files:

-   `config.mk`: This file contains common variables, used within all other make
    files. It also defines the help system, invoked with `make help`.
-   `dicts.mk`: This file is included by every dictionary Makefile and defines
    all the targets to work with a dictionary, e.g. building a release tar ball,
    validating its contents or exporting to other formats.
-   `dictroot.mk`: Only the top-level makefile of a directory with multiple
    dictionaries should include this file (e.g. the root of the dictionary git
    repository). It defines rules to build all dictionaries at once and to install
    and uninstall them.

More information about these files can be found in chapter 8 of the Freedict
HOWTO.

Platforms
---------

To add a new platform or delete an existing one, the following parts have to be
touched:

-   The variable `available_platforms` lists all defined platforms, so adding or
    removing a a name from there will automatically remove the dictionary from
    the build and the release target (and from a few more).
-   Each platform needs to provide a build-PLATFORM and a release-PLATFORM rule,
    where PLATFORM is replaced by the name present in `available_platforms`.\
    Ideally, the release-PLATFORM target depends on the distribution archive, so
    that the build system can infer whether a release has been build. It's best
    to use `$(BUILD_DIR)/PLATFORM/freedict-lg1-lg2-VERSION-PLATFORM.extension``.
    lg1-lg2 is the identifier for the dictionary and the VERSION is available in
    the variable with the same name.\
    Note: an exception is the dictd format, which builds to without the
    -PLATFORM suffix for historical reasons.


## Installing dictionaries

`make install` installs dictd, StarDict, and SLOB outputs; use
`make install-stardict` (or `install-dictd` / `install-slob`) for one format.
`make uninstall` removes all three formats; matching `uninstall-PLATFORM`
targets are available. The default prefix is `/usr/local`.

Use `make install DESTDIR=/tmp/package PREFIX=/usr` to stage a package.
`DICTD_INSTDIR`, `STARDICT_INSTDIR`, and `SLOB_INSTDIR` override destinations
beneath `DESTDIR`; their defaults are `$(PREFIX)/share/dictd`,
`$(PREFIX)/share/stardict/dic`, and `$(PREFIX)/share/slob`.
SLOB is installed as `xxx-yyy.slob` so upgrades replace the previous version.
Only `install-restart` restarts dictd, and never when `DESTDIR` is set.
