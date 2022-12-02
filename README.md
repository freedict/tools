FreeDict Tools
===============

The FreeDict tools are used to import, export (build) and manage FreeDict
dictionaries.

Getting Started
---------------

FreeDict databases are encoded in the TEI XML format (chapter 9), see
<http://www.tei-c.org/release/doc/tei-p5-doc/en/html/DI.html>.

The conversion is based on XSL stylesheets (see directory `xsl/`). These can in
principle transform to any format, but only the .dict format is supported at the
moment.

More information is in the wiki at
<https://github.com/freedict/fd-dictionaries/wiki/>.

### Dependencies

You should have at least the following tools installed, to build the
dictionaries: make, xsltproc, tar, gzip, dictzip, dictfmt

For proper use of all our tools, Perl, eSpeakNG, Python > 3.4 are required, and
Git and a XML-capable editor are recommended.

#### Debian/Ubuntu Dependencies

If you use Debian/Ubuntu, you should install the following packages:

    sudo apt-get install make xsltproc libicu-dev python3 python3-icu virtualenv python3-virtualenv espeak-ng git dictfmt

#### Windows

On Windows, it is easiest to install [Msys2](https://www.msys2.org/) or
[Cygwin](https://www.cygwin.com). Both offer an easy method to install the
required tools.

### Setting Up The Build System

You should clone this repository to a path with no spaces and add an environment
variable `FREEDICT_TOOLS` to point to this directory.

A lot of the internal scripts need additional Python libraries. To fully make
use of them, you should set up a Python virtual environment for that. To help
you getting started, `make mk_venv` is there to guide you through the process
and `make mk_venv-help` will explain you why and how you should use `make mk_venv`.

Hint: It is possible to set up a environment without virtualenv. See the file
requirements.txt for more details.

Once done, you can get help on the available actions in any directory containing a
`Makefile` by typing `make help`.

### Documentation

Most of the documentation can be found in the FreeDict HOWTO at

    https://github.com/freedict/fd-dictionaries/wiki/FreeDict-HOWTO

In general, it is a good idea to have a look at our wiki at
    
    https://github.com/freedict/fd-dictionaries/wiki


Furthermore, the whole build system is explained
in chapter 8 of the HOWTO, mentioned above.

Additional Output Formats
-------------------------

For creating slob files, you need to install tei2slob:

	virtualenv env-slob -p python3 --system-site-packages  # create self contained python env
	source env-slob/bin/activate  # activate it
	pip install git+https://github.com/itkach/slob.git  # install general slob tools
	pip install git+https://github.com/itkach/tei2slob.git  # install tei2slob converter

Now tei2slob will be in your path. For new shells, you will have to execute
`source env-slob/bin/activate` again, or put env-slob/bin/tei2slob into your
PATH.

Sebastian Humenda, Mar 2018

