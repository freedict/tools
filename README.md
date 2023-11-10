FreeDict Tools
===============

The FreeDict tools are used to import, export (build) and manage FreeDict
dictionaries.

As a dictionary data provider, our goal is to use our TEI dialect to be able to
import and export into a variety of output formats. This repository collects the
tooling for this.

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

FreeDict requires a few essential tools to generate phonemes and compile the XML
sources. However, most of the tools are determined by the output format.

0.  Basic tooling:
    perl, python, make, xsltproc, tar, gzip, espeak-ng
1.  For dictd:
    dictzip, dictfmt
2.  For stardict:
    pyglossary
3.  For slob:
    slob, tei2slob

#### Debian/Ubuntu Dependencies

If you use Debian/Ubuntu, you should install the following packages:

0.  Basic: `sudo apt install make xsltproc libicu-dev python3 python3-icu virtualenv python3-virtualenv espeak-ng git`
1.  For dictd: `sudo apt install dictzip dictfmt`
2.  For stardict: install pyglossary, for instance, `pipx install pyglossary`
3.  For slob:
    1.  Get the sources:
        <https://github.com/itkach/slobn>
        <https://github.com/itkach/tei2slob/>
    2.  Use the official installation instructions OR
    3.  Use a virtualenv; we recommend that to bundle all our tooling
    4.  fetch the mentioned sources and use `pipx install .`, which is easiest.

#### Windows

On Windows, it is easiest to install [Msys2](https://www.msys2.org/) or
[Cygwin](https://www.cygwin.com). Both offer an easy method to install the
required tools. Note that we are not officially using Windows, so your mileage
will vary.

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


For creating StarDict files, you need to install [PyGlossary](https://github.com/ilius/pyglossary):

	pip install git+https://github.com/ilius/pyglossary.git



Sebastian Humenda, Mar 2018

