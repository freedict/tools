# This script installs fd_import, it does not and should not install the importer
# scripts.
from setuptools import setup

setup(name='fd_import',
      version='0.2.0',
      description='FreeDict parser and dictionary import helpers',
      author='The FreeDict Developers',
      author_email='freedict@freelists.org',
      url='https://github.com/freedict/tools',
      license='GPL3',
      py_modules=['fd_import'],
)
