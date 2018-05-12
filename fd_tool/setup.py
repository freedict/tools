from setuptools import setup

setup(name='fd_tool',
      version='0.2.0',
      description='The FreeDict tool for file management and more',
      author='The FreeDict Developers',
      author_email='freedict@freelists.org',
      url='https://github.com/freedict/tools',
      license='GPL3',
      py_modules=['fd_tool'],
      install_requires=[],
      entry_points={'console_scripts':
          ['fd_api=fd_api:main',
           'fd_file_mgr=fd_file_mgr:main',
           'fd_changelog=fd_changelog:main',
           'rm_duplicates=rm_duplicates:main']
    }
)
