# This makefile contains all recipes to build and manage the FreeDict
# tools, including the API. It also provides targets to build a release archive
# with the latest tools included.

FREEDICT_TOOLS ?= .
ifeq ($(wildcard $(FREEDICT_TOOLS)/mk/config.mk),)
FREEDICT_TOOLS := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
endif
include $(FREEDICT_TOOLS)/mk/config.mk

XMLLINT ?= xmllint
JING ?= jing

VERSION = 0.4
PREFIX ?= usr
DESTDIR ?= 
INSTALLDIR ?= $(abspath $(DESTDIR)/$(PREFIX)/share/freedict)

dirs = api JMdict lib mk xquery xsl/inc
TARGET_INSTALL_DIRS = $(addprefix $(INSTALLDIR)/tools/, $(dirs))


api: #! generate the api with information about all dictionaries and their downloads at the configured api path
api:
	$(call run_with_files,fd_api)
	@$(MAKE) -C $(FREEDICT_TOOLS) --no-print-directory api-validation

# allow retrieval of API path from Makefile and from rule below
get_api_path=$(call exc_pyscript,fd_file_mgr,-a)
api-path: #! print the output directory to the generated API file (read from configuration) (trailing newline is removed)
	@$(call get_api_path)

api-validation: #! validate the freedict-database.xml against its RNG schema (set USE_JING=1 to use jing instead of xmllint)
	@api_dir="$$( $(call get_api_path) )" || exit $$?; \
	test -n "$$api_dir" || { echo "Empty API output path" >&2; exit 1; }; \
	if [ "$(USE_JING)" = "1" ]; then \
		$(JING) "$(FREEDICT_TOOLS)/freedict-database.rng" "$$api_dir/freedict-database.xml"; \
	else \
		$(XMLLINT) --noout --relaxng "$(FREEDICT_TOOLS)/freedict-database.rng" "$$api_dir/freedict-database.xml"; \
	fi




mount: #! mount or synchronize FreeDict releases / generated dictionaries
	$(call exc_pyscript,fd_file_mgr,-m)

need-update: #! queries for unreleased dictionaries or for those with newer source changes
	@$(call run_with_files,fd_api,-n)

umount: #! runs umount / clean up actions for unmounting remote volumes (if SSH is used)
	@$(call exc_pyscript,fd_file_mgr,-u)

$(BUILD_DIR)/freedict-tools-$(VERSION).tar.bz2: Makefile* *.pl
ifeq ($(wildcard $(BUILD_DIR)),)
	mkdir -p $(BUILD_DIR)
endif
	tar --totals --exclude="*/.svn/*" --exclude="*/.*" \
	  --exclude="*/charlint*" --exclude="*/UnicodeData.txt" \
	  --exclude="*/ergane/jet4/*" \
	  --exclude="*/ergane/unzip/*" \
	  --exclude="*/ergane/zip/*" \
	  --exclude="*/__pycache__/*" \
	  -cvjf $@ ../tools

install-deps: #! probe current operating system to install build prerequisites for dictionary development
	echo -n "Do you want to use unison or sshfs? Enter one of them or nothing: "; \
	read ACCESS_METHOD; \
	if command -v apt-get; then \
		sudo apt-get install unzip tar xsltproc libxml-libxml-perl python3 $$ACCESS_METHOD; \
	elif command -v pacman;  then \
		if [ `uname -o` = 'GNU/Linux' ]; then \
			sudo pacman -S unzip tar libxslt libxml-perl python $$ACCESS_METHOD; \
		else \
			echo "Sorry, but it seems you're not running pacman on GNU/Linux. There's currently no installation rule for this platform."; \
		fi; \
	else \
		echo "Unknown platform. Feel free to report the corresponding packages to us."; \
	fi


# This helps to express \n as a special variable to Make; it **needs** two
# newlines within the define
define newline


endef

#  This may not contain apostrophes
define VENV_HELP
A virtual environment is a local copy of all Python utilities, scripts and
libraries for Python development. With a virtual environment, you do not need to
install all the FreeDict scripts globally in your system, but you are of course
free to do so. The mk_venv command will make sure that the virtual environment
is created at the correct place; use "P=/some/path" to specify the path.
Example:
    make mk_venv P=../fd-venv
Python 3.12 or newer is required. The built-in venv module creates the environment.
PyICU needs ICU headers, pkg-config, a C++ compiler, and Python development headers.
On Debian/Ubuntu install libicu-dev pkg-config build-essential python3-dev python3-venv.
After installation, you will be asked whether the virtual environment should be
added to the FreeDict configuration. This is generally a good idea, because this
means that the FreeDict build system will take care of all the required steps.
endef

mk_venv-help:
	echo -e '$(subst $(newline),\n,${VENV_HELP})'

mk_venv: #! initialise a new (Python) virtual environment; use mk_venv-help for detailled help
	@if [ "${P}" = '' ]; then \
		echo Need to give a path with P=, e.g. "make mk_venv P=/some/dir"; \
		exit 222; fi
	@"$(PYTHON)" "$(FREEDICT_TOOLS)/buildhelpers/create_venv.py" "${P}"
	@if [ -n "$(FREEDICTRC)" ]; then \
		"$(PYTHON)" "$(FREEDICT_TOOLS)/buildhelpers/register_venv.py" \
			"$(FREEDICTRC)" "${P}" --choice "$${REGISTER_VENV:-auto}"; \
	else \
		echo "No FreeDict configuration found; set FREEDICTRC to register this environment."; \
	fi

# NOTE: the directories below HAVE to be on one line
INSTALL_DIRS := api api/generator api/file_manager api/generator/apigen mk xquery xsl/inc
INSTALL_PATHS := $(wildcard api/*.py api/generator/*.py api/generator/apigen/*.py api/file_manager/*.py \
	freedict-database.rng \
	mk/*.mk xsl/inc/* \
	xquery/* xsl/tei2c5.xsl)
install: #! install the tools to $$DESTDIR/$PREFIX/share/freedict (default /usr/local/share/freedict)
install:
	@echo Creating directories in $(INSTALLDIR)…
	@set -e; for d in $(INSTALL_DIRS); do \
		install -d $(INSTALLDIR)/$$d; \
	done
	@echo Copying files to $(INSTALLDIR)…
	@set -e; for f in $(INSTALL_PATHS); do \
		if [ -f $$f ]; then \
			install -m 644 $$f $(INSTALLDIR)/$$f; \
		fi \
	done




release: #! build a release tarball in $$BUILD_DIR, ../build by default
release: $(BUILD_DIR)/freedict-tools-$(VERSION).tar.bz2

release-path: #! print the output directory to which releases are deployed (read from configuration); trailing newline is removed
	@$(call exc_pyscript,fd_file_mgr,-r) | tr -d '\n'

.PHONY: release install api all mount umount api-validation
