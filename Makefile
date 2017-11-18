# This makefile contains all recipes to build and manage the FreeDict
# tools, including the API. It also provides targets to build a release archive
# with the latest tools included.

FREEDICT_TOOLS ?= .
include $(FREEDICT_TOOLS)/mk/config.mk

VERSION = 0.4
PREFIX ?= usr
DESTDIR ?= 
INSTALLDIR ?= $(abspath $(DESTDIR)/$(PREFIX)/share/freedict)

dirs = api JMdict lib mk xquery xsl/inc
TARGET_DIRS = $(addprefix $(INSTALLDIR)/tools/, $(dirs))


api: #! generate the api with information about all dictionaries and their downloads at the configured api path
api:
	# -p: mount / synchronize released and generated files; -o: umount them
	#  again
	$(PYTHON) $(FREEDICT_TOOLS)/api/generator/generator.py \
		-p "$(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -m" \
		-o "$(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -u" \
		|| sleep 1; $(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -u
# the last line above makes sure that sshfs volumes are umounted, if something
# goes wrong

mount: #! mount or synchronize FreeDict releases / generated dictionaries
	@$(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -m

# provide a clean up rule which can be run, if sshfs was not umounted cleanly
umount: #! runs umount / clean up actions if make api failed and cannot be executed in a subsequent run
	@$(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -u

api-path: #! print the output directory to the generated API file (read from configuration) (trailing newline is removed)
	@$(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -a | tr -d '\n'

api-validation: #! validate the freedict-database.xml against its RNG schema
	xmllint --noout --relaxng freedict-database.rng $(shell $(MAKE) api-path)/freedict-database.xml



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

# install a file using its relative path below $(FREEDICT_TOOLS)
define install_relative 
endef

install-deps: #! probe current operating system to install build prerequisites for dictionary development
	echo -n "Do you want to use rsync or sshfs? Enter one of them or nothing: "; \
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


# NOTE: the directories below HAVE to be on one line
DIRS := api api/generator api/file_manager api/generator/apigen mk xquery xsl/inc
PATHS := $(wildcard api/*.py api/*.md api/generator/*.py api/generator/apigen/*.py \ api/file_manager/*.py \
	freedict-database.rng \
	mk/* xsl/inc/* \
	xquery/* xsl/tei2c5.xsl)
install: #! install the tools to $$DESTDIR/$PREFIX/share/freedict (default /usr/local/share/freedict)
install:
	@echo Creating directories in $(INSTALLDIR)…
	@set -e; for d in $(DIRS); do \
		install -d $(INSTALLDIR)/$$d; \
	done
	@echo Copying files to $(INSTALLDIR)…
	@set -e; for f in $(PATHS); do \
		if [ -f $$f ]; then \
			install -m 644 $$f $(INSTALLDIR)/$$f; \
		fi \
	done




need-update: #! queries for unreleased dictionaries or for those with newer source changes
	$(PYTHON) $(FREEDICT_TOOLS)/api/generator/generator.py -n \
		-p "$(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -m" \
		-o "$(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -u" \
		|| sleep 1; $(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -u

release: #! build a release tarball in $$BUILD_DIR, ../build by default
release: $(BUILD_DIR)/freedict-tools-$(VERSION).tar.bz2

release-path: #! print the output directory to which releases are deployed (read from configuration); trailing newline is removed
	@$(PYTHON) $(FREEDICT_TOOLS)/api/file_manager/file_manager.py -r | tr -d '\n'

.PHONY: release install api all mount umount api-validation
