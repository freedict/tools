# This file defines common configuration variables shared across all Makefiles.
# Furthermore, it defines a help target which makes the buildsystem
# self-documenting.

SHELL=bash


# set all as default target, so that help is not executed by default
.DEFAULT_GOAL := all

# `xsltproc' comes with libxml/libxslt, see xmlsoft.org
XSLTPROCESSOR ?= xsltproc
XSLTPROCESSORARGS ?= --novalid --xinclude

DICTFMTFLAGS ?= --utf8
# set default value
FREEDICT_TOOLS ?= .

# where built files should get installed
PREFIX?=/usr/local
DESTDIR ?= 

# name of the nsgmls command
NSGMLS ?= onsgmls

# the file xml.soc file comes with sp/opensp/openjade
# and is required by nsgmls
XMLSOC ?= /usr/share/xml/declaration/xml.soc

# xml.soc normally points to this file
XMLDECLARATION ?= /usr/share/xml/declaration/xml.dcl
#XMLDECLARATION ?= /usr/share/xml/declaration/xml1n.dcl

# you can also set this in ~/.bashrc
SGML_CATALOG_FILES ?= /etc/sgml/catalog

DICTFMT = $(shell command -v dictfmt 2> /dev/null)
ifeq ($(DICTFMT),)
$(warnrning The command dictfmt was not found, you will not be able to convert into the dictd format.)
endif

charlint_in_path := $(shell which charlint.pl 2>/dev/null)
ifneq ($(charlint_in_path), )
CHARLINT := $(charlint_in_path)
else
# if this doesn't exist it will be downloaded
CHARLINT := $(FREEDICT_TOOLS)/charlint.pl
endif
CHARLINT_DATA := $(dir $(CHARLINT))charlint-unicodedata

MBRDICO_PATH = /

BUILDHELPERS_DIR=$(FREEDICT_TOOLS)/buildhelpers/
# Directory containing all build files, usually a tree like build/<platform>
# within the current working directory.
BUILD_DIR=build
RELEASE_DIR=$(BUILD_DIR)/release


# script to restart the dictd daemon, if installed
DICTD_RESTART_SCRIPT = $(BUILDHELPERS_DIR)dict_restart_helper.sh

################################################################################
# FreeDict configuration
# Use FREEDICTRC to discover a configuration. If the variable is empty, no
# configuration was found.
FREEDICTRC = $(wildcard $(HOME)/.config/freedict/freedictrc $(LOCALAPPDATA)/freedict/freedict.ini)


################################################################################
# Python special handling
# First, the interpreter is queried for the correct version (and name). Then the
# setup for a virtual environment is done. For the "end-user", all that matters
# should be the exc_pyscript function, e.g.:
#
# 	foo:
# 		$(exc_pyscript) fd_api --what-ever

# find interpreter with matching name, python3 or python?
PYTHON := $(shell command -v python3 2> /dev/null)
ifeq ($(PYTHON),)
	PYTHON := $(shell command -v python 2> /dev/null)
ifeq ("$(PYTHON)","")
$(error No Python executable found, please make sure that a recent Python is in the search path)
endif
endif

# query version
PYTHON_VERSION_FULL := $(wordlist 2,4,$(subst ., ,$(shell $(PYTHON) --version 2>&1)))
PYTHON_VERSION_MAJOR := $(word 1,${PYTHON_VERSION_FULL})

ifneq ($(PYTHON_VERSION_MAJOR),3)
$(error a python Version >= 3.4 is required, current python major is $(PYTHON_VERSION_MAJOR))
endif

# exc_pyscript is meant to either call a system-wide installed version or one
# from a virtual env. The latter needs to be specified in the FD configuration,
# documented in the wiki.
# This is a fallback for system-wide script usage:
exc_pyscript = $(1) $(2) $(3) $(4) $(5) $(6) $(7) $(8) $(9)

# find a (python) virtual env
# ToDo: look at detection algorithm
FREEDICTRC = $(HOME)/.config/freedict/freedictrc
ifneq ($(wildcard $(FREEDICTRC)),)
VIRTUAL_ENV=$(shell grep -E 'virtual_env.*=.*' < $(FREEDICTRC) | cut -d = -f 2\
			|tr -d ' ')
# if virtual env found
ifneq ($(VIRTUAL_ENV),)
ifneq ($(wildcard $(VIRTUAL_ENV)),)
# call python from the virtual environment
exc_pyscript = source $(VIRTUAL_ENV)/bin/activate; \
	$(1) $(2) $(3) $(4) $(5) $(6) $(7) $(8) $(9)
endif
endif
endif


################################################################################
# Remote file handling
# These two functions are meant to be used within a rule (combined as *one*
# shell line) to remember state. If remote volumes were mounted before, don't
# umount them, otherwise, do. Example:
#
# 	foo:
# 		$(call mount_or_reuse); mycommand; $(call umount_or_keep)
mount_or_reuse=$(call exc_pyscript,fd_file_mgr,-m); \
	if [ $$? -eq 201 ]; then \
		STAY_MOUNTED=1; \
	else \
		STAY_MOUNTED=0; \
	fi
umount_or_keep = \
	if [ $$STAY_MOUNTED -eq 0 ]; then \
		$(call exc_pyscript,fd_file_mgr,-u); \
	fi

################################################################################
# Define the help system, use #! after the colon of a rule to add a
# documentation string
help:
	@echo "Usage: make <command>"
	@echo "The following commands are defined:"
	@echo
	@IFS=$$'\n'; \
	help_lines=`fgrep -h "#!" $(MAKEFILE_LIST) | fgrep -v fgrep | fgrep -v help_line | grep -v 'Define the' | sed -e 's/\\$$//' | sort`; \
	for help_line in $${help_lines[@]}; do \
		help_command=`echo $$help_line | sed -e 's/^\(.*\): .*/\1/' -e 's/^ *//' -e 's/ *$$//' -e 's/:$$//'`; \
		help_info=`echo $$help_line | sed -e 's/.*#!\(.*\)$$/\1/' -e 's/^ *//' -e 's/ *$$//'`; \
		printf "%-19s %s\n" $$help_command $$help_info; \
	done;\
	if [ -n `echo $$HELP_SUFFIX|wc -w` ]; then \
		echo;\
		printf "%s\n" $$HELP_SUFFIX; \
	fi
# NOTE: If you want to add a HELP_SUFFIX, you have to export the variable as
# environment variable.
# Note II: wc -w is necessary, because HELP_SUFFIX might be a multi-line string

