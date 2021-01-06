# This makefile snippet contains all commands which can be performed in a
# directory containing many dictionary (a dictionary root).
# The subdirectories have to be called "lg1-lg2" (so the usual dictionary naming
# conventions).
FREEDICT_TOOLS ?= .
include $(FREEDICT_TOOLS)/mk/config.mk

# this shows that this makefile include may only be used for a directory
# containing many dictionaries
DICTS=$(shell find . -maxdepth 1 -name '???-???' -printf "%P\n"|sort|xargs echo)
# A target per dictionary, i.e. validate-DICTNAME.
VALIDATION_TARGETS=$(foreach DICT,$(DICTS),validate-$(DICT))

# Calls default target for each dictionary module.
# Note: This is a conflict if you wanted to call
# the 'all' target of each dictionary module.
all: #! build all dictionaries (default)
all: build_all

build_all: $(DICTS)

$(DICTS):
	@$(MAKE) -C $@

install: #! install the built files, without attempting to restart any applications using them
install: build_all
	echo Installing $(DICTS)
	@set -e; for dict in $(DICTS); do \
		$(MAKE) -C $$dict install; \
	done

install-restart: #! install built dictionaries and attempt to restart applications using them
install-restart: install-core
	@$(DICTD_RESTART_SCRIPT) || echo "Please make sure to run this command as root"

# ToDo
uninstall:
	$(BUILD)

clean::
	for DICT in $(DICTS); do \
		$(MAKE) -C $$DICT clean; \
	done

validation: #! validate all dictionaries in this directory
validation: $(VALIDATION_TARGETS)

$(VALIDATION_TARGETS):
	@$(MAKE) -C $(patsubst validate-%,%,$@) validation

.PHONY: install uninstall api all clean build_all $(DICTS) $(VALIDATION_TARGETS)
