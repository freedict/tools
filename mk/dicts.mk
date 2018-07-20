# This file contains all targets defined for a dictionary. It is sourced by its
# Makefile.
# It defines targets to convert (build) the TEI
# files to the supported output formats. It also features some release targets
# used for making a release in FreeDict. "install" and "uninstall" targets are
# provided, too.
include $(FREEDICT_TOOLS)/mk/config.mk

export HELP_SUFFIX
define HELP_SUFFIX =
NOTE: For the targets `release` and `build`, there are targets called
    `release-PLATFORM` and `build-PLATFORM` available, if you just want to build
    one platform.
endef


#######################
#### set some variables
#######################

# let the tools from $(toolsdir) override tools
# from /usr/bin
PATH := $(FREEDICT_TOOLS):$(PATH)

ifeq ($(origin UNSUPPORTED_PLATFORMS), undefined)
UNSUPPORTED_PLATFORMS = evolutionary
endif


available_platforms := src dictd slob

xsldir ?= $(FREEDICT_TOOLS)/xsl
XMLLINT := /usr/bin/xmllint

dictname ?= $(shell basename "$(shell pwd)")
rdictname := $(shell export V=$(dictname); echo $${V:4:3}-$${V:0:3})
version1 := $(shell sed -e '100q;/<edition>/!d;s/.*<edition>\(.*\)<\/edition>.*/\1/;q'\
	   $(wildcard $(dictname).tei*) $(dictname)-nophon.tei)
version := $(subst $(space),,$(version1))

# these files are included in each of the  platform releases which are *not* a
# source release.
DISTFILES_BINARY = $(foreach f, README README.md README.txt README.rst \
					COPYING COPYING.txt COPYING.md COPYING.rst LICENSE \
					LICENSE.txt LICENSE.md LICENSE.rst \
					INSTALL INSTALL.md INSTALL.rst INSTALL.txt, \
		$(wildcard $(f))) # only consider these files if they do exist

PREFIX ?= /usr
DESTDIR ?= 

################
# General targets (default target, maintenance targets)
################


all: #! convert the TEI XML source into all supported output formats (see list-platforms)
all: build

build: #! same as all, build all available output formats
build: $(foreach platform,$(available_platforms),build-$(platform) )

$(foreach p, $(available_platforms), $(BUILD_DIR)/$(p)):
	mkdir -p $@

$(RELEASE_DIR):
	mkdir -p $@

changelog-help:
	@$(call exc_pyscript,fd_changelog,-h)

changelog:  #! launch a script to assist in updating a TEI header for next release, try changelog-help for usage help
changelog: $(dictname).tei
	@$(call exc_pyscript,fd_changelog,${E},$<)

# This is a "double colon rule", allowing you to extend this rule in your own
# makefile.
# For example:
#
# clean::
#	-rm -f delete_this_file.too
clean:: #! clean build files
	rm -rf build
	rm -f valid.stamp

# Helper rule to retrieve the release path. We cannot declare the value
# statically, because it is only required for the deploy target and this is only
# executed by admins. The first argument is "optional".
deploy_to = $(shell $(MAKE) --no-print-directory -C $(FREEDICT_TOOLS) release-path)/$(1)
deploy: #! deploy all platforms of a release to the remote file hosting service
deploy: $(foreach r, $(available_platforms), release-$(r))
	@MOUNTED=0; \
	if command -v mountpoint &> /dev/null; then \
		if mountpoint -q "$(deploy_to)"; then \
			echo "Remote file system mounted, skipping this step."; \
		else \
			$(MAKE) --no-print-directory -C $(FREEDICT_TOOLS) mount; \
			MOUNTED=1; \
		fi; \
	else  \
		$(MAKE) --no-print-directory -C $(FREEDICT_TOOLS) mount; fi; \
	if [ ! -d "$(call deploy_to,$(dictname))" ]; then \
		echo "Creating new release directory for first release of $(dictname)"; \
		mkdir -p $(call deploy_to,$(dictname)); fi; \
	if [ -d $(call deploy_to,$(dictname)/$(version)) ]; then \
		if [ "${FORCE}" = "y" ]; then \
			echo "Enforcing deployment…"; \
		else \
			echo "Release $(version) has been deployed already. Use \`make FORCE=y deploy\` to enforce the deployment."; \
			exit 2; fi; \
	else \
		mkdir -p $(call deploy_to,$(dictname)/$(version)); fi; \
	chmod a+r $(foreach p,$(available_platforms), $(call gen_release_path,$(p))); \
	echo "Copying files…";\
	cp $(foreach p,$(available_platforms), $(call gen_release_path,$(p))) \
		$(call deploy_to,$(dictname)/$(version)); \
	cp $(foreach p,$(available_platforms), $(call gen_release_hashpath,$(p))) \
		$(call deploy_to,$(dictname)/$(version)); \
	if [ $$MOUNTED -eq 1 ]; then \
		$(MAKE) --no-print-directory -C $(FREEDICT_TOOLS) umount; \
		fi

find-homographs: #! find all homographs and list them, one per line
find-homographs: $(dictname).tei
	@cat $< | grep orth | \
	sed -e s:'          <orth>':'':g -e s:'<\/orth>':'':g | sort -f | \
	uniq -i -d

list-platforms: #! list all available platforms, AKA output formats
	@echo -n $(available_platforms)

%.sha512: %
	cd $(dir $^); \
		sha512sum $(notdir $^) > $(notdir $@)

print-unsupported: #! print unsupported platforms
	@echo -n $(UNSUPPORTED_PLATFORMS)



pos-statistics: #! print statistics about the number of the different part-of-speech tags used
pos-statistics: $(dictname).tei
	@grep -o "<pos>.*</pos>" $< | perl -p -e 's/<pos>(.*)<\/pos>/$$1/;' | sort | uniq -c |sort -b -g


# Query platform support status
# This yields an exit status of
# 0 for dict supported on this platform
# 1 for dict unsupported on this platform
# 2 FOR unknown platform
query-%: #! query platform support status; 0=dictd supported, 1=dictd unsupported, 2=UNKNOWN platform
	@if [ -z "$(findstring $*,$(available_platforms))" ]; then \
	  echo "Unknown platform: $*"; exit 2; fi
	@if [ -n "$(findstring $*,$(UNSUPPORTED_PLATFORMS))" ]; then \
	  echo "Platform $* does not support this dictionary module."; exit 1; fi
	@echo "Platform $* supports this dictionary module."



release: #! build releases for all available platforms
release: $(foreach platform,$(available_platforms),release-$(platform))

# This function is here to assist the release-% rules. It generates the release
# path for each platform.
# Arg1: platform
gen_release_path = $(RELEASE_DIR)/freedict-$(dictname)-$(version).$(if \
	$(findstring slob,$(1)),slob,$(1).tar.xz)
gen_release_hashpath = $(call gen_release_path,$(1)).sha512

version: #! output current (source) version number
	@echo $(version)

################################################################################
#### Quality Assurance Helpers
################################################################################
# Each helper HAS TO output an error code and each of the error codes need to
# have an explanation in $FREEDICT_TOOLS/mk/qa.

describe: #! `make describe E=<CODE>` explains CODE, as printed by `make qa`
	@if [ -f "$$FREEDICT_TOOLS/mk/qa/${E}.md" ]; then \
			cat "$$FREEDICT_TOOLS/mk/qa/${E}.md"; \
		else \
			echo "No such error code."; \
		fi

# This is a makefile-internal rule. It detects problems (duplicated entries or
# empty nodes) within TEI files and prints a warning, if appropriate. By default
# it does not fail when an issue is encountered to allow the continuation of
# parent targets as `qa`. However, if used from a script, the a non-zero exit
# code can be enforced on error by supplying EXIT=y
report-duplicates:
report-duplicates: $(dictname).tei
	@$(call exc_pyscript,rm_duplicates,-s,$<) || \
		(if [ "${EXIT}" = "y" ]; then exit 1; fi)

qa: #! execute quality assurance helpers, use make explain E='NUM' if an error occurs
qa: report-duplicates validation

rm_duplicates: #! remove duplicated entries and empty XML nodes and present a diff of the changes
rm_duplicates: $(dictname).tei
	@$(call exc_pyscript,rm_duplicates,$<)

validation: #! validate dictionary with FreeDict's TEI XML subset
validation: $(dictname).tei
	@$(XMLLINT) --noout --relaxng freedict-P5.rng $<

######################################################################
#### Dict(d) format as used by the Dictd server and other programs
######################################################################

BUILD_DICTD=$(BUILD_DIR)/dictd

$(BUILD_DICTD)/$(dictname).c5: $(dictname).tei $(BUILD_DICTD) \
		$(xsldir)/tei2c5.xsl $(xsldir)/inc/teientry2txt.xsl \
		$(xsldir)/inc/teiheader2txt.xsl \
		$(xsldir)/inc/indent.xsl
	$(XSLTPROCESSOR) $(XSLTPROCESSORARGS) $(xsldir)/tei2c5.xsl $< >$@


build-dictd: $(BUILD_DICTD)/$(dictname).dict.dz $(BUILD_DICTD)/$(dictname).index

$(BUILD_DICTD)/%.dict $(BUILD_DICTD)/%.index: $(BUILD_DICTD)/%.c5 query-dictd
	cd $(BUILD_DICTD) && \
		dictfmt --without-time -t --headword-separator %%% $(DICTFMTFLAGS) $* < $(notdir $<)

$(BUILD_DICTD)/%.dict.dz: $(BUILD_DICTD)/%.dict
	dictzip -k $<

# prevent make from removing our precious file
.PRECIOUS: $(BUILD_DICTD)/$(dictname).dict

# build release archive
$(call gen_release_path,dictd): $(BUILD_DICTD)/$(dictname).dict.dz $(BUILD_DICTD)/$(dictname).index
	tar --dereference --transform='s/build.dictd.//'   -C .. -cJf $@ \
		$(addprefix $(notdir $(realpath .))/, $^) \
		$(addprefix $(notdir $(realpath .))/, $(DISTFILES_BINARY))

release-dictd: $(RELEASE_DIR) $(call gen_release_path,dictd) \
	$(call gen_release_hashpath,dictd)




######################################
#### targets for evolutionary platform
######################################

date=$(shell date +%Y-%m-%d)

install-base: $(dictname).dict.dz $(dictname).index
	install -d $(DESTDIR)/$(PREFIX)/share/dictd
	install -m 644 $^ $(DESTDIR)/$(PREFIX)/share/dictd


install: #! install the dictionary
install: install-base
	@echo -n 'Sucessfully installed the dictionary. Use `sudo dictdconfig -w` and '
	@if command -v systemctl 2>&1 > /dev/null; then \
		echo -n '`systemctl restart dictd`'; \
	elif command -v service 2>&1 > /dev/null; then \
		echo -n '`service dictd restart`'; \
	else \
		echo -n 'restart your dictd server'; \
	fi
	@echo ' to make use of the new dictionary.'

install-restart: #! same as install, but also restart the dict daemon
install-restart: install-base
	sh $(FREEDICT_TOOLS)/buildhelpers/dict_restart_helper.sh

uninstall: #! uninstall this dictionary
	-rm $(DESTDIR)/$(PREFIX)/share/dictd/$(dictname).dict.dz $(DESTDIR)/$(DESTDIR)/$(dictname).index
	$(DICTD_RESTART_SCRIPT)

################################################################################
#### Source 'platform'
################################################################################

# put all sources of a dictionary module into a tarball for release
# ("distribution").  this only includes the .tei file if it doesn't have to be
# generated from other sources
$(call gen_release_path,src): $(RELEASE_DIR) $(DISTFILES)
	tar --dereference -C .. -cJf $@ \
		$(addprefix $(notdir $(realpath .))/, $(DISTFILES))


# empty rule to fit into build system (build-<PLATFORM>)
build-src: $(dictname).tei

release-src: build-src $(call gen_release_path,src) $(call gen_release_hashpath,src)

##################################
#### targets for StarDict platform
##################################

# This tool comes with stardict
DICTD2DIC ?= dictd2dic

# This is hardcoded into dictd2dic :(
stardict_prefix = dictd_www.dict.org_

# idxhead is required to preexist by dictd2dic. The reason is not documented.
$(dictname).idxhead:
	echo -n "" > $@

$(stardict_prefix)$(dictname).idx $(stardict_prefix)$(dictname).dict.dz \
	dictd2dic.out: $(dictname).index $(dictname).dict $(dictname).idxhead
	$(DICTD2DIC) $(dictname) >dictd2dic.out
	gzip -9 $(stardict_prefix)$(dictname).idx

# $(wordcount) and $(idxfilesize) are a target-specific variables
$(stardict_prefix)$(dictname).ifo: \
	wordcount=$(word 2, $(shell tail -n1 dictd2dic.out))

$(stardict_prefix)$(dictname).ifo: \
	idxfilesize=$(strip $(shell zcat $(stardict_prefix)$(dictname).idx | wc -c))

$(stardict_prefix)$(dictname).ifo: $(stardict_prefix)$(dictname).idx \
	dictd2dic.out authorresp.out title.out sourceurl.out
	@echo "Generating $@..."
	@echo "StarDict's dict ifo file" > $@
	@echo "version=2.4.2" >> $@
	@echo "wordcount=$(wordcount)" >> $@
	@echo "idxfilesize=$(idxfilesize)" >> $@
	@echo "bookname=$(shell cat title.out)" >> $@
	@echo "author=$(shell sed -e "s/ <.*>//" <authorresp.out)" >> $@
	@echo "email=$(shell sed -e "s/.* <\(.*\)>/\1/" <authorresp.out)" >> $@
	@echo "website=$(shell cat sourceurl.out)" >> $@
	@echo "description=Converted to StarDict format by freedict.org" >> $@
	@echo "date=$(shell date +%Y.%m.%d)" >> $@
	@echo "sametypesequence=m" >> $@
	@cat $@

stardict: $(stardict_prefix)$(dictname).ifo

$(BUILD_DIR)/stardict/freedict-$(dictname)-$(version)-stardict.tar.bz2: \
       	$(stardict_prefix)$(dictname).ifo \
	$(stardict_prefix)$(dictname).dict.dz \
	$(stardict_prefix)$(dictname).idx.gz
	@if [ ! -d $(BUILD_DIR)/stardict ]; then \
		mkdir $(BUILD_DIR)/stardict; fi
	tar -C .. -cvjf \
	  $(BUILD_DIR)/stardict/freedict-$(dictname)-$(version)-stardict.tar.bz2 \
	  $(addprefix $(notdir $(realpath .))/, $^)

release-stardict: \
	$(BUILD_DIR)/stardict/freedict-$(dictname)-$(version)-stardict.tar.bz2

clean::
	rm -f $(dictname).idxhead $(stardict_prefix)$(dictname).idx.gz \
	$(stardict_prefix)$(dictname).dict.dz $(stardict_prefix)$(dictname).ifo \
	$(BUILD_DIR)/stardict/freedict-$(dictname)-$(version)-stardict.tar.bz2 \
	dictd2dic.out authorresp.out title.out sourceurl.out

#######################
#### Phonetics import
#######################

# ToDo: should be scripted to let eSpeak emit ISO 639-3 codes
#supported_phonetics ?= $(shell PATH="$(FREEDICT_TOOLS):$(PATH)" teiaddphonetics -li)

la1 := $(shell export V=$(dictname); echo $${V:0:3})
#la2 := $(shell export V=$(dictname); echo $${V:4:3})

ifeq ($(la1),$(findstring $(la1),$(supported_phonetics)))
# TEIADDPHONETICS ?= -v
$(dictname).tei: $(dictname)-nophon.tei
	teiaddphonetics $(TEIADDPHONETICS) -i $< -ou $@ -mbrdico-path $(MBRDICO_PATH)
endif


#######################
#### Slob format for the Aard Android  dictionary client
#######################

build-slob: $(BUILD_DIR)/slob/$(dictname)-$(version).slob

$(BUILD_DIR)/slob/$(dictname)-$(version).slob: $(dictname).tei | $(BUILD_DIR)/slob
	$(call exc_pyscript,tei2slob,-w,$(BUILD_DIR)/slob,-o,$@,$<)

$(call gen_release_path,slob): $(BUILD_DIR)/slob/$(dictname)-$(version).slob $(RELEASE_DIR) 
	cp $< $@

# make the hash depend on the release file
$(call gen_release_hashpath,slob): gen_release_path,slob)

release-slob: $(call gen_release_path,slob) $(call gen_release_hashpath,slob)

#######################
#### Makefile-technical
#######################

# should be default, but is not for make-historic reasons
.DELETE_ON_ERROR:

.PHONY: all build-dictd build-slob build-src clean dist find-homographs \
	install pos-statistics print-unsupported query-% releaase-src release release-dictd \
	test test-reverse tests uninstall validation version
