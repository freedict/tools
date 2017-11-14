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
    one target.
endef


#######################
#### set some variables
#######################

# let the tools from $(toolsdir) override tools
# from /usr/bin
PATH := $(FREEDICT_TOOLS):$(PATH)

ifeq ($(origin UNSUPPORTED_PLATFORMS), undefined)
UNSUPPORTED_PLATFORMS = bedic evolutionary
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
DIST_FILES_BINARY = $(foreach f, README README.md README.txt README.rst \
					COPYING COPYING.txt COPYING.md COPYING.rst LICENSE \
					LICENSE.txt LICENSE.md LICENSE.rst \
					INSTALL INSTALL.md INSTALL.rst INSTALL.txt, \
		$(wildcard $(f))) # only consider these files if they do exist

PREFIX ?= /usr
DESTDIR ?= 

################
# General targets (default target, maintainance targets)
################


all: #! convert the TEI XML source into all supported output formats (see list-platforms)
all: build

build: #! same as all, build all available output formats
build: $(foreach platform,$(available_platforms),build-$(platform) )

$(foreach p, $(available_platforms), $(BUILD_DIR)/$(p)):
	mkdir -p $@

$(RELEASE_DIR):
	mkdir -p $@

# This is a "double colon rule", allowing you to extend this rule in your own
# makefile.
# For example:
#
# clean::
#	-rm -f delete_this_file.too
clean:: #! clean build files
	rm -rf build
	rm -f valid.stamp


find-homographs: #! find all homographs and list them, one per line
find-homographs: $(dictname).tei
	@cat $< | grep orth | \
	sed -e s:'          <orth>':'':g -e s:'<\/orth>':'':g | sort -f | \
	uniq -i -d

list-platforms: #! list all available platforms, AKA output formats
	@echo -n $(available_platforms)

print-unsupported: #! print unsupported platforms
	@echo -n $(UNSUPPORTED_PLATFORMS)

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

version: #! output current (source) version number
	@echo $(version)

validation: #! validate dictionary with FreeDict's TEI XML subset
validation: $(dictname).tei
	$(XMLLINT) --noout --relaxng freedict-P5.rng $<




pos-statistics: #! print statistics about the number of the different part-of-speech tags used
pos-statistics: $(dictname).tei
	@grep -o "<pos>.*</pos>" $< | perl -p -e 's/<pos>(.*)<\/pos>/$$1/;' | sort | uniq -c |sort -b -g

######################################################################
#### Dict(d) format as used by the Dictd server and other programs
######################################################################

BUILD_DICTD=$(BUILD_DIR)/dictd

$(BUILD_DICTD)/$(dictname).c5: $(dictname).tei $(BUILD_DICTD) \
		$(xsldir)/tei2c5.xsl $(xsldir)/inc/teientry2txt.xsl \
		$(xsldir)/inc/teiheader2txt.xsl \
		$(xsldir)/inc/indent.xsl
	$(XSLTPROCESSOR) $(xsldir)/tei2c5.xsl $< >$@


build-dictd: $(BUILD_DICTD)/$(dictname).dict.dz $(BUILD_DICTD)/$(dictname).index

$(BUILD_DICTD)/%.dict $(BUILD_DICTD)/%.index: $(BUILD_DICTD)/%.c5 query-dictd
	cd $(BUILD_DICTD) && \
		dictfmt --without-time -t --headword-separator %%% $(DICTFMTFLAGS) $* < $(notdir $<)

$(BUILD_DICTD)/%.dict.dz: $(BUILD_DICTD)/%.dict
	dictzip -k $<

# prevent make from removing our precious file
.PRECIOUS: $(BUILD_DICTD)/$(dictname).dict

$(RELEASE_DIR)/freedict-$(dictname)-$(version).tar.bz2: \
		$(BUILD_DICTD)/$(dictname).dict.dz $(BUILD_DICTD)/$(dictname).index
	tar --dereference --transform='s/build.dictd.//'   -C .. -cvjf $@ \
		$(addprefix $(notdir $(realpath .))/, $<) \
		$(addprefix $(notdir $(realpath .))/, $(DIST_FILES_BINARY))

# Please note: for historical reasons, the dictd platform is the only one
# without a suffix in the file name.
release-dictd: $(RELEASE_DIR) $(RELEASE_DIR)/freedict-$(dictname)-$(version).tar.bz2


######################################
#### targets for evolutionary platform
######################################

date=$(shell date +%G-%m-%d)

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
$(RELEASE_DIR)/freedict-$(dictname)-$(version).src.zip: $(RELEASE_DIR) $(DISTFILES)
	cd .. && zip -r9 $(dictname)/$(subst ../,,$@) $(addprefix $(dictname)/, $(DISTFILES)) \
      -x build -x \*/.git/\* $(dictname)/freedict-*.tar.bz2 $(dictname)/freedict-*.zip $(dictname)/.* 

# empty rule to fit into build system (build-<PLATFORM>)
build-src: $(dictname).tei
release-src: build-src $(RELEASE_DIR)/freedict-$(dictname)-$(version).src.zip

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

authorresp.out: $(dictname).tei $(xsldir)/getauthor.xsl
	$(XSLTPROCESSOR) $(xsldir)/getauthor.xsl $< >$@

title.out: $(dictname).tei $(xsldir)/gettitle.xsl
	$(XSLTPROCESSOR) $(xsldir)/gettitle.xsl $< >$@

sourceurl.out: $(dictname).tei $(xsldir)/getsourceurl.xsl
	$(XSLTPROCESSOR) $(xsldir)/getsourceurl.xsl $< >$@

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
	@echo "date=$(shell date +%G.%m.%d)" >> $@
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

#####################
#### targets for ding
#####################

%.ding: %.tei $(xsldir)/tei2ding.xsl
	$(XSLTPROCESSOR) $(xsldir)/tei2ding.xsl $< >$@

clean::
	rm -f *.ding

#######################
#### Phonetics import
#######################

# ToDo: should be scripted to let eSpeak emit ISO 639-3 codes
#supported_phonetics ?= $(shell PATH="$(FREEDICT_TOOLS):$(PATH)" teiaddphonetics -li)

la1 := $(shell export V=$(dictname); echo $${V:0:3})
#la2 := $(shell export V=$(dictname); echo $${V:4:3})

ifneq (,$(findstring $(la1),$(supported_phonetics)))

# TEIADDPHONETICS ?= -v
$(dictname).tei: $(dictname)-nophon.tei
	teiaddphonetics $(TEIADDPHONETICS) -i $< -ou $@ -mbrdico-path $(MBRDICO_PATH)

endif


#######################
#### Slob format for the Aard Android  dictionary client
#######################

build-slob: $(BUILD_DIR)/slob/$(dictname)-$(version).slob

$(BUILD_DIR)/slob/$(dictname)-$(version).slob: $(dictname).tei $(BUILD_DIR)/slob
	# tei2slob adds the version number to the filename by itself
	tei2slob -w $(BUILD_DIR)/slob -o $@ $<

$(RELEASE_DIR)/freedict-$(dictname)-$(version).slob.tar.xz: \
		$(BUILD_DIR)/slob/$(dictname)-$(version).slob $(RELEASE_DIR)
	tar --dereference --transform='s/build.slob.//'   -C .. -cvJf $@ \
		$(addprefix $(notdir $(realpath .))/, $<) \
		$(addprefix $(notdir $(realpath .))/, $(DIST_FILES_BINARY))


release-slob: $(RELEASE_DIR) $(RELEASE_DIR)/freedict-$(dictname)-$(version).slob.tar.xz

#######################
#### Makefile-technical
#######################

# should be default, but is not for make-historic reasons
.DELETE_ON_ERROR:

.PHONY: all build-dictd build-slob build-src clean dist find-homographs \
	install pos-statistics print-unsupported query-% releaase-src release release-dictd \
	test test-reverse tests uninstall validation version
