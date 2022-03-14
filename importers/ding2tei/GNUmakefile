#
# GNU Makefile
#
# Copyright 2021,2022 Einhard Leichtfuß
#
# This file is part of ding2tei-haskell.
#
# ding2tei-haskell is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ding2tei-haskell is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with ding2tei-haskell.  If not, see <https://www.gnu.org/licenses/>.
#

SHELL = /bin/sh

# Note: ghc probably cannot be substituted due to the use of GHC extensions.
HC = ghc
HAPPY = happy
ALEX = alex

INSTALL = install
INSTALL_PROGRAM = $(INSTALL) -m 755
INSTALL_DATA = $(INSTALL) -m 644
INSTALL_DIR = $(INSTALL) -d -m 755
DOWNLOAD = curl

# Notes:
#  * Consider to manually add `-jN'.
#  * Some of the below -Wx flags may have merged into -Wall in later releases
#    of ghc.
HCFLAGS = -Wall -Wcompat \
          -Wincomplete-uni-patterns -Wincomplete-record-updates \
          -Wmissing-export-lists \
          -Widentities -Wredundant-constraints -Wpartial-fields -Wcpp-undef \
          -O2

# Notes:
#  * In production, omit `--info'.
#  * Consider the option `--array'.
#  * The option `--coerce'
#    - is claimed to generate
#      - "smaller faster parsers",
#      - at the cost of "some type safety".
#        - Thus, one should always make sure the result compiles w/o the flag.
#    - actually seems to increase run-time.
HAPPYFLAGS = --ghc --info=$(@:.hs=.info)

# Note: In production, omit `--info'.
ALEXFLAGS = --ghc --info=$(@:.hs=.info)

# Clear suffixes, no implicit rules needed.
.SUFFIXES :

srcdir = src
builddir = build
dictdir = dict
dingdir = $(dictdir)/ding
teidir = $(dictdir)/tei

.DEFAULT_GOAL = ding2tei

SRCS_ALEX = $(srcdir)/Language/Ding/AlexScanner.x
SRCS_HAPPY = $(srcdir)/Language/Ding/Parser/Line.y
SRCS_HS := $(filter-out $(srcdir)/Test.hs,$(shell find $(srcdir) -name '*.hs'))
SRCS_SED = $(wildcard $(srcdir)/preprocess/de-en/*.sed)
SRCS = $(SRCS_ALEX) $(SRCS_HAPPY) $(SRCS_HS) $(SRCS_SED)

SRCS_HS_GEN = $(patsubst $(srcdir)/%.x,$(builddir)/%.hs,$(SRCS_ALEX)) \
              $(patsubst $(srcdir)/%.y,$(builddir)/%.hs,$(SRCS_HAPPY))

# Notes:
#  * We could also use `ghc -M' to generate the dependencies in make format.
#  * Prefer $(builddir) over $(srcdir) for `-i' such that $(SRCS_HS_GEN)
#    are always found first in the former.
ding2tei : $(SRCS_HS) $(SRCS_HS_GEN)
	$(HC) $(HCFLAGS) --make \
		-i -i$(builddir):$(srcdir) -outputdir $(builddir) \
		-XRecursiveDo \
		-o $@ \
		$(srcdir)/Main.hs

.PHONY : haskell-source-files
haskell-source-files : $(SRCS_HS_GEN)

$(builddir)/%.hs : $(srcdir)/%.y
	$(INSTALL_DIR) $(@D)
	$(HAPPY) $(HAPPYFLAGS) $< -o $@

$(builddir)/%.hs : $(srcdir)/%.x
	$(INSTALL_DIR) $(@D)
	$(ALEX) $(ALEXFLAGS) $< -o $@

.PHONY : clean
clean : clean-objcode clean-dicts

.PHONY : clean-objcode
clean-objcode :
	rm -f -r $(builddir)/
	rm -f ding2tei

.PHONY : clean-dicts
clean-dicts :
	rm -f $(dingdir)/de-en.txt.pp
	rm -f $(teidir)/deu-eng/deu-eng.tei
	rm -f $(teidir)/eng-deu/eng-deu.tei

.PHONY : clean-ding
clean-ding :
	rm -f $(dingdir)/de-en.txt

.PHONY : deu-eng
deu-eng : $(teidir)/deu-eng/deu-eng.tei
$(teidir)/deu-eng/deu-eng.tei : $(dingdir)/de-en.txt.pp ding2tei
	$(INSTALL_DIR) $(@D)
	./ding2tei -- $< $@

.PHONY : eng-deu
eng-deu : $(teidir)/eng-deu/eng-deu.tei
$(teidir)/eng-deu/eng-deu.tei : $(dingdir)/de-en.txt.pp ding2tei
	$(INSTALL_DIR) $(@D)
	./ding2tei --inverse -- $< $@

.PHONY : validate-ding
validate-ding : $(dingdir)/de-en.txt.pp ding2tei
	./ding2tei --validate -- $<

.PHONY : preprocess
preprocess : $(dingdir)/de-en.txt.pp
$(dingdir)/de-en.txt.pp : $(dingdir)/de-en.txt \
	$(srcdir)/preprocess/de-en/all.bash $(SRCS_SED)
	$(srcdir)/preprocess/de-en/all.bash < $< > $@

# This target must be specified explicitly and fails otherwise.
.PHONY : download-ding-1.9
download-ding-1.9 : $(dingdir)/de-en.txt
$(dingdir)/de-en.txt :
ifneq (,$(filter download-ding-1.9,$(MAKECMDGOALS)))
	$(INSTALL_DIR) $(@D)
	$(DOWNLOAD) \
		'https://ftp.tu-chemnitz.de/pub/Local/urz/ding/de-en/de-en.txt.xz' > $@.xz
	printf '%s %s\n' \
		7373b16fccc5c167298b6cde8e4cdd78b3d48941711f10f39429e761742e3068 $@.xz \
		| sha256sum -c
	xz -d $@.xz
else
	$(warning Run `make download-ding-1.9` first.)
	$(error Refusing to implicitly download Ding source)
endif
