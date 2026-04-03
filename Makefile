# kaakaa --- Tiny, security-focused AI agent in Guile
# Copyright © 2026 Arun Isaac <arunisaac@systemreboot.net>
#
# This file is part of kaakaa.
#
# kaakaa is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# kaakaa is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with kaakaa.  If not, see <https://www.gnu.org/licenses/>.

project = kaakaa
version = 0.1.0

GIT = git
GPG = gpg
GUILD = guild
GUILE = guile
GUILE_RUN64 = guile-run64
LZIP = lzip
SED = sed

prefix ?= /usr/local
bindir ?= $(prefix)/bin
datarootdir ?= $(prefix)/share
libdir ?= $(prefix)/lib

guile_effective_version = 3.0

top_level_module_dir = $(project)
config_file = $(top_level_module_dir)/config.scm
config_file_template = $(config_file).in
sources = $(filter-out $(config_file), \
                       $(wildcard $(top_level_module_dir)/*.scm) \
                       $(wildcard $(top_level_module_dir)/tools/*.scm))
objects = $(sources:.scm=.go) $(config_file:.scm=.go)
scripts = $(wildcard bin/*)
tests = $(wildcard tests/*.scm)
distribute_files = $(sources) $(config_file_template) $(scripts) \
                   $(tests) pre-inst-env guix.scm \
                   .guix/kaakaa-package.scm Makefile \
                   COPYING README.md

scmdir = $(datarootdir)/guile/site/$(guile_effective_version)
godir = $(libdir)/guile/$(guile_effective_version)/site-ccache

.PHONY: all clean install ares

all: $(objects) $(config_file)

%.scm: %.scm.in
	$(SED) -e 's|@PROJECT@|$(project)|' -e 's|@VERSION@|$(version)|' $< > $@

%.go: %.scm $(config_file)
	GUILE_AUTO_COMPILE=0 $(GUILD) compile -L . -o $@ $<

check:
	./pre-inst-env $(GUILE_RUN64) $(tests)

install: $(sources) $(config_file) $(objects) $(scripts)
	install -D $(scripts) --target-directory $(bindir)
	for source in $(sources) $(config_file); do \
		install -D $$source $(scmdir)/$$source; \
	done
	for object in $(objects); do \
		install -D $$object $(godir)/$$object; \
	done

# Build distribution tarball

dist_archive = $(project)-$(version).tar.lz

dist: $(dist_archive)
distsign: $(dist_archive).asc

$(dist_archive): .git/refs/heads/main
	$(GIT) archive --prefix $(basename $(basename $@))/ --format=tar main $(distribute_files) \
		| $(LZIP) --force --output $@

%.asc: %
	$(GPG) --detach-sign --armor $<

clean:
	rm -f $(objects) $(config_file)

ares:
	./pre-inst-env $(GUILE) -c '(begin ((@ (ares server) run-nrepl-server)))'
