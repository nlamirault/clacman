# Copyright (C) 2007-2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


APP = clacman

SHELL = /bin/bash

VERSION=$(shell \
        grep "version" clacman.asd \
	|awk -F'"' '{print $$2}')


NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

ROSWELL_BRANCH = master


all: help

help:
	@echo -e "$(OK_COLOR)==== $(APP) [$(VERSION)] ====$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- init$(NO_COLOR)  : install tools$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- deps$(NO_COLOR)  : install dependencies$(NO_COLOR)"

.PHONY: init
init:
	@echo -e "$(OK_COLOR)[clacman] Install dependencies$(NO_COLOR)"
	@curl -L https://raw.githubusercontent.com/snmsts/roswell/$(ROSWELL_BRANCH)/scripts/install-for-ci.sh | sh
	@ros install qlot

.PHONY: deps
deps:
	@qlot install clacman

.PHONY: lisp
lisp:
	@echo -e "$(OK_COLOR)[clacman] Display Lisp implementation$(NO_COLOR)"
	@ros list installed

.PHONY: test
test:
	@echo -e "$(OK_COLOR)[clacman] Launch unit tests$(NO_COLOR)"
