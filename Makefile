SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

windows-ci: CASK=
windows-ci: clean windows-compile

windows-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-l test/windows-bootstrap.el \
		-L . \
		--eval '(setq byte-compile-error-on-warn nil)' \
		-f batch-byte-compile *.el

clean:
	rm -rf .cask *.elc

.PHONY: clean windows-compile
