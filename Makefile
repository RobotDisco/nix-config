.POSIX:

EMACS ?= emacs

EL := config.el

.PHONY: all
all: $(EL)

help:	## Show help message
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/\\##//'

$(EL):	config.org	## Generate config elisp from org source file.
	$(EMACS) -Q --batch --eval "(progn (require 'org) (org-babel-tangle-file \"$<\" \"$@\"))"

test:	$(EL) ## Evaluate generated config elisp file.
	$(EMACS) -Q --script $<

.PHONY: clean
clean:
	$(RM) $(EL)


