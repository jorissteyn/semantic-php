EMACS ?= emacs
EMACS_CMD := $(EMACS) -Q -batch -L .
EL  := semantic-php.el semantic-php-wy.el
ELC := $(EL:.el=.elc)

dist: parser-create

clean:
	rm -f $(ELC)

compile: $(ELC)

%.elc: %.el
	$(EMACS_CMD) -f batch-byte-compile $<

test: clean
	$(EMACS_CMD) -l semantic-php-test.el -f ert-run-tests-batch-and-exit

parser-test:
	$(EMACS_CMD) -l semantic-php-util.el -f semantic-php-util-batch-scan-project

parser-create:
	$(EMACS_CMD) -l semantic-php-util.el -f semantic-php-util-compile-grammar

.PHONY: clean compile test parse dist autoload
