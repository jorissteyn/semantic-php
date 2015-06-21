EMACS ?= emacs
EMACS_CMD := $(EMACS) -Q -batch -L .
EL  := semantic-php.el semantic-php-wy.el
ELC := $(EL:.el=.elc)

dist: parser autoload

clean:
	rm -f $(ELC)

compile: $(ELC)

%.elc: %.el
	$(EMACS_CMD) -f batch-byte-compile $<

test: clean
	$(EMACS_CMD) -l semantic-php-test.el -f ert-run-tests-batch-and-exit

parser-test:
	$(EMACS_CMD) -l semantic-php-util.el -f semantic-php-util-batch-scan-project

parser:
	$(EMACS_CMD) -l semantic-php-util.el -f semantic-php-util-compile-grammar

autoload:
	$(EMACS_CMD) -l semantic-php-util.el -f semantic-php-util-generate-autoloads

.PHONY: dist clean compile test parser-test parser autoload
