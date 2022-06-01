SHELL = /bin/sh
EMACS ?= emacs
PROFILER =
RM= @rm -rf
EMACS_BATCH_OPTS=--batch -Q \
-l elpa-mirror.el \
--eval "(setq my-test-dir (file-truename \"test-package-output\"))"

.PHONY: test deps clean compile

# Delete byte-compiled files etc.
clean:
	$(RM) *~
	$(RM) \#*\#
	$(RM) *.elc
	$(RM) tests/tar
	$(RM) tests/bsdtar

deps:
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/elpa-mirror-test-common.el -l tests/elpa-mirror-test-deps.el

compile: deps
	$(RM) *.elc
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/my-byte-compile.el 2>&1 | grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

# Run tests.
test: clean deps compile
	@mkdir -p tests/tar tests/bsdtar
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/elpa-mirror-test-common.el -l tests/elpa-mirror-test-gnu-tar.el
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/elpa-mirror-test-common.el -l tests/elpa-mirror-test-bsd-tar.el
