SHELL = /bin/sh
EMACS ?= emacs
PROFILER =

.PHONY: test

# Delete byte-compiled files etc.
clean:
	@rm -f *~
	@rm -f \#*\#
	@rm -f *.elc
	@rm -rf test/tar test/bsdtar

deps:
	$(EMACS) -batch --eval "(setq my-test-dir (file-truename \"test\"))" -l elpa-mirror.el -l test/elpa-mirror-test-common.el -Q -l test/elpa-mirror-test-deps.el

# Run tests.
test: clean deps
	mkdir -p test/tar test/bsdtar
	$(EMACS) -batch -Q --eval "(setq my-test-dir (file-truename \"test\"))" -l elpa-mirror.el -l test/elpa-mirror-test-common.el -l test/elpa-mirror-test-gnu-tar.el
	$(EMACS) -batch -Q --eval "(setq my-test-dir (file-truename \"test\"))" -l elpa-mirror.el -l test/elpa-mirror-test-common.el -l test/elpa-mirror-test-bsd-tar.el
