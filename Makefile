EMACS ?= emacs
EL    := org-draw.el

.PHONY: test compile web clean

test:
	$(EMACS) -Q --batch -L . -l tests/org-draw-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(EL)

web:
	cd web && node e2e.mjs

clean:
	rm -f *.elc tests/*.elc
