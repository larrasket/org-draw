EMACS ?= emacs
EL    := org-pad.el

.PHONY: test compile web clean

test:
	$(EMACS) -Q --batch -L . -l tests/org-pad-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		--eval '(setq byte-compile-docstring-max-column 100)' \
		-f batch-byte-compile $(EL)

# Web canvas end-to-end test (real browser). Needs Playwright:
#   cd web && npm i -D playwright && npx playwright install chromium
# Skips gracefully if Playwright isn't installed.
web:
	cd web && node e2e.mjs

clean:
	rm -f *.elc tests/*.elc
