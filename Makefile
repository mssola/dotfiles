all: test

.PHONY: test
test: vendor emacs scripts lint

.PHONY: vendor
vendor:
	@gem install rubocop

.PHONY: emacs
emacs:
	@lsb_release -a
	@cd .emacs.d && ./test-emacs.sh

.PHONY: scripts
scripts:
	@cd bin/test && ./test.sh

.PHONY: lint
lint:
	@find . -name "*.sh" ! -name ".gitcompletion.sh" ! -name ".hgcompletion.sh" -not -path "./.emacs.d/*" -exec shellcheck \{\} \+
	@rubocop
