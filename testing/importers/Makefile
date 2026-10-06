include ../config.mk

test:
	"$(PYTHON)" -m unittest discover -s .
	"$(PYTHON)" -m unittest discover -s "$(TOOLS_ROOT)/importers/fd_import" -p tests.py

.PHONY: test
