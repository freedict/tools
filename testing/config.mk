# Locate the checkout independently of the directory invoking Make.
TOOLS_ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/..)
PYTHON ?= python3
export PYTHONPATH := $(TOOLS_ROOT)/fd_tool:$(TOOLS_ROOT)/importers$(if $(PYTHONPATH),:$(PYTHONPATH))
