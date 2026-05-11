SHELL := bash
PYTHON ?= python3.11
PYTEST ?= $(PYTHON) -m pytest

.PHONY: default build test validate py-compile coverage-json clean

default: validate

build:
	time rosettacode

py-compile:
	$(PYTHON) -m py_compile $$(find opencog tests -name '*.py' -type f | tr '\n' ' ')

test: py-compile
	$(PYTEST) -q

coverage-json:
	opencog/bin/opencog-bindgen --coverage-json

validate: py-compile coverage-json
	opencog/bin/opencog-bindgen --list-missing

clean:
	$(RM) -r Meta/ rosettacode.log .pytest_cache
	find . -type d -name '__pycache__' -prune -exec $(RM) -r {} +
