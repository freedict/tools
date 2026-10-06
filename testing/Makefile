TEST_DIRS := buildhelpers fd_tool importers mk

test: $(TEST_DIRS)

$(TEST_DIRS):
	$(MAKE) -C "$@" test

.PHONY: test $(TEST_DIRS)
