# Check if FreePascal is installed
FPC := $(shell command -v fpc 2>/dev/null)
ifndef FPC
    $(error FreePascal is not installed. Please install it first.)
endif

.PHONY: test test-ci update update-ci

# Test targets
test:
	@./scripts/run_tests.sh plain

test-ci:
	@./scripts/run_tests.sh ci

# Update targets
update:
	@./scripts/update-compile.sh

update-ci:
	@./scripts/update-compile.sh ci
