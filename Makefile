.PHONY: test test-unit test-integration test-e2e test-all test-interactive clean-test

# Configuration
EMACS ?= emacs
TEST_FILE = test/claudemacs-test.el

# Default target
test: test-unit
	@echo "✓ Unit tests completed"

# Unit tests - fast, no external dependencies
test-unit:
	@echo "Running unit tests..."
	$(EMACS) -batch -l $(TEST_FILE) -f ert-run-tests-batch-and-exit "^claudemacs-test-.*" || exit 1

# Integration tests - with mocked dependencies
test-integration:
	@echo "Running integration tests..."
	$(EMACS) -batch -l $(TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :integration))" || exit 1

# End-to-end tests - requires Claude CLI
test-e2e:
	@echo "Running end-to-end tests..."
	@if command -v claude >/dev/null 2>&1; then \
		$(EMACS) -batch -l $(TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :e2e))"; \
	else \
		echo "⚠ Skipping e2e tests (Claude CLI not found)"; \
	fi

# Run all test categories
test-all: test-unit test-integration test-e2e
	@echo "✓ All test categories completed"

# Interactive testing for debugging
test-interactive:
	@echo "Starting interactive test session..."
	$(EMACS) -l $(TEST_FILE) --eval "(ert 't)"

# Run specific test pattern
test-specific:
	@echo "Running tests matching pattern: $(TEST_PATTERN)"
	$(EMACS) -batch -l $(TEST_FILE) -f ert-run-tests-batch-and-exit "$(TEST_PATTERN)"

# Validate test file loads correctly
test-load:
	@echo "Validating test file loads without errors..."
	$(EMACS) -batch -l $(TEST_FILE) --eval "(message \"✓ Test file loaded successfully\")"

# Clean up test artifacts
clean-test:
	@echo "Cleaning up test artifacts..."
	@find . -name "*-test.log" -delete 2>/dev/null || true
	@find . -name "claudemacs-test-*" -type d -exec rm -rf {} + 2>/dev/null || true

# Help target
help:
	@echo "Claudemacs Test Targets:"
	@echo "  test            - Run unit tests (default)"
	@echo "  test-unit       - Run unit tests only"
	@echo "  test-integration- Run integration tests"
	@echo "  test-e2e        - Run end-to-end tests (requires Claude CLI)"
	@echo "  test-all        - Run all test categories"
	@echo "  test-interactive- Start interactive test session"
	@echo "  test-specific   - Run specific pattern (use TEST_PATTERN=...)"
	@echo "  test-load       - Validate test file loads correctly"
	@echo "  clean-test      - Clean up test artifacts"
	@echo "  help            - Show this help"