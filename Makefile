.PHONY: test test-unit test-integration test-e2e test-e2e-interactive test-e2e-client test-e2e-debug test-all clean-test

# Configuration
EMACS ?= emacs
EMACSCLIENT ?= emacsclient
TEST_FILE = test/claudemacs-test.el
ACTIONS_TEST_FILE = test/claudemacs-actions-test.el
PROJECTILE_INTEGRATION_TEST_FILE = test/claudemacs-projectile-integration-test.el
COMMENT_TEST_FILE = test/claudemacs-comment-test.el
TERMINAL_TEST_FILE = test/claudemacs-terminal-test.el
SESSION_LIST_TEST_FILE = test/claudemacs-session-list-test.el
E2E_TEST_FILE = test/claudemacs-e2e-test.el

# Select the terminal backend used by interactive E2E tests.  Unit tests use
# a registered fake backend and do not require Eat, Ghostel, or a native
# module.  This only affects sessions created by the E2E run.
CLAUDEMACS_TERMINAL_BACKEND ?= eat

# Default target
test: test-unit
	@echo "✓ Unit tests completed"

# Unit tests - fast, no external dependencies
test-unit:
	@echo "Running unit tests..."
	$(EMACS) -batch -l $(TEST_FILE) -f ert-run-tests-batch-and-exit "^claudemacs-test-.*" || exit 1
	$(EMACS) -batch -l $(ACTIONS_TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :unit))" || exit 1
	$(EMACS) -batch -l $(PROJECTILE_INTEGRATION_TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :integration))" || exit 1
	$(EMACS) -batch -l $(COMMENT_TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :unit))" || exit 1
	$(EMACS) -batch -l $(TERMINAL_TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :unit))" || exit 1
	$(EMACS) -batch -l $(SESSION_LIST_TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :unit))" || exit 1

# TDD tests - simple batch-mode tests for development
test-tdd:
	@echo "Running TDD batch-mode tests..."
	$(EMACS) -batch -l $(ACTIONS_TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :tdd))" || exit 1

# Integration tests - with mocked dependencies
test-integration:
	@echo "Running integration tests..."
	$(EMACS) -batch -l $(TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :integration))" || exit 1
	$(EMACS) -batch -l $(ACTIONS_TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :integration))" || exit 1
	$(EMACS) -batch -l $(PROJECTILE_INTEGRATION_TEST_FILE) --eval "(ert-run-tests-batch-and-exit '(tag :integration))" || exit 1

# End-to-end tests - informational target
test-e2e:
	@echo "E2E tests require interactive Emacs with a display."
	@echo "Run: make test-e2e-interactive"
	@echo ""
	@echo "This will open Emacs and run tests that:"
	@echo "  - Start real Claude sessions"
	@echo "  - Send actual messages"
	@echo "  - Verify session lifecycle"

# Interactive E2E tests - use test-e2e-client (recommended)
test-e2e-interactive:
	@echo "E2E tests need your full Emacs config and the selected terminal backend."
	@echo ""
	@echo "Option 1 - Via emacsclient (recommended):"
	@echo "  make test-e2e-client"
	@echo "  CLAUDEMACS_TERMINAL_BACKEND=ghostel make test-e2e-client"
	@echo ""
	@echo "Option 2 - From within Emacs:"
	@echo "  M-x load-file RET test/claudemacs-e2e-test.el RET"
	@echo "  M-x claudemacs-run-e2e-tests"

# E2E tests via emacsclient (use your running Emacs with full config)
test-e2e-client:
	@echo "Running E2E tests in your running Emacs (backend: $(CLAUDEMACS_TERMINAL_BACKEND))..."
	$(EMACSCLIENT) -e '(progn (setq claudemacs-terminal-backend (quote $(CLAUDEMACS_TERMINAL_BACKEND))) (load "$(shell pwd)/claudemacs.el") (load "$(shell pwd)/$(E2E_TEST_FILE)") (claudemacs-run-e2e-tests))'

# E2E tests with debug logging (output in *claudemacs-e2e-debug* buffer)
test-e2e-debug:
	@echo "Running E2E tests with debug logging (backend: $(CLAUDEMACS_TERMINAL_BACKEND))..."
	@echo "Check *claudemacs-e2e-debug* buffer for output"
	$(EMACSCLIENT) -e '(progn (setq claudemacs-terminal-backend (quote $(CLAUDEMACS_TERMINAL_BACKEND))) (load "$(shell pwd)/claudemacs.el") (load "$(shell pwd)/$(E2E_TEST_FILE)") (claudemacs-run-e2e-tests t))'

# Run all test categories
test-all: test-unit test-integration test-e2e
	@echo "✓ All test categories completed"

# Run specific test pattern
test-specific:
	@echo "Running tests matching pattern: $(TEST_PATTERN)"
	$(EMACS) -batch -l $(TEST_FILE) -l $(ACTIONS_TEST_FILE) -l $(PROJECTILE_INTEGRATION_TEST_FILE) -l $(COMMENT_TEST_FILE) -l $(TERMINAL_TEST_FILE) -f ert-run-tests-batch-and-exit "$(TEST_PATTERN)"
	$(EMACS) -batch -l $(SESSION_LIST_TEST_FILE) -f ert-run-tests-batch-and-exit "$(TEST_PATTERN)"

# Validate test file loads correctly
test-load:
	@echo "Validating test files load without errors..."
	$(EMACS) -batch -l $(TEST_FILE) --eval "(message \"✓ Test file loaded successfully\")"
	$(EMACS) -batch -l $(ACTIONS_TEST_FILE) --eval "(message \"✓ Actions test file loaded successfully\")"
	$(EMACS) -batch -l $(PROJECTILE_INTEGRATION_TEST_FILE) --eval "(message \"✓ Projectile integration test file loaded successfully\")"
	$(EMACS) -batch -l $(COMMENT_TEST_FILE) --eval "(message \"✓ Comment test file loaded successfully\")"
	$(EMACS) -batch -l $(TERMINAL_TEST_FILE) --eval "(message \"✓ Terminal backend test file loaded successfully\")"
	$(EMACS) -batch -l $(SESSION_LIST_TEST_FILE) --eval "(message \"✓ Session list test file loaded successfully\")"
	$(EMACS) -batch -l $(E2E_TEST_FILE) --eval "(message \"✓ E2E test file loaded successfully\")"

# Clean up test artifacts
clean-test:
	@echo "Cleaning up test artifacts..."
	@find . -name "*-test.log" -delete 2>/dev/null || true
	@find . -name "claudemacs-test-*" -type d -exec rm -rf {} + 2>/dev/null || true

# Help target
help:
	@echo "Claudemacs Test Targets:"
	@echo "  test                - Run unit tests (default)"
	@echo "  test-unit           - Run unit tests only"
	@echo "  test-integration    - Run integration tests"
	@echo "  test-e2e-client     - Run real E2E tests via emacsclient"
	@echo "  test-e2e-debug      - Run E2E tests with debug logging"
	@echo "  test-all            - Run all batch-mode test categories"
	@echo "  test-specific       - Run specific pattern (use TEST_PATTERN=...)"
	@echo "  test-load           - Validate test files load correctly"
	@echo "  clean-test          - Clean up test artifacts"
	@echo "  help                - Show this help"
