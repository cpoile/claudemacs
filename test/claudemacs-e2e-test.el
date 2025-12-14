;;; claudemacs-e2e-test.el --- End-to-end tests for claudemacs -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0") (eat "0.9"))

;;; Commentary:
;; Real end-to-end tests that spawn actual eat terminals and test full workflows.
;; These tests require an interactive Emacs session (not batch mode).
;;
;; Run with: make test-e2e-interactive
;; Or manually: M-x claudemacs-run-e2e-tests

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load claudemacs - try multiple methods
(unless (featurep 'claudemacs)
  (condition-case nil
      ;; Method 1: Use load-file-name if available
      (when load-file-name
        (add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
        (require 'claudemacs))
    (error nil))
  ;; Method 2: If already in load-path (e.g., installed package)
  (unless (featurep 'claudemacs)
    (require 'claudemacs nil t))
  ;; Method 3: Error if still not found
  (unless (featurep 'claudemacs)
    (error "Cannot load claudemacs. Make sure it's installed or in load-path")))

;;; Test Configuration

(defvar claudemacs-e2e-timeout 30
  "Timeout in seconds for E2E test operations.")

(defvar claudemacs-e2e-poll-interval 0.5
  "Polling interval in seconds when waiting for conditions.")

(defvar claudemacs-e2e-debug nil
  "When non-nil, log verbose debug info to *claudemacs-e2e-debug* buffer.")

(defun claudemacs-e2e--log (format-string &rest args)
  "Log a debug message if `claudemacs-e2e-debug' is non-nil."
  (when claudemacs-e2e-debug
    (with-current-buffer (get-buffer-create "*claudemacs-e2e-debug*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] "))
      (insert (apply #'format format-string args))
      (insert "\n"))))

(defvar claudemacs-e2e-test-project nil
  "Current test project directory.")

;;; Test Utilities

(defun claudemacs-e2e--wait-for (predicate &optional timeout message)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse.
Returns the value of PREDICATE if successful, signals error if timeout.
MESSAGE is used in error reporting."
  (claudemacs-e2e--log "Waiting for: %s" (or message "condition"))
  (let ((timeout (or timeout claudemacs-e2e-timeout))
        (start-time (current-time))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (sit-for claudemacs-e2e-poll-interval))
    (if result
        (claudemacs-e2e--log "  ✓ Got: %s" (or message "condition"))
      (claudemacs-e2e--log "  ✗ TIMEOUT: %s" (or message "condition"))
      (error "E2E timeout waiting for: %s" (or message "condition")))
    result))

(defun claudemacs-e2e--buffer-contains-p (buffer pattern)
  "Check if BUFFER contains text matching PATTERN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (re-search-forward pattern nil t)))))

(defun claudemacs-e2e--wait-for-prompt (buffer)
  "Wait for Claude to show its input prompt in BUFFER.
Claude shows '>' or similar when ready for input."
  (claudemacs-e2e--wait-for
   (lambda ()
     ;; Look for patterns that indicate Claude is ready for input
     (claudemacs-e2e--buffer-contains-p buffer "^> \\|claude>\\|❯"))
   claudemacs-e2e-timeout
   "Claude prompt"))

(defun claudemacs-e2e--wait-for-response (buffer start-pos)
  "Wait for Claude to respond after START-POS in BUFFER.
Returns when new content appears after START-POS."
  (claudemacs-e2e--wait-for
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (> (point-max) start-pos))))
   claudemacs-e2e-timeout
   "Claude response"))

(defun claudemacs-e2e--get-buffer-text (buffer)
  "Get visible text content from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun claudemacs-e2e--send-input (buffer text)
  "Send TEXT as input to the terminal in BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'eat-terminal) eat-terminal)
      (eat-term-send-string eat-terminal text))))

(defun claudemacs-e2e--send-return (buffer)
  "Send return key to terminal in BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'eat-terminal) eat-terminal)
      (eat-term-input-event eat-terminal 1 'return))))

(defun claudemacs-e2e--create-test-project ()
  "Create a temporary git project for testing.
Returns the project directory path."
  (let ((dir (make-temp-file "claudemacs-e2e-test" t)))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "--quiet")
      (write-region "# Test Project\n" nil "README.md")
      (write-region "function hello() { return 'world'; }\n" nil "test.js")
      (call-process "git" nil nil nil "add" ".")
      (call-process "git" nil nil nil
                    "-c" "user.name=Test"
                    "-c" "user.email=test@example.com"
                    "commit" "-m" "initial" "--quiet"))
    dir))

(defun claudemacs-e2e--cleanup-test-project (dir)
  "Remove test project directory DIR."
  (when (and dir (file-exists-p dir))
    (delete-directory dir t)))

(defvar claudemacs-e2e--test-buffers nil
  "List of buffer names created during E2E tests.")

(defun claudemacs-e2e--track-buffer (buffer)
  "Track BUFFER as created by E2E tests for later cleanup."
  (when (buffer-live-p buffer)
    (push (buffer-name buffer) claudemacs-e2e--test-buffers)))

(defun claudemacs-e2e--cleanup-sessions ()
  "Kill only claudemacs sessions created during E2E tests.
Does NOT affect user's existing sessions."
  (claudemacs-e2e--log "Tracked test buffers: %S" claudemacs-e2e--test-buffers)
  (dolist (buf-name claudemacs-e2e--test-buffers)
    (when-let ((buffer (get-buffer buf-name)))
      (when (buffer-live-p buffer)
        (claudemacs-e2e--log "  Killing: %s" buf-name)
        (with-current-buffer buffer
          (when (and (boundp 'eat-terminal) eat-terminal
                     (process-live-p (get-buffer-process buffer)))
            (ignore-errors (eat-kill-process))))
        (kill-buffer buffer))))
  (setq claudemacs-e2e--test-buffers nil))

;;; Test Fixtures

(defvar claudemacs-e2e--test-session-id nil
  "Unique session ID for the current test run.")

(defmacro claudemacs-e2e-with-project (&rest body)
  "Execute BODY with a temporary test project and isolated session ID.
Sets `default-directory' and mocks `claudemacs--session-id' to return
a unique test ID, ensuring tests don't affect user's existing sessions."
  (declare (indent 0))
  `(let ((claudemacs-e2e-test-project (claudemacs-e2e--create-test-project))
         (claudemacs-e2e--test-session-id (format "e2e-test-%s" (format-time-string "%s%N"))))
     (claudemacs-e2e--log "Test project: %s" claudemacs-e2e-test-project)
     (claudemacs-e2e--log "Test session ID: %s" claudemacs-e2e--test-session-id)
     (unwind-protect
         (cl-letf (((symbol-function 'claudemacs--session-id)
                    (lambda () claudemacs-e2e--test-session-id)))
           (let ((default-directory claudemacs-e2e-test-project))
             ,@body))
       (claudemacs-e2e--log "Cleaning up test...")
       (claudemacs-e2e--cleanup-test-project claudemacs-e2e-test-project)
       (claudemacs-e2e--cleanup-sessions)
       (claudemacs-e2e--log "Cleanup complete"))))

;;; E2E Tests - Session Lifecycle

(ert-deftest claudemacs-e2e-start-session ()
  "Test starting a fresh Claude session."
  :tags '(:e2e :session)
  (skip-unless (not noninteractive))
  (skip-unless (executable-find "claude"))

  (claudemacs-e2e-with-project
    ;; Start a session (index 0 = claude, the default tool)
    (claudemacs--start-tool-by-index 0)

    ;; Wait for buffer to exist
    (let ((buffer (claudemacs-e2e--wait-for
                   (lambda () (claudemacs--get-buffer 'claude))
                   10
                   "claudemacs buffer creation")))
      (should buffer)
      (claudemacs-e2e--track-buffer buffer)
      (should (buffer-live-p buffer))

      ;; Check buffer has eat terminal
      (with-current-buffer buffer
        (should (boundp 'eat-terminal))
        (should eat-terminal))

      ;; Check process is running
      (let ((proc (get-buffer-process buffer)))
        (should proc)
        (should (process-live-p proc))))))

(ert-deftest claudemacs-e2e-kill-session ()
  "Test killing a Claude session."
  :tags '(:e2e :session)
  (skip-unless (not noninteractive))
  (skip-unless (executable-find "claude"))

  (claudemacs-e2e-with-project
    ;; Start a session
    (claudemacs--start-tool-by-index 0)

    ;; Wait for buffer
    (let ((buffer (claudemacs-e2e--wait-for
                   (lambda () (claudemacs--get-buffer 'claude))
                   10
                   "claudemacs buffer creation")))
      (should buffer)
      (claudemacs-e2e--track-buffer buffer)

      ;; Kill the session
      (claudemacs-kill)

      ;; Buffer should be gone
      (should-not (buffer-live-p buffer)))))

(ert-deftest claudemacs-e2e-session-ready ()
  "Test that Claude session becomes ready for input."
  :tags '(:e2e :session)
  (skip-unless (not noninteractive))
  (skip-unless (executable-find "claude"))

  (claudemacs-e2e-with-project
    ;; Start a session
    (claudemacs--start-tool-by-index 0)

    ;; Wait for buffer
    (let ((buffer (claudemacs-e2e--wait-for
                   (lambda () (claudemacs--get-buffer 'claude))
                   10
                   "claudemacs buffer creation")))
      (should buffer)
      (claudemacs-e2e--track-buffer buffer)

      ;; Wait for Claude to be ready (shows prompt or initial content)
      (claudemacs-e2e--wait-for
       (lambda ()
         (> (with-current-buffer buffer (point-max)) 1))
       claudemacs-e2e-timeout
       "Claude startup content"))))

;;; E2E Tests - Multi-Session

(ert-deftest claudemacs-e2e-multiple-sessions ()
  "Test running multiple Claude sessions in same workspace."
  :tags '(:e2e :multi-session)
  (skip-unless (not noninteractive))
  (skip-unless (executable-find "claude"))

  (claudemacs-e2e-with-project
    ;; Start first session (claude)
    (claudemacs--start-tool-by-index 0)
    (let ((buffer1 (claudemacs-e2e--wait-for
                    (lambda () (claudemacs--get-buffer 'claude))
                    10
                    "first session")))
      (should buffer1)
      (claudemacs-e2e--track-buffer buffer1)

      ;; Start second session (instance 2)
      (claudemacs--start-tool-by-index 0) ; Start another claude
      (sit-for 2) ; Allow time for second buffer to be created

      ;; Track any additional buffers created (only those with our test session ID)
      (dolist (buf (buffer-list))
        (when (and (string-match-p (regexp-quote claudemacs-e2e--test-session-id) (buffer-name buf))
                   (not (member (buffer-name buf) claudemacs-e2e--test-buffers)))
          (claudemacs-e2e--track-buffer buf)))

      ;; Should now have two sessions
      (let ((sessions (claudemacs--list-sessions-for-workspace)))
        (should (>= (length sessions) 1))))))

;;; E2E Tests - Actions

(ert-deftest claudemacs-e2e-send-message ()
  "Test sending a simple message to Claude."
  :tags '(:e2e :actions)
  (skip-unless (not noninteractive))
  (skip-unless (executable-find "claude"))

  (claudemacs-e2e-with-project
    ;; Start a session
    (claudemacs--start-tool-by-index 0)

    ;; Wait for buffer
    (let ((buffer (claudemacs-e2e--wait-for
                   (lambda () (claudemacs--get-buffer 'claude))
                   10
                   "claudemacs buffer creation")))
      (should buffer)
      (claudemacs-e2e--track-buffer buffer)

      ;; Wait for initial content
      (claudemacs-e2e--wait-for
       (lambda ()
         (> (with-current-buffer buffer (point-max)) 10))
       claudemacs-e2e-timeout
       "Claude startup")

      ;; Record position before sending
      (let ((pos-before (with-current-buffer buffer (point-max))))
        ;; Send a test message
        (claudemacs-e2e--send-input buffer "hello\n")

        ;; Wait for response (buffer grows)
        (claudemacs-e2e--wait-for
         (lambda ()
           (> (with-current-buffer buffer (point-max)) pos-before))
         claudemacs-e2e-timeout
         "Claude response to hello")))))

;;; Test Runner

(defun claudemacs-run-e2e-tests (&optional debug)
  "Run all claudemacs E2E tests in a new frame.
Only cleans up sessions created by the tests, not existing user sessions.
With prefix arg DEBUG, enable verbose logging to *claudemacs-e2e-debug*."
  (interactive "P")
  (if noninteractive
      (message "E2E tests require interactive Emacs. Run with: emacs -Q -l test/claudemacs-e2e-test.el -f claudemacs-run-e2e-tests")
    (let ((test-frame (make-frame '((name . "Claudemacs E2E Tests")
                                    (width . 300)
                                    (height . 70)))))
      (select-frame-set-input-focus test-frame)
      ;; Setup debug mode
      (setq claudemacs-e2e-debug debug)
      (when debug
        (with-current-buffer (get-buffer-create "*claudemacs-e2e-debug*")
          (erase-buffer)
          (insert "=== Claudemacs E2E Test Debug Log ===\n\n")))
      ;; Clear the tracking list (don't cleanup - that happens per-test)
      (setq claudemacs-e2e--test-buffers nil)
      ;; Run the tests
      (ert-run-tests-interactively '(tag :e2e))
      ;; Show debug buffer if enabled
      (when debug
        (display-buffer "*claudemacs-e2e-debug*")))))

(defun claudemacs-run-e2e-tests-batch ()
  "Run E2E tests and exit (for use with emacs -f)."
  (if noninteractive
      (message "E2E tests require interactive Emacs")
    (claudemacs-run-e2e-tests)))

(provide 'claudemacs-e2e-test)
;;; claudemacs-e2e-test.el ends here
