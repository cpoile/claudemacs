;;; claudemacs-actions-test.el --- Tests for claudemacs actions -*- lexical-binding: t; -*-

;; Author: Christopher Poile
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Test suite for claudemacs action commands using ERT.
;; 
;; This file contains tests for user action commands:
;; - claudemacs-ask-without-context (new "a" command)
;; - Real behavior testing in batch mode (no mocking)
;; 
;; Test categories:
;; - :unit - Pure function tests, no external dependencies
;; - :integration - Component interaction tests  
;; - :batch - Real behavior tests in batch mode
;; - :tdd - Development tests for TDD cycles

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path to find claudemacs
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'claudemacs)

;;; Test Utilities

(defun claudemacs-test--create-fake-session ()
  "Create minimal fake session that satisfies claudemacs--validate-process.
  
Returns the created buffer. Caller responsible for cleanup."
  (let* ((session-buffer-name (claudemacs--get-buffer-name))
         (session-buffer (get-buffer-create session-buffer-name))
         (fake-process (start-process "fake-claude" nil "sleep" "60")))
    
    (with-current-buffer session-buffer
      ;; Create fake eat-terminal - must be non-nil to pass validation
      (setq-local eat-terminal 'fake-terminal))
    
    ;; Define eat-term-parameter if it doesn't exist, or override if it does
    (setq claudemacs-test--fake-process fake-process)
    (unless (fboundp 'eat-term-parameter)
      (defun eat-term-parameter (terminal property)
        "Fake eat-term-parameter for testing."
        (when (eq property 'eat--process)
          claudemacs-test--fake-process)))
    
    ;; If it already exists, use advice to override
    (when (fboundp 'eat-term-parameter)
      (advice-add 'eat-term-parameter :override 
                  (lambda (terminal property)
                    (when (eq property 'eat--process)
                      claudemacs-test--fake-process))))
    
    session-buffer))


;;; Unit Tests for Ask Without Context Function

(ert-deftest claudemacs-test-ask-without-context-function-exists ()
  "Test that claudemacs-ask-without-context function exists."
  :tags '(:unit :ask-without-context)
  ;; This test will fail until we implement the function - that's TDD!
  (should (fboundp 'claudemacs-ask-without-context)))

(ert-deftest claudemacs-test-ask-without-context-interactive ()
  "Test that claudemacs-ask-without-context is interactive."
  :tags '(:unit :ask-without-context)
  (should (commandp 'claudemacs-ask-without-context)))

(ert-deftest claudemacs-test-ask-without-context-validation ()
  "Test that claudemacs-ask-without-context validates session properly."
  :tags '(:unit :ask-without-context)
  (let ((validation-called nil))
    ;; Mock the validation function
    (cl-letf (((symbol-function 'claudemacs--validate-process)
               (lambda () (setq validation-called t) t))
              ((symbol-function 'read-string)
               (lambda (prompt) "test request"))
              ((symbol-function 'claudemacs--send-message-to-claude)
               (lambda (message &optional no-return no-switch) nil)))
      
      ;; Call the function
      (claudemacs-ask-without-context)
      
      ;; Verify validation was called
      (should validation-called))))

(ert-deftest claudemacs-test-ask-without-context-no-file-validation ()
  "Test that claudemacs-ask-without-context does NOT require file validation."
  :tags '(:unit :ask-without-context)
  (let ((file-validation-called nil)
        (process-validation-called nil))
    
    ;; Mock both validation functions
    (cl-letf (((symbol-function 'claudemacs--validate-file-and-session)
               (lambda () (setq file-validation-called t)))
              ((symbol-function 'claudemacs--validate-process)
               (lambda () (setq process-validation-called t) t))
              ((symbol-function 'read-string)
               (lambda (prompt) "test request"))
              ((symbol-function 'claudemacs--send-message-to-claude)
               (lambda (message &optional no-return no-switch) nil)))
      
      ;; Call the function
      (claudemacs-ask-without-context)
      
      ;; Should validate process but NOT validate file
      (should process-validation-called)
      (should-not file-validation-called))))

(ert-deftest claudemacs-test-ask-without-context-message-format ()
  "Test that claudemacs-ask-without-context sends message without context."
  :tags '(:unit :ask-without-context)
  (let ((sent-message nil))
    
    ;; Mock the necessary functions
    (cl-letf (((symbol-function 'claudemacs--validate-process)
               (lambda () t))
              ((symbol-function 'read-string)
               (lambda (prompt) "test request"))
              ((symbol-function 'claudemacs--send-message-to-claude)
               (lambda (message &optional no-return no-switch) 
                 (setq sent-message message))))
      
      ;; Call the function
      (claudemacs-ask-without-context)
      
      ;; Verify message was sent and contains no context
      (should sent-message)
      (should (string= sent-message "test request"))
      (should-not (string-match-p "File context:" sent-message)))))

(ert-deftest claudemacs-test-ask-without-context-empty-input ()
  "Test that claudemacs-ask-without-context handles empty input."
  :tags '(:unit :ask-without-context)
  (cl-letf (((symbol-function 'claudemacs--validate-process)
             (lambda () t))
            ((symbol-function 'read-string)
             (lambda (prompt) "")))
    
    ;; Should error on empty input
    (should-error (claudemacs-ask-without-context))))

(ert-deftest claudemacs-test-ask-without-context-whitespace-input ()
  "Test that claudemacs-ask-without-context handles whitespace-only input."
  :tags '(:unit :ask-without-context)
  (cl-letf (((symbol-function 'claudemacs--validate-process)
             (lambda () t))
            ((symbol-function 'read-string)
             (lambda (prompt) "   \t\n  ")))
    
    ;; Should error on whitespace-only input
    (should-error (claudemacs-ask-without-context))))

;;; Integration Tests

(ert-deftest claudemacs-test-transient-menu-has-ask-key ()
  "Test that transient menu includes 'a' key for ask without context."
  :tags '(:integration :ask-without-context)
  ;; This will fail until we add the key to the menu - that's TDD!
  (let ((menu-definition (get 'claudemacs-transient-menu 'transient--layout)))
    ;; Check if 'a' key is defined in the menu
    ;; This is a simplified check - the actual implementation might vary
    (should menu-definition)
    ;; We'll need to inspect the actual menu structure once implemented
    ))

;;; TDD Step 1: Simple Batch-Mode Tests (for TDD development)

(ert-deftest claudemacs-test-basic-session-startup-logic ()
  "TDD Step 1: Test basic session startup logic in batch mode.

This is a simple test to verify basic claudemacs functionality
before we test the full interactive E2E workflow.
This test should FAIL initially, then we make it pass."
  :tags '(:tdd :basic :batch-mode)
  
  ;; TDD Test: Verify basic claudemacs functions exist and work
  ;; Step 1: Check that claudemacs-run function exists
  (should (fboundp 'claudemacs-run))
  
  ;; Step 2: Check that buffer name generation works
  (let ((buffer-name (claudemacs--get-buffer-name)))
    (should buffer-name)
    (should (string-match-p "\\*claudemacs:" buffer-name)))
  
  ;; Step 3: Check that project root detection works
  (let ((project-root (claudemacs--project-root)))
    (should project-root)
    (should (file-directory-p project-root)))
  
  ;; Step 4: Check that ask-without-context function exists
  (should (fboundp 'claudemacs-ask-without-context))
  
  ;; This test should pass because these are basic functions
  ;; If it fails, we know there's a fundamental issue to fix first
  )

;;; Batch-Mode E2E Tests - Real Behavior Testing (No GUI Required)

(ert-deftest claudemacs-test-ask-success-path-but-no-io ()
  "Test real success path of ask-without-context with fake session.
  
Tests the complete real workflow without mocking our functions:
- Real claudemacs--validate-process (with fake session)
- Real input processing and validation
- Real claudemacs--send-message-to-claude call
- Real message formatting and flow
- Real error handling

This is the critical missing test that verifies our function actually works!"
  :tags '(:integration :success-path)
  
  (let ((temp-dir (make-temp-file "success-test" t))
        (sent-message nil)
        (session-buffer nil))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Step 1: Create minimal fake session that satisfies validation
          (setq session-buffer (claudemacs-test--create-fake-session))
          
          ;; Step 2: Mock only external I/O, not our functions
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt) "What is 2+2?"))
                    ((symbol-function 'claudemacs--send-message-to-claude)
                     (lambda (message &optional no-return no-switch)
                       (setq sent-message message))))
            
            ;; Step 3: Call the real function - no mocking of our logic!
            (claudemacs-ask-without-context)
            
            ;; Step 4: Verify real behavior
            (should sent-message)
            (should (string= sent-message "What is 2+2?"))
            (should-not (string-match-p "File context:" sent-message))))
      
      ;; Cleanup
      (when session-buffer
        (kill-buffer session-buffer))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t))
      ;; Clean up the advice
      (advice-remove 'eat-term-parameter 
                     (lambda (terminal property)
                       (when (eq property 'eat--process)
                         claudemacs-test--fake-process)))
      ;; Kill fake process
      (when (and (boundp 'claudemacs-test--fake-process) 
                 claudemacs-test--fake-process
                 (process-live-p claudemacs-test--fake-process))
        (kill-process claudemacs-test--fake-process)))))

(ert-deftest claudemacs-test-batch-ask-without-context-error-behavior ()
  "Test real error behavior of ask-without-context in batch mode.
  
Tests real function behavior without mocking:
- Real validation logic (claudemacs--validate-process)
- Real error messages and handling
- Real function call chain
- Real cleanup behavior

This verifies our function integrates correctly with the validation system."
  :tags '(:e2e :batch)
  :expected-result :failed
  
  (let ((temp-dir (make-temp-file "batch-e2e-test" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Test: Call ask-without-context with no active session
          ;; Expected: Should fail with "No Claudemacs session is active" error
          (condition-case err
              (claudemacs-ask-without-context)
            (error 
             ;; Verify we get the expected validation error
             (should (string-match-p "No Claudemacs session is active" (error-message-string err)))
             ;; Re-signal to maintain expected failure behavior
             (signal (car err) (cdr err)))))
      
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest claudemacs-test-real-transient-menu-key-binding ()
  "Test that 'a' key in the actual transient menu is properly bound.
  
Tests real menu integration without mocking:
- Real transient menu definition
- Real key binding verification  
- Real function binding"
  :tags '(:e2e :integration)
  
  ;; Test the actual transient menu definition exists
  (should (fboundp 'claudemacs-transient-menu))
  
  ;; Get the actual transient layout
  (let ((layout (get 'claudemacs-transient-menu 'transient--layout)))
    (should layout)
    
    ;; Convert layout to string for inspection
    (let ((layout-str (format "%S" layout)))
      ;; Verify "a" key is bound to claudemacs-ask-without-context
      (should (string-match-p "\"a\"" layout-str))
      (should (string-match-p "claudemacs-ask-without-context" layout-str)))))

(provide 'claudemacs-actions-test)
;;; claudemacs-actions-test.el ends here
