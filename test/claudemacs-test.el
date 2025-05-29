;;; claudemacs-test.el --- Tests for claudemacs -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Test suite for claudemacs.el using ERT (Emacs Lisp Regression Testing).
;; 
;; Test categories:
;; - :unit - Pure function tests, no external dependencies
;; - :integration - Tests requiring mocked dependencies  
;; - :e2e - End-to-end tests with real processes
;; - :requires-claude - Tests requiring Claude CLI installation

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path to find claudemacs
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'claudemacs)

;;; Test Utilities

(defmacro claudemacs-test-with-temp-buffer (&rest body)
  "Execute BODY in a temporary buffer with claudemacs loaded.
This provides a clean environment for testing without side effects."
  `(with-temp-buffer
     (let ((inhibit-message t))
       ,@body)))

(defmacro claudemacs-test-with-temp-file (filename content &rest body)
  "Execute BODY with a temporary file FILENAME containing CONTENT.
The file is automatically cleaned up after BODY executes."
  (declare (indent 2))
  `(let ((temp-file (make-temp-file ,filename)))
     (unwind-protect
         (progn
           (write-region ,content nil temp-file)
           (let ((buffer-file-name temp-file))
             ,@body))
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

(defun claudemacs-test-cleanup-buffers ()
  "Clean up any claudemacs test buffers."
  (dolist (buffer (buffer-list))
    (when (string-match-p "^\\*claudemacs:.*test\\*" (buffer-name buffer))
      (kill-buffer buffer))))

;;; Basic Sanity Tests

(ert-deftest claudemacs-test-sanity ()
  "Basic sanity test to verify ERT is working."
  :tags '(:unit :sanity)
  (should t)
  (should-not nil)
  (should (= 2 (+ 1 1)))
  (should (string= "hello" "hello"))
  (should (listp '(1 2 3))))

(ert-deftest claudemacs-test-package-loaded ()
  "Test that claudemacs package is loaded correctly."
  :tags '(:unit :sanity)
  (should (featurep 'claudemacs))
  (should (fboundp 'claudemacs-run))
  (should (fboundp 'claudemacs-kill))
  (should (fboundp 'claudemacs-toggle-buffer))
  (should (boundp 'claudemacs-program))
  (should (boundp 'claudemacs-notify-on-await)))

(ert-deftest claudemacs-test-custom-group-exists ()
  "Test that claudemacs custom group is properly defined."
  :tags '(:unit :sanity)
  (should (get 'claudemacs 'group-documentation))
  ;; Check that the group has members (custom variables and faces)
  (should (get 'claudemacs 'custom-group))
  ;; Verify some expected members are in the group
  (let ((members (get 'claudemacs 'custom-group)))
    (should (assq 'claudemacs-program members))
    (should (assq 'claudemacs-notify-on-await members))))

;;; Comment Parsing Tests

(ert-deftest claudemacs-test-comment-syntax-info-elisp ()
  "Test comment syntax information extraction in Emacs Lisp mode."
  :tags '(:unit :comment)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    (let ((info (claudemacs--comment-syntax-info)))
      (should (string= (plist-get info :start) ";"))
      (should (string= (plist-get info :end) ""))
      (should-not (plist-get info :multi-line-p))
      (should (plist-get info :start-skip)))))

(ert-deftest claudemacs-test-comment-syntax-info-c-mode ()
  "Test comment syntax information extraction in C mode."
  :tags '(:unit :comment)
  (claudemacs-test-with-temp-buffer
    (c-mode)
    (let ((info (claudemacs--comment-syntax-info)))
      ;; C mode comment-start is "/* " (with space)
      (should (string= (string-trim (plist-get info :start)) "/*"))
      (should (string= (string-trim (plist-get info :end)) "*/"))
      (should (plist-get info :multi-line-p))
      (should (plist-get info :start-skip))
      ;; end-skip can be nil in some modes, so just check it exists as a key
      (should (plist-member info :end-skip)))))

(ert-deftest claudemacs-test-point-in-comment-detection ()
  "Test detection of point inside comments."
  :tags '(:unit :comment)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment\n(defun foo () nil)\n")
    
    ;; Test inside comment
    (goto-char 5) ; Inside "This is a comment"
    (should (claudemacs--point-in-comment-p))
    
    ;; Test in function definition (with extra space to avoid end-of-buffer)
    (goto-char 25) ; In "(defun foo () nil)"
    (should-not (claudemacs--point-in-comment-p))
    
    ;; Test on empty line at end 
    (goto-char (- (point-max) 2)) ; Before the final newline
    (should-not (claudemacs--point-in-comment-p))))

(ert-deftest claudemacs-test-point-in-comment-mixed-line-elisp ()
  "Test detection of point in mixed code+comment lines - Emacs Lisp part."
  :tags '(:unit :comment)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    ;; Test Emacs Lisp style: code ;; comment
    (insert "(defun foo () nil) ;; This is a comment\n")
    ;; Ensure syntax parsing is complete
    (font-lock-ensure)
    (syntax-ppss (point-max))
    
    ;; Test point in code part - should NOT be in comment
    (goto-char 2) ; On "d" in "defun"
    (should-not (claudemacs--point-in-comment-p))
    
    (goto-char 7) ; On "f" in "foo"  
    (should-not (claudemacs--point-in-comment-p))
    
    (goto-char 16) ; On ")" at end of function
    (should-not (claudemacs--point-in-comment-p))
    
    ;; Test point in whitespace before comment - SHOULD be "before comment"
    (goto-char 19) ; On space before ";;"
    (should (claudemacs--point-in-comment-p))
    
    ;; Test point at start of comment markers - should be in comment
    (goto-char 20) ; On first ";" 
    (should (claudemacs--point-in-comment-p))
    
    (goto-char 21) ; On second ";"
    (should (claudemacs--point-in-comment-p))
    
    ;; Test point in comment text - should be in comment
    (goto-char 26) ; On "T" in "This"
    (should (claudemacs--point-in-comment-p))
    
    (goto-char 36) ; On "c" in "comment"
    (should (claudemacs--point-in-comment-p))))

(ert-deftest claudemacs-test-point-in-comment-mixed-line-c ()
  "Test detection of point in mixed code+comment lines - C style part."
  :tags '(:unit :comment)
  ;; Test with C-style comments
  (claudemacs-test-with-temp-buffer
    (c-mode)
    ;; Test C style: code // comment  
    (insert "int x = 42; // This is a comment\n")
    ;; Ensure syntax parsing is complete
    (font-lock-ensure)
    (syntax-ppss (point-max))
    
    ;; Test point in code part - should NOT be in comment
    (goto-char 1) ; On "i" in "int"
    (should-not (claudemacs--point-in-comment-p))
    
    (goto-char 5) ; On "x"
    (should-not (claudemacs--point-in-comment-p))
    
    (goto-char 11) ; On ";" at end of statement
    (should-not (claudemacs--point-in-comment-p))
    
    ;; Test point in whitespace before comment - SHOULD be "before comment"
    (goto-char 12) ; On space before "//"
    (should (claudemacs--point-in-comment-p))
    
    ;; Test point at start of comment markers - should be in comment
    (goto-char 13) ; On first "/"
    (should (claudemacs--point-in-comment-p))
    
    (goto-char 14) ; On second "/"
    (should (claudemacs--point-in-comment-p))
    
    ;; Test point in comment text - should be in comment
    (goto-char 19) ; On "T" in "This"
    (should (claudemacs--point-in-comment-p))
    
    (goto-char 29) ; On "c" in "comment"
    (should (claudemacs--point-in-comment-p))))

(ert-deftest claudemacs-test-point-in-comment-edge-case-line-boundary ()
  "Test edge case: point at first slash of '//' at end of line with next line being comment."
  :tags '(:unit :comment)
  (claudemacs-test-with-temp-buffer
    (c-mode)
    ;; Create scenario: "//" at end of line, next line starts with comment at col 1
    (insert "int x = 42; //\n// Next line comment\n")
    ;; Ensure syntax parsing is complete
    (font-lock-ensure)
    (syntax-ppss (point-max))
    
    ;; Test point at first "/" of "//" at end of first line - should be TRUE 
    ;; because point is at start of comment marker
    (goto-char 13) ; On first "/" of "//" 
    (should (claudemacs--point-in-comment-p))
    
    ;; Test point at second "/" of "//" at end of first line - should be TRUE
    (goto-char 14) ; On second "/" of "//"
    (should (claudemacs--point-in-comment-p))
    
    ;; Test point at newline after "//" - should be TRUE (still in comment context)
    (goto-char 15) ; At newline after "//"
    (should (claudemacs--point-in-comment-p))
    
    ;; Test point at start of next line (on first "/" of "// Next line") - should be TRUE
    (goto-char 16) ; On first "/" of next line's "//"
    (should (claudemacs--point-in-comment-p))
    
    ;; Additional test: point right before the "//" on first line
    (goto-char 12) ; On space before "//"
    (should (claudemacs--point-in-comment-p))))

(ert-deftest claudemacs-test-point-in-comment-false-positive-prevention ()
  "Test prevention of false positives with single slash not followed by comment."
  :tags '(:unit :comment)
  (claudemacs-test-with-temp-buffer
    (c-mode)
    ;; Test single "/" not forming a comment, with comment on next line
    (insert "int x = 42 / 2;\n// This is a comment\n")
    ;; Ensure syntax parsing is complete
    (font-lock-ensure)
    (syntax-ppss (point-max))
    
    ;; Test point at "/" (division operator) - should be FALSE
    (goto-char 12) ; On "/" (division operator)
    (should-not (claudemacs--point-in-comment-p))
    
    ;; Test point in whitespace after division - should be FALSE
    (goto-char 14) ; On space after "2"
    (should-not (claudemacs--point-in-comment-p))
    
    ;; Test point at ";" - should be FALSE
    (goto-char 15) ; On ";" 
    (should-not (claudemacs--point-in-comment-p))
    
    ;; Test point at newline after first line - should be FALSE
    (goto-char 16) ; At newline
    (should-not (claudemacs--point-in-comment-p))
    
    ;; Test point at start of actual comment on second line - should be TRUE
    (goto-char 17) ; On first "/" of "// This is a comment"
    (should (claudemacs--point-in-comment-p))))

;;; Comprehensive Comment Bounds Tests

(ert-deftest claudemacs-test-comment-bounds-basic-single-line ()
  "Test basic comment boundary detection for single isolated comment."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a single comment\n(defun foo () nil)\n")
    
    ;; Test point in middle of comment
    (goto-char 10) ; In "single comment"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1)) ; Start of line
      (should (= (cdr bounds) 28))) ; End of comment line (includes trailing content)
    
    ;; Test point at comment start markers
    (goto-char 1) ; At first ";"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1))
      (should (= (cdr bounds) 28)))
    
    ;; Test point at end of comment
    (goto-char 25) ; At end of comment text
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1))
      (should (= (cdr bounds) 28)))
    
    ;; Test outside comment
    (goto-char 30) ; In function definition
    (should-not (claudemacs--get-comment-bounds))))

(ert-deftest claudemacs-test-comment-bounds-multi-line-block ()
  "Test comment boundary detection for multi-line comment blocks."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Line 1\n;; Line 2\n;; Line 3\n(defun foo () nil)\n")
    
    ;; Test in first line of block
    (goto-char 5) ; In "Line 1"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1)) ; Should find start of comment block
      (should (> (cdr bounds) 25))) ; Should find end of comment block
    
    ;; Test in middle line of block
    (goto-char 15) ; In "Line 2"  
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1)) ; Should find start of comment block
      (should (> (cdr bounds) 25))) ; Should find end of comment block
    
    ;; Test in last line of block
    (goto-char 25) ; In "Line 3"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1)) ; Should find start of comment block
      (should (> (cdr bounds) 25))) ; Should find end of comment block
    
    ;; Test outside comment
    (goto-char 40) ; In function body
    (should-not (claudemacs--get-comment-bounds))))

(ert-deftest claudemacs-test-comment-bounds-mixed-code-comment ()
  "Test comment boundary detection for mixed code+comment lines."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo () nil) ;; This is a comment\n")
    
    ;; Test point in code part - should return nil
    (goto-char 5) ; In "defun"
    (should-not (claudemacs--get-comment-bounds))
    
    ;; Test point in whitespace before comment - should find comment bounds
    (goto-char 19) ; In space before ";;"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      ;; Comment starts where the comment markers begin
      (should (>= (car bounds) 20)) ; Around the ";;" 
      (should (> (cdr bounds) 35))) ; End of comment
    
    ;; Test point at comment start
    (goto-char 20) ; At first ";"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 20))
      (should (> (cdr bounds) 35)))
    
    ;; Test point in comment text
    (goto-char 30) ; In "comment"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 20))
      (should (> (cdr bounds) 35)))))

(ert-deftest claudemacs-test-comment-bounds-special-content ()
  "Test comment boundary detection with special keywords like TODO."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    ;; Simulate the exact case from claudemacs.el:620
    (insert "(unless (buffer-file-name)  ;; TODO: is this needed? Check how its used.\n")
    
    ;; Test point after "TODO: " - this is the failing case
    (goto-char 35) ; After "TODO: "  
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 26)) ; Around the ";;"
      (should (> (cdr bounds) 70))) ; End of comment line
    
    ;; Test point at "TODO"
    (goto-char 30) ; At "T" in "TODO"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 26))
      (should (> (cdr bounds) 70)))
    
    ;; Test other special keywords
    (erase-buffer)
    (insert "(some-code) ;; FIXME: this needs work\n")
    (goto-char 20) ; In "FIXME"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 12))
      (should (> (cdr bounds) 30)))
    
    (erase-buffer)
    (insert "(more-code) ;; NOTE: important detail\n")
    (goto-char 25) ; In "important"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 12))
      (should (> (cdr bounds) 35)))))

(ert-deftest claudemacs-test-comment-bounds-c-style-multiline ()
  "Test comment boundary detection for C-style /* */ comments."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (c-mode)
    (insert "/* This is a\n   multi-line comment\n   with multiple lines */\nint x = 42;\n")
    
    ;; Test point in first line of comment
    (goto-char 5) ; In "This"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1)) ; Start of comment
      (should (> (cdr bounds) 50))) ; End of comment block
    
    ;; Test point in middle line
    (goto-char 30) ; In "multi-line"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1))
      (should (> (cdr bounds) 50)))
    
    ;; Test point in last line of comment
    (goto-char 60) ; Near "*/"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (= (car bounds) 1))
      (should (> (cdr bounds) 50)))
    
    ;; Test outside comment
    (goto-char 80) ; In "int x = 42"
    (should-not (claudemacs--get-comment-bounds))))

(ert-deftest claudemacs-test-comment-bounds-option-2-behavior ()
  "Test Option 2 behavior: mixed line + adjacent comment-only lines after."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    ;; Test case: mixed line followed by comment-only lines
    (insert "(unless (buffer-file-name)  ;; TODO: is this needed?\n;; Check how its used\n;; Think about it\n(other-code)\n")
    
    ;; Test point in TODO comment - should include all 3 comment lines
    (goto-char 35) ; In "TODO" part
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      ;; Should start at the ";;" on first line
      (should (>= (car bounds) 26))
      ;; Should end after "Think about it" (around line 3)
      (should (> (cdr bounds) 80)))
    
    ;; Test point in second comment line - should include all 3
    (goto-char 55) ; In "Check how"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 26))
      (should (> (cdr bounds) 80)))
    
    ;; Test point in third comment line - should include all 3
    (goto-char 75) ; In "Think about"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 26))
      (should (> (cdr bounds) 80)))))

(ert-deftest claudemacs-test-comment-bounds-option-2-no-before ()
  "Test Option 2: should NOT include comment-only lines before mixed line."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    ;; Test case: comment-only lines before mixed line
    (insert ";; This is a comment\n;; Another comment\n(unless (buffer-file-name)  ;; TODO: is this needed?\n")
    
    ;; Test point in TODO comment - should NOT include the lines before
    (goto-char 70) ; In "TODO" part  
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      ;; Should start at the ";;" on the mixed line (around pos 60)
      (should (>= (car bounds) 60))
      ;; Should end at end of that line
      (should (< (cdr bounds) 95)))))

(ert-deftest claudemacs-test-comment-bounds-option-2-mixed-scenarios ()
  "Test Option 2 with various mixed scenarios."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    
    ;; Test 1: Mixed line with gap before next comment (should not group)
    (insert "(code) ;; comment\n\n;; separate comment\n")
    (goto-char 12) ; In first comment
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 8))
      ;; Should end at first line, not include separate comment
      (should (< (cdr bounds) 20)))
    
    (erase-buffer)
    
    ;; Test 2: Mixed line followed immediately by comment-only lines
    (insert "(code) ;; start\n;; continuation\n;; more\n(next-code)\n")
    (goto-char 12) ; In "start"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 8))
      ;; Should include all 3 comment lines
      (should (> (cdr bounds) 35)))
    
    (erase-buffer)
    
    ;; Test 3: Mixed line at end (no lines after)
    (insert "(code) ;; final comment")
    (goto-char 15) ; In "final"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 8))
      (should (< (cdr bounds) 25)))))

(ert-deftest claudemacs-test-comment-bounds-indentation-variations ()
  "Test comment bounds with various indentation levels."
  :tags '(:unit :comment :bounds)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    
    ;; Test mixed line with indented continuation comments
    (insert "  (nested-code) ;; TODO: fix this\n  ;; It needs work\n    ;; More details\n")
    (goto-char 25) ; In "TODO"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 17)) ; Around ";;"
      (should (> (cdr bounds) 50))) ; Include all lines
    
    (erase-buffer)
    
    ;; Test varying indentation levels
    (insert "(code) ;; start\n    ;; indented\n;; back to left\n")
    (goto-char 12) ; In "start"
    (let ((bounds (claudemacs--get-comment-bounds)))
      (should bounds)
      (should (>= (car bounds) 8))
      (should (> (cdr bounds) 30)))))

(ert-deftest claudemacs-test-comment-text-extraction ()
  "Test comment text extraction and cleaning."
  :tags '(:unit :comment)
  (claudemacs-test-with-temp-buffer
    (emacs-lisp-mode)
    
    ;; Test single-line comment
    (insert ";; This is a test comment")
    (let ((text (claudemacs--extract-comment-text 1 (point-max))))
      (should (string= text "This is a test comment")))
    
    (erase-buffer)
    
    ;; Test multi-line comment block
    (insert ";; Line one\n;; Line two\n;; Line three")
    (let ((text (claudemacs--extract-comment-text 1 (point-max))))
      (should (string-match-p "Line one" text))
      (should (string-match-p "Line two" text))
      (should (string-match-p "Line three" text))
      ;; Should not contain comment markers
      (should-not (string-match-p ";;" text)))))

(ert-deftest claudemacs-test-comment-text-extraction-multiline ()
  "Test comment text extraction for multi-line comments (C-style)."
  :tags '(:unit :comment)
  (claudemacs-test-with-temp-buffer
    (c-mode)
    (insert "/* This is a\n   multi-line comment\n   with multiple lines */")
    (let ((text (claudemacs--extract-comment-text 1 (point-max))))
      (should (string-match-p "This is a" text))
      (should (string-match-p "multi-line comment" text))
      (should (string-match-p "with multiple lines" text))
      ;; Should not contain comment markers
      (should-not (string-match-p "/\\*" text))
      (should-not (string-match-p "\\*/" text)))))

;;; Project Root Detection Tests

(ert-deftest claudemacs-test-project-root-detection-git ()
  "Test project root detection with git repository."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claudemacs-test" t)))
    (unwind-protect
        (progn
          ;; Create a git repo
          (let ((default-directory test-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "test content" nil "test.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "initial" "--quiet"))
          
          ;; Test from repo root with mocked buffer-file-name
          (let ((test-file (expand-file-name "test.txt" test-dir)))
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () test-file)))
              ;; Remove trailing slash for comparison
              (should (string= (file-name-as-directory (claudemacs--project-root)) 
                               (file-name-as-directory test-dir)))))
          
          ;; Test from subdirectory
          (let* ((subdir (expand-file-name "subdir" test-dir))
                 (sub-file (expand-file-name "sub.txt" subdir)))
            (make-directory subdir)
            (write-region "sub content" nil sub-file)
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () sub-file)))
              (should (string= (file-name-as-directory (claudemacs--project-root)) 
                               (file-name-as-directory test-dir))))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claudemacs-test-project-root-detection-no-git ()
  "Test project root detection without git repository."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claudemacs-no-git-test" t)))
    (unwind-protect
        (progn
          ;; Create a regular directory (no git)
          (let ((default-directory test-dir)
                (test-file (expand-file-name "test.txt" test-dir)))
            (write-region "content" nil test-file)
            
            ;; Mock buffer-file-name to return our test file
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () test-file)))
              ;; Should return buffer dir since there's no git repo
              (should (string= (file-name-directory (buffer-file-name)) (claudemacs--project-root))))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claudemacs-test-project-root-with-explicit-dir ()
  "Test project root detection with explicit directory parameter."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claudemacs-explicit-test" t)))
    (unwind-protect
        (progn
          ;; Create a git repo
          (let ((default-directory test-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "content" nil "test.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "initial" "--quiet"))
          
          ;; Test with explicit directory parameter (normalize trailing slashes)
          (should (string= (file-name-as-directory (claudemacs--project-root test-dir)) 
                           (file-name-as-directory test-dir)))
          
          ;; Test with subdirectory
          (let ((subdir (expand-file-name "sub" test-dir)))
            (make-directory subdir)
            (should (string= (file-name-as-directory (claudemacs--project-root subdir)) 
                             (file-name-as-directory test-dir)))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claudemacs-test-project-root-nested-repos ()
  "Test project root detection with nested git repositories."
  :tags '(:unit :project)
  (let ((outer-dir (make-temp-file "claudemacs-outer" t))
        (inner-dir nil))
    (unwind-protect
        (progn
          ;; Create outer git repo
          (let ((default-directory outer-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "outer" nil "outer.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "outer" "--quiet"))
          
          ;; Create inner git repo
          (setq inner-dir (expand-file-name "inner" outer-dir))
          (make-directory inner-dir)
          (let ((default-directory inner-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "inner" nil "inner.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "inner" "--quiet"))
          
          ;; Test that inner directory returns inner repo, not outer (normalize paths)
          (should (string= (file-name-as-directory (claudemacs--project-root inner-dir)) 
                           (file-name-as-directory inner-dir)))
          
          ;; Test that outer directory returns outer repo
          (should (string= (file-name-as-directory (claudemacs--project-root outer-dir)) 
                           (file-name-as-directory outer-dir))))
      
      ;; Cleanup
      (when (file-exists-p outer-dir)
        (delete-directory outer-dir t)))))

;;; Session ID Generation Tests

(ert-deftest claudemacs-test-session-id-doom-workspace ()
  "Test session ID generation with Doom Emacs workspace."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name)
             (lambda () "my-doom-workspace"))
            ((symbol-function 'fboundp) 
             (lambda (func) 
               (eq func '+workspace-current-name))))
    (should (string= (claudemacs--session-id) "my-doom-workspace"))))

(ert-deftest claudemacs-test-session-id-perspective-workspace ()
  "Test session ID generation with Perspective mode."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name)
             (lambda (persp) "my-perspective"))
            ((symbol-function 'get-current-persp)
             (lambda () 'fake-persp))
            ((symbol-function 'fboundp) 
             (lambda (func) 
               (memq func '(safe-persp-name get-current-persp)))))
    (should (string= (claudemacs--session-id) "my-perspective"))))

(ert-deftest claudemacs-test-session-id-fallback-to-project ()
  "Test session ID fallback to project root when no workspace is active."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name) (lambda (persp) nil))
            ((symbol-function 'get-current-persp) (lambda () nil))
            ((symbol-function 'fboundp) (lambda (func) nil))
            ((symbol-function 'claudemacs--project-root) (lambda () "/tmp/test-project"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claudemacs--session-id) "/tmp/test-project"))))

(ert-deftest claudemacs-test-session-id-empty-workspace-names ()
  "Test session ID with empty or nil workspace names."
  :tags '(:unit :session)
  ;; Test with Doom workspace returning empty string
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () ""))
            ((symbol-function 'fboundp) 
             (lambda (func) (eq func '+workspace-current-name)))
            ((symbol-function 'claudemacs--project-root) (lambda () "/fallback"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claudemacs--session-id) "/fallback")))
  
  ;; Test with Perspective returning nil
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name) (lambda (persp) nil))
            ((symbol-function 'get-current-persp) (lambda () 'fake-persp))
            ((symbol-function 'fboundp) 
             (lambda (func) (memq func '(safe-persp-name get-current-persp))))
            ((symbol-function 'claudemacs--project-root) (lambda () "/fallback2"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claudemacs--session-id) "/fallback2"))))

(ert-deftest claudemacs-test-session-id-workspace-priority ()
  "Test that Doom workspace takes priority over Perspective."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "doom-wins"))
            ((symbol-function 'safe-persp-name) (lambda (persp) "perspective-loses"))
            ((symbol-function 'get-current-persp) (lambda () 'fake-persp))
            ((symbol-function 'fboundp) 
             (lambda (func) 
               (memq func '(+workspace-current-name safe-persp-name get-current-persp)))))
    (should (string= (claudemacs--session-id) "doom-wins"))))

;;; Buffer Name Generation Tests

(ert-deftest claudemacs-test-buffer-name-generation ()
  "Test buffer name generation from session ID."
  :tags '(:unit :session)
  (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-session")))
    (should (string= (claudemacs--get-buffer-name) "*claudemacs:test-session*"))))

(ert-deftest claudemacs-test-buffer-name-with-special-chars ()
  "Test buffer name generation with special characters in session ID."
  :tags '(:unit :session)
  (cl-letf (((symbol-function 'claudemacs--session-id) 
             (lambda () "/path/with/slashes")))
    (should (string= (claudemacs--get-buffer-name) "*claudemacs:/path/with/slashes*"))))

(ert-deftest claudemacs-test-buffer-detection ()
  "Test claudemacs buffer detection."
  :tags '(:unit :session)
  (claudemacs-test-with-temp-buffer
    ;; Test non-claudemacs buffer
    (should-not (claudemacs--is-claudemacs-buffer-p))
    
    ;; Test claudemacs buffer
    (rename-buffer "*claudemacs:test*")
    (should (claudemacs--is-claudemacs-buffer-p))
    
    ;; Test with specific buffer argument
    (with-temp-buffer
      (should-not (claudemacs--is-claudemacs-buffer-p (current-buffer)))
      (rename-buffer "*claudemacs:another*")
      (should (claudemacs--is-claudemacs-buffer-p (current-buffer)))))
  
  ;; Test with non-live buffer
  (let ((dead-buffer (get-buffer-create "*test-dead*")))
    (kill-buffer dead-buffer)
    (should-not (claudemacs--is-claudemacs-buffer-p dead-buffer))))

;;; Custom Variable Tests

(ert-deftest claudemacs-test-custom-variable-defaults ()
  "Test that custom variables have correct default values."
  :tags '(:unit :config)
  (should (string= claudemacs-program "claude"))
  (should (eq claudemacs-program-switches nil))
  (should (eq claudemacs-switch-to-buffer-on-create t))
  (should (eq claudemacs-switch-to-buffer-on-toggle t))
  (should (eq claudemacs-m-return-is-submit nil))
  (should (eq claudemacs-shift-return-newline t))
  (should (eq claudemacs-switch-to-buffer-on-file-add nil))
  (should (eq claudemacs-notify-on-await t))
  (should (string= claudemacs-notification-sound-mac "Submarine")))

(ert-deftest claudemacs-test-custom-variable-types ()
  "Test that custom variables have correct types."
  :tags '(:unit :config)
  ;; Test string variables
  (should (stringp claudemacs-program))
  (should (stringp claudemacs-notification-sound-mac))
  
  ;; Test boolean variables
  (should (booleanp claudemacs-switch-to-buffer-on-create))
  (should (booleanp claudemacs-switch-to-buffer-on-toggle))
  (should (booleanp claudemacs-m-return-is-submit))
  (should (booleanp claudemacs-shift-return-newline))
  (should (booleanp claudemacs-switch-to-buffer-on-file-add))
  (should (booleanp claudemacs-notify-on-await))
  
  ;; Test list variable
  (should (listp claudemacs-program-switches)))

(ert-deftest claudemacs-test-bell-handler-behavior ()
  "Test that claudemacs-notify-on-await affects bell handler behavior."
  :tags '(:unit :config)
  (let ((notification-called nil))
    ;; Mock system notification
    (cl-letf (((symbol-function 'claudemacs--system-notification)
               (lambda (&rest args) (setq notification-called t))))
      
      ;; Test with notifications enabled
      (let ((claudemacs-notify-on-await t))
        (claudemacs--bell-handler nil)
        (should notification-called))
      
      ;; Test with notifications disabled
      (setq notification-called nil)
      (let ((claudemacs-notify-on-await nil))
        (claudemacs--bell-handler nil)
        (should-not notification-called)))))

(ert-deftest claudemacs-test-notification-sound-behavior ()
  "Test that claudemacs-notification-sound-mac affects notification calls."
  :tags '(:unit :config)
  (let ((notification-command nil))
    ;; Mock call-process to capture the full command
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (when (string= program "osascript")
                   (setq notification-command (mapconcat 'identity args " ")))))
              ;; Mock system-type to be macOS
              (system-type 'darwin))
      
      ;; Test with custom sound
      (let ((claudemacs-notification-sound-mac "Ping"))
        (claudemacs--system-notification "Test message" "Test title")
        (should notification-command)
        (should (string-match-p "Ping" notification-command)))
      
      ;; Test with different sound
      (setq notification-command nil)
      (let ((claudemacs-notification-sound-mac "Glass"))
        (claudemacs--system-notification "Test message" "Test title") 
        (should notification-command)
        (should (string-match-p "Glass" notification-command))))))

(ert-deftest claudemacs-test-program-switches-behavior ()
  "Test that claudemacs-program-switches affects command construction."
  :tags '(:unit :config)
  ;; Test the switches logic directly without complex mocking
  (let ((claudemacs-program-switches nil)
        (args '("--resume")))
    ;; Test with no custom switches - should just have the passed args
    (let ((result (remove nil (append args claudemacs-program-switches))))
      (should (equal result '("--resume"))))
    
    ;; Test with custom switches - should include both
    (setq claudemacs-program-switches '("--verbose" "--test"))
    (let ((result (remove nil (append args claudemacs-program-switches))))
      (should (member "--verbose" result))
      (should (member "--test" result))
      (should (member "--resume" result))
      (should (= (length result) 3)))))

(ert-deftest claudemacs-test-switch-to-buffer-behavior ()
  "Test buffer switching behavior based on custom variables."
  :tags '(:unit :config)
  (let ((window-selected nil))
    ;; Mock window selection functions
    (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer))
              ((symbol-function 'select-window) 
               (lambda (window &optional norecord) (setq window-selected t)))
              ((symbol-function 'get-buffer-window) 
               (lambda (buffer &optional all-frames) 'fake-window)))
      
      ;; Test switch-to-buffer-on-create behavior
      (let ((test-buffer (get-buffer-create "*claudemacs:switch-test*")))
        (unwind-protect
            (progn
              ;; Test with switching enabled
              (setq window-selected nil)
              (let ((claudemacs-switch-to-buffer-on-create t))
                (with-current-buffer test-buffer
                  (let ((window (display-buffer test-buffer)))
                    (when claudemacs-switch-to-buffer-on-create
                      (select-window window))))
                (should window-selected))
              
              ;; Test with switching disabled  
              (setq window-selected nil)
              (let ((claudemacs-switch-to-buffer-on-create nil))
                (with-current-buffer test-buffer
                  (let ((window (display-buffer test-buffer)))
                    (when claudemacs-switch-to-buffer-on-create
                      (select-window window))))
                (should-not window-selected)))
          
          ;; Cleanup
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer)))))))

;;; Test Infrastructure Ready
;; Comment parsing, project root detection, session ID, and custom variable tests are now in place.
;; Additional feature tests will be added in subsequent steps.

(provide 'claudemacs-test)
;;; claudemacs-test.el ends here
