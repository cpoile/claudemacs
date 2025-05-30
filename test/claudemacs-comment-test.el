;;; claudemacs-comment-test.el --- Tests for claudemacs comment functionality -*- lexical-binding: t; -*-

;; Author: Claude Code (extracted from claudemacs-test.el)
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Test suite for claudemacs-comment.el using ERT (Emacs Lisp Regression Testing).
;; 
;; Test categories:
;; - :unit - Pure function tests, no external dependencies
;; - :integration - Tests requiring mocked dependencies  
;; - :comment - Comment-related functionality tests

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path to find claudemacs-comment
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'claudemacs-comment)

;;; Test Utilities

(defmacro claudemacs-comment-test-with-temp-buffer (&rest body)
  "Execute BODY in a temporary buffer with claudemacs-comment loaded.
This provides a clean environment for testing without side effects."
  `(with-temp-buffer
     (let ((inhibit-message t))
       ,@body)))

;;; Comment Tests

;;; Comment Syntax Information Tests

(ert-deftest claudemacs-test-comment-syntax-info-elisp ()
  "Test comment syntax information extraction in Emacs Lisp mode."
  :tags '(:unit :comment)
  (claudemacs-comment-test-with-temp-buffer
    (emacs-lisp-mode)
    (let ((info (claudemacs--comment-syntax-info)))
      (should (string= (plist-get info :start) ";"))
      (should (string= (plist-get info :end) ""))
      (should-not (plist-get info :multi-line-p))
      (should (plist-get info :start-skip)))))

(ert-deftest claudemacs-test-comment-syntax-info-c-mode ()
  "Test comment syntax information extraction in C mode."
  :tags '(:unit :comment)
  (claudemacs-comment-test-with-temp-buffer
    (c-mode)
    (let ((info (claudemacs--comment-syntax-info)))
      ;; C mode comment-start is "/* " (with space)
      (should (string= (string-trim (plist-get info :start)) "/*"))
      (should (string= (string-trim (plist-get info :end)) "*/"))
      (should (plist-get info :multi-line-p))
      (should (plist-get info :start-skip))
      ;; end-skip can be nil in some modes, so just check it exists as a key
      (should (plist-member info :end-skip)))))

;;; Point in Comment Detection Tests

(ert-deftest claudemacs-test-point-in-comment-detection ()
  "Test detection of point inside comments."
  :tags '(:unit :comment)
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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

;;; Comment Bounds Detection Tests

(ert-deftest claudemacs-test-comment-bounds-basic-single-line ()
  "Test basic comment boundary detection for single isolated comment."
  :tags '(:unit :comment :bounds)
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
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

;;; Comment Text Extraction Tests

(ert-deftest claudemacs-test-comment-text-extraction ()
  "Test comment text extraction and cleaning."
  :tags '(:unit :comment)
  (claudemacs-comment-test-with-temp-buffer
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
  (claudemacs-comment-test-with-temp-buffer
    (c-mode)
    (insert "/* This is a\n   multi-line comment\n   with multiple lines */")
    (let ((text (claudemacs--extract-comment-text 1 (point-max))))
      (should (string-match-p "This is a" text))
      (should (string-match-p "multi-line comment" text))
      (should (string-match-p "with multiple lines" text))
      ;; Should not contain comment markers
      (should-not (string-match-p "/\\*" text))
      (should-not (string-match-p "\\*/" text)))))

;;;; Module Conclusion
(provide 'claudemacs-comment-test)
;;; claudemacs-comment-test.el ends here