;;; claudemacs-comment.el --- Comment parsing and handling for claudemacs -*- lexical-binding: t; -*-

;; Author: Christopher Poile (extracted from claudemacs.el)
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: comments parsing text-manipulation
;; URL: https://github.com/cpoile/claudemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comment parsing and handling functionality.
;; This module provides functions to:
;; - Detect comment syntax information for different modes
;; - Determine if point is inside or before a comment
;; - Find comment boundaries (start and end positions)
;; - Extract and clean comment text

;;; Code:

;;;; Dependencies
(require 'cl-lib)

;;;; Comment Detection and Parsing Functions

(defun claudemacs--comment-syntax-info ()
  "Get comment syntax information for the current buffer.
Returns a plist with :start, :end, :start-skip, :end-skip, and :multi-line-p."
  (list :start comment-start
        :end comment-end
        :start-skip comment-start-skip
        :end-skip comment-end-skip
        :multi-line-p (and comment-end (not (string-empty-p comment-end)))))

(defun claudemacs--point-in-comment-p ()
  "Return non-nil if point is inside or before a comment.
This includes:
- Being inside a comment
- Being at comment start markers like ';' or '//'  
- Being in whitespace immediately before a comment on the same line"
  (or 
   ;; Already in a comment
   (nth 4 (syntax-ppss))
   ;; In whitespace immediately before a comment on the same line  
   (save-excursion
     (skip-chars-forward " \t")
     ;; Move forward enough to get inside the comment (handle // and /* styles)
     ;; Only advance if we're not at end of line and have room to move forward
     (when (and (not (eolp)) (< (point) (line-end-position)))
       (forward-char (min 2 (- (line-end-position) (point)))))
     (nth 4 (syntax-ppss)))))

(defun claudemacs--find-comment-start ()
  "Find the start of the comment block containing point.
Returns the position of the comment start, or nil if not in a comment."
  (save-excursion
    (let ((syntax-info (claudemacs--comment-syntax-info)))
      (cond
       ;; Multi-line comment (/* ... */) - use syntax-ppss to find bounds
       ((plist-get syntax-info :multi-line-p)
        ;; Move to beginning of comment using syntax-ppss
        ;; syntax-ppss returns (depth start-paren start-string start-comment ...)
        ;; The 8th element (index 8) is the comment start position
        (let ((comment-start-pos (nth 8 (syntax-ppss))))
          (if comment-start-pos
              comment-start-pos
            ;; Fallback: scan backwards to find comment start
            (while (and (nth 4 (syntax-ppss)) (not (bobp)))
              (backward-char))
            ;; Move forward to first char of comment
            (when (nth 4 (syntax-ppss (1+ (point))))
              (point)))))
       
       ;; Single-line comment (// or # or ;;) - join next line if it is comment-only
       (t
        (let ((start-regex (plist-get syntax-info :start-skip)))
          (when start-regex
            ;; Check if current line is a comment-only line or mixed line
            (beginning-of-line)
            ;; Determine if this is a comment-only line by checking if comment starts at beginning
            ;; vs a mixed line where comment starts after code
            (let ((comment-pos (when (re-search-forward start-regex (line-end-position) t)
                                 (match-beginning 0))))
              (cond
               ;; Case 1: True comment-only line (comment at or near start of line)
               ((and comment-pos
                     (save-excursion
                       (goto-char comment-pos)
                       (skip-chars-backward " \t")
                       (bolp)))
                ;; For comment-only lines, find the start of consecutive comment block
                (while (and (not (bobp))
                            (save-excursion
                              (forward-line -1)
                              (beginning-of-line)
                              (looking-at-p (concat "^[[:space:]]*" start-regex))))
                  (forward-line -1))
                (beginning-of-line)
                (point))
               ;; Case 2: Mixed line (code + comment) 
               (comment-pos
                ;; For mixed lines, start from the comment marker position
                comment-pos)
               ;; Case 3: No comment found
               (t nil))))))))))

(defun claudemacs--find-comment-end ()
  "Find the end of the comment block containing point.
Returns the position of the comment end, or nil if not in a comment."
  (save-excursion
    (let ((syntax-info (claudemacs--comment-syntax-info)))
      (cond
       ;; Multi-line comment (/* ... */) - use syntax-ppss to find end
       ((plist-get syntax-info :multi-line-p)
        ;; Move forward while still in comment, then find exact end
        (while (and (nth 4 (syntax-ppss)) (not (eobp)))
          (forward-char))
        ;; We're now just past the comment end
        (point))
       
       ;; Single-line comment (// or # or ;;) - Option 2 behavior
       (t
        (let ((start-regex (plist-get syntax-info :start-skip)))
          (when start-regex
            ;; Determine if current line is comment-only or mixed, similar to start function
            (beginning-of-line)
            (let* ((comment-pos (when (re-search-forward start-regex (line-end-position) t)
                                  (match-beginning 0)))
                   (is-comment-only (and comment-pos
                                         (save-excursion
                                           (goto-char comment-pos)
                                           (skip-chars-backward " \t")
                                           (bolp)))))
              (cond
               ;; Case 1: Comment-only line - find end of consecutive comment block  
               (is-comment-only
                (while (and (not (eobp))
                            (save-excursion
                              (forward-line 1)
                              (beginning-of-line)
                              (looking-at-p (concat "^[[:space:]]*" start-regex))))
                  (forward-line 1))
                (end-of-line)
                (point))
               ;; Case 2: Mixed line - look ahead for comment-only lines after
               (comment-pos
                (end-of-line)
                ;; Look ahead for consecutive comment-only lines (not mixed lines)
                (while (and (not (eobp))
                            (save-excursion
                              (forward-line 1)
                              (beginning-of-line)
                              ;; Only include lines that start with whitespace + comment
                              ;; This excludes mixed lines like "(code) ;; comment"
                              (looking-at-p (concat "^[[:space:]]*" start-regex))))
                  (forward-line 1)
                  (end-of-line))
                (point))
               ;; Case 3: No comment found
               (t nil))))))))))

(defun claudemacs--get-comment-bounds ()
  "Get the bounds of the comment block at point.
Returns (START . END) if point is in a comment, nil otherwise."
  (when (claudemacs--point-in-comment-p)
    (save-excursion
      ;; If we're before a comment on the same line, move to the comment
      (unless (nth 4 (syntax-ppss))
        (skip-chars-forward " \t"))
      
      (let ((start (claudemacs--find-comment-start))
            (end (claudemacs--find-comment-end)))
        (when (and start end)
          (cons start end))))))

(defun claudemacs--extract-comment-text (start end)
  "Extract and clean comment text between START and END positions.
Removes comment markers and normalizes whitespace."
  (let* ((raw-text (buffer-substring-no-properties start end))
         (syntax-info (claudemacs--comment-syntax-info))
         (comment-start (plist-get syntax-info :start))
         (comment-end (plist-get syntax-info :end))
         (cleaned-text raw-text))

    ;; Handle multi-line comments (/* ... */)
    (when (and comment-end (not (string-empty-p comment-end)))
      ;; Remove opening comment marker
      (when comment-start
        (setq cleaned-text
              (replace-regexp-in-string
               (concat "^[[:space:]]*" (regexp-quote comment-start) "[[:space:]]*")
               "" cleaned-text)))
      ;; Remove closing comment marker
      (setq cleaned-text
            (replace-regexp-in-string
             (concat "[[:space:]]*" (regexp-quote comment-end) "[[:space:]]*$")
             "" cleaned-text)))

    ;; Handle single-line comments (// or # or ;; etc.)
    (when (and comment-start (or (not comment-end) (string-empty-p comment-end)))
      ;; For single-char comment markers like #, remove all consecutive occurrences
      (if (= 1 (length (string-trim comment-start)))
          (let ((char (string-to-char (string-trim comment-start))))
            (setq cleaned-text
                  (replace-regexp-in-string
                   (concat "^[[:space:]]*" (regexp-quote (char-to-string char)) "+[[:space:]]*")
                   "" cleaned-text)))
        ;; For multi-char comment markers like //, remove each occurrence
        (setq cleaned-text
              (replace-regexp-in-string
               (concat "^[[:space:]]*" (regexp-quote comment-start) "[[:space:]]*")
               "" cleaned-text))))

    ;; Clean up whitespace and empty lines
    (setq cleaned-text (string-trim cleaned-text))
    (setq cleaned-text (replace-regexp-in-string "^[[:space:]]*\n" "" cleaned-text))
    (setq cleaned-text (replace-regexp-in-string "\n[[:space:]]*$" "" cleaned-text))
    
    cleaned-text))

;;;; Module Conclusion
(provide 'claudemacs-comment)
;;; claudemacs-comment.el ends here
