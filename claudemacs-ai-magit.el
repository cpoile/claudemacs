;;; claudemacs-ai-magit.el --- Magit section querying for claudemacs -*- lexical-binding: t; -*-

;; This file is part of claudemacs.

;;; Commentary:

;; This module provides a clean API for querying magit-section buffers.
;; Magit sections are used by various Emacs tools (not just magit itself)
;; to display structured, collapsible content.
;;
;; Main functions:
;; - claudemacs-ai-magit-section-query-find: Find sections by criteria
;; - claudemacs-ai-magit-section-query-content: Extract section content
;; - claudemacs-ai-magit-section-query-get: Get section metadata
;; - claudemacs-ai-magit-section-query-children: Get child sections

;;; Code:

(require 'magit-section nil t)

(defun claudemacs-ai-magit-section-query--strip-indent (text)
  "Remove common leading whitespace from TEXT.
Designed to be called via emacsclient by Claude AI."
  (let* ((lines (split-string text "\n"))
         (non-empty-lines (seq-filter (lambda (line) (not (string-empty-p (string-trim-left line)))) lines))
         (indents (mapcar (lambda (line) (length (replace-regexp-in-string "^\\( *\\).*" "\\1" line))) non-empty-lines))
         (min-indent (if indents (apply 'min indents) 0)))
    (mapconcat (lambda (line)
                 (if (> (length line) min-indent)
                     (substring line min-indent)
                   line))
               lines "\n")))

(defun claudemacs-ai-magit-section-query--walk (section fn)
  "Walk SECTION tree applying FN to each section.
FN should accept a section and return non-nil to continue walking.
Designed to be called via emacsclient by Claude AI."
  (when (funcall fn section)
    (dolist (child (eieio-oref section 'children))
      (claudemacs-ai-magit-section-query--walk child fn))))

(defun claudemacs-ai-magit-section-query--matches-p (section criteria)
  "Check if SECTION matches CRITERIA (a plist).
Supported criteria: :type, :heading (regex), :hidden, :value
Designed to be called via emacsclient by Claude AI."
  (let ((matches t))
    (when (plist-member criteria :type)
      (let ((expected-type (plist-get criteria :type)))
        (unless (eq (eieio-oref section 'type) expected-type)
          (setq matches nil))))
    (when (and matches (plist-member criteria :heading))
      (let ((heading-pattern (plist-get criteria :heading))
            (content (eieio-oref section 'content)))
        ;; Match against the content text if available
        (unless (and content (string-match-p heading-pattern content))
          (setq matches nil))))
    (when (and matches (plist-member criteria :hidden))
      (let ((expected-hidden (plist-get criteria :hidden))
            (hidden (eieio-oref section 'hidden)))
        (unless (eq hidden expected-hidden)
          (setq matches nil))))
    (when (and matches (plist-member criteria :value))
      (let ((expected-value (plist-get criteria :value)))
        (unless (equal (eieio-oref section 'value) expected-value)
          (setq matches nil))))
    matches))

(defun claudemacs-ai-magit-section-query-content (buffer-name section-position &optional strip-indent)
  "Extract content of magit section at SECTION-POSITION in BUFFER-NAME as string.
SECTION-POSITION should be a buffer position (integer) within the section.
If STRIP-INDENT is non-nil, remove common leading whitespace.
Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (save-excursion
      (goto-char section-position)
      (let ((section (magit-current-section)))
        (unless section
          (error "No magit section found at position %d" section-position))
        (let* ((start (marker-position (eieio-oref section 'start)))
               (end (marker-position (eieio-oref section 'end)))
               (content (buffer-substring-no-properties start end)))
          (if strip-indent
              (claudemacs-ai-magit-section-query--strip-indent content)
            content))))))

(defun claudemacs-ai-magit-section-query-find (buffer-name &rest criteria)
  "Find magit sections in BUFFER-NAME matching CRITERIA.
CRITERIA is a plist that can include:
  :type TYPE - section type symbol
  :heading REGEX - regex to match heading
  :hidden BOOL - whether section is hidden
  :value VALUE - section value

Returns list of positions (integers) for matching sections.
Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (let ((results '())
          (root (magit-current-section)))
      ;; Get the root section
      (save-excursion
        (goto-char (point-min))
        (setq root (magit-current-section))
        (when root
          (claudemacs-ai-magit-section-query--walk
           root
           (lambda (section)
             (when (claudemacs-ai-magit-section-query--matches-p section criteria)
               (push (marker-position (eieio-oref section 'start)) results))
             t))))
      (nreverse results))))

(defun claudemacs-ai-magit-section-query-children (buffer-name section-position &rest criteria)
  "Get child sections of section at SECTION-POSITION in BUFFER-NAME matching CRITERIA.
SECTION-POSITION should be a buffer position (integer) within the parent section.
CRITERIA is a plist (same format as magit-section-query-find).
Returns list of positions (integers) for matching child sections.
Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (save-excursion
      (goto-char section-position)
      (let* ((section (magit-current-section))
             (results '()))
        (unless section
          (error "No magit section found at position %d" section-position))
        (dolist (child (eieio-oref section 'children))
          (when (claudemacs-ai-magit-section-query--matches-p child criteria)
            (push (marker-position (eieio-oref child 'start)) results)))
        (nreverse results)))))

(defun claudemacs-ai-magit-section-query-get (buffer-name section-position &optional include-content)
  "Get metadata for magit section at SECTION-POSITION in BUFFER-NAME.
SECTION-POSITION should be a buffer position (integer) within the section.
Returns a list with metadata: (type heading hidden start end [content])
If INCLUDE-CONTENT is non-nil, includes section content as last element.
Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (save-excursion
      (goto-char section-position)
      (let ((section (magit-current-section)))
        (unless section
          (error "No magit section found at position %d" section-position))
        (let* ((start-pos (marker-position (eieio-oref section 'start)))
               (end-pos (marker-position (eieio-oref section 'end)))
               (result (list
                        (eieio-oref section 'type)
                        (eieio-oref section 'content)  ; Use content as heading
                        (eieio-oref section 'hidden)
                        start-pos
                        end-pos)))
          (when include-content
            (setq result (append result (list (buffer-substring-no-properties start-pos end-pos)))))
          result)))))

(provide 'claudemacs-ai-magit)
;;; claudemacs-ai-magit.el ends here
