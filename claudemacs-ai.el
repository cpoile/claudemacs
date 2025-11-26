;;; claudemacs-ai.el --- AI helper functions for Claude integration -*- lexical-binding: t; -*-
;; Author: Christopher Poile <cpoile@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: claudecode ai emacs llm tools
;; URL: https://github.com/cpoile/claudemacs
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functions designed to be called via emacsclient by Claude AI
;; for programmatic interaction with Emacs buffers.
;;
;; These functions provide a safe, well-defined API for Claude to:
;; - Read buffer contents
;; - Insert and modify text
;; - Query buffer information
;; - Send input to REPL buffers

;;; Code:

(require 'comint nil 'noerror)

;; Dynamic variable for session context (set by MCP server via let binding)
(defvar claudemacs-session-cwd nil
  "The working directory for the current claudemacs session.
Set by the MCP server via a let binding to provide session context.
This must be defvar'd to be dynamically scoped in lexical-binding mode.")

;;;; Buffer Content Operations

(defun claudemacs-ai-insert-in-buffer (buffer-name text)
  "Insert TEXT into BUFFER-NAME at point.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (progn
        (with-current-buffer buffer-name
          (insert text))
        (format "Inserted %d characters into buffer '%s'" (length text) buffer-name))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-get-buffer-content (buffer-name &optional tail-lines)
  "Return the content of BUFFER-NAME.
If TAIL-LINES is provided, return only the last TAIL-LINES lines.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if tail-lines
            (save-excursion
              (goto-char (point-max))
              (forward-line (- tail-lines))
              (buffer-substring-no-properties (point) (point-max)))
          (buffer-substring-no-properties (point-min) (point-max))))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-get-region (buffer-name start end)
  "Return content of BUFFER-NAME from START to END.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (buffer-substring-no-properties start end))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-replace-region (buffer-name start end text)
  "Replace content in BUFFER-NAME from START to END with TEXT.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (progn
        (with-current-buffer buffer-name
          (delete-region start end)
          (goto-char start)
          (insert text))
        (format "Replaced region [%d:%d] with %d characters in buffer '%s'"
                start end (length text) buffer-name))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Buffer Navigation

(defun claudemacs-ai-goto-point (buffer-name position)
  "Move point to POSITION in BUFFER-NAME.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (progn
        (with-current-buffer buffer-name
          (goto-char position))
        (format "Moved to position %d in buffer '%s'" position buffer-name))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Buffer Information

(defun claudemacs-ai-list-buffers ()
  "Return a list of buffer names.
Designed to be called via emacsclient by Claude AI."
  (mapcar #'buffer-name (buffer-list)))

(defun claudemacs-ai-buffer-info (buffer-name)
  "Return information about BUFFER-NAME as a property list.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (list :name buffer-name
              :file (buffer-file-name)
              :modified (buffer-modified-p)
              :size (buffer-size)
              :major-mode major-mode
              :point (point)
              :point-min (point-min)
              :point-max (point-max)))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; REPL Integration

(defun claudemacs-ai-send-to-eat-terminal (buffer-name text)
  "Send TEXT to eat terminal in BUFFER-NAME and submit with return.
Designed for eat-mode terminals like claudemacs buffers.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if (and (boundp 'eat-terminal) eat-terminal)
            (progn
              (eat-term-send-string eat-terminal text)
              (eat-term-input-event eat-terminal 1 'return)
              (format "Sent input to eat terminal '%s'" buffer-name))
          (error "Buffer '%s' is not an eat terminal" buffer-name)))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-send-input (buffer-name text)
  "Insert TEXT into BUFFER-NAME and send input (useful for REPL buffers).
Tries eat-terminal, comint-send-input, eshell-send-input, or just inserts with newline.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (cond
         ;; eat-mode terminals (like claudemacs)
         ((and (boundp 'eat-terminal) eat-terminal)
          (eat-term-send-string eat-terminal text)
          (eat-term-input-event eat-terminal 1 'return)
          (format "Sent input to eat terminal '%s'" buffer-name))
         ;; comint-mode buffers
         ((and (boundp 'comint-mode) (derived-mode-p 'comint-mode))
          (goto-char (point-max))
          (insert text)
          (comint-send-input)
          (format "Sent input to comint buffer '%s'" buffer-name))
         ;; eshell
         ((and (boundp 'eshell-mode) (derived-mode-p 'eshell-mode))
          (goto-char (point-max))
          (insert text)
          (eshell-send-input)
          (format "Sent input to eshell buffer '%s'" buffer-name))
         ;; fallback: just insert with newline
         (t
          (goto-char (point-max))
          (insert text)
          (insert "\n")
          (format "Inserted text with newline to buffer '%s'" buffer-name))))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-exec-in-eat-terminal (buffer-name command &optional timeout)
  "Execute COMMAND in eat terminal BUFFER-NAME and wait for completion.
Returns the output of the command. TIMEOUT defaults to 30 seconds.
Designed to be called via emacsclient by Claude AI.

This function uses eat's shell integration if available (via
eat--shell-prompt-begin text property) for reliable prompt detection.
Falls back to heuristic-based detection if shell integration is not enabled.

Note: This function blocks but uses non-blocking waits to avoid freezing Emacs."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if (and (boundp 'eat-terminal) eat-terminal)
            (let* ((timeout-secs (or timeout 30))
                   (start-pos (point-max))
                   (start-time (current-time))
                   (has-shell-integration (and (boundp 'eat--shell-prompt-begin)
                                               eat--shell-prompt-begin))
                   (initial-prompt-pos (when has-shell-integration
                                        (save-excursion
                                          (goto-char (point-max))
                                          (when (get-text-property (point) 'eat--shell-prompt-end)
                                            (point)))))
                   (last-size 0)
                   (stable-count 0))
              ;; Send the command
              (eat-term-send-string eat-terminal command)
              (eat-term-input-event eat-terminal 1 'return)

              ;; Wait for command to complete
              (catch 'done
                (while (< (float-time (time-subtract (current-time) start-time))
                         timeout-secs)
                  ;; Process any pending output without blocking UI
                  (accept-process-output nil 0.05 nil t)

                  ;; Check completion based on shell integration or heuristics
                  (if has-shell-integration
                      ;; Use shell integration: look for new prompt
                      (save-excursion
                        (goto-char (point-max))
                        (when (and (get-text-property (point) 'eat--shell-prompt-end)
                                  (or (null initial-prompt-pos)
                                      (> (point) initial-prompt-pos)))
                          (throw 'done t)))
                    ;; Fall back to heuristic detection
                    (let ((current-size (buffer-size)))
                      (if (= current-size last-size)
                          (setq stable-count (1+ stable-count))
                        (setq stable-count 0
                              last-size current-size))
                      ;; If stable, check for prompt patterns
                      (when (>= stable-count 3)
                        (let ((recent-text (buffer-substring-no-properties
                                           (max (point-min) (- (point-max) 300))
                                           (point-max))))
                          (when (string-match-p "[$#%>❯λ][ \t]*\\(?:\n\\|$\\|\\[\\)" recent-text)
                            (throw 'done t))))))))

              ;; Capture output
              (let* ((output (buffer-substring-no-properties start-pos (point-max)))
                     (lines (split-string output "\n" t)))
                ;; Remove first line (command echo) if it matches the command
                (when (and lines (string-match-p (regexp-quote command) (car lines)))
                  (setq lines (cdr lines)))
                ;; Join and return
                (string-trim (string-join lines "\n"))))
          (error "Buffer '%s' is not an eat terminal" buffer-name)))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Notes File Operations

(defun claudemacs-ai--get-notes-work-dir ()
  "Get the working directory for the notes file.
Checks for `claudemacs-session-cwd' (set by MCP server via let binding),
then falls back to other methods."
  (cond
   ;; First priority: MCP server provides cwd via let binding
   ((and (boundp 'claudemacs-session-cwd) claudemacs-session-cwd)
    claudemacs-session-cwd)
   ;; If in a claudemacs buffer, use its cwd
   ((and (boundp 'claudemacs--cwd) claudemacs--cwd)
    claudemacs--cwd)
   ;; Extract from buffer name pattern *claudemacs:/path/to/dir/*
   ((and (buffer-name)
         (string-match "^\\*claudemacs:\\(.*\\)\\*$" (buffer-name)))
    (match-string 1 (buffer-name)))
   ;; Fallback to default-directory
   (t default-directory)))

(defun claudemacs-ai--get-notes-file-path ()
  "Get the file path for the memory file.
Returns path to .claude/claudemacs-notes.org in the project directory."
  (let ((work-dir (claudemacs-ai--get-notes-work-dir)))
    (expand-file-name ".claude/claudemacs-notes.org" work-dir)))

(defun claudemacs-ai--get-notes-buffer-name ()
  "Generate a nice buffer name for the notes file.
Uses the full path for consistency with other claudemacs buffers."
  (let ((work-dir (claudemacs-ai--get-notes-work-dir)))
    (format "*claudemacs-notes:%s*" work-dir)))

(defun claudemacs-ai--ensure-notes-buffer ()
  "Ensure the notes file exists and return it.
The buffer is backed by .claude/claudemacs-notes.org for persistence.
Uses `org-mode' for structured note-taking."
  (let* ((file-path (claudemacs-ai--get-notes-file-path))
         (dir (file-name-directory file-path))
         (nice-name (claudemacs-ai--get-notes-buffer-name))
         (existing-buffer (or (find-buffer-visiting file-path)
                              (get-buffer nice-name))))
    (if existing-buffer
        existing-buffer
      ;; Ensure .claude directory exists
      (unless (file-directory-p dir)
        (make-directory dir t))
      ;; Open or create the file
      (let ((buf (find-file-noselect file-path)))
        (with-current-buffer buf
          (unless (derived-mode-p 'org-mode)
            (org-mode))
          (setq-local buffer-read-only nil)
          ;; Rename to a nicer name (still saves to file-path)
          (rename-buffer nice-name)
          ;; Auto-save when buffer is modified
          (add-hook 'after-change-functions
                    (lambda (&rest _)
                      (when (buffer-modified-p)
                        (save-buffer)))
                    nil t))
        buf))))

(defun claudemacs-ai-get-notes ()
  "Get the content of the notes file for this session.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun claudemacs-ai-set-notes (content)
  "Set the notes file content to CONTENT, replacing any existing content.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (erase-buffer)
    (insert content)
    (format "Notesupdated: %d characters" (length content))))

(defun claudemacs-ai-append-notes (content)
  "Append CONTENT to the notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert content)
    (format "Appended %d characters to notes" (length content))))

(defun claudemacs-ai-clear-notes ()
  "Clear the notes file for this session.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (erase-buffer)
    "Notescleared"))

;;;; Org-mode Notes Operations

(defun claudemacs-ai-notes-add-heading (level title)
  "Add a heading with LEVEL stars and TITLE to the notes file.
LEVEL should be 1-6. Adds at point-max.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert (make-string level ?*) " " title "\n")
    (format "Added level %d heading: %s" level title)))

(defun claudemacs-ai-notes-add-todo (level title &optional priority)
  "Add a TODO heading with LEVEL stars and TITLE to notes file.
Optional PRIORITY should be A, B, or C.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert (make-string level ?*) " TODO ")
    (when priority
      (insert (format "[#%s] " (upcase priority))))
    (insert title "\n")
    (format "Added TODO: %s" title)))

(defun claudemacs-ai-notes-toggle-todo ()
  "Toggle TODO state of the heading at point in notes file.
Cycles through: unmarked -> TODO -> DONE -> unmarked.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-todo)
    (let ((state (org-get-todo-state)))
      (format "TODO state: %s" (or state "none")))))

(defun claudemacs-ai-notes-set-todo-state (state)
  "Set the TODO STATE of heading at point in notes file.
STATE should be TODO, DONE, or empty string to clear.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-todo (if (string-empty-p state) 'none state))
    (format "Set TODO state to: %s" (or state "none"))))

(defun claudemacs-ai-notes-add-timestamp (&optional inactive)
  "Add current timestamp at point-max in notes file.
If INACTIVE is non-nil, use inactive timestamp [date] instead of <date>.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (goto-char (point-max))
    (let ((ts (format-time-string (if inactive "[%Y-%m-%d %a %H:%M]" "<%Y-%m-%d %a %H:%M>"))))
      (insert ts)
      (format "Added timestamp: %s" ts))))

(defun claudemacs-ai-notes-get-headings ()
  "Get all headings from the notes file as a structured list.
Returns list of (level title todo-state) for each heading.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-map-entries
     (lambda ()
       (list (org-current-level)
             (org-get-heading t t t t)
             (org-get-todo-state)))
     nil nil)))

(defun claudemacs-ai-notes-goto-heading (title)
  "Go to the first heading matching TITLE in notes file.
Returns the position or nil if not found.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (goto-char (point-min))
    (if (re-search-forward (format org-complex-heading-regexp-format (regexp-quote title)) nil t)
        (progn
          (org-beginning-of-line)
          (format "Moved to heading: %s at position %d" title (point)))
      (format "Heading not found: %s" title))))

(defun claudemacs-ai-notes-add-list-item (text &optional checkbox)
  "Add a list item with TEXT to notes file at point-max.
If CHECKBOX is non-nil, add a checkbox.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert "- ")
    (when checkbox
      (insert "[ ] "))
    (insert text "\n")
    (format "Added list item: %s" text)))

(defun claudemacs-ai-notes-toggle-checkbox ()
  "Toggle checkbox at current line in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-toggle-checkbox)
    "Toggled checkbox"))

(defun claudemacs-ai-notes-schedule (timestamp)
  "Add SCHEDULED timestamp to heading at point in notes file.
TIMESTAMP should be org-compatible like '<2024-01-15 Mon>' or '+1d'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-schedule nil timestamp)
    (format "Scheduled: %s" timestamp)))

(defun claudemacs-ai-notes-deadline (timestamp)
  "Add DEADLINE timestamp to heading at point in notes file.
TIMESTAMP should be org-compatible like '<2024-01-15 Mon>' or '+1d'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-deadline nil timestamp)
    (format "Deadline: %s" timestamp)))

(defun claudemacs-ai-notes-set-property (property value)
  "Set PROPERTY to VALUE on heading at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-set-property property value)
    (format "Set property %s = %s" property value)))

(defun claudemacs-ai-notes-set-tags (tags)
  "Set TAGS on heading at point in notes file.
TAGS should be a colon-separated string like ':tag1:tag2:'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-set-tags tags)
    (format "Set tags: %s" tags)))

(defun claudemacs-ai-notes-clock-in ()
  "Start clocking time on heading at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-clock-in)
    "Clock started"))

(defun claudemacs-ai-notes-clock-out ()
  "Stop clocking time in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-clock-out)
    "Clock stopped"))

(defun claudemacs-ai-notes-add-note ()
  "Add a note to heading at point in notes file (with timestamp).
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-add-note)
    "Note drawer added - ready for input"))

(defun claudemacs-ai-notes-set-effort (effort)
  "Set effort estimate EFFORT on heading at point.
EFFORT should be like '1:30' for 1 hour 30 minutes.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-set-effort nil effort)
    (format "Effort set: %s" effort)))

(defun claudemacs-ai-notes-archive-subtree ()
  "Archive the subtree at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-archive-subtree)
    "Subtree archived"))

(defun claudemacs-ai-notes-insert-link (url &optional description)
  "Insert an org link to URL with optional DESCRIPTION at point-max.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (goto-char (point-max))
    (if description
        (insert (format "[[%s][%s]]" url description))
      (insert (format "[[%s]]" url)))
    (format "Inserted link: %s" url)))

(defun claudemacs-ai-notes-sparse-tree (query)
  "Create a sparse tree in notes file matching QUERY.
QUERY can be a tag match like '+work-urgent' or a property match.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-match-sparse-tree nil query)
    (format "Sparse tree created for: %s" query)))

(defun claudemacs-ai-notes-get-property (property)
  "Get the value of PROPERTY from heading at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (or (org-entry-get (point) property)
        "nil")))

(defun claudemacs-ai-notes-promote ()
  "Promote heading at point (decrease level) in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-promote-subtree)
    "Heading promoted"))

(defun claudemacs-ai-notes-demote ()
  "Demote heading at point (increase level) in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-demote-subtree)
    "Heading demoted"))

(defun claudemacs-ai-notes-move-up ()
  "Move subtree at point up in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-move-subtree-up)
    "Subtree moved up"))

(defun claudemacs-ai-notes-move-down ()
  "Move subtree at point down in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-move-subtree-down)
    "Subtree moved down"))

(defun claudemacs-ai-notes-get-all-properties ()
  "Get all properties for heading at point in notes file.
Returns an alist of (property . value) pairs.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (let ((props (org-entry-properties (point))))
      (or props "No properties at point"))))

(defun claudemacs-ai-notes-delete-property (property)
  "Delete PROPERTY from heading at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-entry-delete (point) property)
    (format "Deleted property: %s" property)))

(defun claudemacs-ai-notes-refile (target-heading)
  "Refile (move) subtree at point to TARGET-HEADING in notes file.
TARGET-HEADING should be the heading title to refile under.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (let* ((target-pos (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward
                                (format org-complex-heading-regexp-format
                                        (regexp-quote target-heading))
                                nil t)
                           (point-at-bol)))))
      (if target-pos
          (let ((org-refile-targets `((nil . (:regexp . ,(regexp-quote target-heading))))))
            (org-refile nil nil (list target-heading (buffer-file-name) nil target-pos))
            (format "Refiled to: %s" target-heading))
        (format "Target heading not found: %s" target-heading)))))

(defun claudemacs-ai-notes-sort (&optional sorting-type)
  "Sort children of current heading in notes file.
SORTING-TYPE can be: alpha, num, time, func, priority, todo.
Defaults to alpha (alphabetical).
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (let ((type (pcase (or sorting-type "alpha")
                  ("alpha" ?a)
                  ("num" ?n)
                  ("time" ?t)
                  ("func" ?f)
                  ("priority" ?p)
                  ("todo" ?o)
                  (_ ?a))))
      (org-sort-entries nil type)
      (format "Sorted entries by: %s" (or sorting-type "alpha")))))

(defun claudemacs-ai-notes-up-heading ()
  "Move to parent heading in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (if (org-up-heading-safe)
        (format "Moved to parent: %s at position %d"
                (org-get-heading t t t t) (point))
      "Already at top level")))

(defun claudemacs-ai-notes-next-heading ()
  "Move to next heading at same level in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (if (org-forward-heading-same-level 1)
        "No more headings at this level"
      (format "Moved to: %s at position %d"
              (org-get-heading t t t t) (point)))))

(defun claudemacs-ai-notes-prev-heading ()
  "Move to previous heading at same level in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (if (org-backward-heading-same-level 1)
        "No previous headings at this level"
      (format "Moved to: %s at position %d"
              (org-get-heading t t t t) (point)))))

(defun claudemacs-ai-notes-occur (regexp)
  "Search for REGEXP in notes file and return matches.
Returns a list of matching lines with positions.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (save-excursion
      (goto-char (point-min))
      (let (matches)
        (while (re-search-forward regexp nil t)
          (push (list :pos (match-beginning 0)
                      :line (line-number-at-pos)
                      :match (match-string 0)
                      :context (string-trim (thing-at-point 'line t)))
                matches))
        (or (nreverse matches)
            (format "No matches for: %s" regexp))))))

(defun claudemacs-ai-notes-get-tags ()
  "Get tags for heading at point in notes file.
Returns the tags as a list.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (let ((tags (org-get-tags)))
      (or tags "No tags"))))

(defun claudemacs-ai-notes-cut-subtree ()
  "Cut (kill) subtree at point in notes file.
The subtree is stored in the kill ring for later pasting.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (let ((heading (org-get-heading t t t t)))
      (org-cut-subtree)
      (format "Cut subtree: %s" heading))))

(defun claudemacs-ai-notes-copy-subtree ()
  "Copy subtree at point in notes file.
The subtree is stored in the kill ring for later pasting.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (let ((heading (org-get-heading t t t t)))
      (org-copy-subtree)
      (format "Copied subtree: %s" heading))))

(defun claudemacs-ai-notes-paste-subtree (&optional level)
  "Paste subtree from kill ring in notes file.
Optional LEVEL specifies the heading level for pasted tree.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (org-paste-subtree level)
    "Pasted subtree"))

(defun claudemacs-ai-notes-get-heading-at-point ()
  "Get detailed info about heading at point in notes file.
Returns plist with level, title, todo-state, tags, priority, and position.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (if (org-at-heading-p)
        (list :level (org-current-level)
              :title (org-get-heading t t t t)
              :todo (org-get-todo-state)
              :tags (org-get-tags)
              :priority (org-get-priority (thing-at-point 'line t))
              :pos (point))
      "Not at a heading")))

(defun claudemacs-ai-notes-get-subtree ()
  "Get the content of subtree at point in notes file.
Returns the full text of the subtree including the heading.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (if (org-at-heading-p)
        (org-copy-subtree)
      (org-back-to-heading t)
      (org-copy-subtree))
    (current-kill 0 t)))

(defun claudemacs-ai-notes-get-children ()
  "Get direct children headings of current heading in notes file.
Returns list of (level, title, todo-state) for immediate children only.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-notes-buffer)
    (let ((parent-level (org-current-level))
          children)
      (when parent-level
        (save-excursion
          (org-back-to-heading t)
          (let ((end (save-excursion (org-end-of-subtree t) (point))))
            (forward-line 1)
            (while (< (point) end)
              (when (and (org-at-heading-p)
                         (= (org-current-level) (1+ parent-level)))
                (push (list :level (org-current-level)
                            :title (org-get-heading t t t t)
                            :todo (org-get-todo-state))
                      children))
              (forward-line 1)))))
      (or (nreverse children) "No children"))))

;;;; Buffer Watching and Streaming

(defun claudemacs-ai-watch-buffer (buffer-name &optional timeout stable-time)
  "Watch BUFFER-NAME until content stabilizes or TIMEOUT seconds.
Returns buffer content after no changes for STABLE-TIME seconds (default 0.5).
TIMEOUT defaults to 30 seconds.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (let ((timeout-secs (or timeout 30))
            (stable-secs (or stable-time 0.5))
            (start-time (current-time))
            (last-content "")
            (last-change-time (current-time)))
        (catch 'done
          (while (< (float-time (time-subtract (current-time) start-time)) timeout-secs)
            (let ((current-content (with-current-buffer buffer-name
                                     (buffer-substring-no-properties (point-min) (point-max)))))
              (if (string= current-content last-content)
                  ;; Content stable - check if stable long enough
                  (when (>= (float-time (time-subtract (current-time) last-change-time)) stable-secs)
                    (throw 'done current-content))
                ;; Content changed - reset timer
                (setq last-content current-content
                      last-change-time (current-time))))
            (accept-process-output nil 0.1)))
        ;; Timeout - return current content
        (with-current-buffer buffer-name
          (buffer-substring-no-properties (point-min) (point-max))))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-watch-for-pattern (buffer-name pattern &optional timeout)
  "Watch BUFFER-NAME until PATTERN appears or TIMEOUT seconds.
Returns plist with :match, :line, and :pos, or nil if timeout.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (let ((timeout-secs (or timeout 30))
            (start-time (current-time)))
        (catch 'found
          (while (< (float-time (time-subtract (current-time) start-time)) timeout-secs)
            (with-current-buffer buffer-name
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward pattern nil t)
                  (throw 'found (list :match (match-string 0)
                                      :line (thing-at-point 'line t)
                                      :pos (match-beginning 0))))))
            (accept-process-output nil 0.1))
          nil))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-send-and-watch (buffer-name input &optional done-pattern timeout)
  "Send INPUT to BUFFER-NAME and watch until DONE-PATTERN or stable.
If DONE-PATTERN is provided, wait for it. Otherwise wait for stability.
Returns new content added after sending input.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (let ((start-pos (with-current-buffer buffer-name (point-max))))
        ;; Send input
        (with-current-buffer buffer-name
          (cond
           ((and (boundp 'eat-terminal) eat-terminal)
            (eat-term-send-string eat-terminal input)
            (eat-term-input-event eat-terminal 1 'return))
           ((derived-mode-p 'comint-mode)
            (goto-char (point-max))
            (insert input)
            (comint-send-input))
           (t
            (goto-char (point-max))
            (insert input "\n"))))
        ;; Watch for completion
        (if done-pattern
            (claudemacs-ai-watch-for-pattern buffer-name done-pattern timeout)
          ;; Return new content after stabilization
          (claudemacs-ai-watch-buffer buffer-name timeout 1.0)
          (with-current-buffer buffer-name
            (buffer-substring-no-properties start-pos (point-max)))))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Agent Spawning

(defun claudemacs-ai--send-to-agent-when-ready (buffer-name prompt &optional attempt)
  "Send PROMPT to BUFFER-NAME when eat-terminal is ready.
ATTEMPT is the retry count (max 20 attempts, ~10 seconds total)."
  (let ((attempt (or attempt 0)))
    (if (>= attempt 20)
        (message "Failed to send prompt to %s: terminal not ready after 10s" buffer-name)
      (let ((buf (get-buffer buffer-name)))
        (if (and buf (buffer-live-p buf))
            (with-current-buffer buf
              (if (and (boundp 'eat-terminal) eat-terminal)
                  ;; Terminal ready - send the prompt
                  (progn
                    (eat-term-send-string eat-terminal prompt)
                    (run-at-time 0.1 nil
                                 (lambda (b)
                                   (when (buffer-live-p b)
                                     (with-current-buffer b
                                       (when (and (boundp 'eat-terminal) eat-terminal)
                                         (eat-term-input-event eat-terminal 1 'return)))))
                                 buf)
                    (message "Sent initial prompt to %s" buffer-name))
                ;; Not ready yet - retry
                (run-at-time 0.5 nil
                             #'claudemacs-ai--send-to-agent-when-ready
                             buffer-name prompt (1+ attempt))))
          ;; Buffer doesn't exist yet - retry
          (run-at-time 0.5 nil
                       #'claudemacs-ai--send-to-agent-when-ready
                       buffer-name prompt (1+ attempt)))))))

(defun claudemacs-ai-spawn-agent (directory &optional initial-prompt)
  "Spawn a new Claude agent in DIRECTORY.
If INITIAL-PROMPT is provided, send it to the agent after startup.
Returns the buffer name of the new agent.
Designed to be called via emacsclient by Claude AI."
  (require 'claudemacs)
  (let* ((work-dir (expand-file-name directory))
         (buffer-name (format "*claudemacs:%s*" work-dir)))
    ;; Check if session already exists
    (if (get-buffer buffer-name)
        (format "Agent already running in %s (buffer: %s)" work-dir buffer-name)
      ;; Start new session
      (claudemacs--start work-dir)
      ;; Send initial prompt when terminal is ready
      (when initial-prompt
        (claudemacs-ai--send-to-agent-when-ready buffer-name initial-prompt))
      (format "Spawned agent in %s (buffer: %s)" work-dir buffer-name))))

(defun claudemacs-ai-list-agents ()
  "List all running claudemacs agent sessions.
Returns a list of (buffer-name directory) pairs.
Designed to be called via emacsclient by Claude AI."
  (let (agents)
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (string-match "^\\*claudemacs:\\(.*\\)\\*$" name)
          (push (list name (match-string 1 name)) agents))))
    (or agents "No agents running")))

(defun claudemacs-ai-message-agent (buffer-name message)
  "Send MESSAGE to the agent in BUFFER-NAME.
This sends the message as user input to the Claude session.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if (and (boundp 'eat-terminal) eat-terminal)
            (let ((buf (current-buffer)))
              (eat-term-send-string eat-terminal message)
              ;; Small delay before sending return to ensure text is processed
              (run-at-time 0.1 nil
                           (lambda (b)
                             (when (buffer-live-p b)
                               (with-current-buffer b
                                 (when (and (boundp 'eat-terminal) eat-terminal)
                                   (eat-term-input-event eat-terminal 1 'return)))))
                           buf)
              (format "Sent message to %s" buffer-name))
          (error "Buffer '%s' is not a claudemacs terminal" buffer-name)))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Session Management

(defun claudemacs-ai-restart-and-resume (&optional buffer-name)
  "Restart the claudemacs session in BUFFER-NAME and resume the conversation.
If BUFFER-NAME is not provided, tries to determine from `claudemacs-session-cwd'
\(set by MCP server) or falls back to current buffer.
This reloads elisp files and restarts the MCP server with any code changes.
Designed to be called via emacsclient by Claude AI."
  (let* ((target-buffer (or buffer-name
                            ;; Try to compute from MCP session cwd
                            (when (and (boundp 'claudemacs-session-cwd) claudemacs-session-cwd)
                              (format "*claudemacs:%s*" claudemacs-session-cwd))
                            ;; Fallback to current buffer
                            (buffer-name))))
    ;; Check if this is a claudemacs buffer
    (if (and (get-buffer target-buffer)
             (string-match-p "^\\*claudemacs:" target-buffer))
        (let ((work-dir (with-current-buffer target-buffer
                         (or claudemacs--cwd
                             ;; Fallback: extract directory from buffer name
                             ;; *claudemacs:/path/to/dir/* -> /path/to/dir
                             (when (string-match "^\\*claudemacs:\\(.*\\)\\*$" target-buffer)
                               (match-string 1 target-buffer))))))
          (unless work-dir
            (error "Cannot determine working directory for buffer '%s'" target-buffer))
          ;; Use run-at-time to defer execution so we can return a response first
          ;; Pass work-dir to claudemacs-restart to target the correct session
          (run-at-time 0.5 nil
                       (lambda (dir)
                         (require 'claudemacs)
                         (claudemacs-restart dir))
                       work-dir)
          (format "Restart scheduled for %s - session will reload elisp files, restart MCP server, and resume shortly"
                  work-dir))
      (error "Buffer '%s' is not a claudemacs buffer" target-buffer))))

;;;; Project Shell for Bash Execution

(defun claudemacs-ai-get-project-shell (directory)
  "Get or create an eat shell for DIRECTORY.
Returns the buffer name. Creates the shell if it doesn't exist.
Designed to be called via emacsclient by Claude AI."
  (require 'eat)
  (let* ((work-dir (expand-file-name directory))
         ;; eat-make wraps name with *...*, so we use name without asterisks
         (shell-name (format "eat-shell:%s" (file-name-nondirectory
                                              (directory-file-name work-dir))))
         (buffer-name (format "*%s*" shell-name)))
    (if (get-buffer buffer-name)
        ;; Buffer exists - return it
        buffer-name
      ;; Create new shell - eat-make adds *...* wrapper automatically
      (let ((default-directory work-dir))
        (with-current-buffer (eat-make shell-name
                                       (or (getenv "SHELL") "/bin/bash")
                                       nil)
          (setq-local default-directory work-dir)))
      buffer-name)))

(defun claudemacs-ai-project-shell-ready-p (buffer-name)
  "Check if project shell BUFFER-NAME has an active eat terminal.
Returns t if ready, nil otherwise."
  (when-let ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (and (boundp 'eat-terminal) eat-terminal t))))

;;;; Elisp Debugging and Formatting

(defun claudemacs-ai-elisp-check-parens (file-path)
  "Check FILE-PATH for unbalanced parentheses in elisp.
Analyzes top-level forms and reports which ones have unbalanced parens.
Returns a structured report with line numbers and error descriptions.
Designed to be called via emacsclient by Claude AI."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file-path)
        (emacs-lisp-mode)
        (let ((forms nil)
              (line-num 1)
              (start-pos (point-min)))
          ;; Parse all top-level forms
          (goto-char (point-min))
          (condition-case parse-err
              (while (not (eobp))
                (let* ((form-start (point))
                       (form-start-line (line-number-at-pos form-start)))
                  (condition-case form-err
                      (progn
                        ;; Try to read the form
                        (forward-sexp 1)
                        (let ((form-end (point)))
                          ;; Successfully parsed this form
                          (push (list :status "ok"
                                    :start-line form-start-line
                                    :end-line (line-number-at-pos form-end)
                                    :text (buffer-substring-no-properties
                                           form-start
                                           (min form-end (+ form-start 100))))
                                forms))
                        ;; Skip whitespace and comments to next form
                        (forward-comment most-positive-fixnum))
                    (scan-error
                     ;; This form has unbalanced parens
                     (let* ((error-pos (or (nth 2 form-err) form-start))
                            (error-line (line-number-at-pos error-pos))
                            (form-text (buffer-substring-no-properties
                                       form-start
                                       (min (point-max) (+ form-start 200)))))
                       (push (list :status "error"
                                 :start-line form-start-line
                                 :error-line error-line
                                 :error (nth 1 form-err)
                                 :text (substring form-text 0 (min 200 (length form-text))))
                             forms))
                     ;; Try to skip to next top-level form
                     (goto-char (point-max))))))
            (end-of-file
             ;; Hit premature end of file - entire rest of file has issues
             (let ((remaining-text (buffer-substring-no-properties
                                   start-pos
                                   (min (point-max) (+ start-pos 200)))))
               (push (list :status "error"
                         :start-line (line-number-at-pos start-pos)
                         :error "Premature end of file"
                         :text (substring remaining-text 0 (min 200 (length remaining-text))))
                     forms))))

          ;; Format the report
          (let ((errors (seq-filter (lambda (f) (equal (plist-get f :status) "error"))
                                   (nreverse forms)))
                (total-forms (length forms)))
            (if errors
                (format "Found %d forms, %d with errors:\n\n%s"
                       total-forms
                       (length errors)
                       (mapconcat
                        (lambda (err)
                          (format "Line %d: %s\n  Preview: %s..."
                                 (plist-get err :start-line)
                                 (plist-get err :error)
                                 (string-trim (plist-get err :text))))
                        errors
                        "\n\n"))
              (format "All %d forms are balanced correctly" total-forms)))))
    (file-error
     (format "Error reading file: %s" (error-message-string err)))
    (error
     (format "Error checking parens: %s" (error-message-string err)))))

(defun claudemacs-ai-elisp-format (file-path)
  "Format FILE-PATH using elisp-format if available.
Returns formatted content if elisp-format is available, or error message.
Designed to be called via emacsclient by Claude AI."
  (let ((elisp-format-bin (executable-find "elisp-format")))
    (if elisp-format-bin
        (condition-case err
            (with-temp-buffer
              (let ((exit-code (call-process elisp-format-bin nil t nil file-path)))
                (if (zerop exit-code)
                    (buffer-substring-no-properties (point-min) (point-max))
                  (format "elisp-format exited with code %d:\n%s"
                         exit-code
                         (buffer-substring-no-properties (point-min) (point-max))))))
          (error
           (format "Error running elisp-format: %s" (error-message-string err))))
      "elisp-format not found in PATH. Install it to enable elisp formatting.")))

;;;; Setup and Integration

(defun claudemacs-ai-get-cli-path ()
  "Get the path to the claudemacs-cli executable.
Assumes it's in the same directory as this file."
  (let* ((this-file (or load-file-name
                        buffer-file-name
                        (locate-library "claudemacs-ai")))
         (this-dir (when this-file (file-name-directory this-file))))
    (if this-dir
        (expand-file-name "claudemacs-cli" this-dir)
      (error "Cannot determine claudemacs-ai.el location"))))

(defun claudemacs-ai-setup-claude-environment ()
  "Add claudemacs-cli to PATH and set up environment for Claude.
This should be called during claudemacs startup to expose the CLI to Claude."
  (let ((cli-dir (file-name-directory (claudemacs-ai-get-cli-path))))
    ;; Add to PATH via setenv (affects child processes)
    (setenv "PATH" (concat cli-dir ":" (getenv "PATH")))
    ;; Set CLAUDEMACS_SOCKET using the actual server-socket-dir
    (when (and (boundp 'server-socket-dir)
               server-socket-dir
               (server-running-p))
      (let ((socket-file (expand-file-name "server" server-socket-dir)))
        (when (file-exists-p socket-file)
          (setenv "CLAUDEMACS_SOCKET" socket-file))))))

(provide 'claudemacs-ai)
;;; claudemacs-ai.el ends here
