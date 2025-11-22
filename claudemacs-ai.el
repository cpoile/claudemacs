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

;;;; Memory Buffer Operations

(defun claudemacs-ai--get-memory-buffer-name ()
  "Get the name of the memory buffer for the current session.
Uses the session's working directory to create a unique buffer per session.
Checks for `claudemacs-session-cwd' (set by MCP server via let binding),
then falls back to other methods."
  (let ((work-dir (cond
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
                   (t default-directory))))
    (format "*claudemacs-memory:%s*" (file-name-nondirectory (directory-file-name work-dir)))))

(defun claudemacs-ai--ensure-memory-buffer ()
  "Ensure the memory buffer exists and return it.
The buffer uses `org-mode' for structured note-taking."
  (let ((buffer-name (claudemacs-ai--get-memory-buffer-name)))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (org-mode)
          (setq-local buffer-read-only nil)
          (current-buffer)))))

(defun claudemacs-ai-get-memory ()
  "Get the content of the memory buffer for this session.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun claudemacs-ai-set-memory (content)
  "Set the memory buffer content to CONTENT, replacing any existing content.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (erase-buffer)
    (insert content)
    (format "Memory updated: %d characters" (length content))))

(defun claudemacs-ai-append-memory (content)
  "Append CONTENT to the memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert content)
    (format "Appended %d characters to memory" (length content))))

(defun claudemacs-ai-clear-memory ()
  "Clear the memory buffer for this session.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (erase-buffer)
    "Memory cleared"))

;;;; Org-mode Memory Operations

(defun claudemacs-ai-memory-add-heading (level title)
  "Add a heading with LEVEL stars and TITLE to the memory buffer.
LEVEL should be 1-6. Adds at point-max.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert (make-string level ?*) " " title "\n")
    (format "Added level %d heading: %s" level title)))

(defun claudemacs-ai-memory-add-todo (level title &optional priority)
  "Add a TODO heading with LEVEL stars and TITLE to memory buffer.
Optional PRIORITY should be A, B, or C.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert (make-string level ?*) " TODO ")
    (when priority
      (insert (format "[#%s] " (upcase priority))))
    (insert title "\n")
    (format "Added TODO: %s" title)))

(defun claudemacs-ai-memory-toggle-todo ()
  "Toggle TODO state of the heading at point in memory buffer.
Cycles through: unmarked -> TODO -> DONE -> unmarked.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-todo)
    (let ((state (org-get-todo-state)))
      (format "TODO state: %s" (or state "none")))))

(defun claudemacs-ai-memory-set-todo-state (state)
  "Set the TODO STATE of heading at point in memory buffer.
STATE should be TODO, DONE, or empty string to clear.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-todo (if (string-empty-p state) 'none state))
    (format "Set TODO state to: %s" (or state "none"))))

(defun claudemacs-ai-memory-add-timestamp (&optional inactive)
  "Add current timestamp at point-max in memory buffer.
If INACTIVE is non-nil, use inactive timestamp [date] instead of <date>.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (goto-char (point-max))
    (let ((ts (format-time-string (if inactive "[%Y-%m-%d %a %H:%M]" "<%Y-%m-%d %a %H:%M>"))))
      (insert ts)
      (format "Added timestamp: %s" ts))))

(defun claudemacs-ai-memory-get-headings ()
  "Get all headings from the memory buffer as a structured list.
Returns list of (level title todo-state) for each heading.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-map-entries
     (lambda ()
       (list (org-current-level)
             (org-get-heading t t t t)
             (org-get-todo-state)))
     nil nil)))

(defun claudemacs-ai-memory-goto-heading (title)
  "Go to the first heading matching TITLE in memory buffer.
Returns the position or nil if not found.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (goto-char (point-min))
    (if (re-search-forward (format org-complex-heading-regexp-format (regexp-quote title)) nil t)
        (progn
          (org-beginning-of-line)
          (format "Moved to heading: %s at position %d" title (point)))
      (format "Heading not found: %s" title))))

(defun claudemacs-ai-memory-add-list-item (text &optional checkbox)
  "Add a list item with TEXT to memory buffer at point-max.
If CHECKBOX is non-nil, add a checkbox.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert "- ")
    (when checkbox
      (insert "[ ] "))
    (insert text "\n")
    (format "Added list item: %s" text)))

(defun claudemacs-ai-memory-toggle-checkbox ()
  "Toggle checkbox at current line in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-toggle-checkbox)
    "Toggled checkbox"))

(defun claudemacs-ai-memory-schedule (timestamp)
  "Add SCHEDULED timestamp to heading at point in memory buffer.
TIMESTAMP should be org-compatible like '<2024-01-15 Mon>' or '+1d'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-schedule nil timestamp)
    (format "Scheduled: %s" timestamp)))

(defun claudemacs-ai-memory-deadline (timestamp)
  "Add DEADLINE timestamp to heading at point in memory buffer.
TIMESTAMP should be org-compatible like '<2024-01-15 Mon>' or '+1d'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-deadline nil timestamp)
    (format "Deadline: %s" timestamp)))

(defun claudemacs-ai-memory-set-property (property value)
  "Set PROPERTY to VALUE on heading at point in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-set-property property value)
    (format "Set property %s = %s" property value)))

(defun claudemacs-ai-memory-set-tags (tags)
  "Set TAGS on heading at point in memory buffer.
TAGS should be a colon-separated string like ':tag1:tag2:'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-set-tags tags)
    (format "Set tags: %s" tags)))

(defun claudemacs-ai-memory-clock-in ()
  "Start clocking time on heading at point in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-clock-in)
    "Clock started"))

(defun claudemacs-ai-memory-clock-out ()
  "Stop clocking time in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-clock-out)
    "Clock stopped"))

(defun claudemacs-ai-memory-add-note ()
  "Add a note to heading at point in memory buffer (with timestamp).
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-add-note)
    "Note drawer added - ready for input"))

(defun claudemacs-ai-memory-set-effort (effort)
  "Set effort estimate EFFORT on heading at point.
EFFORT should be like '1:30' for 1 hour 30 minutes.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-set-effort nil effort)
    (format "Effort set: %s" effort)))

(defun claudemacs-ai-memory-archive-subtree ()
  "Archive the subtree at point in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-archive-subtree)
    "Subtree archived"))

(defun claudemacs-ai-memory-insert-link (url &optional description)
  "Insert an org link to URL with optional DESCRIPTION at point-max.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (goto-char (point-max))
    (if description
        (insert (format "[[%s][%s]]" url description))
      (insert (format "[[%s]]" url)))
    (format "Inserted link: %s" url)))

(defun claudemacs-ai-memory-sparse-tree (query)
  "Create a sparse tree in memory buffer matching QUERY.
QUERY can be a tag match like '+work-urgent' or a property match.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-match-sparse-tree nil query)
    (format "Sparse tree created for: %s" query)))

(defun claudemacs-ai-memory-get-property (property)
  "Get the value of PROPERTY from heading at point in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (or (org-entry-get (point) property)
        "nil")))

(defun claudemacs-ai-memory-promote ()
  "Promote heading at point (decrease level) in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-promote-subtree)
    "Heading promoted"))

(defun claudemacs-ai-memory-demote ()
  "Demote heading at point (increase level) in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-demote-subtree)
    "Heading demoted"))

(defun claudemacs-ai-memory-move-up ()
  "Move subtree at point up in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-move-subtree-up)
    "Subtree moved up"))

(defun claudemacs-ai-memory-move-down ()
  "Move subtree at point down in memory buffer.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai--ensure-memory-buffer)
    (org-move-subtree-down)
    "Subtree moved down"))

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

;;;; Session Management

(defun claudemacs-ai-restart-and-resume (&optional buffer-name)
  "Restart the claudemacs session in BUFFER-NAME and resume the conversation.
If BUFFER-NAME is not provided, uses the current buffer.
This reloads the MCP server with any code changes.
Designed to be called via emacsclient by Claude AI."
  (let ((target-buffer (or buffer-name (buffer-name))))
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
          (run-at-time 0.5 nil
                       (lambda (buf dir)
                         (when (get-buffer buf)
                           (with-current-buffer buf
                             ;; Kill the current session
                             (eat-kill-process)
                             (kill-buffer buf)
                             ;; Wait a moment, then restart with resume in the same directory
                             (run-at-time 0.3 nil
                                         (lambda (work-dir)
                                           (require 'claudemacs)
                                           (claudemacs--start work-dir "--resume")
                                           ;; Auto-select first session after resume prompt appears
                                           (run-at-time 2.0 nil
                                                       (lambda ()
                                                         (let ((resume-buffer (get-buffer (format "*claudemacs:%s*" work-dir))))
                                                           (when (and resume-buffer (buffer-live-p resume-buffer))
                                                             (with-current-buffer resume-buffer
                                                               (when (and (boundp 'eat-terminal) eat-terminal)
                                                                 ;; Send "1" and Enter to select first session
                                                                 (eat-term-send-string eat-terminal "1")
                                                                 (eat-term-input-event eat-terminal 1 'return))))))))
                                         dir))))
                       target-buffer work-dir)
          (format "Restart scheduled for buffer '%s' in directory '%s' - session will reload and resume shortly"
                  target-buffer work-dir))
      (error "Buffer '%s' is not a claudemacs buffer" target-buffer))))

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
