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
Uses CLAUDEMACS_SOCKET environment variable to create a unique buffer per session."
  (let ((socket (or (getenv "CLAUDEMACS_SOCKET")
                    (and (boundp 'server-socket-dir)
                         server-socket-dir
                         (expand-file-name "server" server-socket-dir))
                    "default")))
    (format "*claudemacs-memory:%s*" (file-name-nondirectory socket))))

(defun claudemacs-ai--ensure-memory-buffer ()
  "Ensure the memory buffer exists and return it."
  (let ((buffer-name (claudemacs-ai--get-memory-buffer-name)))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (text-mode)
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
