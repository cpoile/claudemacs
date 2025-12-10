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
(require 'claudemacs-ai-messaging)
(require 'claudemacs-ai-magit)
(require 'claudemacs-ai-notes)

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

(defun claudemacs-ai-get-buffer-content (buffer-name &optional tail-lines head-lines start-line end-line)
  "Return the content of BUFFER-NAME.
If TAIL-LINES is provided, return only the last TAIL-LINES lines.
If HEAD-LINES is provided, return only the first HEAD-LINES lines.
If START-LINE and END-LINE are provided, return lines in that range (1-indexed, inclusive).
Only one of TAIL-LINES, HEAD-LINES, or START-LINE/END-LINE should be used.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (cond
         ;; Line range
         ((and start-line end-line)
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- start-line))
            (let ((start-pos (point)))
              (forward-line (1+ (- end-line start-line)))
              (buffer-substring-no-properties start-pos (point)))))
         ;; Head lines
         (head-lines
          (save-excursion
            (goto-char (point-min))
            (forward-line head-lines)
            (buffer-substring-no-properties (point-min) (point))))
         ;; Tail lines
         (tail-lines
          (save-excursion
            (goto-char (point-max))
            (forward-line (- tail-lines))
            (buffer-substring-no-properties (point) (point-max))))
         ;; Full buffer
         (t
          (buffer-substring-no-properties (point-min) (point-max)))))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-get-region (buffer-name start end)
  "Return content of BUFFER-NAME from START to END.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (buffer-substring-no-properties start end))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claudemacs-ai-search-buffer (buffer-name pattern &optional context-before context-after case-insensitive limit)
  "Search for PATTERN in BUFFER-NAME and return matches with context.
CONTEXT-BEFORE: number of lines to show before each match (default 0)
CONTEXT-AFTER: number of lines to show after each match (default 0)
CASE-INSENSITIVE: if non-nil, ignore case (default nil)
LIMIT: maximum number of matches to return (default nil for unlimited)

Returns matches as a formatted string similar to grep output, where:
- Lines before the match have '  ' prefix
- The matching line has '> ' prefix and shows the line number
- Lines after the match have '  ' prefix
- Match groups are separated by '--'

Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))

  (with-current-buffer buffer-name
    (let ((output '())
          (case-fold-search case-insensitive)
          (before (or context-before 0))
          (after (or context-after 0))
          (count 0))
      (save-excursion
        (goto-char (point-min))
        (while (and (re-search-forward pattern nil t)
                    (or (not limit) (< count limit)))
          (let* ((match-line (line-number-at-pos))
                 (match-line-content (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))
                 (lines '()))

            ;; Collect context before
            (when (> before 0)
              (save-excursion
                (forward-line (- before))
                (dotimes (_ before)
                  (let ((line-num (line-number-at-pos)))
                    (push (format "%6d:  %s" line-num
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))
                          lines))
                  (forward-line 1))))

            ;; Add the matching line with > prefix
            (push (format "%6d:> %s" match-line match-line-content) lines)

            ;; Collect context after
            (when (> after 0)
              (save-excursion
                (forward-line 1)
                (dotimes (_ after)
                  (unless (eobp)
                    (let ((line-num (line-number-at-pos)))
                      (push (format "%6d:  %s" line-num
                                    (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                            lines))
                    (forward-line 1)))))

            ;; Add this match group to output
            (setq output (append output (nreverse lines)))
            (setq count (1+ count))

            ;; Add separator between matches (but not after the last one)
            (when (and (< count (or limit most-positive-fixnum))
                       (not (eobp)))
              (setq output (append output (list "--"))))

            ;; Move to next line to avoid matching same line multiple times
            (forward-line 1))))

      ;; Return as newline-separated string
      (mapconcat 'identity output "\n"))))

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
          ;; Clear any partial input first with Ctrl+U
          (eat-term-send-string eat-terminal "\C-u")
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

;;;; Notes - See claudemacs-ai-notes.el for all notes functionality
;; All notes functions have been moved to claudemacs-ai-notes.el
;; They are re-exported from that module for backward compatibility

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
If BUFFER-NAME is not provided, uses `claudemacs-session-cwd' (set by MCP server).
This reloads elisp files and restarts the MCP server with any code changes.
Designed to be called via emacsclient by Claude AI."
  (require 'claudemacs-ai-messaging)
  (let* ((target-buffer (or buffer-name
                            ;; Find the agent buffer from MCP session cwd
                            (when (and (boundp 'claudemacs-session-cwd) claudemacs-session-cwd)
                              (claudemacs-ai-find-agent-by-cwd claudemacs-session-cwd))
                            ;; Error if we can't determine the session
                            (error "Cannot determine claudemacs session - claudemacs-session-cwd not set"))))
    ;; Check if this is a claudemacs buffer
    (if (and (get-buffer target-buffer)
             (string-match-p "^\\*claudemacs:" target-buffer))
        (let ((work-dir (with-current-buffer target-buffer
                         (or claudemacs--cwd
                             ;; Fallback: extract directory from buffer name
                             ;; *claudemacs:/path/to/dir/* or *claudemacs:/path/to/dir:agent-name*
                             (when (string-match "^\\*claudemacs:\\([^:]+\\)" target-buffer)
                               (match-string 1 target-buffer))))))
          (unless work-dir
            (error "Cannot determine working directory for buffer '%s'" target-buffer))
          ;; Use run-at-time to defer execution so we can return a response first
          ;; Pass work-dir and buffer name to claudemacs-restart to target the correct session
          (run-at-time 0.5 nil
                       (lambda (dir buf)
                         (require 'claudemacs)
                         ;; Call claudemacs-restart with both work-dir and buffer-name
                         (claudemacs-restart dir buf))
                       work-dir target-buffer)
          (format "Restart scheduled for %s (buffer: %s) - session will reload elisp files, restart MCP server, and resume shortly"
                  work-dir target-buffer))
      (error "Buffer '%s' is not a claudemacs buffer" target-buffer))))

;;;; Project Shell for Bash Execution


(defun claudemacs-ai-bash-hook-script ()
  "Generate shell hook script for bash/zsh command completion callbacks.
Returns a string containing the hook setup script."
  "
__claudemacs_post_command() {
    local exit_code=$?
    # Run in subshell to hide job control messages
    if command -v curl >/dev/null 2>&1; then
        (curl -X POST -H 'Content-Type: application/json' \\
             -d '{\"shell_id\":\"'$CLAUDEMACS_SHELL_ID'\",\"exit_code\":'$exit_code'}' \\
             \"http://localhost:$CLAUDEMACS_MCP_PORT/bash-command\" >/dev/null 2>&1 &)
    elif command -v python3 >/dev/null 2>&1; then
        (python3 -c \"
import urllib.request, json
try:
    data = json.dumps({'shell_id':'$CLAUDEMACS_SHELL_ID','exit_code':$exit_code}).encode()
    req = urllib.request.Request('http://localhost:$CLAUDEMACS_MCP_PORT/bash-command', data=data, headers={'Content-Type':'application/json'})
    urllib.request.urlopen(req, timeout=1)
except: pass
\" &)
    fi
    return $exit_code
}

if [ -n \"$BASH_VERSION\" ]; then
    if [ -n \"$PROMPT_COMMAND\" ]; then
        PROMPT_COMMAND=\"__claudemacs_post_command; $PROMPT_COMMAND\"
    else
        PROMPT_COMMAND=\"__claudemacs_post_command\"
    fi
elif [ -n \"$ZSH_VERSION\" ]; then
    if ! (( \${precmd_functions[(I)__claudemacs_post_command]} )); then
        precmd_functions+=(__claudemacs_post_command)
    fi
fi
")

(defun claudemacs-ai-inject-bash-hooks (buffer-name)
  "Inject bash/zsh completion hooks into shell BUFFER-NAME.
This sets up PROMPT_COMMAND/precmd_functions to call back to MCP server."
  (when-let ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (when (and (boundp 'eat-terminal) eat-terminal)
        ;; Write hook script to a temp file and source it
        (let* ((temp-file (make-temp-file "claudemacs-hooks-" nil ".sh"))
               (hook-script (claudemacs-ai-bash-hook-script)))
          (with-temp-file temp-file
            (insert hook-script))
          ;; Source the file in the shell
          (eat-term-send-string eat-terminal (format "source %s && rm %s" temp-file temp-file))
          (eat-term-input-event eat-terminal 1 'return)
          (message "Injected bash hooks into %s via %s" buffer-name temp-file))))))

(defun claudemacs-ai-get-project-shell (directory &optional mcp-port)
  "Get or create an eat shell for DIRECTORY with optional MCP-PORT.
Returns the buffer name. Creates the shell if it doesn't exist.
If MCP-PORT is provided, sets up bash/zsh hooks for event-driven command completion.
Designed to be called via emacsclient by Claude AI."
  (require 'eat)
  (let* ((work-dir (expand-file-name directory))
         ;; eat-make wraps name with *...*, so we use name without asterisks
         (shell-name (format "eat-shell:%s" (file-name-nondirectory
                                              (directory-file-name work-dir))))
         (buffer-name (format "*%s*" shell-name)))
    (if (get-buffer buffer-name)
        ;; Buffer exists - return it (hooks were already injected when created)
        buffer-name
      ;; Create new shell with environment variables for HTTP callback
      (let* ((default-directory work-dir)
             ;; Set environment variables for the shell process
             ;; Use buffer-name as shell_id so Python and shell agree on the identifier
             (process-environment
              (if mcp-port
                  (append process-environment
                         (list (format "CLAUDEMACS_MCP_PORT=%d" mcp-port)
                               (format "CLAUDEMACS_SHELL_ID=%s" buffer-name)))
                process-environment)))
        (with-current-buffer (eat-make shell-name
                                       (or (getenv "SHELL") "/bin/bash")
                                       nil)
          (setq-local default-directory work-dir))

        ;; Inject hooks asynchronously after shell is ready
        (when mcp-port
          (run-at-time 2.0 nil #'claudemacs-ai-inject-bash-hooks buffer-name))

        buffer-name))))

(defun claudemacs-ai-project-shell-ready-p (buffer-name)
  "Check if project shell BUFFER-NAME has an active eat terminal.
Returns t if ready, nil otherwise."
  (when-let ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (and (boundp 'eat-terminal) eat-terminal t))))

(defun claudemacs-ai-interrupt-shell (buffer-name)
  "Send interrupt signal (Ctrl+C) to shell BUFFER-NAME.
This kills the currently running command and returns to the prompt.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if (and (boundp 'eat-terminal) eat-terminal)
            (progn
              ;; Send Ctrl+C (interrupt signal)
              (eat-term-send-string-as-yank eat-terminal "\C-c")
              (format "Sent interrupt signal (Ctrl+C) to %s" buffer-name))
          (error "Buffer '%s' is not an eat terminal" buffer-name)))
    (error "Buffer '%s' does not exist" buffer-name)))

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

(defun claudemacs-ai-clear-buffer (buffer-name)
  "Clear the terminal content in BUFFER-NAME by truncating the buffer.
This removes accumulated history to improve performance.
Designed to be called via MCP by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))

  (let ((original-size 0)
        (new-size 0))
    (with-current-buffer buffer-name
      (unless (and (boundp 'eat-terminal) eat-terminal)
        (error "Buffer '%s' is not a claudemacs buffer (no eat-terminal)" buffer-name))

      (let ((process (eat-term-parameter eat-terminal 'eat--process)))
        (unless (and process (process-live-p process))
          (error "Claudemacs agent in '%s' is not running" buffer-name))

        ;; Capture original size
        (setq original-size (buffer-size))

        ;; Truncate buffer if too large
        (when (> original-size 50000)
          (let ((inhibit-read-only t))
            ;; Keep only last 10000 chars
            (delete-region (point-min) (max (point-min) (- (point-max) 10000)))
            (setq new-size (buffer-size))))))

    (if (> original-size 50000)
        (format "Truncated buffer %s: %d → %d chars (removed %d)"
                buffer-name original-size new-size (- original-size new-size))
      (format "Buffer %s is only %d chars, no truncation needed" buffer-name original-size))))

(provide 'claudemacs-ai)
;;; claudemacs-ai.el ends here
