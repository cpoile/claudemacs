;;; claudemacs.el --- AI pair programming with Claude Code -*- lexical-binding: t; -*-
;; Author: Christopher Poile <cpoile@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; Keywords: claudecode ai emacs llm ai-pair-programming tools
;; URL: https://github.com/cpoile/claudemacs
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Claudemacs integrates with Claude Code (https://docs.anthropic.com/en/docs/claude-code/overview)
;; for AI-assisted programming in Emacs using the eat terminal emulator.
;;
;; Inspired by Aidermacs: https://github.com/MatthewZMD/aidermacs and
;; claude-code.el: https://github.com/stevemolitor/claude-code.el

;;; Changelog:

;; Version 0.2.0 (2025-12-14)
;; - Multi-tool support: Added `claudemacs-tool-registry' to support multiple
;;   AI coding tools (Claude, Codex, Gemini, etc.) with per-tool configuration
;; - Multiple instances: Run multiple sessions of the same tool per workspace
;;   (claude, claude-2, claude-3, etc.)
;; - New session management system with workspace-aware session tracking
;; - New commands: `claudemacs-switch-to-session', `claudemacs-switch-other',
;;   `claudemacs-kill-specific-session'
;; - New transient menus: `claudemacs-start-menu' and `claudemacs-resume-menu'
;;   with dynamic tool selection and custom flags (-d, -f, -p)
;; - Action system: Send commands to all sessions with C-u prefix
;; - Improved workspace detection supporting Doom, perspective.el, and
;;   vanilla Emacs
;; - Improved projectile support with `.projectile' marker file detection
;; - Error handling: Validate program exists before starting, catch startup
;;   failures and clean up orphaned buffers
;; - New `claudemacs-setup' function for explicit initialization
;; - Added `claudemacs-unload-function' for proper cleanup on unload
;; - Explicitly declared `transient' dependency in Package-Requires
;; - New `claudemacs-tool-name-face' for menu highlighting
;; - Buffer naming: `*claudemacs:TOOL:SESSION-ID*' format
;; - Fixed unreachable code in `claudemacs-toggle-buffer'

;; Version 0.1.0 (2024-11-01)
;; - Initial release

;;; Code:

;;;; Dependencies
(require 'cl-lib)
(require 'transient)
(require 'project)
(require 'vc-git)
(require 'eat nil 'noerror)
(require 'claudemacs-comment)

;; Declare functions from optional packages
(declare-function safe-persp-name "perspective")
(declare-function get-current-persp "perspective")
(declare-function flycheck-error-message "flycheck")
(declare-function flycheck-overlay-errors-in "flycheck")
(declare-function projectile-project-root "projectile")

;;;; Customization
(defgroup claudemacs nil
  "AI pair programming with Claude Code."
  :group 'tools)

(defcustom claudemacs-program "claude"
  "The name or path of the claude-code program."
  :type 'string
  :group 'claudemacs)

(defcustom claudemacs-program-switches nil
  "List of command line switches to pass to the Claude program.
These are passed as SWITCHES parameters to `eat-make`.
E.g, `\'(\"--verbose\" \"--dangerously-skip-permissions\")'"
  :type '(repeat string)
  :group 'claudemacs)

(defcustom claudemacs-tool-registry
  '((claude :program "claude" :switches nil)
    (codex :program "codex" :switches nil)
    (gemini :program "gemini-cli" :switches nil))
  "Registry of AI coding tools available for use with claudemacs.
Each entry is a list of the form (TOOL-NAME PLIST) where PLIST contains:
  :program  - The name or path of the tool's executable
  :switches - List of command line switches to pass to the program

Example:
  ((claude :program \"claude\" :switches nil)
   (codex :program \"codex\" :switches '(\"--model\" \"gpt-4\"))
   (gemini :program \"gemini\" :switches nil)
   (aider :program \"aider\" :switches '(\"--no-auto-commits\")))"
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'claudemacs)

(defcustom claudemacs-default-tool 'claude
  "The default AI coding tool to use when starting a new session.
Must be a symbol corresponding to a key in `claudemacs-tool-registry'.
When using the smart session start (key 's'), this tool will be used
if no other sessions exist in the current workspace."
  :type 'symbol
  :group 'claudemacs)

(defcustom claudemacs-prefer-projectile-root nil
  "Whether to prefer projectile root over git root when available.
If non-nil and projectile is loaded, use `projectile-project-root' to
determine the project root instead of `vc-git-root'. If projectile is
not available or fails to find a project root, falls back to git root
detection. This option has no effect if projectile is not installed."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-create t
  "Whether to switch to the Claudemacs buffer when creating a new session.
If non-nil, automatically switch to the Claude buffer after starting.
If nil, create the session but don't switch focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-toggle t
  "Whether to switch to the Claudemacs buffer when toggling to show it.
If non-nil, switch to the Claude buffer when toggling from hidden to visible.
If nil, show the buffer but don't switch focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-m-return-is-submit nil
  "Swap the behavior of RET and M-RET in claudemacs buffers.
If nil (default): RET submits input, M-RET creates new line (standard behavior).
If non-nil: M-RET submits input, RET creates new line (swapped behavior).

This setting only affects claudemacs buffers and does not impact other
eat buffers."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-shift-return-newline t
  "Whether Shift-Return creates a newline in claudemacs buffers.
If non-nil: S-RET acts like M-RET (creates a newline).
If nil (default): S-RET has default behavior.

This provides an alternative way to create newlines without using M-RET."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-file-add nil
  "Whether to switch to the Claudemacs buffer when adding file references.
If non-nil, automatically switch to the Claude buffer after adding files.
If nil, add the file reference but don't switch focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-use-shell-env nil
  "Whether to run Claude through an interactive shell to load shell environment.
If non-nil, Claude is invoked through the user's interactive shell (e.g., zsh -i -c)
which sources rc files like .zshrc or .bashrc, making shell-configured PATH and
environment variables available to Claude.
If nil (default), Claude is invoked directly without shell environment loading.
This preserves backward compatibility for users whose existing setup works correctly."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-send-error nil
  "Whether to switch to the Claudemacs buffer when sending error fix requests.
If non-nil, automatically switch to the Claude buffer after sending
error fix requests. If nil, send the error fix request but don't switch
focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-switch-to-buffer-on-add-context t
  "Whether to switch to the Claudemacs buffer when adding context.
If non-nil, automatically switch to the Claude buffer after adding context.
If nil, add the context but don't switch focus to it."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-notify-on-await t
  "Whether to show a system notification when Claude Code is awaiting the user.
When non-nil, display an OS notification popup when Claude completes a task.
When nil, no notification is shown (silent operation)."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-notification-sound-mac "Submarine"
  "The sound to use when displaying system notifications on macOS.

System sounds include: `Basso', `Blow', `Bottle', `Frog', `Funk',
`Glass', `Hero', `Morse', `Ping', `Pop', `Purr', `Sosumi', `Submarine',
`Tink'. Or put more sounds in the `/Library/Sound' folder and use those."
  :type 'string
  :group 'claudemacs)

(defcustom claudemacs-notification-auto-dismiss-linux t
  "Whether to auto-dismiss notifications on Linux (don't persist to system tray).
When non-nil, notifications will automatically disappear and not stay in the tray.
When nil, notifications will persist in the system tray according to system defaults.
This setting only affects Linux systems using notify-send."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-notification-sound-linux "bell"
  "The sound to use when displaying system notifications on Linux.
Uses canberra-gtk-play if available.  Common sound IDs include:
`message-new-instant', `bell', `dialog-error', `dialog-warning'.
When empty string, no sound is played."
  :type 'string
  :group 'claudemacs)

(defcustom claudemacs-startup-hook nil
  "Hook run after a claudemacs session has finished starting up.
This hook is called after the eat terminal is initialized, keymaps
are set up, and bell handlers are configured. The hook functions
are executed with the claudemacs buffer as the current buffer."
  :type 'hook
  :group 'claudemacs)

(defface claudemacs-repl-face
  nil
  "Face for Claude REPL."
  :group 'claudemacs)

(defface claudemacs-tool-name-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for highlighting tool names in transient menu descriptions."
  :group 'claudemacs)

;;;; Buffer-local Variables
(defvar-local claudemacs--cwd nil
  "Buffer-local variable storing the current working directory for this Claude session.")

(defvar-local claudemacs--tool nil
  "Buffer-local variable storing the AI tool name (symbol) for this session.")

;;;;
;;;; Utility Functions
;;;;

(defun claudemacs--find-projectile-root (dir)
  "Find project root by looking for .projectile marker file from DIR.
Returns the directory containing .projectile, or nil if not found."
  (when-let ((root (locate-dominating-file dir ".projectile")))
    (file-name-as-directory root)))

(defun claudemacs--project-root (&optional dir)
  "Get the project root for starting the CLI.
If DIR is given, use it as the starting location.
When `claudemacs-prefer-projectile-root' is enabled, tries in order:
1. `projectile-project-root' (if projectile is installed)
2. .projectile marker file (works without projectile)
Then falls back to `vc-git-root', then to the directory itself."
  (let ((loc (or dir
                 (when (buffer-file-name)
                   (file-name-directory (buffer-file-name)))
                 default-directory)))
    (or
     ;; Try projectile if enabled
     (when claudemacs-prefer-projectile-root
       (or
        ;; First try projectile-project-root if available
        (when (fboundp 'projectile-project-root)
          (condition-case nil
              (let ((proj-root (projectile-project-root)))
                (when (and proj-root (file-directory-p proj-root))
                  proj-root))
            (error nil)))
        ;; Fall back to .projectile marker file
        (claudemacs--find-projectile-root loc)))
     ;; Fallback to vc-git-root (built-in via vc-git)
     (vc-git-root loc)
     ;; Final fallback to location itself
     loc)))

(defun claudemacs--get-tool-config (tool)
  "Get the configuration plist for TOOL from `claudemacs-tool-registry'.
Returns nil if the tool is not found in the registry."
  (cdr (assq tool claudemacs-tool-registry)))

(defun claudemacs--get-resume-flag (tool)
  "Get the resume flag for TOOL.
Returns '--resume' for claude, 'resume' for codex, '--resume' for others."
  (pcase tool
    ('claude "--resume")
    ('codex "resume")
    (_ "--resume")))

(defun claudemacs--get-current-tool-name ()
  "Get the display name (capitalized) of the current session's tool.
Returns the tool name as a string (e.g., \"Claude\", \"Codex\").
Returns \"Claude\" as fallback if no session is found."
  (let* ((session-buffer (claudemacs--get-current-session-buffer))
         (tool (when session-buffer
                 (with-current-buffer session-buffer
                   claudemacs--tool))))
    (capitalize (symbol-name (or tool claudemacs-default-tool)))))

(defun claudemacs--get-workspace-name ()
  "Return the current workspace name if available, or nil.
Checks workspace systems in priority order:
1. Doom Emacs (+workspace-current-name)
2. Doom's persp wrapper (safe-persp-name + get-current-persp)
3. Vanilla perspective.el (persp-current-name)"
  (cl-flet ((valid-ws-p (ws) (and ws (stringp ws) (not (string-empty-p ws)))))
    (cond
     ;; Doom Emacs workspace
     ((fboundp '+workspace-current-name)
      (let ((ws (+workspace-current-name)))
        (when (valid-ws-p ws) ws)))
     ;; Doom's perspective wrapper
     ((and (fboundp 'safe-persp-name) (fboundp 'get-current-persp))
      (let ((ws (safe-persp-name (get-current-persp))))
        (when (valid-ws-p ws) ws)))
     ;; Vanilla perspective.el
     ((fboundp 'persp-current-name)
      (let ((ws (persp-current-name)))
        (when (valid-ws-p ws) ws))))))

(defun claudemacs--session-id ()
  "Return an identifier for the current Claudemacs session.
If a workspace is active (checking various workspace packages),
use its name, otherwise fall back to the project root."
  (or (claudemacs--get-workspace-name)
      (file-truename (claudemacs--project-root))))

(defun claudemacs--get-instance-numbers-for-tool (tool)
  "Get a list of instance numbers currently in use for TOOL in current workspace.
Returns a sorted list of integers (e.g., (1 2 3) if claude, claude-2, claude-3 exist)."
  (let ((session-id (claudemacs--session-id))
        (tool-str (symbol-name tool))
        (numbers '()))
    (dolist (buf (buffer-list))
      (when (claudemacs--is-claudemacs-buffer-p buf)
        (let ((buf-name (buffer-name buf)))
          ;; Match both "tool:session" and "tool-N:session" patterns
          (when (string-match (format "^\\*claudemacs:%s\\(-\\([0-9]+\\)\\)?:%s\\*$"
                                      (regexp-quote tool-str)
                                      (regexp-quote session-id))
                              buf-name)
            (let ((num-str (match-string 2 buf-name)))
              (push (if num-str (string-to-number num-str) 1) numbers))))))
    (sort numbers #'<)))

(defun claudemacs--get-next-instance-number (tool)
  "Get the next available instance number for TOOL.
Returns 1 if no instances exist, or the next sequential number."
  (let ((used (claudemacs--get-instance-numbers-for-tool tool)))
    (if (null used)
        1
      ;; Find first gap or use max+1
      (let ((n 1))
        (while (member n used)
          (setq n (1+ n)))
        n))))

(defun claudemacs--format-tool-instance-name (tool &optional instance-num)
  "Format TOOL with INSTANCE-NUM into display name.
Returns 'tool' for instance 1, 'tool-N' for N > 1."
  (if (or (null instance-num) (= instance-num 1))
      (symbol-name tool)
    (format "%s-%d" tool instance-num)))

(defun claudemacs--get-buffer-name-for-instance (tool instance-num)
  "Generate buffer name for TOOL at INSTANCE-NUM.
Format: *claudemacs:TOOL:SESSION-ID* or *claudemacs:TOOL-N:SESSION-ID*"
  (let ((instance-name (claudemacs--format-tool-instance-name tool instance-num)))
    (format "*claudemacs:%s:%s*" instance-name (claudemacs--session-id))))

(defun claudemacs--get-buffer-name (&optional tool)
  "Generate the claudemacs buffer name based on TOOL and workspace session ID.
TOOL defaults to `claudemacs-default-tool' if not specified.
Format: *claudemacs:TOOL:SESSION-ID*
Note: This returns the name for the first instance. Use
`claudemacs--get-buffer-name-for-instance' for specific instances."
  (let ((tool-name (or tool claudemacs-default-tool)))
    (format "*claudemacs:%s:%s*" tool-name (claudemacs--session-id))))

(defun claudemacs--get-buffer (&optional tool)
  "Return existing claudemacs buffer for current session and TOOL.
TOOL defaults to `claudemacs-default-tool' if not specified."
  (get-buffer (claudemacs--get-buffer-name tool)))

(defun claudemacs--get-current-session-buffer ()
  "Return the most relevant claudemacs buffer for the current context.
Priority:
1. Current buffer if it's a claudemacs buffer
2. Most recently used session in current workspace
3. Session for default tool in current workspace
4. nil if no sessions exist"
  (cond
   ;; Already in a claudemacs buffer
   ((claudemacs--is-claudemacs-buffer-p (current-buffer))
    (current-buffer))
   ;; Get most recent session in workspace
   (t
    (let ((sessions (claudemacs--list-sessions-for-workspace)))
      (if sessions
          (plist-get (car sessions) :buffer)
        ;; Try default tool as fallback
        (claudemacs--get-buffer claudemacs-default-tool))))))

(defun claudemacs--is-claudemacs-buffer-p (&optional buffer)
  "Return t if BUFFER (or current buffer) is a claudemacs buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (string-match-p "^\\*claudemacs:" (buffer-name buf)))))

(defun claudemacs--switch-to-buffer (&optional tool)
  "Switch to the claudemacs buffer for current session and TOOL.
TOOL defaults to `claudemacs-default-tool' if not specified.
Returns t if switched successfully, nil if no buffer exists."
  (if-let* ((buffer (claudemacs--get-buffer tool)))
      (progn
        (with-current-buffer buffer
          (if (not eat-terminal)
              (error "Claudemacs session exists but no eat-terminal found. Please kill *claudemacs:...* buffer and re-start")
            (let ((process (eat-term-parameter eat-terminal 'eat--process)))
              (if (not (and process (process-live-p process)))
                (error "Claudemacs session exists but process is not running. Please kill *claudemacs:...* buffer and re-start")))))
        ;; we have a running eat-terminal
        (display-buffer buffer)
        (select-window (get-buffer-window buffer))
        t)
    nil))

;;;;
;;;; Session Management
;;;;

(defun claudemacs--list-all-sessions ()
  "Return a list of all active claudemacs session buffers.
Each element is a buffer object."
  (seq-filter #'claudemacs--is-claudemacs-buffer-p (buffer-list)))

(defun claudemacs--get-session-info (buffer)
  "Extract session information from BUFFER.
Returns a plist with :tool, :instance, :session-id, :buffer, :buffer-name.
The :tool is the base tool symbol (e.g., claude even for claude-2).
The :instance is the instance number (1 for claude, 2 for claude-2, etc.).
Returns nil if the buffer is not a claudemacs buffer."
  (when (claudemacs--is-claudemacs-buffer-p buffer)
    (let ((buf-name (buffer-name buffer)))
      ;; Buffer name format: *claudemacs:TOOL:SESSION-ID* or *claudemacs:TOOL-N:SESSION-ID*
      (when (string-match "^\\*claudemacs:\\([^-:]+\\)\\(?:-\\([0-9]+\\)\\)?:\\(.+\\)\\*$" buf-name)
        (let* ((tool-str (match-string 1 buf-name))
               (instance-str (match-string 2 buf-name))
               (session-id (match-string 3 buf-name))
               (instance (if instance-str (string-to-number instance-str) 1)))
          (list :tool (intern tool-str)
                :instance instance
                :session-id session-id
                :buffer buffer
                :buffer-name buf-name))))))

(defun claudemacs--list-sessions-for-workspace ()
  "Return a list of all active sessions in the current workspace.
Returns a list of plists with :tool, :session-id, :buffer, :buffer-name.
Only includes sessions that match the current workspace's session-id.
Sessions are sorted by most recently accessed (using buffer-display-time)."
  (let* ((current-session-id (claudemacs--session-id))
         (filtered-sessions
          (seq-filter (lambda (info)
                        (and info  ; Filter out nil entries first
                             (string= (plist-get info :session-id) current-session-id)))
                      (mapcar #'claudemacs--get-session-info
                              (claudemacs--list-all-sessions)))))
    ;; Sort by most recently accessed (buffer-display-time)
    (seq-sort-by (lambda (info)
                   (let ((buf (plist-get info :buffer)))
                     (or (condition-case nil
                             (when (and buf (buffer-live-p buf))
                               (with-current-buffer buf
                                 (when (and (boundp 'buffer-display-time)
                                            buffer-display-time)
                                   ;; Convert time value to float for comparison
                                   (float-time buffer-display-time))))
                           (error nil))
                         0.0)))
                 #'>
                 filtered-sessions)))

(defun claudemacs--list-available-tools ()
  "Return a list of tools from registry not yet started in current workspace.
Returns a list of symbols (tool names)."
  (let* ((active-tools (mapcar (lambda (info) (plist-get info :tool))
                               (claudemacs--list-sessions-for-workspace)))
         (all-tools (mapcar #'car claudemacs-tool-registry)))
    (seq-difference all-tools active-tools)))

;;;;
;;;; Session/Tool Selection
;;;;

(defun claudemacs--format-session-choice (info &optional is-current)
  "Format a session INFO plist as a choice string for selection.
If IS-CURRENT is non-nil, mark it as the current session.
Format: 'TOOL(-N):SESSION-ID (current)' or 'TOOL(-N):SESSION-ID'."
  (let* ((tool (plist-get info :tool))
         (instance (plist-get info :instance))
         (session-id (plist-get info :session-id))
         (instance-name (claudemacs--format-tool-instance-name tool instance)))
    (if is-current
        (format "%s:%s (current)" instance-name session-id)
      (format "%s:%s" instance-name session-id))))

(defun claudemacs--switch-to-session ()
  "Switch to a session in current workspace.
If one session exists, switch directly to it.
If multiple sessions exist, prompt for selection.
If no sessions exist, open the Start Session menu."
  (let ((active-sessions (claudemacs--list-sessions-for-workspace)))
    (cond
     ;; No sessions - open start menu
     ((null active-sessions)
      (claudemacs-start-menu))
     ;; Exactly one session - switch directly
     ((= (length active-sessions) 1)
      (let ((tool (plist-get (car active-sessions) :tool)))
        (claudemacs--switch-to-buffer tool)))
     ;; Multiple sessions - prompt for selection
     (t
      (let* ((choices (mapcar (lambda (info)
                                (cons (claudemacs--format-session-choice info)
                                      info))
                              active-sessions))
             (selected-name (completing-read "Switch to session: "
                                            (mapcar #'car choices) nil t))
             (info (cdr (assoc selected-name choices))))
        (when info
          (let ((buffer (plist-get info :buffer)))
            (display-buffer buffer)
            (select-window (get-buffer-window buffer)))))))))

(defun claudemacs--get-other-session ()
  "Get the second most recent session (the 'other' session).
Returns the session info plist, or nil if there aren't enough sessions."
  (let ((active-sessions (claudemacs--list-sessions-for-workspace)))
    (when (>= (length active-sessions) 2)
      (cadr active-sessions))))

(defun claudemacs--get-flycheck-errors-on-line ()
  "Get all flycheck errors on the current line."
  (when (and (bound-and-true-p flycheck-mode)
             (fboundp 'flycheck-overlay-errors-in))
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (flycheck-overlay-errors-in line-start line-end))))

(defun claudemacs--format-flycheck-errors (errors)
  "Format flycheck ERRORS for display to Claude."
  (cond
   ((null errors) "")
   ((= 1 (length errors))
    (flycheck-error-message (car errors)))
   ((<= (length errors) 3)
    (format "(%d errors: %s)"
            (length errors)
            (mapconcat (lambda (err) (flycheck-error-message err))
                      errors "; ")))
   (t
    (format "(%d errors including: %s; ...)"
            (length errors)
            (mapconcat (lambda (err) (flycheck-error-message err))
                      (seq-take errors 2) "; ")))))

;;;; Terminal Integration
;; Eat terminal emulator functions
(declare-function eat-make "eat")
(declare-function eat-term-send-string "eat")
(declare-function eat-term-send-string-as-yank "eat")
(declare-function eat-term-input-event "eat")
(declare-function eat-kill-process "eat")
(declare-function eat-term-parameter "eat")

;;;; Bell Handling
(defun claudemacs--bell-handler (terminal)
  "Handle bell events from Claude Code in TERMINAL.
This function is called when Claude Code sends a bell character."
  (ignore terminal)
  (when claudemacs-notify-on-await
    (let ((tool-name (capitalize (symbol-name (or claudemacs--tool claudemacs-default-tool)))))
      (claudemacs--system-notification (format "%s finished and is awaiting your input" tool-name)))))


(defun claudemacs--system-notification (message &optional title)
  "Show a system notification with MESSAGE and optional TITLE.
This works across macOS, Linux, and Windows platforms."
  (let ((title (or title "Claudemacs"))
        (message (or message "Claudemacs is finished and awaiting your input")))
    (cond
     ;; macOS
     ((eq system-type 'darwin)
      (call-process "osascript" nil nil nil
                    "-e" (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                                message title claudemacs-notification-sound-mac)))
     ;; Linux with notify-send and canberra-gtk-play
     ((and (eq system-type 'gnu/linux)
           (executable-find "notify-send"))
      (let ((args (if claudemacs-notification-auto-dismiss-linux
                      (list "--hint=int:transient:1" title message)
                    (list title message))))
        (apply #'call-process "notify-send" nil nil nil args))
      (when (and (not (string-empty-p claudemacs-notification-sound-linux))
                 (executable-find "canberra-gtk-play"))
        (call-process "canberra-gtk-play" nil nil nil
                      "--id" claudemacs-notification-sound-linux)))
     ;; Linux with kdialog (KDE)
     ((and (eq system-type 'gnu/linux)
           (executable-find "kdialog"))
      (call-process "kdialog" nil nil nil "--passivepopup"
                    (format "%s: %s" title message) "3"))
     ;; Windows with PowerShell
     ((eq system-type 'windows-nt)
      (call-process "powershell" nil nil nil
                    "-Command" 
                    (format "[System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms'); [System.Windows.Forms.MessageBox]::Show('%s', '%s')"
                            message title)))
     ;; Fallback: show in Emacs message area
     (t (message "%s: %s" title message)))))

(defun claudemacs--setup-eat-integration (buffer &optional retry-count)
  "Set up eat integration (keymap and bell handler) for BUFFER.
Retries using RETRY-COUNT up to 10 times if eat is not ready yet."
  (let ((retry-count (or retry-count 0)))
    (if (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (boundp 'eat-terminal) eat-terminal)))
        ;; Eat is ready, set up integration
        (progn
          (message "Eat is ready, setting up integrations")
          (with-current-buffer buffer
            (claudemacs--setup-buffer-keymap)
            (claudemacs-setup-bell-handler)
            ;; Run startup hook after setup is complete
            (run-hooks 'claudemacs-startup-hook)))
      ;; Eat not ready yet, retry if we haven't exceeded max attempts
      (when (< retry-count 10)
        (message "Eat not ready yet, retrying in 0.5s (attempt %d/10)" (1+ retry-count))
        (run-with-timer 0.5 nil
                        (lambda ()
                          (claudemacs--setup-eat-integration buffer (1+ retry-count))))))))

;;;###autoload
(defun claudemacs-setup-bell-handler ()
  "Set up or re-setup the completion notification handler.
Use this if system notifications aren't working after starting a session."
  (interactive)
  (with-current-buffer (claudemacs--get-current-session-buffer)
    (when (boundp 'eat-terminal)
      (setf (eat-term-parameter eat-terminal 'ring-bell-function)
            #'claudemacs--bell-handler)
      (message "Bell handler configured for claudemacs session"))))

(defun claudemacs--setup-repl-faces ()
  "Setup faces for the Claude REPL buffer.
Applies consistent styling to all eat-mode terminal faces."
  
  ;; Helper function to remap a face to inherit from claudemacs-repl-face
  (cl-flet ((remap-face (face &rest props)
              (apply #'face-remap-add-relative face :inherit 'claudemacs-repl-face props)))
    
    ;; Set buffer default face
    (buffer-face-set :inherit 'claudemacs-repl-face)
    
    ;; Remap all eat terminal faces to inherit from claudemacs-repl-face
    (mapc #'remap-face
          '(eat-shell-prompt-annotation-running
            eat-shell-prompt-annotation-success
            eat-shell-prompt-annotation-failure
            eat-term-bold eat-term-faint eat-term-italic
            eat-term-slow-blink eat-term-fast-blink))
    
    ;; Remap font faces (eat-term-font-0 through eat-term-font-9)
    (dotimes (i 10)
      (remap-face (intern (format "eat-term-font-%d" i))))
    
    ;; Specific overrides
    (face-remap-add-relative 'nobreak-space :underline nil)
    (remap-face 'eat-term-faint :foreground "#999999" :weight 'light)))

(defun claudemacs--ret-key ()
  "Send return key event to eat terminal."
  (interactive)
  (eat-term-input-event eat-terminal 1 'return))

(defun claudemacs--meta-ret-key ()
  "Send meta + return to eat terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\e\C-m"))

(defun claudemacs--send-escape ()
  "Send ESC to eat terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\e"))

;;;###autoload
(defun claudemacs-send-yes ()
  "Send yes (RET) to the active Claudemacs session."
  (interactive)
  (claudemacs--validate-process)
  (let ((buffer (claudemacs--get-current-session-buffer)))
    (with-current-buffer buffer
      (claudemacs--send-return-for-tool eat-terminal buffer))))

;;;###autoload
(defun claudemacs-send-no ()
  "Send no (ESC) to the active Claudemacs session."
  (interactive)
  (claudemacs--validate-process)
  (let ((buffer (claudemacs--get-current-session-buffer)))
    (with-current-buffer buffer
      (eat-term-send-string eat-terminal "\e"))))

(defun claudemacs--setup-buffer-keymap ()
  "Set up truly buffer-local keymap for claudemacs buffers with custom key bindings."
  (when (claudemacs--is-claudemacs-buffer-p)
    (message "Setting up buffer-local keymap for claudemacs buffer: %s" (buffer-name))
    
    ;; Create a new keymap that inherits from the current local map (eat-mode)
    (let ((map (make-sparse-keymap)))
      ;; Inherit all eat functionality by setting parent keymap
      (set-keymap-parent map (current-local-map))
      
      ;; Override specific keys for claudemacs functionality
      (define-key map (kbd "C-g") #'claudemacs--send-escape)
      (message "Defined C-g -> claudemacs--send-escape")

      ;; Handle return key swapping if enabled
      (when claudemacs-m-return-is-submit
        (define-key map (kbd "<return>") #'claudemacs--meta-ret-key)
        (define-key map (kbd "<M-return>") #'claudemacs--ret-key)
        (message "Swapped RET and M-RET"))
      
      ;; Handle shift-return newline if enabled
      (when claudemacs-shift-return-newline
        (define-key map (kbd "<S-return>") #'claudemacs--meta-ret-key)
        ;; alternative key representations that eat might use:
        ;(define-key map (kbd "S-RET") #'claudemacs--meta-ret-key)
        ;(define-key map (kbd "<shift-return>") #'claudemacs--meta-ret-key)
        (message "Defined S-RET -> newline"))

      ;; Apply the keymap as truly buffer-local
      (use-local-map map)
      (message "Applied buffer-local keymap successfully"))))

(defun claudemacs--get-shell-name ()
  "Get the path to the user's shell (e.g., '/bin/zsh', '/bin/bash').
Falls back to '/bin/sh' if SHELL environment variable is not set."
  (or (getenv "SHELL") "/bin/sh"))

(defun claudemacs--start (work-dir &optional tool instance-num &rest args)
  "Start AI coding tool TOOL in WORK-DIR with ARGS.
TOOL defaults to `claudemacs-default-tool' if not specified.
INSTANCE-NUM specifies which instance number to use (1, 2, 3, etc.).
If INSTANCE-NUM is nil, the next available instance number is used.
The tool configuration is looked up in `claudemacs-tool-registry'."
  (require 'eat)
  (let* ((tool-name (or tool claudemacs-default-tool))
         (tool-config (claudemacs--get-tool-config tool-name))
         (instance (or instance-num (claudemacs--get-next-instance-number tool-name)))
         (default-directory work-dir)
         (buffer-name (claudemacs--get-buffer-name-for-instance tool-name instance))
         (buffer (get-buffer-create buffer-name))
         ;; Capture buffer-local and tool-specific values before switching buffers
         (program (or (plist-get tool-config :program) claudemacs-program))
         (program-switches (or (plist-get tool-config :switches) claudemacs-program-switches))
         (use-shell-env claudemacs-use-shell-env)
         (process-environment
          (append '("TERM=xterm-256color")
                  process-environment)))
    ;; Verify program exists before attempting to start
    (unless (or use-shell-env (executable-find program))
      (kill-buffer buffer)
      (error "Program '%s' not found in PATH" program))

    (with-current-buffer buffer
      (cd work-dir)
      (setq-local eat-term-name "xterm-256color")
      (let ((process-adaptive-read-buffering nil)
            (switches (remove nil (append args program-switches))))
        (condition-case err
            (if use-shell-env
                ;; New behavior: Run through shell to source profile (e.g., .zprofile, .bash_profile)
                (let* ((shell (claudemacs--get-shell-name))
                       (claude-cmd (format "%s %s" program
                                           (mapconcat 'shell-quote-argument switches " "))))
                  (eat-make (substring buffer-name 1 -1) shell nil "-c" claude-cmd))
              ;; Original behavior: Run Claude directly without shell environment
              (apply #'eat-make (substring buffer-name 1 -1) program nil switches))
          (error
           (kill-buffer buffer)
           (error "Failed to start %s: %s" program (error-message-string err)))))

      ;; Set buffer-local variables after eat-make to ensure they persist
      (setq-local claudemacs--cwd work-dir)
      (setq-local claudemacs--tool tool-name)

      (claudemacs--setup-repl-faces)
      ;; Optimize scrolling for terminal input - allows text to go to bottom
      (setq-local scroll-conservatively 10000)  ; Never recenter
      (setq-local scroll-margin 0)              ; No margin so text goes to edge
      (setq-local maximum-scroll-margin 0)      ; No maximum margin
      (setq-local scroll-preserve-screen-position t)  ; Preserve position during scrolling
      
      ;; Additional stabilization for blinking character height changes
      (setq-local auto-window-vscroll nil)      ; Disable automatic scrolling adjustments
      (setq-local scroll-step 1)                ; Scroll one line at a time
      (setq-local hscroll-step 1)               ; Horizontal scroll one column at a time
      (setq-local hscroll-margin 0)             ; No horizontal scroll margin
      
      ;; Force consistent line spacing to prevent height fluctuations
      (setq-local line-spacing 0)               ; No extra line spacing
      
      ;; Disable eat's text blinking to reduce display changes
      (when (bound-and-true-p eat-enable-blinking-text)
        (setq-local eat-enable-blinking-text nil))
      
      ;; Force consistent character metrics for blinking symbols
      ;;(setq-local char-width-table nil)         ; causes emacs to crash!
      (setq-local vertical-scroll-bar nil)      ; Disable scroll bar
      (setq-local fringe-mode 0)                ; Disable fringes that can cause reflow
      
      ;; Replace problematic blinking character with consistent asterisk
      (let ((display-table (make-display-table)))
        (aset display-table #x23fa [?✽])  ; Replace ⏺ (U+23FA) with ✽
        (setq-local buffer-display-table display-table))
      
      ;; Set up custom key mappings & completion notifications after eat initialization
      (run-with-timer 0.1 nil
                      (lambda ()
                        (claudemacs--setup-eat-integration buffer))))
    
    (let ((window (display-buffer buffer)))
      (when claudemacs-switch-to-buffer-on-create
        (select-window window)))))

(defun claudemacs--translate-args-for-tool (tool args)
  "Translate generic ARGS to tool-specific arguments for TOOL.
Converts generic switches like '--dangerous-skip-permissions' to
tool-specific equivalents."
  (mapcar (lambda (arg)
            (cond
             ;; Translate dangerous skip permissions
             ((string= arg "--dangerous-skip-permissions")
              (cond
               ((eq tool 'claude) "--dangerously-skip-permissions")
               ((eq tool 'codex) "--dangerously-bypass-approvals-and-sandbox")
               (t arg)))  ; Unknown tool, pass through
             ;; All other args pass through unchanged
             (t arg)))
          args))

(defun claudemacs--run-with-args (tool &optional arg &rest args)
  "Start a new instance of AI coding tool TOOL with ARGS.
TOOL should be a symbol from `claudemacs-tool-registry'.
With prefix ARG, prompt for the project directory.
ARGS are translated to tool-specific arguments.
Always creates a new instance (claude, claude-2, etc.)."
  (let* ((explicit-dir (when arg (read-directory-name "Project directory: ")))
         (work-dir (or explicit-dir (claudemacs--project-root)))
         (translated-args (claudemacs--translate-args-for-tool tool args)))
    (apply #'claudemacs--start work-dir tool nil translated-args)))

;;;; Interactive Commands
;;;###autoload
(defun claudemacs-switch-to-session ()
  "Switch to the most recent session in current workspace.
Errors if no sessions exist (use Start submenu to create one)."
  (interactive)
  (claudemacs--switch-to-session))

;;;###autoload
(defun claudemacs-switch-other ()
  "Switch to the other (second most recent) session.
Useful when you have 2+ sessions and want to toggle between them."
  (interactive)
  (if-let* ((other-session (claudemacs--get-other-session)))
      (let ((buffer (plist-get other-session :buffer)))
        (display-buffer buffer)
        (select-window (get-buffer-window buffer)))
    (error "Need at least 2 sessions to switch to 'other' session")))

(defun claudemacs--get-next-available-tool ()
  "Get the next tool that isn't currently running.
Returns the first available tool from registry that doesn't have a session,
or nil if all tools are running."
  (car (claudemacs--list-available-tools)))

;;;###autoload
(defun claudemacs-kill ()
  "Kill Claudemacs process and close its window.
Works with the most relevant session (current buffer, or most recent)."
  (interactive)
  (if-let* ((claudemacs-buffer (claudemacs--get-current-session-buffer)))
      (progn
        (let ((tool (buffer-local-value 'claudemacs--tool claudemacs-buffer)))
          (with-current-buffer claudemacs-buffer
            (eat-kill-process)
            (kill-buffer claudemacs-buffer))
          (message "Claudemacs session (%s) killed" tool)))
    (error "There is no Claudemacs session in this workspace or project")))

(defun claudemacs-kill-specific-session ()
  "Select and kill a specific Claudemacs session.
Presents a list of all active sessions in the workspace for selection."
  (interactive)
  (let ((sessions (claudemacs--list-sessions-for-workspace)))
    (if (not sessions)
        (error "No Claudemacs sessions active in this workspace")
      (let* ((choices (mapcar (lambda (info)
                                (cons (claudemacs--format-session-choice info)
                                      info))
                              sessions))
             (selected-name (completing-read "Kill session: " choices nil t))
             (info (cdr (assoc selected-name choices))))
        (when info
          (let* ((buffer (plist-get info :buffer))
                 (tool (plist-get info :tool)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (eat-kill-process)
                (kill-buffer buffer))
              (message "Claudemacs session (%s) killed" tool))))))))

(defun claudemacs--validate-process ()
  "Validate that the Claudemacs process is alive and running.
Works with the most relevant session (current buffer, or most recent)."
  (let ((buffer (claudemacs--get-current-session-buffer)))
    (unless buffer
      (error "No Claudemacs session is active"))
    (with-current-buffer buffer
      (unless (and (boundp 'eat-terminal) eat-terminal)
        (error "Claudemacs session exists but terminal is not initialized. Please kill buffer and restart"))
      (let ((process (eat-term-parameter eat-terminal 'eat--process)))
        (unless (and process (process-live-p process))
          (error "Claudemacs session exists but process is not running. Please kill buffer and restart")))))
  t)

(defun claudemacs--validate-file-and-session ()
  "Validate that we have a file, project, and active Claudemacs session."
  ;; Buffer must be visiting a file because all calling functions use claudemacs--get-file-context
  ;; which depends on buffer-file-name for relative path calculation and Claude context
  (unless (buffer-file-name)
    (error "Buffer is not visiting a file - save the buffer first or switch to a file buffer"))
  (unless (claudemacs--project-root)
    (error "Not in a project"))
  (claudemacs--validate-process))

(defun claudemacs--get-session-cwd ()
  "Get the stored cwd from the current session."
  (if-let* ((buffer (claudemacs--get-current-session-buffer)))
      (with-current-buffer buffer
        claudemacs--cwd)))

(defun claudemacs--get-file-context ()
  "Get file context information for the current buffer.
Returns a plist with :file-path, :project-cwd, and :relative-path."
  (let* ((file-path (buffer-file-name))
         (cwd (claudemacs--get-session-cwd))
         (relative-path (file-relative-name file-path cwd)))
    (list :file-path file-path
          :project-cwd cwd
          :relative-path relative-path)))

(defun claudemacs--send-return-for-tool (terminal _buffer &optional _tool)
  "Send return key event to TERMINAL.
Uses eat-term-input-event which sends an actual key event rather than
a raw string, working reliably across different CLI tools."
  (eat-term-input-event terminal 1 'return))

(defun claudemacs--send-to-buffer (buffer message &optional no-return tool)
  "Send MESSAGE to BUFFER's eat terminal.
If NO-RETURN is non-nil, don't send a return/newline."
  (with-current-buffer buffer
    (let* ((resolved-tool (or tool
                              claudemacs--tool
                              (when (string-match "^\\*claudemacs:\\([^:]+\\):" (buffer-name buffer))
                                (intern (match-string 1 (buffer-name buffer))))
                              'claude))
           (plain-message (substring-no-properties message)))
      (if (and (eq resolved-tool 'codex)
               (fboundp 'eat-term-send-string-as-yank))
          (eat-term-send-string-as-yank eat-terminal (list plain-message))
        (eat-term-send-string eat-terminal plain-message))
      (unless no-return
        (claudemacs--send-return-for-tool eat-terminal buffer resolved-tool)))))

(defun claudemacs--build-prompt (base-prompt)
  "Build a dynamic prompt based on whether C-u was pressed and which tool(s) are active.
BASE-PROMPT is the action description (e.g., 'request', 'question')."
  (if current-prefix-arg
      ;; C-u was pressed - sending to all sessions
      (let* ((sessions (claudemacs--list-sessions-for-workspace))
             (tools (delete-dups (mapcar (lambda (s) (plist-get s :tool)) sessions)))
             (tool-names (mapconcat #'symbol-name tools ", ")))
        (format "Ask all sessions (%s) - %s: " tool-names base-prompt))
    ;; Single session - get the tool name
    (let* ((buffer (claudemacs--get-current-session-buffer))
           (tool (when buffer
                   (buffer-local-value 'claudemacs--tool buffer))))
      (if tool
          (format "Ask %s - %s: " (capitalize (symbol-name tool)) base-prompt)
        (format "Ask Claude - %s: " base-prompt)))))

(defun claudemacs--read-multiline-string (prompt)
  "Read a string from the minibuffer with multi-line support.
PROMPT is the prompt to display.
Shift+Return inserts a newline, Return submits the input."
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "S-RET") #'newline)
    (read-from-minibuffer prompt nil map)))

(defun claudemacs--send-message-to-claude (message &optional no-return no-switch)
  "Send MESSAGE to the active Claudemacs session.
If NO-RETURN is non-nil, don't send a return/newline.
If NO-SWITCH is non-nil, don't switch to the Claude buffer."
  (claudemacs--validate-process)
  (let ((claude-buffer (claudemacs--get-current-session-buffer)))
    (claudemacs--send-to-buffer claude-buffer message no-return)
    (unless no-switch
      (display-buffer claude-buffer)
      (select-window (get-buffer-window claude-buffer)))))

(defun claudemacs--format-context-line-range (relative-path start-line end-line)
  "Format context for a line range in RELATIVE-PATH from START-LINE to END-LINE."
  (if (= start-line end-line)
      (format "File context: %s:%d\n" relative-path start-line)
    (format "File context: %s:%d-%d\n" relative-path start-line end-line)))

(defun claudemacs--scroll-to-bottom ()
  "Scroll the claudemacs buffer to bottom without switching to it."
  (interactive)
  (when-let* ((claude-buffer (claudemacs--get-current-session-buffer))
              (claude-window (get-buffer-window claude-buffer)))
    (with-current-buffer claude-buffer
      (goto-char (point-max))
      (set-window-point claude-window (point-max)))))

(defun claudemacs--scroll-to-top ()
  "Scroll the claudemacs buffer to top without switching to it."
  (interactive)
  (when-let* ((claude-buffer (claudemacs--get-current-session-buffer))
              (claude-window (get-buffer-window claude-buffer)))
    (with-current-buffer claude-buffer
      (goto-char (point-min))
      (set-window-point claude-window (point-min)))))

;;;;
;;;; Action Processing System
;;;;

(defun claudemacs--send-action-to-sessions (action-function send-to-all)
  "Call ACTION-FUNCTION to get message, then send to session(s).
ACTION-FUNCTION should return a plist with keys:
  :message - the text to send to Claude
  :no-return - if non-nil, don't send newline
  :no-switch - if non-nil, don't switch to buffer
  :user-message - message to show user after sending

If SEND-TO-ALL is non-nil, send to all active sessions in current workspace.
Otherwise, send to current/active session only."
  (let* ((action-result (funcall action-function))
         (message-text (plist-get action-result :message))
         (no-return (plist-get action-result :no-return))
         (no-switch (plist-get action-result :no-switch))
         (user-message (plist-get action-result :user-message)))

    (if send-to-all
        ;; Send to all sessions in current workspace
        (let ((sessions (claudemacs--list-sessions-for-workspace)))
          (if (null sessions)
              (error "No active Claudemacs sessions found in current workspace")
            (dolist (session-info sessions)
              (let* ((session-buffer (plist-get session-info :buffer))
                     (session-tool (plist-get session-info :tool))
                     ;; Copy to avoid potential destructive mutations by terminal input handlers.
                     (session-message (copy-sequence message-text)))
                (claudemacs--send-to-buffer session-buffer session-message no-return session-tool)
                ;; Small delay to ensure eat terminal processes the input
                (sit-for 0.05)))
            (message "%s (sent to %d session%s in current workspace)"
                    user-message
                    (length sessions)
                    (if (= (length sessions) 1) "" "s"))))

      ;; Send to active session only
      (claudemacs--send-message-to-claude message-text no-return no-switch)
      (message "%s" user-message))))

(defun claudemacs--fix-error-at-point-action ()
  "Generate fix error action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         (line-number (line-number-at-pos))
         (errors (claudemacs--get-flycheck-errors-on-line))
         (error-message (claudemacs--format-flycheck-errors errors))
         (message-text (if (string-empty-p error-message)
                          (format "Please fix any issues at %s:%d"
                                  relative-path line-number)
                        (format "Please fix the error at %s:%d, error message: %s"
                                relative-path line-number error-message))))
    (list :message message-text
          :no-return nil
          :no-switch (not claudemacs-switch-to-buffer-on-send-error)
          :user-message (format "Sent error fix request to %s" (claudemacs--get-current-tool-name)))))

;;;###autoload
(defun claudemacs-fix-error-at-point (&optional send-to-all)
  "Send a request to Claude to fix the error at point using flycheck.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--fix-error-at-point-action send-to-all))

(defun claudemacs--execute-request-action ()
  "Generate execute request action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         (has-region (use-region-p))
         (start-line (if has-region
                         (line-number-at-pos (region-beginning))
                       (line-number-at-pos)))
         (end-line (if has-region
                       (line-number-at-pos (region-end))
                     (line-number-at-pos)))
         (context-text (claudemacs--format-context-line-range relative-path start-line end-line))
         (request (claudemacs--read-multiline-string (claudemacs--build-prompt "request (with context)")))
         (message-text (concat context-text request)))
    (when (string-empty-p (string-trim request))
      (error "Request cannot be empty"))
    (list :message message-text
          :no-return nil
          :no-switch nil
          :user-message (format "Sent request to %s with context" (claudemacs--get-current-tool-name)))))

;;;###autoload
(defun claudemacs-execute-request (&optional send-to-all)
  "Execute a Claude request with file context.
If a region is selected, use it as context with line range.
Otherwise, use current line as context.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--execute-request-action send-to-all))

(defun claudemacs--ask-without-context-action ()
  "Generate ask without context action message. Returns plist for action processor."
  (claudemacs--validate-process)
  (let ((request (claudemacs--read-multiline-string (claudemacs--build-prompt "question (without context)"))))
    (when (string-empty-p (string-trim request))
      (error "Request cannot be empty"))
    (list :message request
          :no-return nil
          :no-switch nil
          :user-message (format "Sent question to %s" (claudemacs--get-current-tool-name)))))

;;;###autoload
(defun claudemacs-ask-without-context (&optional send-to-all)
  "Ask Claude a question without file or line context.
Prompts for a question and sends it directly to Claude without any
file location or context information.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--ask-without-context-action send-to-all))

(defun claudemacs--add-file-reference-action ()
  "Generate add file reference action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (cwd (plist-get context :project-cwd))
         (selected-file (read-file-name "Add file reference: "))
         (relative-path (file-relative-name selected-file cwd))
         (reference-text (format "@%s " relative-path)))
    (list :message reference-text
          :no-return t
          :no-switch (not claudemacs-switch-to-buffer-on-file-add)
          :user-message (format "Added file reference: @%s" relative-path))))

;;;###autoload
(defun claudemacs-add-file-reference (&optional send-to-all)
  "Add a file reference to the Claude conversation.
Prompts for a file and sends @rel/path/to/file without newline.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--add-file-reference-action send-to-all))

(defun claudemacs--add-current-file-reference-action ()
  "Generate add current file reference action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         (reference-text (format "@%s " relative-path)))
    (list :message reference-text
          :no-return t
          :no-switch (not claudemacs-switch-to-buffer-on-file-add)
          :user-message (format "Added current file reference: @%s" relative-path))))

;;;###autoload
(defun claudemacs-add-current-file-reference (&optional send-to-all)
  "Add current file reference to the Claude conversation.
Sends @rel/path/to/current/file without newline.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--add-current-file-reference-action send-to-all))

(defun claudemacs--add-context-action ()
  "Generate add context action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         (has-region (use-region-p))
         (start-line (if has-region
                         (line-number-at-pos (region-beginning))
                       (line-number-at-pos)))
         (end-line (if has-region
                       (line-number-at-pos (region-end))
                     (line-number-at-pos)))
         (context-text (if (and has-region (not (= start-line end-line)))
                           (format "%s:%d-%d " relative-path start-line end-line)
                         (format "%s:%d " relative-path start-line))))
    (list :message context-text
          :no-return t
          :no-switch (not claudemacs-switch-to-buffer-on-add-context)
          :user-message (format "Added context: %s" (string-trim context-text)))))

;;;###autoload
(defun claudemacs-add-context (&optional send-to-all)
  "Add file context with line number(s) to the Claude conversation.
If a region is selected, uses line range (path:start-end).
Otherwise, uses current line (path:line).
Sends without newline so you can continue typing.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--add-context-action send-to-all))


(defun claudemacs--implement-comment-action ()
  "Generate implement comment action message. Returns plist for action processor."
  (claudemacs--validate-file-and-session)
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         comment-bounds
         comment-text
         start-line
         end-line)

    (cond
     ;; Case 1: Region is active - use exact region
     ((use-region-p)
      (let ((region-start (region-beginning))
            (region-end (region-end)))
        (setq start-line (line-number-at-pos region-start))
        (setq end-line (line-number-at-pos region-end))
        (setq comment-text (claudemacs--extract-comment-text region-start region-end))))

     ;; Case 2: No region - find comment at point
     (t
      (setq comment-bounds (claudemacs--get-comment-bounds))
      (unless comment-bounds
        (error "Point is not inside a comment"))
      (setq start-line (line-number-at-pos (car comment-bounds)))
      (setq end-line (line-number-at-pos (cdr comment-bounds)))
      (setq comment-text (claudemacs--extract-comment-text
                         (car comment-bounds)
                         (cdr comment-bounds)))))

    ;; Validate we have comment text
    (when (string-empty-p (string-trim comment-text))
      (error "No comment text found to implement"))

    ;; Format the message with file context and implementation request
    (let* ((context-text (claudemacs--format-context-line-range
                         relative-path start-line end-line))
           (message-text (format "%sPlease implement this comment:\n\n%s"
                                context-text comment-text)))
      (list :message message-text
            :no-return nil
            :no-switch nil
            :user-message (format "Sent comment implementation request to %s (%d lines)"
                                 (claudemacs--get-current-tool-name)
                                 (1+ (- end-line start-line)))))))

;;;###autoload
(defun claudemacs-implement-comment (&optional send-to-all)
  "Send comment at point or region to Claude for implementation.
If region is active, uses the exact region.
If no region, finds the comment block at point.
Extracts comment text and sends it to Claude with implementation instructions.
With prefix argument (C-u), send to all active sessions."
  (interactive "P")
  (claudemacs--send-action-to-sessions #'claudemacs--implement-comment-action send-to-all))

;;;###autoload
(defun claudemacs-toggle-buffer ()
  "Toggle Claude buffer visibility.
Hide if current, focus if visible elsewhere, show if hidden.
Works with the most relevant session (current buffer, or most recent)."
  (interactive)
  (claudemacs--validate-process)
  (let ((claude-buffer (claudemacs--get-current-session-buffer)))
    (cond
     ;; Case 1: Current buffer IS the Claude buffer - hide it
     ((eq (current-buffer) claude-buffer)
      (quit-window))

     ;; Case 2: Claude buffer visible in another window
     ((get-buffer-window claude-buffer)
      ;; Quit that window (automatically handles created vs reused)
      ;;
      ;; Edge case: the window was created for Claude, but in the meantime you
      ;; have switched to another workspace and back, the window is no longer
      ;; created just for claudemacs -- it has shown something previous, so it
      ;; will no longer go away if you toggle. Them's the breaks.
      (with-selected-window (get-buffer-window claude-buffer)
        (quit-window)))
     
     ;; Case 3: Claude buffer exists but not visible - show it
     (t
      (display-buffer claude-buffer)
      (when claudemacs-switch-to-buffer-on-toggle
        (select-window (get-buffer-window claude-buffer)))
      (with-current-buffer claude-buffer
        (set-window-point (get-buffer-window claude-buffer) (point-max)))))))

;;;; User Interface

(defun claudemacs--smart-switch-description ()
  "Generate dynamic description for switch menu item.
Shows the tool hint only when exactly one session exists."
  (condition-case err
      (let ((sessions (ignore-errors (claudemacs--list-sessions-for-workspace))))
        (cond
         ;; No sessions
         ((null sessions)
          "Switch to Session (none active)")
         ;; Exactly one session - show tool hint
         ((= (length sessions) 1)
          (let ((tool (ignore-errors (plist-get (car sessions) :tool))))
            (if tool
                (format "Switch to Session (→ %s)"
                        (propertize (symbol-name tool) 'face 'claudemacs-tool-name-face))
              "Switch to Session")))
         ;; Multiple sessions - no hint, will show menu
         (t "Switch to Session...")))
    (error
     (message "Error in smart-switch-description: %S" err)
     "Switch to Session")))

(defun claudemacs--kill-description ()
  "Generate dynamic description for kill menu item.
Shows which session will be killed."
  (condition-case err
      (let ((buffer (ignore-errors (claudemacs--get-current-session-buffer))))
        (if buffer
            (let ((tool (ignore-errors (buffer-local-value 'claudemacs--tool buffer))))
              (if tool
                  (format "Kill Session (→ %s)"
                          (propertize (symbol-name tool) 'face 'claudemacs-tool-name-face))
                "Kill Session"))
          "Kill Session"))
    (error
     (message "Error in kill-description: %S" err)
     "Kill Session")))

(defun claudemacs--smart-resume-description ()
  "Generate dynamic description for smart resume menu item.
Shows which tool will be resumed."
  (condition-case err
      (let ((sessions (ignore-errors (claudemacs--list-sessions-for-workspace))))
        (if (not sessions)
            ;; No sessions, will resume default
            (format "Smart Resume (→ %s)"
                    (propertize (symbol-name claudemacs-default-tool) 'face 'claudemacs-tool-name-face))
          ;; Sessions exist, will resume next available
          (let ((next-tool (ignore-errors (claudemacs--get-next-available-tool))))
            (if next-tool
                (format "Smart Resume (→ %s)"
                        (propertize (symbol-name next-tool) 'face 'claudemacs-tool-name-face))
              "Smart Resume (all running)"))))
    (error
     (message "Error in smart-resume-description: %S" err)
     "Smart Resume")))

(defun claudemacs--get-tool-start-description (tool &optional is-default)
  "Get the description for starting TOOL, showing next instance name.
If IS-DEFAULT is non-nil, append a default indicator."
  (let* ((next-instance (claudemacs--get-next-instance-number tool))
         (instance-name (claudemacs--format-tool-instance-name tool next-instance)))
    (if is-default
        (format "%s %s"
                (propertize instance-name 'face 'claudemacs-tool-name-face)
                (propertize "(default)" 'face 'font-lock-comment-face))
      (propertize instance-name 'face 'claudemacs-tool-name-face))))

(defun claudemacs--get-tool-resume-description (tool &optional is-default)
  "Get the description for resuming TOOL, showing next instance name with (resume).
If IS-DEFAULT is non-nil, also append a default indicator."
  (let* ((next-instance (claudemacs--get-next-instance-number tool))
         (instance-name (claudemacs--format-tool-instance-name tool next-instance)))
    (format "%s %s%s"
            (propertize instance-name 'face 'claudemacs-tool-name-face)
            (propertize "(resume)" 'face 'font-lock-comment-face)
            (if is-default
                (concat " " (propertize "(default)" 'face 'font-lock-comment-face))
              ""))))

(defun claudemacs--start-tool-by-index (index)
  "Start the tool at INDEX in the tool registry.
INDEX is 0-based."
  (let* ((tools (mapcar #'car claudemacs-tool-registry))
         (tool (nth index tools)))
    (when tool
      (let* ((args (transient-args 'claudemacs-start-menu))
             (prompt-for-dir (member "--prompt-project-root" args))
             (filtered-args (remove "--prompt-project-root" args)))
        (apply #'claudemacs--run-with-args tool prompt-for-dir filtered-args)))))

(defun claudemacs--resume-tool-by-index (index)
  "Resume the tool at INDEX in the tool registry.
INDEX is 0-based."
  (let* ((tools (mapcar #'car claudemacs-tool-registry))
         (tool (nth index tools)))
    (when tool
      (let* ((resume-flag (claudemacs--get-resume-flag tool))
             (args (transient-args 'claudemacs-resume-menu))
             (prompt-for-dir (member "--prompt-project-root" args))
             (filtered-args (remove "--prompt-project-root" args))
             (claudemacs-switch-to-buffer-on-create t))  ; Always switch when resuming
        (apply #'claudemacs--run-with-args tool prompt-for-dir resume-flag filtered-args)))))

(defun claudemacs--setup-start-tool-suffixes (_)
  "Generate tool suffixes dynamically for the start menu.
Returns a list of parsed transient suffix objects."
  (cl-loop for index from 0
           for (tool . _) in claudemacs-tool-registry
           for key = (number-to-string (1+ index))
           for is-default = (= index 0)
           collect (let ((i index)
                         (desc (claudemacs--get-tool-start-description tool is-default)))
                     (transient-parse-suffix
                      'claudemacs-start-menu
                      (list key desc
                            (lambda () (interactive) (claudemacs--start-tool-by-index i)))))))

(defun claudemacs--setup-resume-tool-suffixes (_)
  "Generate tool suffixes dynamically for the resume menu.
Returns a list of parsed transient suffix objects."
  (cl-loop for index from 0
           for (tool . _) in claudemacs-tool-registry
           for key = (number-to-string (1+ index))
           for is-default = (= index 0)
           collect (let ((i index)
                         (desc (claudemacs--get-tool-resume-description tool is-default)))
                     (transient-parse-suffix
                      'claudemacs-resume-menu
                      (list key desc
                            (lambda () (interactive) (claudemacs--resume-tool-by-index i)))))))

;;;###autoload (autoload 'claudemacs-start-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-start-menu ()
  "Start a new AI coding session."
  ["Start New Session\n"
   ("-d" "Skip permissions on start" "--dangerous-skip-permissions")
   ("-p" "Prompt for project root" "--prompt-project-root")
   ("-f" "Add custom flag to start command" "" :class transient-option :prompt "Custom flag: ")]
  ["Tools"
   :class transient-column
   :setup-children claudemacs--setup-start-tool-suffixes]
  ["" ("<return>" "Start default tool" (lambda () (interactive) (claudemacs--start-tool-by-index 0)))])

;;;###autoload (autoload 'claudemacs-resume-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-resume-menu ()
  "Resume a previous AI coding session."
  ["Resume Session\n"
   ("-d" "Skip permissions on start" "--dangerous-skip-permissions")
   ("-p" "Prompt for project root" "--prompt-project-root")
   ("-f" "Add custom flag to start command" "" :class transient-option :prompt "Custom flag: ")]
  ["Tools"
   :class transient-column
   :setup-children claudemacs--setup-resume-tool-suffixes]
  ["" ("<return>" "Resume default tool" (lambda () (interactive) (claudemacs--resume-tool-by-index 0)))])

;;;###autoload (autoload 'claudemacs-transient-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-transient-menu ()
  "Claude Code AI Pair Programming Interface."
  ["Claudemacs: AI pair programming with Claude Code\n"
   ["Core"
    ("s" "Switch to Session" claudemacs-switch-to-session
     :description claudemacs--smart-switch-description)
    ("S" "Start Session..." claudemacs-start-menu)
    ("o" "Switch Other Session" claudemacs-switch-other)
    ("r" "Resume Session..." claudemacs-resume-menu)
    ("k" "Kill Session..." claudemacs-kill-specific-session)
    ("t" "Toggle Buffer" claudemacs-toggle-buffer)]
   ["Actions (Use C-u to send to all sessions)"
    ("e" "Fix Error at Point" claudemacs-fix-error-at-point)
    ("x" "Execute Request (with context)" claudemacs-execute-request)
    ("X" "Execute Request (no context)" claudemacs-ask-without-context)
    ("i" "Implement Comment" claudemacs-implement-comment)
    ("f" "Add File Reference" claudemacs-add-file-reference)
    ("F" "Add Current File" claudemacs-add-current-file-reference)
    ("a" "Add Context" claudemacs-add-context)]
   ["Quick Responses"
     ("y" "Send Yes (RET)" claudemacs-send-yes)
     ("n" "Send No (ESC)" claudemacs-send-no)]]
  ["Maintenance"
    ("u" "Unstick Claude buffer" claudemacs-unstick-terminal)])

;;;###autoload
(defvar claudemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'claudemacs-transient-menu)
    map)
  "Keymap for `claudemacs-mode'.")

;;;###autoload
(define-minor-mode claudemacs-mode
  "Minor mode for Claude Code AI pair programming.

\\{claudemacs-mode-map}"
  :lighter " Claude"
  :keymap claudemacs-mode-map
  :group 'claudemacs)

(defun claudemacs--show-cursor (&rest _args)
  "Show cursor in Claudemacs buffers when in Emacs mode."
  (when (claudemacs--is-claudemacs-buffer-p)
    (setq-local cursor-type 'box)))

(defun claudemacs--hide-cursor (&rest _args)
  "Hide cursor in Claudemacs buffers when in semi-char mode."
  (when (claudemacs--is-claudemacs-buffer-p)
    ;; Only force terminal cursor visibility for Claude, not other tools like Codex
    (when (and (boundp 'eat-terminal) eat-terminal
               (eq claudemacs--tool 'claude))
      (setq-local cursor-type nil))))

(defun claudemacs--check-and-disable-window-adjust (&rest _)
  "Check if buffer is longer than one screen and disable window adjustment if so."
  (when (and (not (eq window-adjust-process-window-size-function 'ignore))
             (claudemacs--is-claudemacs-buffer-p))
    (let* ((claude-buffer (current-buffer))
           (claude-window (get-buffer-window claude-buffer))
           (window-ht (when claude-window (window-height claude-window)))
           (buffer-lines (count-lines (point-min) (point-max))))
      ;; If buffer has more lines than window height, switch to 'ignore mode
      (when (and window-ht (> buffer-lines window-ht))
        (goto-char (point-min))
        (redisplay)
        (goto-char (point-max))
        (redisplay)
        ;; CRITICAL: Disable window-adjust-process-window-size-function to prevent
        ;; terminal redraw/scroll reset on buffer switching (same issue as vterm #149)
        (setq-local window-adjust-process-window-size-function 'ignore)))))

(defun claudemacs--eat-force-redraw ()
  "Forces the eat terminal and the underlying program to redraw.

This is useful if the display becomes corrupted after Emacs window
resizes or other external changes that might not have been fully
propagated. It attempts to resynchronize the PTY size, the
eat emulator's internal dimensions, and trigger a redisplay."
  (interactive)
  (with-current-buffer (claudemacs--get-current-session-buffer)
    (when (and (boundp 'eat-terminal) eat-terminal)
        (let* ((process (eat-term-parameter eat-terminal 'eat--process))
               (claude-window (get-buffer-window (claudemacs--get-current-session-buffer))))
          (if (and process (process-live-p process) claude-window)
              (eat--adjust-process-window-size process (list claude-window)))))))

(defun claudemacs-unstick-terminal ()
  "Reset the claudemacs buffer's vertical rest point.
Sometimes the input box gets stuck mid or top of the buffer because of
the idiosyncracies of eat-mode. This will reset the input box to the
bottom of the buffer."
  (interactive)
  (claudemacs--validate-process)
  (when (claudemacs--is-claudemacs-buffer-p)
    (error "Reset buffer cannot be used while visiting the claudemacs buffer itself"))
  (claudemacs--eat-force-redraw)
  (with-current-buffer (claudemacs--get-current-session-buffer)
    (setq-local window-adjust-process-window-size-function
                'window-adjust-process-window-size-smallest))
  (claudemacs--scroll-to-top)
  (redisplay)
  (claudemacs--scroll-to-bottom)
  (redisplay)
  (with-current-buffer (claudemacs--get-current-session-buffer)
    ;; CRITICAL: Disable window-adjust-process-window-size-function to prevent
    ;; terminal redraw/scroll reset on buffer switching (same issue as vterm #149)
    (setq-local window-adjust-process-window-size-function 'ignore)))

;;;###autoload
(defun claudemacs-setup ()
  "Set up claudemacs hooks and advice.
This is called automatically when the package is loaded.
Safe to call multiple times - will not add duplicate hooks or advice."
  (interactive)
  ;; Hook to manage window adjustment for terminal buffers
  (unless (memq #'claudemacs--check-and-disable-window-adjust window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'claudemacs--check-and-disable-window-adjust))
  ;; Advice for cursor visibility in eat modes
  (unless (advice-member-p #'claudemacs--show-cursor 'eat-emacs-mode)
    (advice-add 'eat-emacs-mode :after #'claudemacs--show-cursor))
  (unless (advice-member-p #'claudemacs--hide-cursor 'eat-semi-char-mode)
    (advice-add 'eat-semi-char-mode :after #'claudemacs--hide-cursor)))

(defun claudemacs-unload-function ()
  "Cleanup when unloading claudemacs.
Removes advice and hooks added by `claudemacs-setup'."
  (advice-remove 'eat-emacs-mode #'claudemacs--show-cursor)
  (advice-remove 'eat-semi-char-mode #'claudemacs--hide-cursor)
  (remove-hook 'window-buffer-change-functions #'claudemacs--check-and-disable-window-adjust)
  nil)

;; Auto-setup when package is loaded
(claudemacs-setup)

(provide 'claudemacs)
;;; claudemacs.el ends here
