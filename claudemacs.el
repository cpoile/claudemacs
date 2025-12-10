;;; claudemacs.el --- AI pair programming with Claude Code -*- lexical-binding: t; -*-
;; Author: Christopher Poile <cpoile@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
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

;;; Code:

;;;; Dependencies
(require 'cl-lib)
(require 'json)
(require 'transient)
(require 'project)
(require 'vc-git)
(require 'eat nil 'noerror)
(require 'claudemacs-comment)
(require 'claudemacs-ai)
(require 'claudemacs-sessions)

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

(defcustom claudemacs-auto-allow-cli-reads t
  "Whether to automatically allow read-only claudemacs-cli commands.
When non-nil, Claude Code will auto-approve permissions for read-only
claudemacs-cli commands (get-buffer-content, get-region, list-buffers,
buffer-info). This allows seamless integration without permission prompts
for safe read operations.

When nil, all claudemacs-cli commands will require explicit permission."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-use-mcp t
  "Whether to use MCP (Model Context Protocol) for Emacs integration.
When non-nil, claudemacs will load the MCP server for buffer operations,
exposing tools like get_buffer_content, list_buffers, etc. as native MCP tools.

When nil, falls back to bash-based claudemacs-cli tools."
  :type 'boolean
  :group 'claudemacs)

(defcustom claudemacs-additional-tools-files nil
  "List of additional tools.yaml files to load for MCP tools.
Each file should contain tool definitions in the same format as the main tools.yaml.
Tools from additional files are merged with the built-in tools.

This can be set via .dir-locals.el to provide project-specific MCP tools.

Example:
  ((nil . ((claudemacs-additional-tools-files . (\"~/my-project/.claudemacs-tools.yaml\")))))"
  :type '(repeat file)
  :safe #'listp
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

;;;; Buffer-local Variables
(defvar-local claudemacs--cwd nil
  "Buffer-local variable storing the current working directory for this Claude session.")

(defvar-local claudemacs--session-id nil
  "Buffer-local variable storing the session ID (UUID) for this Claude session.
Used to resume the correct session when restarting agents with custom names.")

;;;;
;;;; Utility Functions
;;;;

(defun claudemacs--project-root (&optional dir)
  "Get the project root, optionally preferring projectile if enabled.
If DIR is given, use it as the starting location.
When `claudemacs-prefer-projectile-root' is enabled and projectile is 
available, tries `projectile-project-root' first. Falls back to 
`vc-git-root', then to the directory itself."
  (let ((loc (or dir 
                 (when (buffer-file-name)
                   (file-name-directory (buffer-file-name)))
                 default-directory)))
    (or 
     ;; Try projectile first if enabled and available
     (when (and claudemacs-prefer-projectile-root
                (fboundp 'projectile-project-root))
       (condition-case nil
         (let ((proj-root (projectile-project-root)))
           (when (and proj-root (file-directory-p proj-root))
             proj-root))
         (error nil)))
     ;; Fallback to vc-git-root
     (vc-git-root loc)
     ;; Final fallback to location itself
     loc)))

(defun claudemacs--session-id ()
  "Return an identifier for the current Claudemacs session.
If a workspace is active (checking various workspace packages),
use its name, otherwise fall back to the project root."
  (cond
   ;; Doom Emacs workspace
   ((and (fboundp '+workspace-current-name)
         (let ((ws (+workspace-current-name)))
           (and ws (stringp ws) (not (string-empty-p ws)))))
    (+workspace-current-name))
   ;; Perspective mode
   ((and (and (fboundp 'safe-persp-name) (fboundp 'get-current-persp))
         (let ((ws (safe-persp-name (get-current-persp))))
           (and ws (stringp ws) (not (string-empty-p ws)))))
    (safe-persp-name (get-current-persp)))
   ;; Fall back to project root
   (t (file-truename (claudemacs--project-root)))))

(defun claudemacs--get-buffer-name ()
  "Generate the claudemacs buffer name based on workspace session ID."
  (format "*claudemacs:%s*" (claudemacs--session-id)))

(defun claudemacs--get-buffer ()
  "Return existing claudemacs buffer for current session."
  (get-buffer (claudemacs--get-buffer-name)))

(defun claudemacs--is-claudemacs-buffer-p (&optional buffer)
  "Return t if BUFFER (or current buffer) is a claudemacs buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (string-match-p "^\\*claudemacs:" (buffer-name buf)))))

(defun claudemacs--parse-buffer-name (buffer-name)
  "Parse claudemacs buffer name into components.
Buffer name format:
  *claudemacs:/path/to/dir* or
  *claudemacs:/path/to/dir:agent-name*
Returns cons cell (directory . agent-name) or (directory . nil)."
  (when (string-match "^\\*claudemacs:\\([^:*]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name)
    (let ((dir (match-string 1 buffer-name))
          (agent (match-string 2 buffer-name)))
      (cons dir agent))))

(defun claudemacs--switch-to-buffer ()
  "Switch to the claudemacs buffer for current session.
Returns t if switched successfully, nil if no buffer exists."
  (if-let* ((buffer (claudemacs--get-buffer)))
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
(declare-function eat-term-input-event "eat")
(declare-function eat-kill-process "eat")
(declare-function eat-term-parameter "eat")
(declare-function setf "cl-lib")

;;;; Bell Handling
(defun claudemacs--bell-handler (terminal)
  "Handle bell events from Claude Code in TERMINAL.
This function is called when Claude Code sends a bell character."
  (ignore terminal)
  (when claudemacs-notify-on-await
    (claudemacs--system-notification "Claude Code finished and is awaiting your input")))


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
            (claudemacs-setup-bell-handler buffer)
            ;; Run startup hook after setup is complete
            (run-hooks 'claudemacs-startup-hook)))
      ;; Eat not ready yet, retry if we haven't exceeded max attempts
      (when (< retry-count 10)
        (message "Eat not ready yet, retrying in 0.5s (attempt %d/10)" (1+ retry-count))
        (run-with-timer 0.5 nil
                        (lambda ()
                          (claudemacs--setup-eat-integration buffer (1+ retry-count))))))))

;;;###autoload
(defun claudemacs-setup-bell-handler (&optional buffer)
  "Set up or re-setup the completion notification handler for BUFFER.
If BUFFER is not specified, uses the current buffer if it's a claudemacs buffer,
otherwise finds the buffer using `claudemacs--get-buffer'.
Use this if system notifications aren't working after starting a session."
  (interactive)
  (let ((target-buffer (or buffer
                           (when (claudemacs--is-claudemacs-buffer-p)
                             (current-buffer))
                           (claudemacs--get-buffer))))
    (when target-buffer
      (with-current-buffer target-buffer
        (when (boundp 'eat-terminal)
          (setf (eat-term-parameter eat-terminal 'ring-bell-function)
                #'claudemacs--bell-handler)
          (message "Bell handler configured for claudemacs session"))))))

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
  (let ((buffer (claudemacs--get-buffer)))
    (with-current-buffer buffer
      (eat-term-send-string eat-terminal (kbd "RET")))))

;;;###autoload
(defun claudemacs-send-no ()
  "Send no (ESC) to the active Claudemacs session."
  (interactive)
  (claudemacs--validate-process)
  (let ((buffer (claudemacs--get-buffer)))
    (with-current-buffer buffer
      (eat-term-send-string eat-terminal (kbd "ESC")))))

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

(defun claudemacs--get-mcp-safe-tools ()
  "Get list of safe MCP tools from the YAML configuration.
Returns a list of tool names marked as safe."
  (let* ((this-file (or load-file-name buffer-file-name
                        (locate-library "claudemacs")))
         (this-dir (when this-file (file-name-directory this-file)))
         (mcp-dir (when this-dir
                    (expand-file-name "claudemacs_mcp" this-dir))))
    (when (and mcp-dir (file-directory-p mcp-dir))
      (let ((output (shell-command-to-string
                     (format "uv run --directory %s python -m claudemacs_mcp.server --safe-tools 2>/dev/null"
                             (shell-quote-argument mcp-dir)))))
        (when (and output (not (string-empty-p output)))
          (split-string (string-trim output) "\n" t))))))

(defun claudemacs--get-auto-allow-permissions ()
  "Generate --allowedTools flag for safe tools.
Includes both CLI commands (if enabled) and safe MCP tools."
  (let ((tools '()))
    ;; Add CLI tools if enabled
    (when claudemacs-auto-allow-cli-reads
      (setq tools (append tools
                          (mapcar (lambda (cmd) (format "Bash(claudemacs-cli %s:*)" cmd))
                                  '("get-buffer-content" "get-region" "list-buffers" "buffer-info")))))
    ;; Add safe MCP tools
    (when claudemacs-use-mcp
      (let ((mcp-safe-tools (claudemacs--get-mcp-safe-tools)))
        (when mcp-safe-tools
          (setq tools (append tools
                              (mapcar (lambda (tool) (format "mcp__claudemacs__%s" tool))
                                      mcp-safe-tools))))))
    (when tools
      (list "--allowedTools" (string-join tools " ")))))

(defvar claudemacs--mcp-config-file nil
  "Path to the dynamically generated MCP config file.")

(defun claudemacs--generate-mcp-config (work-dir buffer-name)
  "Generate a temporary MCP config file with dynamic paths.
WORK-DIR is the session's working directory, used to isolate memory buffers.
BUFFER-NAME is the claudemacs buffer name for this session.
Returns the path to the generated config file."
  (let* ((this-file (or load-file-name buffer-file-name
                        (locate-library "claudemacs")))
         (this-dir (when this-file (file-name-directory this-file)))
         (mcp-dir (when this-dir
                    (expand-file-name "claudemacs_mcp" this-dir)))
         (expanded-work-dir (expand-file-name work-dir))
         (config-file (make-temp-file "claudemacs-mcp-config-" nil ".json"))
         ;; Build environment with optional additional tools files
         (env-vars `((CLAUDEMACS_CWD . ,expanded-work-dir)
                    (CLAUDEMACS_BUFFER_NAME . ,buffer-name)))
         (env-vars (if claudemacs-additional-tools-files
                      (append env-vars
                              `((CLAUDEMACS_ADDITIONAL_TOOLS_FILES . ,(string-join claudemacs-additional-tools-files ":"))))
                    env-vars))
         (config-json (json-encode
                       `((mcpServers
                          . ((claudemacs
                              . ((command . "uv")
                                 (args . ["run" "--python-preference" "managed" "--directory" ,mcp-dir
                                          "-m" "claudemacs_mcp.server"])
                                 (env . ,env-vars)))))))))
    (with-temp-file config-file
      (insert config-json))
    (setq claudemacs--mcp-config-file config-file)
    config-file))

(defun claudemacs--get-custom-prompt ()
  "Generate --append-system-prompt flag if custom prompt file exists.
Looks for claudemacs-prompt.md in the claudemacs package directory.
Returns nil if file doesn't exist."
  (let* ((this-file (or load-file-name buffer-file-name
                        (locate-library "claudemacs")))
         (this-dir (when this-file (file-name-directory this-file)))
         (prompt-file (when this-dir
                        (expand-file-name "claudemacs-prompt.md" this-dir))))
    (when (and prompt-file (file-exists-p prompt-file))
      (let ((content (with-temp-buffer
                       (insert-file-contents prompt-file)
                       (buffer-string))))
        (when (not (string-empty-p (string-trim content)))
          (list "--append-system-prompt" content))))))

(defun claudemacs--get-mcp-config (work-dir buffer-name)
  "Generate --mcp-config flag if MCP is enabled.
WORK-DIR is the session's working directory for memory buffer isolation.
BUFFER-NAME is the claudemacs buffer name for this session.
Returns nil if `claudemacs-use-mcp' is nil."
  (when claudemacs-use-mcp
    (let* ((this-file (or load-file-name buffer-file-name
                          (locate-library "claudemacs")))
           (this-dir (when this-file (file-name-directory this-file)))
           (mcp-dir (when this-dir
                      (expand-file-name "claudemacs_mcp" this-dir))))
      (when (and mcp-dir (file-directory-p mcp-dir))
        (list "--mcp-config" (claudemacs--generate-mcp-config work-dir buffer-name))))))

(defun claudemacs--start (work-dir &rest args)
  "Start Claude Code in WORK-DIR with ARGS.
WORK-DIR can be either:
  - A string: \"/path\" creates buffer *claudemacs:/path*
  - A list: '(\"/path\" \"agent-name\") creates *claudemacs:/path:agent-name*"
  (require 'eat)
  ;; Set up environment variables BEFORE spawning the Claude process
  (claudemacs-ai-setup-claude-environment)

  ;; Parse work-dir - it can be a string or (dir agent-name) list
  (let* ((dir-string (if (listp work-dir) (car work-dir) work-dir))
         (agent-name (when (listp work-dir) (cadr work-dir)))
         (expanded-dir (expand-file-name dir-string))
         (buffer-name (if agent-name
                         (format "*claudemacs:%s:%s*" expanded-dir agent-name)
                       (format "*claudemacs:%s*" expanded-dir)))
         (default-directory dir-string)
         (buffer (get-buffer-create buffer-name))
         (cli-dir (file-name-directory (claudemacs-ai-get-cli-path)))
         (claudemacs-socket (when (and (boundp 'server-socket-dir)
                                        server-socket-dir
                                        (server-running-p))
                              (expand-file-name "server" server-socket-dir)))
         (process-environment
          (append (list (format "PATH=%s:%s" cli-dir (getenv "PATH"))
                        "TERM=xterm-256color"
                        "CLAUDEMACS_SESSION=1")
                  (when claudemacs-socket
                    (list (format "CLAUDEMACS_SOCKET=%s" claudemacs-socket)))
                  process-environment)))
    (with-current-buffer buffer
      (cd dir-string)
      (setq-local eat-term-name "xterm-256color")
      (let ((process-adaptive-read-buffering nil)
            (switches (remove nil (append args
                                         claudemacs-program-switches
                                         (claudemacs--get-auto-allow-permissions)
                                         (claudemacs--get-custom-prompt)
                                         (claudemacs--get-mcp-config dir-string buffer-name)))))
        (if claudemacs-use-shell-env
            ;; New behavior: Run through shell to source profile (e.g., .zprofile, .bash_profile)
            ;; Explicitly set environment variables in the shell command to survive shell config sourcing
            (let* ((shell (claudemacs--get-shell-name))
                   (env-vars (format "PATH=%s:$PATH CLAUDEMACS_SESSION=1%s"
                                   cli-dir
                                   (if claudemacs-socket
                                       (format " CLAUDEMACS_SOCKET=%s" claudemacs-socket)
                                     "")))
                   (claude-cmd (format "%s %s %s"
                                      env-vars
                                      claudemacs-program
                                      (mapconcat 'shell-quote-argument switches " "))))
              (eat-make (substring buffer-name 1 -1) shell nil "-c" claude-cmd))
          ;; Original behavior: Run Claude directly without shell environment
          (apply #'eat-make (substring buffer-name 1 -1) claudemacs-program nil switches)))
      
      ;; Set buffer-local variables after eat-make to ensure they persist
      (setq-local claudemacs--cwd dir-string)

      ;; Store session ID for this buffer
      ;; For multi-agent setups, try to read from cache file first, otherwise use most recent
      (let* ((session-cache-dir (expand-file-name ".claude/" dir-string))
             (session-cache-file (expand-file-name
                                  (format "session-%s" (md5 buffer-name))
                                  session-cache-dir)))
        (run-at-time 2 nil
                     (lambda (buf dir cache-file)
                       (when (buffer-live-p buf)
                         (let ((session-id (claudemacs--get-most-recent-session-id dir)))
                           (when session-id
                             (with-current-buffer buf
                               (setq-local claudemacs--session-id session-id))
                             ;; Cache it to a file for future restarts
                             (make-directory (file-name-directory cache-file) t)
                             (with-temp-file cache-file
                               (insert session-id))))))
                     (current-buffer) dir-string session-cache-file))

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
      
      ;; Enable claudemacs-mode for keybindings
      (claudemacs-mode 1)

      ;; Set up custom key mappings & completion notifications after eat initialization
      (run-with-timer 0.1 nil
                      (lambda ()
                        (claudemacs--setup-eat-integration buffer))))
    
    (let ((window (display-buffer buffer)))
      (when claudemacs-switch-to-buffer-on-create
        (select-window window)))))

(defun claudemacs--run-with-args (&optional arg &rest args)
  "Start Claude Code with ARGS or switch to existing session.
With prefix ARG, prompt for the project directory."
  (let* ((explicit-dir (when arg (read-directory-name "Project directory: ")))
         (work-dir (or explicit-dir (claudemacs--project-root))))
    (unless (claudemacs--switch-to-buffer)
      (apply #'claudemacs--start work-dir args))))

;;;; Interactive Commands
;;;###autoload
(defun claudemacs-run (&optional arg)
  "Start Claude Code or switch to existing session.
With prefix ARG, prompt for the project directory."
  (interactive "P")
  (claudemacs--run-with-args arg))

;;;###autoload
(defun claudemacs-resume (&optional arg)
  "Start Claude Code with resume or switch to existing session.
With prefix ARG, prompt for the project directory."
  (interactive "P")
  (let ((claudemacs-switch-to-buffer-on-create t))
    (claudemacs--run-with-args arg "--resume")))

;;;###autoload
(defun claudemacs-kill ()
  "Kill Claudemacs process and close its window."
  (interactive)
  (if-let* ((claudemacs-buffer (claudemacs--get-buffer)))
      (progn
        (with-current-buffer claudemacs-buffer
          (eat-kill-process)
          (kill-buffer claudemacs-buffer))
        (message "Claudemacs session killed"))
    (error "There is no Claudemacs session in this workspace or project")))

;;;###autoload
(defun claudemacs-clear-buffer ()
  "Clear/trim the current claudemacs buffer to improve performance.
Removes accumulated history, keeping only the last 10KB of content."
  (interactive)
  (if (claudemacs--is-claudemacs-buffer-p)
      (let ((result (claudemacs-ai-clear-buffer (buffer-name))))
        (message "%s" result))
    (error "Not in a claudemacs buffer")))

;;;###autoload
(defun claudemacs-spawn-agent (directory &optional agent-name &rest extra-args)
  "Spawn a new claudemacs agent in DIRECTORY with optional AGENT-NAME.
When called interactively, uses current directory and prompts for agent identifier.
If AGENT-NAME is nil or empty, buffer will be named *claudemacs:/path*.
If provided, buffer will be named *claudemacs:/path:agent-name*.
EXTRA-ARGS are additional command-line arguments to pass to Claude.
Returns the buffer name."
  (interactive
   (list (if (claudemacs--is-claudemacs-buffer-p)
            (or claudemacs--cwd default-directory)
          (claudemacs--project-root))
         (let ((input (read-string "Agent identifier (leave empty for primary): " nil nil "")))
           (if (string-empty-p input) nil input))))
  (let* ((expanded-dir (expand-file-name directory))
         (buffer-name (if agent-name
                         (format "*claudemacs:%s:%s*" expanded-dir agent-name)
                       (format "*claudemacs:%s*" expanded-dir)))
         (work-dir-arg (if agent-name
                          (list directory agent-name)
                        directory)))
    ;; Check if buffer already exists
    (when (get-buffer buffer-name)
      (error "Agent already exists with buffer name: %s" buffer-name))

    ;; Check directory exists
    (unless (file-directory-p expanded-dir)
      (error "Directory does not exist: %s" expanded-dir))

    ;; Spawn the agent with any extra args
    (apply #'claudemacs--start work-dir-arg extra-args)

    (when (called-interactively-p 'interactive)
      (message "Spawned agent: %s" buffer-name))

    buffer-name))

(defun claudemacs--get-most-recent-session-id (work-dir)
  "Get the most recent session ID for WORK-DIR.
Returns the UUID of the most recently modified session file, or nil if none found."
  (let* ((expanded-dir (expand-file-name work-dir))
         ;; Convert /home/user/.path/to/dir to -home-user--path-to-dir
         ;; Claude's format: replace / with -, replace . with -
         (slug-with-slashes (replace-regexp-in-string "/" "-" expanded-dir))
         (project-slug (replace-regexp-in-string "\\." "-" slug-with-slashes))
         (sessions-dir (expand-file-name project-slug "~/.claude/projects/")))
    (when (file-directory-p sessions-dir)
      (let* ((files (directory-files sessions-dir t "\\.jsonl$"))
             (sorted-files (sort files
                                 (lambda (a b)
                                   (time-less-p (nth 5 (file-attributes b))
                                               (nth 5 (file-attributes a)))))))
        (when sorted-files
          ;; Extract UUID from filename (remove path and .jsonl extension)
          (file-name-sans-extension (file-name-nondirectory (car sorted-files))))))))

(defun claudemacs--send-message-when-ready (work-dir message delay &optional attempt target-buffer-name)
  "Send MESSAGE to Claude after DELAY seconds.
WORK-DIR identifies the session (can be nil if TARGET-BUFFER-NAME is provided).
DELAY is the number of seconds to wait before sending.
TARGET-BUFFER-NAME is the exact buffer name to use (optional)."
  (let ((buffer-name (or target-buffer-name
                         (when work-dir (format "*claudemacs:%s*" work-dir)))))
    (unless buffer-name
      (error "claudemacs--send-message-when-ready: Cannot determine buffer name. work-dir=%S target-buffer-name=%S"
             work-dir target-buffer-name))
    (run-with-timer
     delay nil
     (lambda (buf-name msg)
       (let ((buffer (get-buffer buf-name)))
         (if (and buffer
                  (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (and (boundp 'eat-terminal) eat-terminal)))
             ;; Send the message
             (with-current-buffer buffer
               (eat-term-send-string eat-terminal msg)
               (sit-for 0.1)
               (eat-term-send-string eat-terminal "\r")
               (message "Continuation message sent to Claude"))
           (message "Warning: Buffer %s not ready to receive message" buf-name))))
     buffer-name message)))

;;;###autoload
(defun claudemacs-restart (&optional target-work-dir target-buffer-name)
  "Restart Claudemacs session, reloading elisp files and MCP server.
This kills the current session, reloads claudemacs elisp files,
and starts a new session with --resume to continue the conversation.
If TARGET-WORK-DIR is provided, restart the session for that directory.
If TARGET-BUFFER-NAME is provided, restart that specific buffer (for custom-named agents).
Otherwise, restart the session for the current project."
  (interactive)
  (let* ((work-dir (when-let ((dir (or target-work-dir
                                       (when-let ((buf (or (when target-buffer-name
                                                             (get-buffer target-buffer-name))
                                                           (claudemacs--get-buffer))))
                                         (with-current-buffer buf claudemacs--cwd)))))
                     (expand-file-name dir)))
         ;; FIX: Find any claudemacs buffer for work-dir, not just simple pattern
         (claudemacs-buffer (or (when target-buffer-name
                                  (get-buffer target-buffer-name))
                                (when work-dir
                                  (cl-find-if
                                   (lambda (buf)
                                     (and (string-match-p "^\\*claudemacs:" (buffer-name buf))
                                          (with-current-buffer buf
                                            (equal (expand-file-name claudemacs--cwd) work-dir))))
                                   (buffer-list)))))
         ;; FIX: Parse buffer name to extract agent name
         (buffer-components (when claudemacs-buffer
                              (claudemacs--parse-buffer-name (buffer-name claudemacs-buffer))))
         (agent-name (when buffer-components (cdr buffer-components)))
         ;; FIX: Build work-dir-arg as list for agent-name sessions
         (work-dir-arg (if agent-name
                          (list work-dir agent-name)
                        work-dir))
         ;; For now, don't try to restore specific sessions for multi-agent buffers
         ;; Just use --continue which will resume the most recent session
         (session-id nil)
         (this-file (or load-file-name
                        (locate-library "claudemacs")))
         (this-dir (when this-file (file-name-directory this-file)))
         ;; Check if buffer was visible before we kill it
         (buffer-was-visible (and claudemacs-buffer
                                  (get-buffer-window claudemacs-buffer t))))
    ;; Validate we have a session to restart
    (unless work-dir
      (error "No Claudemacs session to restart (no work-dir)"))
    (unless claudemacs-buffer
      (error "No Claudemacs session found for directory: %s (buffer: %s)"
             work-dir target-buffer-name))

    ;; Kill the target session
    (message "Killing claudemacs session for %s..." work-dir)
    (with-current-buffer claudemacs-buffer
      (eat-kill-process)
      (kill-buffer claudemacs-buffer))

    ;; Reload elisp files
    (message "Reloading claudemacs elisp files...")
    (when this-dir
      (load-file (expand-file-name "claudemacs-ai.el" this-dir))
      (load-file (expand-file-name "claudemacs.el" this-dir)))

    ;; Start new session with either --resume <id> or --continue
    ;; Always spawn without stealing focus
    (let ((session-args (if session-id
                           (list "--resume" session-id)
                         (list "--continue"))))
      (message "Starting new claudemacs session with %s %s..."
               (car session-args)
               (or (cadr session-args) ""))
      (let ((claudemacs-switch-to-buffer-on-create nil)
            ;; FIX: Compute expected buffer name from work-dir-arg
            (new-buffer-name (if agent-name
                                (format "*claudemacs:%s:%s*" work-dir agent-name)
                              (format "*claudemacs:%s*" work-dir))))
        ;; FIX: Pass work-dir-arg instead of just work-dir
        (apply #'claudemacs--start work-dir-arg session-args)

        ;; If buffer wasn't visible before, hide it now
        (unless buffer-was-visible
          (when-let* ((new-buffer (get-buffer new-buffer-name))
                      (window (get-buffer-window new-buffer t)))
            (delete-window window)))

        ;; Send continuation message after a delay
        (claudemacs--send-message-when-ready
         work-dir
         "Session restarted - elisp and MCP server reloaded. Please continue."
         5  ;; delay in seconds
         nil  ;; attempt parameter (unused but kept for backwards compatibility)
         new-buffer-name)

        (message "Claudemacs restarted successfully for %s" work-dir)))))

(defun claudemacs--validate-process ()
  "Validate that the Claudemacs process is alive and running."
  (let ((buffer (claudemacs--get-buffer)))
    (unless buffer
      (error "No Claudemacs session is active"))
    (with-current-buffer buffer
      (unless (and (boundp 'eat-terminal) eat-terminal)
        (error "Claudemacs session exists but terminal is not initialized. Please kill buffer and restart"))
      (let ((process (eat-term-parameter eat-terminal 'eat--process)))
        (unless (and process (process-live-p process))
          (error "Claudemacs session exists but process is not running. Please  kill buffer and restart")))))
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
  (if-let* ((buffer (claudemacs--get-buffer)))
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

(defun claudemacs--send-message-to-claude (message &optional no-return no-switch clear-first)
  "Send MESSAGE to the active Claudemacs session.
If NO-RETURN is non-nil, don't send a return/newline.
If NO-SWITCH is non-nil, don't switch to the Claude buffer.
If CLEAR-FIRST is non-nil, send C-u to clear any partial input first."
  (claudemacs--validate-process)
  (let ((claude-buffer (claudemacs--get-buffer)))
    (with-current-buffer claude-buffer
      (when clear-first
        (eat-term-send-string eat-terminal "\C-u"))
      (eat-term-send-string eat-terminal message)
      (unless no-return
        ;; Use eat-term-input-event for proper terminal input handling
        (eat-term-input-event eat-terminal 1 'return)))
    (unless no-switch
      (claudemacs--switch-to-buffer))))

(defun claudemacs--format-context-line-range (relative-path start-line end-line)
  "Format context for a line range in RELATIVE-PATH from START-LINE to END-LINE."
  (if (= start-line end-line)
      (format "File context: %s:%d\n" relative-path start-line)
    (format "File context: %s:%d-%d\n" relative-path start-line end-line)))

(defun claudemacs--scroll-to-bottom ()
  "Scroll the claudemacs buffer to bottom without switching to it."
  (interactive)
  (when-let* ((claude-buffer (claudemacs--get-buffer))
              (claude-window (get-buffer-window claude-buffer)))
    (with-current-buffer claude-buffer
      (goto-char (point-max))
      (set-window-point claude-window (point-max)))))

(defun claudemacs--scroll-to-top ()
  "Scroll the claudemacs buffer to top without switching to it."
  (interactive)
  (when-let* ((claude-buffer (claudemacs--get-buffer))
              (claude-window (get-buffer-window claude-buffer)))
    (with-current-buffer claude-buffer
      (goto-char (point-min))
      (set-window-point claude-window (point-min)))))

;;;###autoload
(defun claudemacs-fix-error-at-point ()
  "Send a request to Claude to fix the error at point using flycheck."
  (interactive)
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
    
    (claudemacs--send-message-to-claude message-text
                                        nil
                                        (not claudemacs-switch-to-buffer-on-send-error))
    (message "Sent error fix request to Claude")))

;;;###autoload
(defun claudemacs-execute-request ()
  "Execute a Claude request with file context.
If a region is selected, use it as context with line range.
Otherwise, use current line as context."
  (interactive)
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
         (request (read-string "Claude request: "))
         (message-text (concat context-text request)))
    
    (when (string-empty-p (string-trim request))
      (error "Request cannot be empty"))
    
    (claudemacs--send-message-to-claude message-text)
    (message "Sent request to Claude with context")))

;;;###autoload
(defun claudemacs-ask-without-context ()
  "Ask Claude a question without file or line context.
Prompts for a question and sends it directly to Claude without any 
file location or context information."
  (interactive)
  (claudemacs--validate-process)
  
  (let ((request (read-string "Ask Claude: ")))
    (when (string-empty-p (string-trim request))
      (error "Request cannot be empty"))
    
    (claudemacs--send-message-to-claude request)
    (message "Sent question to Claude")))

;;;###autoload
(defun claudemacs-add-file-reference ()
  "Add a file reference to the Claude conversation.
Prompts for a file and sends @rel/path/to/file without newline."
  (interactive)
  (claudemacs--validate-file-and-session)
  
  (let* ((context (claudemacs--get-file-context))
         (cwd (plist-get context :project-cwd))
         (selected-file (read-file-name "Add file reference: "))
         (relative-path (file-relative-name selected-file cwd))
         (reference-text (format "@%s " relative-path)))
    
    (claudemacs--send-message-to-claude reference-text t (not claudemacs-switch-to-buffer-on-file-add))
    (message "Added file reference: @%s" relative-path)))

;;;###autoload
(defun claudemacs-add-current-file-reference ()
  "Add current file reference to the Claude conversation.
Sends @rel/path/to/current/file without newline."
  (interactive)
  (claudemacs--validate-file-and-session)
  
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         (reference-text (format "@%s " relative-path)))
    
    (claudemacs--send-message-to-claude reference-text t (not claudemacs-switch-to-buffer-on-file-add))
    (message "Added current file reference: @%s" relative-path)))

;;;###autoload
(defun claudemacs-add-context ()
  "Add file context with line number(s) to the Claude conversation.
If a region is selected, uses line range (path:start-end).
Otherwise, uses current line (path:line).
Sends without newline so you can continue typing."
  (interactive)
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

    (claudemacs--send-message-to-claude context-text t (not claudemacs-switch-to-buffer-on-add-context))
    (message "Added context: %s" (string-trim context-text))))

;;;###autoload
(defun claudemacs-paste-context-to-shell ()
  "Paste current point/selection context into claudemacs shell without sending.
Shows buffer name, file name, line numbers, and the actual content with line number prefixes.
Works with both file buffers and non-file buffers.
This allows you to review and edit the context before sending to Claude."
  (interactive)
  (claudemacs--validate-process)

  (let* ((has-file (buffer-file-name))
         (context (when has-file
                    (condition-case nil
                        (claudemacs--get-file-context)
                      (error nil))))
         (relative-path (when context (plist-get context :relative-path)))
         (buffer-identifier (if has-file
                               (or relative-path (file-name-nondirectory has-file))
                             (buffer-name)))
         (has-region (use-region-p))
         (start-pos (if has-region
                        (region-beginning)
                      (line-beginning-position)))
         (end-pos (if has-region
                      (region-end)
                    (line-end-position)))
         (start-line (line-number-at-pos start-pos))
         (end-line (line-number-at-pos end-pos))
         (content (buffer-substring-no-properties start-pos end-pos))
         ;; Split content into lines and add line number prefixes
         (content-lines (split-string content "\n"))
         (numbered-lines (let ((line-num start-line)
                               (result '()))
                           (dolist (line content-lines)
                             (push (format "%4d %s" line-num line) result)
                             (setq line-num (1+ line-num)))
                           (nreverse result)))
         (numbered-content (string-join numbered-lines "\n"))
         ;; Build header with buffer name and location
         (header (if (= start-line end-line)
                    (format "Buffer: %s\nFile: %s:%d\n"
                            (buffer-name)
                            buffer-identifier
                            start-line)
                  (format "Buffer: %s\nFile: %s:%d-%d\n"
                          (buffer-name)
                          buffer-identifier
                          start-line
                          end-line)))
         (message-text (format "%s%s\n\n" header numbered-content)))

    ;; Send to Claude without return and without switching
    (claudemacs--send-message-to-claude message-text t t)
    ;; Now switch to Claude buffer so user can see and edit
    (claudemacs--switch-to-buffer)
    (message "Pasted context to Claude shell (not sent)")))

;;;###autoload
(defun claudemacs-generate-commit-message ()
  "Generate a commit message using Claude based on staged git changes.
This runs a one-shot Claude session in the background and inserts the result.
No interaction with the Claude buffer is needed."
  (interactive)

  ;; Check if we're in a commit buffer
  (unless (or (and (buffer-file-name)
                   (string-match-p "COMMIT_EDITMSG" (buffer-file-name)))
              (and (boundp 'git-commit-mode) git-commit-mode)
              (string-match-p "\\*magit.*commit\\*" (buffer-name)))
    (error "This command should be run from a git commit message buffer"))

  ;; Get the staged diff
  (let* ((default-directory (or (vc-git-root default-directory)
                                default-directory))
         (diff-output (shell-command-to-string "git diff --staged")))

    (when (string-empty-p (string-trim diff-output))
      (error "No staged changes found. Stage some changes first with 'git add'"))

    ;; Save current buffer to insert into later
    (let ((target-buffer (current-buffer))
          (temp-buffer (generate-new-buffer " *claude-commit-temp*")))

      (message "Generating commit message with Claude...")

      ;; Create the prompt
      (let* ((prompt (format "Analyze these git staged changes and generate ONLY a commit message (no extra text, no markdown, no explanations).

Format:
- First line: Clear title in imperative mood, under 50 characters
- Blank line
- Description: Explain what changed and why (2-4 sentences)

Staged changes:
```
%s
```

Return ONLY the commit message, nothing else." diff-output))
             (prompt-file (make-temp-file "claude-commit-prompt-" nil ".txt" prompt)))

        ;; Run Claude asynchronously
        (set-process-sentinel
         (start-process "claude-commit" temp-buffer
                       claudemacs-program
                       "--dangerously-skip-permissions"
                       "--prompt" (format "@%s" prompt-file))
         (lambda (process event)
           (when (string-match-p "finished" event)
             (with-current-buffer (process-buffer process)
               ;; Extract commit message from Claude's output
               (goto-char (point-min))
               ;; Skip to the actual response (after prompt echo and thinking)
               (let ((response-start (or (search-forward "\n\n" nil t)
                                        (point-min))))
                 (goto-char response-start)
                 (let ((commit-msg (buffer-substring-no-properties (point) (point-max))))
                   ;; Clean up the message
                   (setq commit-msg (string-trim commit-msg))
                   ;; Remove any markdown code blocks
                   (setq commit-msg (replace-regexp-in-string "^```.*\n" "" commit-msg))
                   (setq commit-msg (replace-regexp-in-string "\n```$" "" commit-msg))

                   ;; Insert into target buffer
                   (when (buffer-live-p target-buffer)
                     (with-current-buffer target-buffer
                       (goto-char (point-min))
                       (insert commit-msg "\n\n")
                       (goto-char (point-min))
                       (message "Commit message generated!")))

                   ;; Clean up
                   (delete-file prompt-file)
                   (kill-buffer (process-buffer process))))))

           (when (string-match-p "\\(exited\\|failed\\)" event)
             (delete-file prompt-file)
             (kill-buffer temp-buffer)
             (message "Failed to generate commit message: %s" event))))))))



;;;###autoload
(defun claudemacs-implement-comment ()
  "Send comment at point or region to Claude for implementation.
If region is active, uses the exact region.
If no region, finds the comment block at point.
Extracts comment text and sends it to Claude with implementation instructions."
  (interactive)
  (claudemacs--validate-file-and-session)
  
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         comment-bounds
         comment-text
         start-line
         end-line)
    
    (cond
     ;; Case 1: Region is active - use exact region (respect user's intentions)
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
      
      (claudemacs--send-message-to-claude message-text)
      (message "Sent comment implementation request to Claude (%d lines)" 
               (1+ (- end-line start-line))))))

;;;###autoload
(defun claudemacs-toggle-buffer ()
  "Toggle Claude buffer visibility.
Hide if current, focus if visible elsewhere, show if hidden."
  (interactive)
  (let ((claude-buffer (claudemacs--get-buffer)))
    (cond
     ;; Case 1: No Claude session exists
     ((not (claudemacs--validate-process))
      (error "No Claudemacs session is active"))
     
     ;; Case 2: Current buffer IS the Claude buffer
     ((eq (current-buffer) claude-buffer)
      ;; Hide using quit-window (automatically handles window vs buffer logic)
      (quit-window))
     
     ;; Case 3: Claude buffer visible in another window
     ((get-buffer-window claude-buffer)
      ;; Quit that window (automatically handles created vs reused)
      ;;
      ;; Edge case: the window was created for Claude, but in the meantime you
      ;; have switched to another workspace and back, the window is no longer
      ;; created just for claudemacs -- it has shown something previous, so it
      ;; will no longer go away if you toggle. Them's the breaks.
      (with-selected-window (get-buffer-window claude-buffer)
        (quit-window)))
     
     ;; Case 4: Claude buffer exists but not visible
     (t
      ;; Show Claude buffer
      (if claudemacs-switch-to-buffer-on-toggle
          (claudemacs--switch-to-buffer)
        (progn
          (display-buffer claude-buffer)
          ;; Move to bottom without switching focus
          (with-current-buffer claude-buffer
            (set-window-point (get-buffer-window claude-buffer) (point-max)))))))))

;;;; User Interface
;;;###autoload (autoload 'claudemacs-transient-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-transient-menu ()
  "Claude Code AI Pair Programming Interface."
  ["Claudemacs: AI pair programming with Claude Code"
   ["Core"
    ("s" "Start/Open Session" claudemacs-run)
    ("r" "Start with Resume" claudemacs-resume)
    ("R" "Restart Session" claudemacs-restart)
    ("k" "Kill Session" claudemacs-kill)
    ("t" "Toggle Buffer" claudemacs-toggle-buffer)
    ("l" "List All Sessions" claudemacs-list-sessions)]
   ["Actions"
    ("e" "Fix Error at Point" claudemacs-fix-error-at-point)
    ("x" "Execute Request (with context)" claudemacs-execute-request)
    ("X" "Execute Request (no context)" claudemacs-ask-without-context)
    ("i" "Implement Comment" claudemacs-implement-comment)
    ("c" "Generate Commit Message" claudemacs-generate-commit-message)
    ("f" "Add File Reference" claudemacs-add-file-reference)
    ("F" "Add Current File" claudemacs-add-current-file-reference)
    ("a" "Add Context" claudemacs-add-context)
    ("p" "Paste Context to Shell" claudemacs-paste-context-to-shell)]
   ["Quick Responses"
     ("y" "Send Yes (RET)" claudemacs-send-yes)
     ("n" "Send No (ESC)" claudemacs-send-no)]]
   ["Maintenance"
     ("u" "Unstick Claude buffer" claudemacs-unstick-terminal)])

;;;###autoload
(defvar claudemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") #'claudemacs-clear-buffer)
    (define-key map (kbd "C-c s") #'claudemacs-spawn-agent)
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
    (setq-local cursor-type nil)))

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
  (with-current-buffer (claudemacs--get-buffer)
    (when (and (boundp 'eat-terminal) eat-terminal)
        (let* ((process (eat-term-parameter eat-terminal 'eat--process))
               (claude-window (get-buffer-window (claudemacs--get-buffer))))
          (if (and process (process-live-p process) claude-window)
              (eat--adjust-process-window-size process (list claude-window)))))))

;; You might want to bind this to a key, for example:
;; (define-key eat-mode-map (kbd "C-c C-r") #'eat-force-redraw) ;; 'r' for redraw

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
  (with-current-buffer (claudemacs--get-buffer)
    (setq-local window-adjust-process-window-size-function
                'window-adjust-process-window-size-smallest))
  (claudemacs--scroll-to-top)
  (redisplay)
  (claudemacs--scroll-to-bottom)
  (redisplay)
  (with-current-buffer (claudemacs--get-buffer)
    ;; CRITICAL: Disable window-adjust-process-window-size-function to prevent
    ;; terminal redraw/scroll reset on buffer switching (same issue as vterm #149)
    (setq-local window-adjust-process-window-size-function 'ignore)))

;; Set up hooks when package is loaded
(unless (memq 'claudemacs--check-and-disable-window-adjust window-buffer-change-functions)
  (add-hook 'window-buffer-change-functions #'claudemacs--check-and-disable-window-adjust))

;; Set up advice when package is loaded
(unless (advice-member-p #'claudemacs--show-cursor 'eat-emacs-mode)
  (advice-add 'eat-emacs-mode :after #'claudemacs--show-cursor))

(unless (advice-member-p #'claudemacs--hide-cursor 'eat-semi-char-mode)
  (advice-add 'eat-semi-char-mode :after #'claudemacs--hide-cursor))

(provide 'claudemacs)
;;; claudemacs.el ends here
