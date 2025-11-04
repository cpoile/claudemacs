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
  (with-current-buffer (claudemacs--get-buffer)
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

(defun claudemacs--start (work-dir &rest args)
  "Start Claude Code in WORK-DIR with ARGS."
  (require 'eat)
  (let* ((default-directory work-dir)
         (buffer-name (claudemacs--get-buffer-name))
         (buffer (get-buffer-create buffer-name))
         (process-environment
          (append '("TERM=xterm-256color")
                  process-environment)))
    (with-current-buffer buffer
      (cd work-dir)
      (setq-local eat-term-name "xterm-256color")
      (let ((process-adaptive-read-buffering nil)
            (switches (remove nil (append args claudemacs-program-switches))))
        (if claudemacs-use-shell-env
            ;; New behavior: Run through shell to source profile (e.g., .zprofile, .bash_profile)
            (let* ((shell (claudemacs--get-shell-name))
                   (claude-cmd (format "%s %s" claudemacs-program
                                      (mapconcat 'shell-quote-argument switches " "))))
              (eat-make (substring buffer-name 1 -1) shell nil "-c" claude-cmd))
          ;; Original behavior: Run Claude directly without shell environment
          (apply #'eat-make (substring buffer-name 1 -1) claudemacs-program nil switches)))
      
      ;; Set buffer-local variables after eat-make to ensure they persist
      (setq-local claudemacs--cwd work-dir)
      
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

(defun claudemacs--send-message-to-claude (message &optional no-return no-switch)
  "Send MESSAGE to the active Claudemacs session.
If NO-RETURN is non-nil, don't send a return/newline.
If NO-SWITCH is non-nil, don't switch to the Claude buffer."
  (claudemacs--validate-process)
  (let ((claude-buffer (claudemacs--get-buffer)))
    (with-current-buffer claude-buffer
      (eat-term-send-string eat-terminal message)
      (unless no-return
        (eat-term-send-string eat-terminal (kbd "RET"))))
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
(defun claudemacs-execute-request (start end)
  "Execute a Claude request with file context.
If a region is selected, use it as context with line range.
Otherwise, use current line as context."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (claudemacs--validate-file-and-session)
  
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         (has-region (not (= start end)))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (context-text (claudemacs--format-context-line-range relative-path start-line end-line))
         (request (if has-region
                      (buffer-substring-no-properties start end)
                    (read-string "Claude request: ")))
         (message-text (concat context-text request)))
    
    (when (string-empty-p (string-trim request))
      (error "Request cannot be empty"))
    
    (claudemacs--send-message-to-claude message-text)
    (message "Sent request to Claude with context")))

;;;###autoload
(defun claudemacs-ask-without-context (start end)
  "Ask Claude a question without file or line context.
If a region is selected, uses the region text as the request.
Otherwise, prompts for a question and sends it directly to Claude
without any file location or context information."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (claudemacs--validate-process)
  
  (let* ((has-region (not (= start end)))
         (request (if has-region
                      (buffer-substring-no-properties start end)
                    (read-string "Ask Claude: "))))
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
    ("k" "Kill Session" claudemacs-kill)
    ("t" "Toggle Buffer" claudemacs-toggle-buffer)]
   ["Actions"
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
