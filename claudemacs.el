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

;; Declare functions from optional packages
(declare-function safe-persp-name "perspective")
(declare-function get-current-persp "perspective")
(declare-function flycheck-error-message "flycheck")
(declare-function flycheck-overlay-errors-in "flycheck")

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

This setting only affects claudemacs buffers and does not impact other eat buffers."
  :type 'boolean
  :group 'claudemacs)

(defface claudemacs-repl-face
  nil
  "Face for Claude REPL."
  :group 'claudemacs)

;;;; Utility Functions
(defun claudemacs--project-root (&optional dir)
  "Get the project root using VC-git, or fallback to current buffer's directory.
If DIR is given, use the vc-git-root of DIR."
  (let ((loc (or dir (file-name-directory (buffer-file-name)))))
    (vc-git-root loc)))

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
;; (defvar eat-terminal)
;; (defvar eat-term-name)
;; (defvar eat-mode-map)
;; (defvar eat-semi-char-mode-map)

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

(defun claudemacs--setup-buffer-keymap ()
  "Set up buffer-local keymap for claudemacs buffers with custom key bindings."
  ;; Set up C-g binding
  (local-set-key (kbd "C-g") #'claudemacs--send-escape)

  ;; This was a pain to make work.
  ;; Use the nuclear option - force override in minor mode maps
  (when (and claudemacs-m-return-is-submit (boundp 'minor-mode-map-alist))
    (setq-local minor-mode-map-alist
                (cons `(t . ,(let ((map (make-sparse-keymap)))
                               (define-key map (kbd "RET") #'claudemacs--meta-ret-key)
                               (define-key map (kbd "M-RET") #'claudemacs--ret-key)
                               map))
                      minor-mode-map-alist))))

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
        (apply #'eat-make (substring buffer-name 1 -1) claudemacs-program nil switches))
      
      (claudemacs--setup-repl-faces)
      ;; Optimize scrolling for terminal input - allows text to go to bottom
      (setq-local scroll-conservatively 10000)  ; Never recenter
      (setq-local scroll-margin 0)              ; No margin so text goes to edge
      (setq-local maximum-scroll-margin 0)      ; No maximum margin
      (setq-local scroll-preserve-screen-position t)  ; Preserve position during scrolling
      
      ;; Set up custom key mappings for claudemacs buffers
      (claudemacs--setup-buffer-keymap))
    
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
  (claudemacs--run-with-args arg "--resume"))

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

(defun claudemacs--validate-session ()
  "Validate that we have a file, project, and active Claudemacs session."
  (unless (buffer-file-name)
    (error "Buffer is not visiting a file"))
  (unless (claudemacs--project-root)
    (error "Not in a project"))
  (unless (claudemacs--get-buffer)
    (error "No Claudemacs session is active")))

(defun claudemacs--get-file-context ()
  "Get file context information for the current buffer.
Returns a plist with :file-path, :project-root, and :relative-path."
  (let* ((file-path (buffer-file-name))
         (project-root (claudemacs--project-root))
         (relative-path (file-relative-name file-path project-root)))
    (list :file-path file-path
          :project-root project-root
          :relative-path relative-path)))

(defun claudemacs--send-message-to-claude (message &optional no-return)
  "Send MESSAGE to the active Claudemacs session.
If NO-RETURN is non-nil, don't send a return/newline."
  (let ((claude-buffer (claudemacs--get-buffer)))
    (with-current-buffer claude-buffer
      (eat-term-send-string eat-terminal message)
      (unless no-return
        (eat-term-send-string eat-terminal (kbd "RET"))))
    (claudemacs--switch-to-buffer)))

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
  (claudemacs--validate-session)
  
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         (line-number (line-number-at-pos))
         (errors (claudemacs--get-flycheck-errors-on-line))
         (error-message (claudemacs--format-flycheck-errors errors))
         (message-text (if (string-empty-p error-message)
                          (format "Please fix any issues at @%s line %d"
                                  relative-path line-number)
                        (format "Please fix the error at @%s line %d: %s"
                                relative-path line-number error-message))))
    
    (claudemacs--send-message-to-claude message-text)
    (message "Sent error fix request to Claude")))

;;;###autoload
(defun claudemacs-execute-request ()
  "Execute a Claude request with file context.
If a region is selected, use it as context with line range.
Otherwise, use current line as context."
  (interactive)
  (claudemacs--validate-session)
  
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
(defun claudemacs-add-file-reference ()
  "Add a file reference to the Claude conversation.
Prompts for a file and sends @rel/path/to/file without newline."
  (interactive)
  (claudemacs--validate-session)
  
  (let* ((context (claudemacs--get-file-context))
         (project-root (plist-get context :project-root))
         (selected-file (read-file-name "Add file reference: " project-root))
         (relative-path (file-relative-name selected-file project-root))
         (reference-text (format "@%s " relative-path)))
    
    (claudemacs--send-message-to-claude reference-text t)
    (message "Added file reference: @%s" relative-path)))

;;;###autoload
(defun claudemacs-add-current-file-reference ()
  "Add current file reference to the Claude conversation.
Sends @rel/path/to/current/file without newline."
  (interactive)
  (claudemacs--validate-session)
  
  (let* ((context (claudemacs--get-file-context))
         (relative-path (plist-get context :relative-path))
         (reference-text (format "@%s " relative-path)))
    
    (claudemacs--send-message-to-claude reference-text t)
    (message "Added current file reference: @%s" relative-path)))

;;;###autoload
(defun claudemacs-toggle-buffer ()
  "Toggle Claude buffer visibility.
Hide if current, focus if visible elsewhere, show if hidden."
  (interactive)
  (let ((claude-buffer (claudemacs--get-buffer)))
    (cond
     ;; Case 1: No Claude session exists
     ((not claude-buffer)
      (error "No Claudemacs session is active"))
     
     ;; Case 2: Current buffer IS the Claude buffer
     ((eq (current-buffer) claude-buffer)
      ;; Hide using quit-window (automatically handles window vs buffer logic)
      (quit-window))
     
     ;; Case 3: Claude buffer visible in another window
     ((get-buffer-window claude-buffer)
      ;; Quit that window (automatically handles created vs reused)
      ;; 
      ;; Edge case: (note in the README) if the window was created for Claude,
      ;; but in the meantime you have switched to another workspace and back,
      ;; the window is no longer created just for claudemacs -- it has shown
      ;; something previous, so it will no longer go away if you toggle. Them's
      ;; the breaks.
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
  ["Claudemacs: AI Pair Programming"
   ["Core"
    ("c" "Start/Open Session" claudemacs-run)
    ("r" "Start with Resume" claudemacs-resume)
    ("k" "Kill Session" claudemacs-kill)
    ("t" "Toggle Buffer" claudemacs-toggle-buffer)]
   ["Actions"
    ("e" "Fix Error at Point" claudemacs-fix-error-at-point)
    ("x" "Execute Request with Context" claudemacs-execute-request)
    ("f" "Add File Reference" claudemacs-add-file-reference)
    ("F" "Add Current File" claudemacs-add-current-file-reference)]
   ["Maintenance"
    ("u" "Unstick Claude input box location" claudemacs-reset-buffer-tracking)]])

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


(defun claudemacs-reset-buffer-tracking ()
  "Reset the claudemacs buffer's vertical rest point.
Sometimes the input box gets stuck mid or top of the buffer because of
the idiosyncracies of eat-mode. This will reset the input box to the
bottom of the buffer."
  (interactive)
  (when (claudemacs--is-claudemacs-buffer-p)
    (error "Reset buffer cannot be used while visiting the claudemacs buffer itself"))
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
