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

(defface claudemacs-repl-face
  nil
  "Face for Claude REPL."
  :group 'claudemacs)

;;;; Utility Functions
(defun claudemacs--comment-syntax-info ()
  "Get comment syntax information for the current buffer.
Returns a plist with :start, :end, :start-skip, :end-skip, and :multi-line-p."
  (list :start comment-start
        :end comment-end
        :start-skip comment-start-skip
        :end-skip comment-end-skip
        :multi-line-p (and comment-end (not (string-empty-p comment-end)))))

;; no, the issue is (nth 4 (syntax-ppss)) does not detect a comment when the
;; point is at the first char of the comment
(defun claudemacs--point-in-comment-p ()
  "Return non-nil if point is inside or before a comment."
  (or (nth 4 (syntax-ppss))
      (save-excursion
        (skip-chars-forward " \t")
        (forward-char 1)  ;; being at the start of a comment is not "in" a comment
        (nth 4 (syntax-ppss)))))

(defun claudemacs--find-comment-start ()
  "Find the start of the comment block containing point.
Returns the position of the comment start, or nil if not in a comment."
  (save-excursion
    (let ((syntax-info (claudemacs--comment-syntax-info)))
      (cond
       ;; Multi-line comment (/* ... */)
       ((plist-get syntax-info :multi-line-p)
        (let ((start-regex (plist-get syntax-info :start-skip)))
          (when start-regex
            (while (and (claudemacs--point-in-comment-p)
                        (not (bobp)))
              (forward-line -1)
              (beginning-of-line))
            ;; Find the actual comment start on this line or nearby
            (while (and (not (bobp))
                        (not (looking-at-p start-regex)))
              (forward-line -1)
              (beginning-of-line))
            (when (looking-at-p start-regex)
              (point)))))
       
       ;; Single-line comment (// or # or ;;)
       (t
        (let ((start-regex (plist-get syntax-info :start-skip)))
          (when start-regex
            ;; Find the first line of consecutive comment lines
            (while (and (not (bobp))
                        (save-excursion
                          (forward-line -1)
                          (beginning-of-line)
                          (looking-at-p (concat "^[[:space:]]*" start-regex))))
              (forward-line -1))
            (beginning-of-line)
            (when (looking-at-p (concat "^[[:space:]]*" start-regex))
              (point)))))))))

(defun claudemacs--find-comment-end ()
  "Find the end of the comment block containing point.
Returns the position of the comment end, or nil if not in a comment."
  (save-excursion
    (let ((syntax-info (claudemacs--comment-syntax-info)))
      (cond
       ;; Multi-line comment (/* ... */)
       ((plist-get syntax-info :multi-line-p)
        (let ((end-regex (plist-get syntax-info :end-skip)))
          (when end-regex
            ;; Move to end of comment
            (while (and (claudemacs--point-in-comment-p)
                        (not (eobp)))
              (forward-char))
            ;; Back up to find the actual comment end
            (while (and (not (bobp))
                        (not (looking-back end-regex (line-beginning-position))))
              (backward-char))
            (when (looking-back end-regex (line-beginning-position))
              (point)))))
       
       ;; Single-line comment (// or # or ;;)
       (t
        (let ((start-regex (plist-get syntax-info :start-skip)))
          (when start-regex
            ;; Find the last line of consecutive comment lines
            (while (and (not (eobp))
                        (save-excursion
                          (forward-line 1)
                          (beginning-of-line)
                          (looking-at-p (concat "^[[:space:]]*" start-regex))))
              (forward-line 1))
            (end-of-line)
            (point))))))))

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
  (when (and (or claudemacs-m-return-is-submit claudemacs-shift-return-newline)
             (boundp 'minor-mode-map-alist))
    (setq-local minor-mode-map-alist
                (cons `(t . ,(let ((map (make-sparse-keymap)))
                               ;; Swap RET and M-RET if enabled
                               (when claudemacs-m-return-is-submit
                                 (define-key map (kbd "RET") #'claudemacs--meta-ret-key)
                                 (define-key map (kbd "M-RET") #'claudemacs--ret-key))
                               ;; Bind S-RET to newline if enabled
                               (when claudemacs-shift-return-newline
                                 (define-key map (kbd "S-RET") #'claudemacs--meta-ret-key))
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

(defun claudemacs--send-message-to-claude (message &optional no-return no-switch)
  "Send MESSAGE to the active Claudemacs session.
If NO-RETURN is non-nil, don't send a return/newline.
If NO-SWITCH is non-nil, don't switch to the Claude buffer."
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
    
    (claudemacs--send-message-to-claude reference-text t (not claudemacs-switch-to-buffer-on-file-add))
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
    
    (claudemacs--send-message-to-claude reference-text t (not claudemacs-switch-to-buffer-on-file-add))
    (message "Added current file reference: @%s" relative-path)))


;;;###autoload
(defun claudemacs-implement-comment ()
  "Send comment at point or region to Claude for implementation.
If region is active, uses the exact region.
If no region, finds the comment block at point.
Extracts comment text and sends it to Claude with implementation instructions."
  (interactive)
  (claudemacs--validate-session)
  
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
    ("i" "Implement Comment" claudemacs-implement-comment)
    ("f" "Add File Reference" claudemacs-add-file-reference)
    ("F" "Add Current File" claudemacs-add-current-file-reference)]
   ["Maintenance"
    ("u" "Unstick Claude input box location" claudemacs-unstick-terminal)]])

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
              (eat--adjust-process-window-size process claude-window))))))

;; You might want to bind this to a key, for example:
;; (define-key eat-mode-map (kbd "C-c C-r") #'eat-force-redraw) ;; 'r' for redraw

(defun claudemacs-unstick-terminal ()
  "Reset the claudemacs buffer's vertical rest point.
Sometimes the input box gets stuck mid or top of the buffer because of
the idiosyncracies of eat-mode. This will reset the input box to the
bottom of the buffer."
  (interactive)
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
