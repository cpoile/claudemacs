;;; claudemacs.el --- AI pair programming with Claude Code -*- lexical-binding: t; -*-
;; Author: Christopher Poile <cpoile@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0") (compat "30.0.2.0") (eat "0.9.2"))
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

(defun claudemacs--get-buffer-name (&optional dir)
  "Generate the claudemacs buffer name based on project root.
If DIR is supplied, generate a name for that directory's session;
otherwise use the current project root."
  (let ((root (claudemacs--project-root)))
    (format "*claudemacs:%s*" (file-truename root))))

(defun claudemacs--get-buffer (&optional dir)
  "Return existing claudemacs buffer for DIR or nil."
  (get-buffer (claudemacs--get-buffer-name dir)))

(defun claudemacs--is-claudemacs-buffer-p (&optional buffer)
  "Return t if BUFFER (or current buffer) is a claudemacs buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (string-match-p "^\\*claudemacs:" (buffer-name buf)))))

;;;; Terminal Integration
;; Eat terminal emulator functions  
(declare-function eat-make "eat")
(declare-function eat-term-send-string "eat")
(declare-function eat-kill-process "eat")
(defvar eat-terminal)
(defvar eat-term-name)

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


(defun claudemacs--start (dir &rest args)
  "Start Claude Code in directory DIR with optional ARGS."
  (require 'eat)
  (let* ((default-directory dir)
         (buffer-name (claudemacs--get-buffer-name dir))
         (buffer (get-buffer-create buffer-name))
         (process-environment 
          (append '("TERM=xterm-256color")
                  process-environment)))
    (with-current-buffer buffer
      (cd dir)
      (setq-local eat-term-name "xterm-256color")
      (let ((process-adaptive-read-buffering nil)
            (switches (remove nil (append args claudemacs-program-switches))))
        (apply #'eat-make (substring buffer-name 1 -1) claudemacs-program nil switches))
      
      (claudemacs--setup-repl-faces)
      ;; Keep cursor at end of buffer for terminal interaction
      (setq-local scroll-conservatively 101)
      (setq-local scroll-margin 0)
      (setq-local maximum-scroll-margin 0)
      
      ;; Bind C-g to send ESC to terminal in Claude buffers
      (local-set-key (kbd "C-g") (lambda () (interactive) 
                                    (eat-term-send-string eat-terminal (kbd "ESC")))))
    
    (let ((window (display-buffer buffer)))
      (select-window window))))

;;;; Interactive Commands
;;;###autoload
(defun claudemacs-run (&optional arg)
  "Start Claude Code.
With prefix ARG, prompt for the project directory."
  (interactive "P")
  (let* ((dir (if arg
                  (read-directory-name "Project directory: ")
                (claudemacs--project-root))))
    (claudemacs--start dir)))

;;;###autoload
(defun claudemacs-resume (&optional arg)
  "Start Claude Code, resuming a previous session.
With prefix ARG, prompt for the project directory."
  (interactive "P")
  (let* ((dir (if arg
                  (read-directory-name "Project directory: ")
                (claudemacs--project-root))))
    (claudemacs--start dir "--resume")))

;;;; User Interface
;;;###autoload (autoload 'claudemacs-transient-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-transient-menu ()
  "Claude Code AI Pair Programming Interface."
  ["Claudemacs: AI Pair Programming"
   ["Core"
    ("c" "Start/Open Session" claudemacs-run)
    ("r" "Start with Resume" claudemacs-resume)]])

;;;###autoload
(defvar claudemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") #'claudemacs-transient-menu)
    map)
  "Keymap for `claudemacs-mode'.")

;;;###autoload
(define-minor-mode claudemacs-mode
  "Minor mode for Claude Code AI pair programming.

\\{claudemacs-mode-map}"
  :lighter " Claude"
  :keymap claudemacs-mode-map
  :group 'claudemacs)

(provide 'claudemacs)
;;; claudemacs.el ends here
