;;; claudemacs.el --- AI pair programming with Claude Code -*- lexical-binding: t; -*-
;; Author: Christopher Poile <cpoile@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0") (compat "30.0.2.0") (markdown-mode "2.7"))
;; Keywords: claudecode ai emacs llm ai-pair-programming tools
;; URL: https://github.com/cpoile/claudemacs
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Claudemacs integrates with Claude Code (https://docs.anthropic.com/en/docs/claude-code/overview)
;; for AI-assisted programming in Emacs.
;;
;; Inspired by Aidermacs: https://github.com/MatthewZMD/aidermacs and
;; and claude-code.el: https://github.com/stevemolitor/claude-code.el

;;; Code:

(require 'compat)
(require 'transient)
(require 'comint)
(require 'project)
(require 'vc-git)

(defgroup claudemacs nil
  "AI pair programming with Claude Code."
  :group 'tools)

(defcustom claudemacs-program "claude"
  "The name or path of the claude-code program."
  :type 'string
  :group 'claudemacs)

(defvar-local claudemacs--ready nil
  "Buffer-local variable to track whether claude is ready to accept commands.")

(defvar-local claudemacs--current-mode nil
  "Buffer-local variable to track the current claudemacs mode.")

(defun claudemacs--project-root ()
  "Get the project root using VC-git, or fallback to file directory."
  (or (vc-git-root default-directory)
      (when buffer-file-name
        (file-name-directory buffer-file-name))
      default-directory))

(defun claudemacs--get-buffer-name ()
  "Generate the claudemacs buffer name based on project root."
  (let ((root (claudemacs--project-root)))
    (format "*claudemacs:%s*" (file-truename root))))

(defun claudemacs--is-claudemacs-buffer-p (&optional buffer)
  "Return t if BUFFER (or current buffer) is a claudemacs buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (string-match-p "^\\*claudemacs:" (buffer-name buf)))))

(defun claudemacs--live-p (&optional buffer-name)
  "Return t if the claudemacs buffer is available and process is running."
  (let ((buf-name (or buffer-name (claudemacs--get-buffer-name))))
    (and (get-buffer buf-name)
         (process-live-p (get-buffer-process buf-name)))))

(defun claudemacs--start-process (buffer-name program args)
  "Start the claudemacs comint process in BUFFER-NAME with PROGRAM and ARGS."
  (let* ((buffer (get-buffer-create buffer-name))
         (process-environment process-environment)
         (default-directory (claudemacs--project-root)))
    (with-current-buffer buffer
      (unless (process-live-p (get-buffer-process buffer))
        (let ((process (apply #'make-comint-in-buffer
                              "claudemacs" buffer program nil args)))
          (set-process-filter process #'comint-output-filter)
          (setq-local claudemacs--ready t)
          (setq-local claudemacs--current-mode 'code))))
    buffer))

(defun claudemacs--switch-to-buffer (buffer-name)
  "Switch to the claudemacs buffer."
  (let ((buffer (get-buffer buffer-name)))
    (cond
     ((and buffer (get-buffer-window buffer))
      (select-window (get-buffer-window buffer)))
     (buffer
      (pop-to-buffer buffer))
     (t
      (error "No claudemacs buffer exists")))))

;;;###autoload
(defun claudemacs-run ()
  "Run claudemacs process using comint."
  (interactive)
  (let* ((buffer-name (claudemacs--get-buffer-name))
         (args '()))  ; Claude Code CLI arguments can be added here
    (if (claudemacs--live-p buffer-name)
        (claudemacs--switch-to-buffer buffer-name)
      (let ((buffer (claudemacs--start-process buffer-name claudemacs-program args)))
        (claudemacs--switch-to-buffer buffer-name)))))

;;;###autoload (autoload 'claudemacs-transient-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-transient-menu ()
  "Claude Code AI Pair Programming Interface."
  ["Claudemacs: AI Pair Programming"
   ["Core"
    ("c" "Start/Open Session" claudemacs-run)]])

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
