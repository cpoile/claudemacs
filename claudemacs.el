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
(require 'vterm nil 'noerror)

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
  (let ((default-directory (claudemacs--project-root))
        (process-environment 
         (append '("TERM=dumb"
                   "INSIDE_EMACS=t"
                   "NO_COLOR=1"
                   "COLUMNS=80"
                   "LINES=25")
                 process-environment))
        (process-connection-type nil)) ; Force pipe mode
    (unless (comint-check-proc buffer-name)
      (condition-case err
          (progn
            (apply #'make-comint-in-buffer "claudemacs" buffer-name program nil args)
            (with-current-buffer buffer-name
              (claudemacs--comint-mode)
              (setq-local claudemacs--ready nil)
              (setq-local claudemacs--current-mode 'code)
              ;; Ensure buffer is writable
              (setq buffer-read-only nil)
              (setq inhibit-read-only t)
              ;; Add debugging info
              (let ((process (get-buffer-process buffer-name)))
                (when process
                  (set-process-query-on-exit-flag process nil)
                  (message "Claude process started: %s" (process-status process))
                  ;; Wait a moment for process to settle
                  (sit-for 0.5))))))
        (error (err)
         (error "Failed to start %s: %s" program (error-message-string err))))))

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

(define-derived-mode claudemacs--comint-mode comint-mode "Claude"
  "Major mode for Claude Code comint sessions."
  (setq-local comint-prompt-regexp "^[>]+\\|^.*> *")
  (setq-local comint-input-ignoredups t)
  (setq-local comint-process-echoes nil)
  (setq-local comint-use-prompt-regexp nil) ; Let comint handle prompts naturally
  (setq-local comint-input-sender 'comint-simple-send)
  (setq-local comint-eol-on-send t)
  (setq-local comint-scroll-to-bottom-on-input t)
  (setq-local comint-scroll-to-bottom-on-output t)
  (setq-local comint-move-point-for-output t)
  (setq-local comint-input-ring-size 1000)
  ;; Explicitly set buffer as writable
  (setq buffer-read-only nil)
  (setq inhibit-read-only t)
  ;; Add a hook to keep buffer writable
  (add-hook 'comint-output-filter-functions
            (lambda (_output)
              (setq buffer-read-only nil)
              (setq inhibit-read-only t))
            nil t))

(defun claudemacs--debug-process ()
  "Debug function to check Claude process status."
  (interactive)
  (let* ((buffer-name (claudemacs--get-buffer-name))
         (buffer (get-buffer buffer-name))
         (process (and buffer (get-buffer-process buffer))))
    (if process
        (progn
          (message "Process status: %s" (process-status process))
          (message "Process command: %s" (process-command process))
          (when (eq (process-status process) 'run)
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n;; Sending test command...\n")
              (process-send-string process "/help\n"))))
      (message "No Claude process found"))))

;; VTerm backend functions
(declare-function vterm-other-window "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

(defun claudemacs--run-vterm-simple ()
  "Simple test function to run Claude Code in vterm."
  (interactive)
  (unless (require 'vterm nil t)
    (error "VTerm package is not available. Please install vterm"))
  (let* ((buffer-name "*claude-vterm-test*")
         (vterm-buffer-name buffer-name)
         (vterm-shell "claude")
         (vterm-kill-buffer-on-exit nil))
    (message "Starting Claude Code in vterm...")
    (vterm-other-window)
    (message "Claude Code started in vterm buffer: %s" buffer-name)))

;;;###autoload
(defun claudemacs-run ()
  "Run claudemacs process using comint."
  (interactive)
  (let* ((buffer-name (claudemacs--get-buffer-name))
         (args '()))  ; Claude Code CLI arguments can be added here
    (if (claudemacs--live-p buffer-name)
        (claudemacs--switch-to-buffer buffer-name)
      (progn
        ;; Kill existing buffer if it exists but process is dead
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))
        (claudemacs--start-process buffer-name claudemacs-program args)
        (claudemacs--switch-to-buffer buffer-name)))))

;;;###autoload (autoload 'claudemacs-transient-menu "claudemacs" nil t)
(transient-define-prefix claudemacs-transient-menu ()
  "Claude Code AI Pair Programming Interface."
  ["Claudemacs: AI Pair Programming"
   ["Core"
    ("c" "Start/Open Session (Comint)" claudemacs-run)
    ("v" "Start/Open Session (VTerm)" claudemacs--run-vterm-simple)]
   ["Debug"
    ("d" "Debug Process" claudemacs--debug-process)]])

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
