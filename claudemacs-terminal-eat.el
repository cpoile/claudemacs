;;; claudemacs-terminal-eat.el --- Eat backend for Claudemacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Eat implementation of the terminal operations defined by
;; `claudemacs-terminal'.  All Eat-specific behavior and workarounds live here.

;;; Code:

(require 'cl-lib)
(require 'claudemacs-terminal)
(require 'eat)

(declare-function eat--adjust-process-window-size "eat")
(declare-function eat-emacs-mode "eat")
(declare-function eat-kill-process "eat")
(declare-function eat-make "eat")
(declare-function eat-self-input "eat")
(declare-function eat-semi-char-mode "eat")
(declare-function eat-term-cursor-type "eat")
(declare-function eat-term-input-event "eat")
(declare-function eat-term-parameter "eat")
(declare-function eat-term-redisplay "eat")
(declare-function eat-term-resize "eat")
(declare-function eat-term-send-string "eat")
(declare-function eat-term-send-string-as-yank "eat")

(defvar claudemacs--tool)
(defvar eat-default-cursor-type)
(defvar eat-enable-blinking-text)
(defvar eat-horizontal-bar-cursor-type)
(defvar eat-semi-char-mode-map)
(defvar eat-term-name)
(defvar eat-terminal)
(defvar eat-vertical-bar-cursor-type)
(defvar eat-very-visible-cursor-type)
(defvar eat-very-visible-horizontal-bar-cursor-type)
(defvar eat-very-visible-vertical-bar-cursor-type)

(defvar claudemacs--eat-original-c-b-binding nil
  "Original `C-b' binding in `eat-semi-char-mode-map'.")

(defvar claudemacs--eat-global-setup-done nil
  "Non-nil after the Eat backend's global integration is installed.")

(defun claudemacs--eat-session-buffer-p ()
  "Return non-nil when the current buffer is owned by the Eat backend."
  (eq claudemacs--terminal-backend 'eat))

(defun claudemacs--eat-process ()
  "Return the current Eat terminal process, or nil."
  (or (get-buffer-process (current-buffer))
      (when (and (boundp 'eat-terminal) eat-terminal)
        ;; Older Eat versions expose no public process accessor.
        (eat-term-parameter eat-terminal 'eat--process))))

(defun claudemacs--eat-start (buffer program switches)
  "Start PROGRAM with SWITCHES in Eat BUFFER and return its process."
  (with-current-buffer buffer
    (when-let ((term (getenv "TERM")))
      (setq-local eat-term-name term))
    (apply #'eat-make
           (substring (buffer-name buffer) 1 -1)
           program nil switches)
    (claudemacs--eat-process)))

(defun claudemacs--eat-ready-p ()
  "Return non-nil when the current Eat terminal is initialized."
  (and (boundp 'eat-terminal) eat-terminal (claudemacs--eat-process)))

(defun claudemacs--eat-live-p ()
  "Return non-nil when the current Eat process is live."
  (let ((process (claudemacs--eat-process)))
    (and process (process-live-p process))))

(defun claudemacs--eat-kill ()
  "Terminate the current Eat process."
  (eat-kill-process))

(defun claudemacs--eat-send-string (string)
  "Send STRING verbatim to the current Eat terminal."
  (eat-term-send-string eat-terminal string))

(defun claudemacs--eat-paste-string (string)
  "Send STRING to Eat using its bracketed-yank support."
  (eat-term-send-string-as-yank eat-terminal (list string)))

(defun claudemacs--eat-send-key (key)
  "Send semantic KEY to the current Eat terminal."
  (pcase key
    ('return (eat-term-input-event eat-terminal 1 'return))
    ('meta-return (eat-term-send-string eat-terminal "\e\C-m"))
    ('left (eat-term-input-event eat-terminal 1 'left))
    ('escape (eat-term-send-string eat-terminal "\e"))
    (_ (error "Unsupported Eat key: %S" key))))

(defun claudemacs--eat-disable-codex-cursor-blink ()
  "Disable Eat's expensive frame-redrawing cursor blink for Codex."
  (when (eq claudemacs--tool 'codex)
    (setq-local eat-very-visible-cursor-type
                (copy-tree eat-default-cursor-type))
    (setq-local eat-very-visible-vertical-bar-cursor-type
                (copy-tree eat-vertical-bar-cursor-type))
    (setq-local eat-very-visible-horizontal-bar-cursor-type
                (copy-tree eat-horizontal-bar-cursor-type))
    (funcall (eat-term-parameter eat-terminal 'set-cursor-function)
             eat-terminal
             (eat-term-cursor-type eat-terminal))))

(defun claudemacs--eat-setup-buffer (bell-function)
  "Configure the current Eat buffer to invoke BELL-FUNCTION on BEL."
  (setf (eat-term-parameter eat-terminal 'ring-bell-function)
        (lambda (&rest _arguments)
          (funcall bell-function)))
  ;; Eat-specific display stabilization.  Keep these settings out of the core:
  ;; Ghostel owns its scrolling, anchoring, and row geometry itself.
  (setq-local scroll-conservatively 10000)
  (setq-local scroll-margin 0)
  (setq-local maximum-scroll-margin 0)
  (setq-local scroll-preserve-screen-position t)
  (setq-local auto-window-vscroll nil)
  (setq-local scroll-step 1)
  (setq-local hscroll-step 1)
  (setq-local hscroll-margin 0)
  (setq-local line-spacing 0)
  (setq-local vertical-scroll-bar nil)
  (setq-local fringe-mode 0)
  (when (bound-and-true-p eat-enable-blinking-text)
    (setq-local eat-enable-blinking-text nil))
  ;; Eat can render Claude's blinking status glyph with changing metrics.
  ;; Substitute a stable same-width glyph to prevent terminal row jitter.
  (let ((display-table (make-display-table)))
    (aset display-table #x23fa [?✽])
    (setq-local buffer-display-table display-table))
  (claudemacs--eat-disable-codex-cursor-blink))

(defun claudemacs--eat-setup-faces ()
  "Apply Claudemacs face remapping to the current Eat buffer."
  (buffer-face-set :inherit 'claudemacs-repl-face)
  (cl-flet ((remap-face (face &rest properties)
              (apply #'face-remap-add-relative
                     face :inherit 'claudemacs-repl-face properties)))
    (mapc #'remap-face
          '(eat-shell-prompt-annotation-running
            eat-shell-prompt-annotation-success
            eat-shell-prompt-annotation-failure
            eat-term-bold eat-term-italic
            eat-term-slow-blink eat-term-fast-blink))
    (dotimes (index 10)
      (remap-face (intern (format "eat-term-font-%d" index))))
    (face-remap-add-relative 'nobreak-space :underline nil)
    (remap-face 'eat-term-faint :foreground "#999999" :weight 'light)))

(defun claudemacs--eat-resize-to-window (buffer)
  "Resize Eat BUFFER and its PTY to the displayed window."
  (when-let* ((window (get-buffer-window buffer))
              (width (max (window-body-width window) 1))
              (height (max (window-body-height window) 1))
              (process (claudemacs--eat-process)))
    (eat-term-resize eat-terminal width height)
    (when (process-live-p process)
      (set-process-window-size process height width))
    (eat-term-redisplay eat-terminal)))

(defun claudemacs--eat-post-display (buffer)
  "Adjust Eat BUFFER after it has been displayed."
  (claudemacs--eat-resize-to-window buffer))

(defun claudemacs--eat-force-redraw ()
  "Force the current Eat terminal and process to adopt its window size."
  (when-let* ((process (claudemacs--eat-process))
              ((process-live-p process))
              (window (get-buffer-window (current-buffer))))
    (eat--adjust-process-window-size process (list window))))

(defun claudemacs--eat-unstick ()
  "Reset Eat's window tracking and return the display to its live bottom."
  (claudemacs--eat-force-redraw)
  (setq-local window-adjust-process-window-size-function
              'window-adjust-process-window-size-smallest)
  (when-let ((window (get-buffer-window (current-buffer))))
    (goto-char (point-min))
    (set-window-point window (point-min))
    (redisplay)
    (goto-char (point-max))
    (set-window-point window (point-max))
    (redisplay))
  (setq-local window-adjust-process-window-size-function 'ignore))

(defun claudemacs--eat-show-cursor (&rest _arguments)
  "Show the Emacs cursor in Claudemacs Eat buffers using Emacs mode."
  (when (claudemacs--eat-session-buffer-p)
    (setq-local cursor-type 'box)))

(defun claudemacs--eat-hide-cursor (&rest _arguments)
  "Hide the Emacs cursor in Claude sessions using Eat semi-char mode."
  (when (and (claudemacs--eat-session-buffer-p)
             (eq claudemacs--tool 'claude))
    (setq-local cursor-type nil)))

(defun claudemacs--eat-maybe-left-key ()
  "Send left in Claudemacs Eat buffers, otherwise preserve Eat behavior."
  (interactive)
  (if (claudemacs--eat-session-buffer-p)
      (claudemacs--eat-send-key 'left)
    (call-interactively #'eat-self-input)))

(defun claudemacs--eat-check-and-disable-window-adjust (&rest _arguments)
  "Disable Emacs process resizing after a long Claudemacs Eat buffer appears."
  (when (and (claudemacs--eat-session-buffer-p)
             (not (eq window-adjust-process-window-size-function 'ignore)))
    (when-let* ((window (get-buffer-window (current-buffer)))
                (height (window-height window))
                ((> (count-lines (point-min) (point-max)) height)))
      (goto-char (point-min))
      (redisplay)
      (goto-char (point-max))
      (redisplay)
      (setq-local window-adjust-process-window-size-function 'ignore))))

(defun claudemacs--eat-global-setup ()
  "Install global integration needed by Claudemacs Eat buffers."
  (unless claudemacs--eat-global-setup-done
    (advice-add 'eat-emacs-mode :after #'claudemacs--eat-show-cursor)
    (advice-add 'eat-semi-char-mode :after #'claudemacs--eat-hide-cursor)
    (add-hook 'window-buffer-change-functions
              #'claudemacs--eat-check-and-disable-window-adjust)
    (when (boundp 'eat-semi-char-mode-map)
      (setq claudemacs--eat-original-c-b-binding
            (lookup-key eat-semi-char-mode-map (kbd "C-b")))
      (define-key eat-semi-char-mode-map (kbd "C-b")
                  #'claudemacs--eat-maybe-left-key))
    (setq claudemacs--eat-global-setup-done t)))

(defun claudemacs--eat-global-teardown ()
  "Remove global integration installed for Claudemacs Eat buffers."
  (when claudemacs--eat-global-setup-done
    (advice-remove 'eat-emacs-mode #'claudemacs--eat-show-cursor)
    (advice-remove 'eat-semi-char-mode #'claudemacs--eat-hide-cursor)
    (remove-hook 'window-buffer-change-functions
                 #'claudemacs--eat-check-and-disable-window-adjust)
    (when (and (boundp 'eat-semi-char-mode-map)
               (eq (lookup-key eat-semi-char-mode-map (kbd "C-b"))
                   #'claudemacs--eat-maybe-left-key))
      (define-key eat-semi-char-mode-map (kbd "C-b")
                  claudemacs--eat-original-c-b-binding))
    (setq claudemacs--eat-global-setup-done nil)))

(claudemacs--terminal-register-backend
 'eat
 :start #'claudemacs--eat-start
 :ready-p #'claudemacs--eat-ready-p
 :live-p #'claudemacs--eat-live-p
 :kill #'claudemacs--eat-kill
 :send-string #'claudemacs--eat-send-string
 :paste-string #'claudemacs--eat-paste-string
 :send-key #'claudemacs--eat-send-key
 :setup-buffer #'claudemacs--eat-setup-buffer
 :setup-faces #'claudemacs--eat-setup-faces
 :post-display #'claudemacs--eat-post-display
 :force-redraw #'claudemacs--eat-force-redraw
 :unstick #'claudemacs--eat-unstick
 :global-setup #'claudemacs--eat-global-setup
 :global-teardown #'claudemacs--eat-global-teardown)

(provide 'claudemacs-terminal-eat)
;;; claudemacs-terminal-eat.el ends here
