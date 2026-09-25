;;; claudemacs-terminal-ghostel.el --- Ghostel terminal backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christopher Poile

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the Claudemacs adapter for Ghostel.  Ghostel is loaded
;; lazily with this adapter, so selecting another terminal
;; backend does not make Ghostel (or its native module) a package dependency.
;; Registration uses the operation registrar provided by
;; `claudemacs-terminal.el'.

;;; Code:

(require 'claudemacs-terminal)
(require 'ghostel)
(require 'subr-x)

(declare-function ghostel-exec "ghostel"
                  (buffer program &optional args identity))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ghostel-force-redraw "ghostel")
(declare-function claudemacs--terminal-register-backend "claudemacs-terminal"
                  (backend &rest operations))

(defvar-local claudemacs--ghostel-face-remap-cookie nil
  "Face-remapping cookie installed by the Ghostel backend.
This is buffer-local because Ghostel owns the terminal buffer's display
faces, while Claudemacs may have several sessions with different themes or
local face customizations.")

(defvar claudemacs-ghostel-query-before-killing)

(defun claudemacs--ghostel-apply-kill-query-setting ()
  "Apply Claudemacs' Ghostel kill-query setting to the current buffer.

Fall back to the safe default when this adapter is evaluated without the
main Claudemacs customization definitions, such as during isolated loading."
  (setq-local ghostel-query-before-killing
              (if (boundp 'claudemacs-ghostel-query-before-killing)
                  claudemacs-ghostel-query-before-killing
                t)))

(defun claudemacs--ghostel-process ()
  "Return the process stored by the Claudemacs terminal facade.

The facade owns `claudemacs--terminal-process'.  Keeping this small helper in
the adapter means the adapter can still be loaded in isolation by tests or by
an older Claudemacs build where that variable has not been declared yet.  In
particular, this adapter does not inspect Ghostel's private
`ghostel--process' variable; `ghostel-exec' returns the lifecycle process for
that purpose."
  (and (boundp 'claudemacs--terminal-process)
       claudemacs--terminal-process))

(defun claudemacs--ghostel-start (buffer program switches)
  "Start PROGRAM with SWITCHES in BUFFER using Ghostel.

BUFFER must be a live Claudemacs buffer.  The process returned by
`ghostel-exec' is returned to the terminal facade, which stores it as
`claudemacs--terminal-process'.  Ghostel's public `ghostel-exec' API
  initializes the buffer without displaying or selecting it, so this operation
does not create any UI on its own."
  (unless (buffer-live-p buffer)
    (error "Cannot start Ghostel in dead buffer: %S" buffer))
  (let ((process (ghostel-exec buffer program switches)))
    (claudemacs--ghostel-apply-kill-query-setting)
    process))

(defun claudemacs--ghostel-ready-p ()
  "Return non-nil when the current buffer has an initialized Ghostel session.

Ghostel does not expose a public terminal-ready predicate.  Its public
`ghostel-exec' call returns the lifecycle process after initializing
`ghostel-mode', so the adapter treats that mode plus the facade's stored
process as the readiness boundary."
  (and (derived-mode-p 'ghostel-mode)
       (claudemacs--ghostel-process)))

(defun claudemacs--ghostel-live-p ()
  "Return non-nil when the current buffer's Ghostel process is live."
  (let ((process (claudemacs--ghostel-process)))
    (and (processp process)
         (process-live-p process))))

(defun claudemacs--ghostel-kill ()
  "Stop the current buffer's Ghostel process.

Ghostel intentionally has no public process-kill command.  The process
returned by `ghostel-exec' is an ordinary Emacs process for both its native
event-pipe and Emacs-PTY implementations; deleting it invokes Ghostel's
normal sentinel/cleanup path.  Buffer destruction remains the terminal
facade's responsibility."
  (let ((process (claudemacs--ghostel-process)))
    (when (and (processp process)
               (process-live-p process))
      ;; Claudemacs owns the session buffer's lifetime.  Prevent Ghostel's
      ;; normal exit sentinel from racing the facade's subsequent
      ;; `kill-buffer' call when `ghostel-kill-buffer-on-exit' is enabled.
      (when (boundp 'ghostel-kill-buffer-on-exit)
        (setq-local ghostel-kill-buffer-on-exit nil))
      (delete-process process))
    (when (boundp 'claudemacs--terminal-process)
      (setq-local claudemacs--terminal-process nil))))

(defun claudemacs--ghostel-send-string (string)
  "Send STRING unchanged to the current Ghostel terminal."
  (ghostel-send-string string))

(defun claudemacs--ghostel-paste-string (string)
  "Send STRING as a bracketed paste to the current Ghostel terminal."
  (ghostel-paste-string string))

(defun claudemacs--ghostel-modifiers-string (modifiers)
  "Convert MODIFIERS to Ghostel's comma-separated modifier format.

Ghostel accepts a string such as \"meta\" or \"shift,ctrl\".  The
facade normally passes that representation already, but accepting symbols
and lists keeps the adapter tolerant of callers that use Emacs event-style
modifier values."
  (cond
   ((null modifiers) "")
   ((stringp modifiers) modifiers)
   ((symbolp modifiers)
    (claudemacs--ghostel-modifier-name modifiers))
   ((listp modifiers)
    (mapconcat #'claudemacs--ghostel-modifier-name modifiers ","))
   (t (format "%s" modifiers))))

(defun claudemacs--ghostel-modifier-name (modifier)
  "Return Ghostel's modifier name for Emacs event MODIFIER."
  (pcase modifier
    ((or 'control 'ctrl) "ctrl")
    ((or 'meta 'alt) "meta")
    ('shift "shift")
    ('hyper "hyper")
    ('super "super")
    (_ (if (symbolp modifier)
           (symbol-name modifier)
         (format "%s" modifier)))))

(defun claudemacs--ghostel-send-key (key &optional modifiers)
  "Send semantic KEY with optional MODIFIERS to Ghostel.

The terminal facade uses the semantic keys `return', `meta-return', `left',
and `escape'.  Ghostel's key encoder receives the corresponding key name and
modifier string, allowing it to honor the terminal's current keyboard
protocol.  String key names are accepted as a small compatibility convenience
for callers outside the facade."
  (let* ((semantic-key (if (stringp key) (intern key) key))
         (key-name (cond
                    ((memq semantic-key '(return meta-return)) "return")
                    ((eq semantic-key 'left) "left")
                    ((eq semantic-key 'escape) "escape")
                    ((and (stringp key) (not (string-empty-p key))) key)
                    (t (user-error "Unsupported Ghostel key: %S" key))))
         (default-modifiers (and (eq semantic-key 'meta-return) "meta"))
         (modifiers (claudemacs--ghostel-modifiers-string modifiers))
         (modifier-string
          (cond
           ((and default-modifiers (string-empty-p modifiers))
            default-modifiers)
           ((and default-modifiers (not (string-empty-p modifiers)))
            (concat default-modifiers "," modifiers))
           (t modifiers))))
    (ghostel-send-key key-name modifier-string)))

(defun claudemacs--ghostel-setup-buffer (bell-function)
  "Set up the current Ghostel buffer with BELL-FUNCTION.

Ghostel routes terminal BEL characters through Emacs's
`ring-bell-function'.  Install BELL-FUNCTION buffer-locally so completion
  notifications from one Claudemacs session do not affect other buffers."
  (claudemacs--ghostel-apply-kill-query-setting)
  (setq-local ring-bell-function bell-function)
  ;; Claudemacs owns completion notifications.  Avoid a second notification
  ;; when a tool emits OSC 9/777 as well as BEL.
  ;; `claudemacs--ghostel-setup-notifications' re-points this hook at
  ;; Claudemacs when a notification should carry the tool's own message.
  (when (boundp 'ghostel-notification-function)
    (setq-local ghostel-notification-function nil))
  (current-buffer))

(defun claudemacs--ghostel-setup-notifications (handler)
  "Report the OSC 9 / OSC 777 notifications Ghostel parses to HANDLER.

Ghostel already parses these sequences, so the adapter only has to redirect
its notification hook into Claudemacs instead of letting Ghostel raise its
own notification."
  (when (boundp 'ghostel-notification-function)
    (setq-local ghostel-notification-function
                (lambda (title body)
                  (funcall handler body title)))
    t))

(defun claudemacs--ghostel-setup-faces ()
  "Apply the Claudemacs REPL face to the current Ghostel terminal.

Ghostel already installs its own renderer face remapping.  Add one relative
remapping for Claudemacs' configurable REPL face and retain its cookie so
repeated backend setup calls do not stack duplicate remappings."
  (when (and (fboundp 'face-remap-add-relative)
             (facep 'claudemacs-repl-face)
             (not claudemacs--ghostel-face-remap-cookie))
    (require 'face-remap)
    (setq-local claudemacs--ghostel-face-remap-cookie
                (face-remap-add-relative 'default 'claudemacs-repl-face)))
  claudemacs--ghostel-face-remap-cookie)

(defun claudemacs--ghostel-post-display (buffer)
  "Redraw Ghostel in BUFFER after the terminal has been displayed.

Ghostel's public `ghostel-force-redraw' is intentionally a no-op for hidden
buffers.  The facade calls this operation after displaying BUFFER so the
terminal's window hooks can establish its real dimensions before this redraw."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ghostel-force-redraw)))
  buffer)

(defun claudemacs--ghostel-force-redraw ()
  "Force an immediate redraw of the current Ghostel terminal."
  (ghostel-force-redraw))

(defun claudemacs--ghostel-unstick ()
  "Recover a visible Ghostel terminal from a stale synchronized-output frame."
  (claudemacs--ghostel-force-redraw))

(claudemacs--terminal-register-backend
 'ghostel
 :start #'claudemacs--ghostel-start
 :ready-p #'claudemacs--ghostel-ready-p
 :live-p #'claudemacs--ghostel-live-p
 :kill #'claudemacs--ghostel-kill
 :send-string #'claudemacs--ghostel-send-string
 :paste-string #'claudemacs--ghostel-paste-string
 :send-key #'claudemacs--ghostel-send-key
 :setup-buffer #'claudemacs--ghostel-setup-buffer
 :setup-notifications #'claudemacs--ghostel-setup-notifications
 :setup-faces #'claudemacs--ghostel-setup-faces
 :post-display #'claudemacs--ghostel-post-display
 :force-redraw #'claudemacs--ghostel-force-redraw
 :unstick #'claudemacs--ghostel-unstick)

(provide 'claudemacs-terminal-ghostel)

;;; claudemacs-terminal-ghostel.el ends here
