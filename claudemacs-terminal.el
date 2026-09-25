;;; claudemacs-terminal.el --- Terminal backend abstraction for Claudemacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file defines the backend contract used by Claudemacs.  Backend
;; implementations register operations and are loaded lazily, so installing
;; Claudemacs does not force a particular terminal package to be present.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar claudemacs-terminal-backend)

(defvar claudemacs--terminal-backends nil
  "Alist mapping terminal backend names to operation plists.")

(defvar claudemacs--terminal-initialized-backends nil
  "Terminal backends whose global setup operation has run.")

(defvar-local claudemacs--terminal-backend nil
  "Terminal backend owning the current Claudemacs session buffer.")

(defvar-local claudemacs--terminal-process nil
  "Lifecycle process returned by the current terminal backend.")

(defconst claudemacs--terminal-required-operations
  '(:start :ready-p :live-p :kill :send-string :paste-string :send-key
    :setup-buffer :setup-faces :post-display :force-redraw :unstick)
  "Operations every Claudemacs terminal backend must implement.")

(defun claudemacs--terminal-register-backend (backend &rest operations)
  "Register BACKEND with an OPERATIONS plist.

Backend functions operate with the Claudemacs terminal buffer current, except
`:start', which receives BUFFER, PROGRAM, and SWITCHES.  Registering an
existing BACKEND replaces its operation plist, which makes backend files safe
to evaluate repeatedly during development."
  (unless (symbolp backend)
    (error "Terminal backend name must be a symbol: %S" backend))
  (let ((tail operations))
    (while tail
      (unless (and (keywordp (car tail)) (cdr tail)
                   (functionp (cadr tail)))
        (error "Invalid operation registration for terminal backend %S" backend))
      (setq tail (cddr tail))))
  (setf (alist-get backend claudemacs--terminal-backends) operations)
  backend)

(defun claudemacs--terminal-backend-operations (backend)
  "Return the registered operation plist for BACKEND, or nil."
  (alist-get backend claudemacs--terminal-backends))

(defun claudemacs--terminal-load-backend (backend)
  "Load and validate BACKEND, then return its operation plist."
  (unless (symbolp backend)
    (user-error "Invalid Claudemacs terminal backend: %S" backend))
  (unless (claudemacs--terminal-backend-operations backend)
    (let ((feature (intern (format "claudemacs-terminal-%s" backend))))
      (condition-case err
          (require feature)
        (error
         (user-error "Unable to load Claudemacs terminal backend `%s': %s"
                     backend (error-message-string err))))))
  (let ((operations (claudemacs--terminal-backend-operations backend)))
    (unless operations
      (user-error "Unsupported Claudemacs terminal backend: %s" backend))
    (dolist (operation claudemacs--terminal-required-operations)
      (unless (functionp (plist-get operations operation))
        (error "Terminal backend `%s' does not implement %s"
               backend operation)))
    operations))

(defun claudemacs--terminal-ensure-backend (backend)
  "Ensure BACKEND is loaded and globally initialized."
  (let ((operations (claudemacs--terminal-load-backend backend)))
    (unless (memq backend claudemacs--terminal-initialized-backends)
      (when-let* ((setup (plist-get operations :global-setup)))
        (funcall setup))
      (push backend claudemacs--terminal-initialized-backends))
    operations))

(defun claudemacs--terminal-current-backend ()
  "Return the backend owning the current buffer.
Signal a useful error when the current buffer is not an initialized session."
  (or claudemacs--terminal-backend
      (user-error "Current buffer has no Claudemacs terminal backend")))

(defun claudemacs--terminal-operation (operation)
  "Return the current backend function for OPERATION."
  (let* ((backend (claudemacs--terminal-current-backend))
         (operations (claudemacs--terminal-ensure-backend backend))
         (function (plist-get operations operation)))
    (or function
        (error "Terminal backend `%s' has no %s operation"
               backend operation))))

(defun claudemacs--terminal-call (operation &rest arguments)
  "Call the current backend OPERATION with ARGUMENTS."
  (apply (claudemacs--terminal-operation operation) arguments))

(defun claudemacs--terminal-start (buffer backend program switches)
  "Start PROGRAM with SWITCHES in BUFFER using BACKEND.
Record BACKEND and the returned lifecycle process buffer-locally."
  (unless (buffer-live-p buffer)
    (error "Cannot start a terminal in a dead buffer"))
  (with-current-buffer buffer
    (setq-local claudemacs--terminal-backend backend)
    (condition-case err
        (let* ((operations (claudemacs--terminal-ensure-backend backend))
               (start (plist-get operations :start))
               (process (funcall start buffer program switches)))
          ;; Terminal start functions establish a major mode, which clears
          ;; ordinary buffer-local variables.  Record ownership after that
          ;; boundary as well as before it.
          (setq-local claudemacs--terminal-backend backend)
          (setq-local claudemacs--terminal-process process)
          process)
      (error
       (setq-local claudemacs--terminal-backend nil)
       (setq-local claudemacs--terminal-process nil)
       (signal (car err) (cdr err))))))

(defun claudemacs--terminal-ready-p ()
  "Return non-nil when the current buffer's terminal is initialized."
  (claudemacs--terminal-call :ready-p))

(defun claudemacs--terminal-live-p ()
  "Return non-nil when the current buffer's terminal process is live."
  (claudemacs--terminal-call :live-p))

(defun claudemacs--terminal-kill ()
  "Ask the current buffer's backend to terminate its process."
  (claudemacs--terminal-call :kill))

(defun claudemacs--terminal-send-string (string)
  "Send STRING verbatim to the current terminal."
  (claudemacs--terminal-call :send-string string))

(defun claudemacs--terminal-paste-string (string)
  "Send STRING as a bracketed paste when supported by the terminal."
  (claudemacs--terminal-call :paste-string string))

(defun claudemacs--terminal-send-key (key)
  "Send semantic KEY to the current terminal.
KEY is one of `return', `meta-return', `left', or `escape'."
  (unless (memq key '(return meta-return left escape))
    (error "Unsupported Claudemacs terminal key: %S" key))
  (claudemacs--terminal-call :send-key key))

(defun claudemacs--terminal-setup-buffer (bell-function)
  "Configure the current terminal buffer to call BELL-FUNCTION on BEL."
  (claudemacs--terminal-call :setup-buffer bell-function))

(defun claudemacs--terminal-setup-notifications (handler)
  "Ask the current backend to report tool notifications to HANDLER.

HANDLER is called with a BODY string and an optional TITLE string whenever
the tool emits a desktop-notification escape sequence (OSC 9 or OSC 777).
`:setup-notifications' is optional: backends that cannot observe those
sequences simply omit it, and this returns nil for them."
  (let* ((backend (claudemacs--terminal-current-backend))
         (operations (claudemacs--terminal-ensure-backend backend))
         (function (plist-get operations :setup-notifications)))
    (when function
      (funcall function handler))))

;;;; Desktop Notification Escape Sequences

;; Terminal programs that want to raise a desktop notification write it as an
;; OSC sequence rather than as a plain BEL: OSC 9 carries a body only
;; (iTerm2 style), OSC 777 carries a title and a body (urxvt style).  Codex
;; uses OSC 9 when its `tui.notification_method' is set to "osc9", and the
;; body is the text it would have shown in a desktop notification.  Backends
;; that see raw terminal output use the helpers below to pull those
;; sequences out of the stream.

(defconst claudemacs--terminal-osc-notification-regexp
  "\e\\]\\(9\\|777\\);\\([^\a\e]*\\)\\(?:\a\\|\e\\\\\\)"
  "Regexp matching an OSC 9 or OSC 777 desktop notification sequence.")

(defconst claudemacs--terminal-osc-carry-limit 4096
  "Maximum number of bytes of a partial OSC sequence to keep between chunks.
Terminal output arrives in arbitrary chunks, so a notification sequence can
be split across two of them.  Anything longer than this is not a
notification Claudemacs can use, and is dropped instead of buffered.")

(defun claudemacs--terminal-decode-osc-text (text)
  "Decode TEXT from raw terminal bytes to a string.
Process output reaches backends undecoded, but callers may also pass an
already-decoded string."
  (if (multibyte-string-p text)
      text
    (decode-coding-string text 'utf-8 t)))

(defun claudemacs--terminal-osc-carry (text)
  "Return the tail of TEXT that may be the start of an unterminated OSC.
TEXT is the part of a chunk left over after every complete notification
sequence was consumed."
  (let* ((start (claudemacs--terminal-last-osc-introducer text))
         (carry (cond
                 ((and start
                       (not (string-match-p "[\a]" (substring text (+ start 2))))
                       (not (string-match-p "\e\\\\" (substring text (+ start 2)))))
                  (substring text start))
                 ((string-suffix-p "\e" text) "\e")
                 (t ""))))
    (if (> (length carry) claudemacs--terminal-osc-carry-limit) "" carry)))

(defun claudemacs--terminal-last-osc-introducer (text)
  "Return the position of the last OSC introducer (ESC ]) in TEXT, or nil."
  (let ((position nil)
        (search 0))
    (while (setq search (string-match-p "\e\\]" text search))
      (setq position search)
      (setq search (1+ search)))
    position))

(defun claudemacs--terminal-parse-osc-notifications (text)
  "Extract desktop notifications from terminal output TEXT.

Return a cons of (NOTIFICATIONS . CARRY).  NOTIFICATIONS is a list of
\(TITLE . BODY) conses in the order they appeared, where TITLE is nil for
the title-less OSC 9 form.  CARRY is the trailing text of a sequence that
was cut in half by the chunk boundary; prepend it to the next chunk."
  (let ((position 0)
        (notifications nil))
    (while (string-match claudemacs--terminal-osc-notification-regexp text position)
      (let ((command (match-string 1 text))
            (payload (match-string 2 text)))
        (setq position (match-end 0))
        (when-let* ((notification
                     (claudemacs--terminal-osc-notification command payload)))
          (push notification notifications))))
    (cons (nreverse notifications)
          (claudemacs--terminal-osc-carry (substring text position)))))

(defun claudemacs--terminal-osc-notification (command payload)
  "Return a (TITLE . BODY) notification for OSC COMMAND with PAYLOAD, or nil."
  (cond
   ;; OSC 9 ; <body>.  ConEmu reuses OSC 9 for several terminal controls.
   ((equal command "9")
    (unless (or (string-match-p "\\`[1234];" payload)
                (string-prefix-p "5" payload)
                (string-prefix-p "9;" payload)
                (string= payload "10")
                (string-match-p "\\`10;[0-3]" payload)
                (string-prefix-p "12" payload))
      (let ((body (claudemacs--terminal-decode-osc-text payload)))
        (unless (string-empty-p body)
          (cons nil body)))))
   ;; OSC 777 ; notify ; <title> ; <body>.
   ((and (equal command "777")
         (string-prefix-p "notify;" payload))
    (let* ((rest (substring payload (length "notify;")))
           (separator (cl-position ?\; rest))
           (title (claudemacs--terminal-decode-osc-text
                   (if separator (substring rest 0 separator) rest)))
           (body (claudemacs--terminal-decode-osc-text
                  (if separator (substring rest (1+ separator)) ""))))
      (cond
       ((not (string-empty-p body)) (cons title body))
       ((not (string-empty-p title)) (cons title "")))))))

(defun claudemacs--terminal-setup-faces ()
  "Apply backend-specific face configuration in the current buffer."
  (claudemacs--terminal-call :setup-faces))

(defun claudemacs--terminal-post-display (buffer)
  "Let the current backend adjust BUFFER after it is displayed."
  (claudemacs--terminal-call :post-display buffer))

(defun claudemacs--terminal-force-redraw ()
  "Force the current terminal to redraw."
  (claudemacs--terminal-call :force-redraw))

(defun claudemacs--terminal-unstick ()
  "Apply the current backend's recovery procedure for a stuck display."
  (claudemacs--terminal-call :unstick))

(defun claudemacs--terminal-setup-loaded-backends ()
  "Run global setup for every backend that is already registered."
  (dolist (entry claudemacs--terminal-backends)
    (claudemacs--terminal-ensure-backend (car entry))))

(defun claudemacs--terminal-teardown-loaded-backends ()
  "Run global teardown for initialized backends."
  (dolist (backend claudemacs--terminal-initialized-backends)
    (when-let* ((operations (claudemacs--terminal-backend-operations backend))
                (teardown (plist-get operations :global-teardown)))
      (funcall teardown)))
  (setq claudemacs--terminal-initialized-backends nil))

(provide 'claudemacs-terminal)
;;; claudemacs-terminal.el ends here
