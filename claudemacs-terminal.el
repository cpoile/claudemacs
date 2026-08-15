;;; claudemacs-terminal.el --- Terminal backend abstraction for Claudemacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file defines the backend contract used by Claudemacs.  Backend
;; implementations register operations and are loaded lazily, so installing
;; Claudemacs does not force a particular terminal package to be present.

;;; Code:

(require 'cl-lib)

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
      (when-let ((setup (plist-get operations :global-setup)))
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
