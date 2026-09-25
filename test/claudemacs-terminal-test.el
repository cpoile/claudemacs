;;; claudemacs-terminal-test.el --- Tests for terminal backends -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests exercise the terminal adapter without loading Eat, Ghostel, or
;; either backend's native module.  A small in-process backend is registered
;; below and records the operations dispatched through the public adapter.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the package from the checkout when this file is run in batch mode.
(add-to-list 'load-path
             (file-name-directory
              (directory-file-name (file-name-directory load-file-name))))
(require 'claudemacs-terminal)
(require 'claudemacs)

;;; Fake backend

(defvar claudemacs-terminal-test--events nil
  "Events recorded by the fake terminal backend.")

(defvar claudemacs-terminal-test--global-setup-count 0
  "Number of times the fake backend global setup ran.")

(defvar claudemacs-terminal-test--global-teardown-count 0
  "Number of times the fake backend global teardown ran.")

(defvar-local claudemacs-terminal-test--ready nil
  "Whether the fake terminal in the current buffer is initialized.")

(defvar-local claudemacs-terminal-test--live nil
  "Whether the fake terminal process in the current buffer is live.")

(defvar-local claudemacs-terminal-test--bell-function nil
  "Bell callback configured by the fake backend.")

(defvar-local claudemacs-terminal-test--faces-set nil
  "Whether fake backend faces were configured.")

(defun claudemacs-terminal-test--record (event &rest data)
  "Record EVENT and DATA for assertions." 
  (push (cons event data) claudemacs-terminal-test--events))

(defun claudemacs-terminal-test--start (buffer program switches)
  "Start a fake terminal for BUFFER running PROGRAM with SWITCHES." 
  (with-current-buffer buffer
    ;; Real terminal backends establish their major mode during start, which
    ;; clears ordinary buffer-local state owned by the facade.
    (fundamental-mode)
    (setq-local claudemacs-terminal-test--ready t)
    (setq-local claudemacs-terminal-test--live t))
  (claudemacs-terminal-test--record :start program switches)
  ;; The adapter stores this lifecycle value in
  ;; `claudemacs--terminal-process'; it deliberately need not be an OS
  ;; process for this batch-only contract test.
  (list :fake-process program switches))

(defun claudemacs-terminal-test--ready-p ()
  "Return whether the fake terminal is initialized." 
  claudemacs-terminal-test--ready)

(defun claudemacs-terminal-test--live-p ()
  "Return whether the fake terminal process is live." 
  claudemacs-terminal-test--live)

(defun claudemacs-terminal-test--kill ()
  "Stop the fake terminal process." 
  (setq claudemacs-terminal-test--live nil)
  (claudemacs-terminal-test--record :kill))

(defun claudemacs-terminal-test--send-string (string)
  "Record STRING sent to the fake terminal." 
  (claudemacs-terminal-test--record :send-string string))

(defun claudemacs-terminal-test--paste-string (string)
  "Record STRING pasted into the fake terminal." 
  (claudemacs-terminal-test--record :paste-string string))

(defun claudemacs-terminal-test--send-key (key)
  "Record semantic KEY sent to the fake terminal." 
  (claudemacs-terminal-test--record :send-key key))

(defun claudemacs-terminal-test--setup-buffer (bell-function)
  "Configure BELL-FUNCTION for the fake terminal." 
  (setq claudemacs-terminal-test--bell-function bell-function)
  (claudemacs-terminal-test--record :setup-buffer bell-function))

(defun claudemacs-terminal-test--setup-faces ()
  "Mark fake backend faces as configured." 
  (setq claudemacs-terminal-test--faces-set t)
  (claudemacs-terminal-test--record :setup-faces))

(defun claudemacs-terminal-test--post-display (buffer)
  "Record post-display processing for BUFFER." 
  (claudemacs-terminal-test--record :post-display buffer))

(defun claudemacs-terminal-test--force-redraw ()
  "Record a fake terminal redraw." 
  (claudemacs-terminal-test--record :force-redraw))

(defun claudemacs-terminal-test--unstick ()
  "Record a fake terminal recovery operation." 
  (claudemacs-terminal-test--record :unstick))

(defun claudemacs-terminal-test--global-setup ()
  "Record fake backend global setup." 
  (setq claudemacs-terminal-test--global-setup-count
        (1+ claudemacs-terminal-test--global-setup-count)))

(defun claudemacs-terminal-test--global-teardown ()
  "Record fake backend global teardown." 
  (setq claudemacs-terminal-test--global-teardown-count
        (1+ claudemacs-terminal-test--global-teardown-count)))

(claudemacs--terminal-register-backend
 'claudemacs-terminal-test-fake
 :start #'claudemacs-terminal-test--start
 :ready-p #'claudemacs-terminal-test--ready-p
 :live-p #'claudemacs-terminal-test--live-p
 :kill #'claudemacs-terminal-test--kill
 :send-string #'claudemacs-terminal-test--send-string
 :paste-string #'claudemacs-terminal-test--paste-string
 :send-key #'claudemacs-terminal-test--send-key
 :setup-buffer #'claudemacs-terminal-test--setup-buffer
 :setup-faces #'claudemacs-terminal-test--setup-faces
 :post-display #'claudemacs-terminal-test--post-display
 :force-redraw #'claudemacs-terminal-test--force-redraw
 :unstick #'claudemacs-terminal-test--unstick
 :global-setup #'claudemacs-terminal-test--global-setup
 :global-teardown #'claudemacs-terminal-test--global-teardown)

(defmacro claudemacs-terminal-test--with-buffer (&rest body)
  "Run BODY in a temporary buffer with a fake backend session." 
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq claudemacs-terminal-test--events nil)
     (claudemacs--terminal-start (current-buffer)
                                 'claudemacs-terminal-test-fake
                                 "fake-cli"
                                 '("--test"))
     ,@body))

(defmacro claudemacs-terminal-test--with-stubbed-adapter
    (feature filename &rest body)
  "Load terminal adapter FILENAME with FEATURE provided, then run BODY.

The adapter is evaluated against a private copy of the backend registry while
FEATURE is accepted by a temporary require stub.  This keeps tests independent
of optional terminal packages and prevents their registration or fake package
loading from leaking into another ERT test."
  (declare (indent 2) (debug t))
  `(let ((original-require (symbol-function 'require))
         (claudemacs--terminal-backends
          (copy-tree claudemacs--terminal-backends)))
     (cl-letf (((symbol-function 'require)
                (lambda (required-feature &optional filename noerror)
                  (if (eq required-feature ,feature)
                      t
                    (funcall original-require required-feature filename
                             noerror)))))
       (load ,filename nil nil t)
       ,@body)))

;;; Distinct lightweight backends for ownership/coexistence coverage

(defvar claudemacs-terminal-test--mixed-events nil
  "Events recorded by the two mixed-backend fakes.")

(defvar-local claudemacs-terminal-test--mixed-ready nil
  "Whether the mixed-backend fake in the current buffer is initialized.")

(defvar-local claudemacs-terminal-test--mixed-live nil
  "Whether the mixed-backend fake in the current buffer is live.")

(defun claudemacs-terminal-test--mixed-record (backend event &rest data)
  "Record EVENT and DATA for mixed BACKEND."
  (push (append (list backend event) data)
        claudemacs-terminal-test--mixed-events))

(defun claudemacs-terminal-test--mixed-start
    (backend buffer program switches)
  "Start mixed BACKEND in BUFFER with PROGRAM and SWITCHES."
  (with-current-buffer buffer
    ;; Match the mode-changing boundary of real terminal backends.  The
    ;; facade must restore ownership after this call returns.
    (fundamental-mode)
    (setq-local claudemacs-terminal-test--mixed-ready t)
    (setq-local claudemacs-terminal-test--mixed-live t))
  (claudemacs-terminal-test--mixed-record
   backend :start program switches)
  (list :mixed-process backend))

(defun claudemacs-terminal-test--mixed-start-a (buffer program switches)
  "Start the first mixed fake backend."
  (claudemacs-terminal-test--mixed-start
   'claudemacs-terminal-test-mixed-a buffer program switches))

(defun claudemacs-terminal-test--mixed-start-b (buffer program switches)
  "Start the second mixed fake backend."
  (claudemacs-terminal-test--mixed-start
   'claudemacs-terminal-test-mixed-b buffer program switches))

(defun claudemacs-terminal-test--mixed-ready-p ()
  "Return whether the mixed fake in the current buffer is initialized."
  claudemacs-terminal-test--mixed-ready)

(defun claudemacs-terminal-test--mixed-live-p ()
  "Return whether the mixed-backend fake in the current buffer is live."
  claudemacs-terminal-test--mixed-live)

(defun claudemacs-terminal-test--mixed-kill (backend)
  "Kill the mixed BACKEND in the current buffer."
  (setq claudemacs-terminal-test--mixed-live nil)
  (claudemacs-terminal-test--mixed-record backend :kill))

(defun claudemacs-terminal-test--mixed-kill-a ()
  "Kill the first mixed fake backend."
  (claudemacs-terminal-test--mixed-kill
   'claudemacs-terminal-test-mixed-a))

(defun claudemacs-terminal-test--mixed-kill-b ()
  "Kill the second mixed fake backend."
  (claudemacs-terminal-test--mixed-kill
   'claudemacs-terminal-test-mixed-b))

(defun claudemacs-terminal-test--mixed-send-string (backend string)
  "Send STRING through mixed BACKEND."
  (claudemacs-terminal-test--mixed-record backend :send-string string))

(defun claudemacs-terminal-test--mixed-send-string-a (string)
  "Send STRING through the first mixed fake backend."
  (claudemacs-terminal-test--mixed-send-string
   'claudemacs-terminal-test-mixed-a string))

(defun claudemacs-terminal-test--mixed-send-string-b (string)
  "Send STRING through the second mixed fake backend."
  (claudemacs-terminal-test--mixed-send-string
   'claudemacs-terminal-test-mixed-b string))

(defun claudemacs-terminal-test--mixed-noop (&rest _arguments)
  "Implement an unused operation for the mixed fake backends."
  nil)

(dolist (backend-spec
         '((claudemacs-terminal-test-mixed-a
            claudemacs-terminal-test--mixed-start-a
            claudemacs-terminal-test--mixed-kill-a
            claudemacs-terminal-test--mixed-send-string-a)
           (claudemacs-terminal-test-mixed-b
            claudemacs-terminal-test--mixed-start-b
            claudemacs-terminal-test--mixed-kill-b
            claudemacs-terminal-test--mixed-send-string-b)))
  (claudemacs--terminal-register-backend
   (nth 0 backend-spec)
   :start (nth 1 backend-spec)
   :ready-p #'claudemacs-terminal-test--mixed-ready-p
   :live-p #'claudemacs-terminal-test--mixed-live-p
   :kill (nth 2 backend-spec)
   :send-string (nth 3 backend-spec)
   :paste-string #'claudemacs-terminal-test--mixed-noop
   :send-key #'claudemacs-terminal-test--mixed-noop
   :setup-buffer #'claudemacs-terminal-test--mixed-noop
   :setup-faces #'claudemacs-terminal-test--mixed-noop
   :post-display #'claudemacs-terminal-test--mixed-noop
   :force-redraw #'claudemacs-terminal-test--mixed-noop
   :unstick #'claudemacs-terminal-test--mixed-noop))

;;; Adapter behavior

(ert-deftest claudemacs-terminal-test-default-backend-prefers-ghostel ()
  "The default prefers Ghostel when it is available on `load-path'." 
  :tags '(:unit :terminal-backend)
  (should (eq claudemacs-terminal-backend
              (if (locate-library "ghostel") 'ghostel 'eat))))

(ert-deftest claudemacs-terminal-test-ghostel-key-bindings-survive-map-changes ()
  "Ghostel key overrides survive map replacement and option changes."
  :tags '(:unit :terminal-backend :ghostel)
  (with-temp-buffer
    (rename-buffer "*claudemacs:ghostel-key-test*" t)
    (setq-local claudemacs--terminal-backend 'ghostel)
    (setq-local emulation-mode-map-alists nil)
    (let ((claudemacs-m-return-is-submit t)
          (claudemacs-shift-return-newline t)
          (initial-map (make-sparse-keymap)))
      ;; Sentinel bindings make stale overrides observable when both options
      ;; are subsequently disabled.
      (define-key initial-map (kbd "C-g") #'ignore)
      (define-key initial-map (kbd "<return>") #'ignore)
      (define-key initial-map (kbd "<M-return>") #'ignore)
      (define-key initial-map (kbd "<S-return>") #'ignore)
      (use-local-map initial-map)
      (claudemacs--setup-buffer-keymap)
      (dolist (binding '(("C-g" . claudemacs--send-escape)
                         ("<return>" . claudemacs--meta-ret-key)
                         ("<M-return>" . claudemacs--ret-key)
                         ("<S-return>" . claudemacs--meta-ret-key)))
        (should (eq (key-binding (kbd (car binding))) (cdr binding))))
      ;; Ghostel can replace its local map when it changes input modes.
      (let ((replacement-map (make-sparse-keymap)))
        (dolist (key '("C-g" "<return>" "<M-return>" "<S-return>"))
          (define-key replacement-map (kbd key) #'ignore))
        (use-local-map replacement-map))
      (dolist (binding '(("C-g" . claudemacs--send-escape)
                         ("<return>" . claudemacs--meta-ret-key)
                         ("<M-return>" . claudemacs--ret-key)
                         ("<S-return>" . claudemacs--meta-ret-key)))
        (should (eq (key-binding (kbd (car binding))) (cdr binding))))
      ;; Reconfigure the same session with both optional overrides disabled.
      ;; The emulation map must remove old return bindings as well as
        ;; retaining C-g across a later Ghostel map replacement.
      (let ((claudemacs-m-return-is-submit nil)
            (claudemacs-shift-return-newline nil))
        (claudemacs--setup-buffer-keymap)
        (dolist (binding '(("C-g" . claudemacs--send-escape)
                           ("<return>" . ignore)
                           ("<M-return>" . ignore)
                           ("<S-return>" . ignore)))
          (should (eq (key-binding (kbd (car binding))) (cdr binding))))
        (let ((replacement-map (make-sparse-keymap)))
          (dolist (key '("C-g" "<return>" "<M-return>" "<S-return>"))
            (define-key replacement-map (kbd key) #'ignore))
          (use-local-map replacement-map))
        (dolist (binding '(("C-g" . claudemacs--send-escape)
                           ("<return>" . ignore)
                           ("<M-return>" . ignore)
                           ("<S-return>" . ignore)))
          (should (eq (key-binding (kbd (car binding))) (cdr binding))))))))

(ert-deftest claudemacs-terminal-test-start-records-backend-and-process ()
  "Starting a session records its backend and lifecycle process." 
  :tags '(:unit :terminal-backend)
  (with-temp-buffer
    (claudemacs--terminal-start (current-buffer)
                                'claudemacs-terminal-test-fake
                                "fake-cli"
                                '("--one" "two"))
    (should (eq claudemacs--terminal-backend
               'claudemacs-terminal-test-fake))
    (should (equal claudemacs--terminal-process
                   '(:fake-process "fake-cli" ("--one" "two"))))
    (should (equal (car claudemacs-terminal-test--events)
                   '(:start "fake-cli" ("--one" "two"))))
    (should (claudemacs--terminal-ready-p))
    (should (claudemacs--terminal-live-p))))

(ert-deftest claudemacs-terminal-test-dispatches-current-buffer-operations ()
  "Current-buffer adapter calls dispatch to the registered backend." 
  :tags '(:unit :terminal-backend)
  (let ((bell (lambda () :bell)))
    (claudemacs-terminal-test--with-buffer
      (claudemacs--terminal-send-string "hello")
      (claudemacs--terminal-paste-string "paste me")
      (claudemacs--terminal-send-key 'return)
      (claudemacs--terminal-setup-buffer bell)
      (claudemacs--terminal-setup-faces)
      (claudemacs--terminal-post-display (current-buffer))
      (claudemacs--terminal-force-redraw)
      (claudemacs--terminal-unstick)
      (should (eq claudemacs-terminal-test--bell-function bell))
      (should claudemacs-terminal-test--faces-set)
      (should (equal
               (mapcar #'car claudemacs-terminal-test--events)
               '(:unstick :force-redraw :post-display :setup-faces
                 :setup-buffer :send-key :paste-string :send-string :start))))))

(ert-deftest claudemacs-terminal-test-rejects-unsupported-key ()
  "The adapter rejects semantic keys outside its contract." 
  :tags '(:unit :terminal-backend)
  (claudemacs-terminal-test--with-buffer
    (should-error (claudemacs--terminal-send-key 'tab))))

(ert-deftest claudemacs-terminal-test-kill-updates-live-state ()
  "Killing a session delegates to the backend and makes it non-live." 
  :tags '(:unit :terminal-backend)
  (claudemacs-terminal-test--with-buffer
    (should (claudemacs--terminal-live-p))
    (claudemacs--terminal-kill)
    (should-not (claudemacs--terminal-live-p))
    (should (equal (car claudemacs-terminal-test--events) '(:kill)))))

(ert-deftest claudemacs-terminal-test-mixed-backends-dispatch-by-session-owner ()
  "Existing sessions keep dispatching to their original backend."
  :tags '(:unit :terminal-backend)
  (let ((claudemacs-terminal-test--mixed-events nil)
        (buffer-a (generate-new-buffer "*claudemacs-terminal-mixed-a*"))
        (buffer-b (generate-new-buffer "*claudemacs-terminal-mixed-b*")))
    (unwind-protect
        (progn
          ;; The selector is consulted only while each new session starts.
          ;; Changing it afterwards must not redirect existing buffers.
          (let ((claudemacs-terminal-backend
                 'claudemacs-terminal-test-mixed-a))
            (claudemacs--terminal-start
             buffer-a claudemacs-terminal-backend "tool-a" nil))
          (let ((claudemacs-terminal-backend
                 'claudemacs-terminal-test-mixed-b))
            (claudemacs--terminal-start
             buffer-b claudemacs-terminal-backend "tool-b" nil))
          (setq claudemacs-terminal-backend
                'claudemacs-terminal-test-mixed-a)
          (with-current-buffer buffer-a
            (claudemacs--terminal-send-string "from-a")
            (claudemacs--terminal-kill))
          (with-current-buffer buffer-b
            (claudemacs--terminal-send-string "from-b")
            (claudemacs--terminal-kill))
          (should (member
                   '(claudemacs-terminal-test-mixed-a :send-string "from-a")
                   claudemacs-terminal-test--mixed-events))
          (should (member
                   '(claudemacs-terminal-test-mixed-a :kill)
                   claudemacs-terminal-test--mixed-events))
          (should (member
                   '(claudemacs-terminal-test-mixed-b :send-string "from-b")
                   claudemacs-terminal-test--mixed-events))
          (should (member
                   '(claudemacs-terminal-test-mixed-b :kill)
                   claudemacs-terminal-test--mixed-events)))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b)
        (kill-buffer buffer-b)))))

(ert-deftest claudemacs-terminal-test-eat-codex-cursor-configuration-is-stable ()
  "Eat copies Codex cursor shapes and reapplies the active cursor state."
  :tags '(:unit :terminal-backend :eat)
  (claudemacs-terminal-test--with-stubbed-adapter
   'eat "claudemacs-terminal-eat.el"
   (let (set-cursor-arguments)
     (with-temp-buffer
       (setq-local claudemacs--tool 'codex)
       (setq-local eat-terminal :fake-eat-terminal)
       (setq-local eat-default-cursor-type '((box . red)))
       (setq-local eat-vertical-bar-cursor-type '((bar . blue)))
       (setq-local eat-horizontal-bar-cursor-type '((bar . green)))
       (setq-local eat-very-visible-cursor-type 'old-box)
       (setq-local eat-very-visible-vertical-bar-cursor-type 'old-vertical)
       (setq-local eat-very-visible-horizontal-bar-cursor-type 'old-horizontal)
       (cl-letf (((symbol-function 'eat-term-parameter)
                  (lambda (terminal parameter)
                    (should (eq terminal eat-terminal))
                    (should (eq parameter 'set-cursor-function))
                    (lambda (current-terminal current-state)
                      (setq set-cursor-arguments
                            (list current-terminal current-state)))))
                 ((symbol-function 'eat-term-cursor-type)
                  (lambda (terminal)
                    (should (eq terminal eat-terminal))
                    'current-cursor-state)))
         (claudemacs--eat-disable-codex-cursor-blink)
         (should (equal eat-very-visible-cursor-type
                        eat-default-cursor-type))
         (should (equal eat-very-visible-vertical-bar-cursor-type
                        eat-vertical-bar-cursor-type))
         (should (equal eat-very-visible-horizontal-bar-cursor-type
                        eat-horizontal-bar-cursor-type))
         (should-not (eq eat-very-visible-cursor-type
                         eat-default-cursor-type))
         (should-not (eq eat-very-visible-vertical-bar-cursor-type
                         eat-vertical-bar-cursor-type))
         (should-not (eq eat-very-visible-horizontal-bar-cursor-type
                         eat-horizontal-bar-cursor-type))
         (should (equal set-cursor-arguments
                        (list eat-terminal 'current-cursor-state))))))))

(ert-deftest claudemacs-terminal-test-eat-defers-notification-delivery ()
  "Eat's process filter queues notifications instead of displaying them."
  :tags '(:unit :terminal-backend :eat)
  (claudemacs-terminal-test--with-stubbed-adapter
      'eat "claudemacs-terminal-eat.el"
    (let ((buffer (generate-new-buffer " *claudemacs-eat-notification-test*"))
          scheduled
          delivered)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local claudemacs--eat-notification-handler
                          (lambda (body title)
                            (setq delivered
                                  (list body title (current-buffer)))))
              (cl-letf (((symbol-function 'process-buffer)
                         (lambda (_process) buffer))
                        ((symbol-function 'run-at-time)
                         (lambda (delay repeat function &rest arguments)
                           (setq scheduled
                                 (list delay repeat function arguments)))))
                (claudemacs--eat-scan-notifications
                 :fake-process "\e]9;turn complete\a")))
            (should-not delivered)
            (pcase-let ((`(,delay ,repeat ,function ,arguments) scheduled))
              (should (= delay 0))
              (should-not repeat)
              (apply function arguments))
            (should (equal delivered
                           (list "turn complete" nil buffer))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest claudemacs-terminal-test-ghostel-adapter-captures-lifecycle ()
  "Ghostel registration and lifecycle operations use its public API."
  :tags '(:unit :terminal-backend :ghostel)
  (let ((fake-process (list :fake-ghostel-process))
        (process-live t)
        exec-arguments
        deleted-process)
    (claudemacs-terminal-test--with-stubbed-adapter
     'ghostel "claudemacs-terminal-ghostel.el"
     (should (claudemacs--terminal-backend-operations 'ghostel))
     (cl-letf (((symbol-function 'ghostel-exec)
                (lambda (buffer program switches &optional _identity)
                  (setq exec-arguments (list buffer program switches))
                  (with-current-buffer buffer
                    (setq-local major-mode 'ghostel-mode)
                    (setq-local ghostel-kill-buffer-on-exit t))
                  fake-process))
               ((symbol-function 'processp)
                (lambda (process) (eq process fake-process)))
               ((symbol-function 'process-live-p)
                (lambda (process) (and (eq process fake-process)
                                       process-live)))
               ((symbol-function 'delete-process)
                (lambda (process)
                  (setq deleted-process process
                        process-live nil))))
       (with-temp-buffer
         (let ((claudemacs-ghostel-query-before-killing t))
           (should (eq (claudemacs--terminal-start
                        (current-buffer) 'ghostel "ghostel-cli" '("--test"))
                       fake-process)))
         (should (equal exec-arguments
                        (list (current-buffer) "ghostel-cli" '("--test"))))
         (should (eq claudemacs--terminal-process fake-process))
         (should (claudemacs--terminal-ready-p))
         (should (claudemacs--terminal-live-p))
         (claudemacs--terminal-kill)
         (should (eq deleted-process fake-process))
         (should-not (claudemacs--terminal-live-p))
         (should-not claudemacs--terminal-process)
         (should-not ghostel-kill-buffer-on-exit))))))

(ert-deftest claudemacs-terminal-test-ghostel-adapter-forwards-terminal-operations ()
  "Ghostel input, bell, face, redraw, and recovery operations are forwarded."
  :tags '(:unit :terminal-backend :ghostel)
  (let (sent-strings pasted-strings sent-keys
        (redraw-count 0)
        (face-remap-arguments nil)
        (bell (lambda (&rest _arguments) :bell)))
    (claudemacs-terminal-test--with-stubbed-adapter
     'ghostel "claudemacs-terminal-ghostel.el"
     (require 'face-remap)
     (cl-letf (((symbol-function 'ghostel-send-string)
                (lambda (string) (push string sent-strings)))
               ((symbol-function 'ghostel-paste-string)
                (lambda (string) (push string pasted-strings)))
               ((symbol-function 'ghostel-send-key)
                (lambda (key &optional modifiers)
                  (push (list key modifiers) sent-keys)))
               ((symbol-function 'ghostel-force-redraw)
                (lambda () (setq redraw-count (1+ redraw-count))))
               ((symbol-function 'face-remap-add-relative)
                (lambda (&rest arguments)
                  (push arguments face-remap-arguments)
                  :fake-face-cookie)))
       (with-temp-buffer
         (setq-local claudemacs--terminal-backend 'ghostel)
         (claudemacs--terminal-send-string "hello")
         (claudemacs--terminal-paste-string "pasted")
         (claudemacs--terminal-send-key 'return)
         (claudemacs--terminal-send-key 'meta-return)
         (claudemacs--terminal-send-key 'left)
         (claudemacs--terminal-send-key 'escape)
         (claudemacs--terminal-setup-buffer bell)
         (claudemacs--terminal-setup-faces)
         (claudemacs--terminal-post-display (current-buffer))
         (claudemacs--terminal-force-redraw)
         (claudemacs--terminal-unstick)
         (should (equal sent-strings '("hello")))
         (should (equal pasted-strings '("pasted")))
         (should (equal sent-keys
                        '(("escape" "")
                          ("left" "")
                          ("return" "meta")
                          ("return" ""))))
         (should (eq ring-bell-function bell))
         (should face-remap-arguments)
         (should (= redraw-count 3)))))))

(ert-deftest claudemacs-terminal-test-ghostel-kill-query-is-buffer-local ()
  "Ghostel kill-query values apply to sessions without touching other buffers."
  :tags '(:unit :terminal-backend :ghostel)
  (let ((unrelated-buffer
         (generate-new-buffer "*claudemacs-terminal-unrelated-ghostel*")))
    (unwind-protect
        (progn
          (with-current-buffer unrelated-buffer
            (setq-local ghostel-query-before-killing :unrelated))
          (dolist (setting '(t nil auto))
            (let ((session-buffer
                   (generate-new-buffer "*claudemacs-terminal-ghostel*")))
              (unwind-protect
                  (let ((claudemacs-ghostel-query-before-killing setting))
                    (claudemacs-terminal-test--with-stubbed-adapter
                     'ghostel "claudemacs-terminal-ghostel.el"
                     (cl-letf (((symbol-function 'ghostel-exec)
                                (lambda (_buffer _program _switches
                                         &optional _identity)
                                  (list :fake-process))))
                       (claudemacs--terminal-start
                        session-buffer 'ghostel "ghostel-cli" nil)
                       (with-current-buffer session-buffer
                         (should (eq ghostel-query-before-killing setting))))))
                (when (buffer-live-p session-buffer)
                  (kill-buffer session-buffer)))))
          (with-current-buffer unrelated-buffer
            (should (eq ghostel-query-before-killing :unrelated))))
      (when (buffer-live-p unrelated-buffer)
        (kill-buffer unrelated-buffer)))))

;;; Selection and lifecycle errors

(ert-deftest claudemacs-terminal-test-no-automatic-fallback-for-missing-backend ()
  "A missing selected backend signals an error instead of falling back." 
  :tags '(:unit :terminal-backend)
  (with-temp-buffer
    (should-error
     (claudemacs--terminal-start (current-buffer)
                                 'claudemacs-terminal-test-not-installed
                                 "fake-cli"
                                 nil)
     :type 'user-error)
    (should-not claudemacs--terminal-backend)
    (should-not claudemacs--terminal-process)))

(ert-deftest claudemacs-terminal-test-invalid-backend-name-errors ()
  "A non-symbol backend selection reports a user-facing error." 
  :tags '(:unit :terminal-backend)
  (with-temp-buffer
    (should-error
     (claudemacs--terminal-start (current-buffer) "eat" "fake-cli" nil)
     :type 'user-error)))

(ert-deftest claudemacs-terminal-test-incomplete-backend-errors-on-use ()
  "A registered backend missing required operations cannot start." 
  :tags '(:unit :terminal-backend)
  (let ((backend 'claudemacs-terminal-test-incomplete))
    (claudemacs--terminal-register-backend backend :start #'ignore)
    (with-temp-buffer
      (should-error
       (claudemacs--terminal-start (current-buffer) backend "fake-cli" nil))
      (should-not claudemacs--terminal-backend)
      (should-not claudemacs--terminal-process))))

(ert-deftest claudemacs-terminal-test-global-setup-is-idempotent-and-teardown-clears-state ()
  "Loaded backend setup runs once and teardown resets initialization state." 
  :tags '(:unit :terminal-backend)
  (let ((old-initialized claudemacs--terminal-initialized-backends)
        (old-setup-count claudemacs-terminal-test--global-setup-count)
        (old-teardown-count claudemacs-terminal-test--global-teardown-count))
    (unwind-protect
        (progn
          (setq claudemacs--terminal-initialized-backends nil)
          (setq claudemacs-terminal-test--global-setup-count 0)
          (setq claudemacs-terminal-test--global-teardown-count 0)
          (claudemacs--terminal-ensure-backend
           'claudemacs-terminal-test-fake)
          (claudemacs--terminal-ensure-backend
           'claudemacs-terminal-test-fake)
          (should (= claudemacs-terminal-test--global-setup-count 1))
          (claudemacs--terminal-teardown-loaded-backends)
          (should (= claudemacs-terminal-test--global-teardown-count 1))
          (should-not claudemacs--terminal-initialized-backends))
      (setq claudemacs--terminal-initialized-backends old-initialized)
      (setq claudemacs-terminal-test--global-setup-count old-setup-count)
      (setq claudemacs-terminal-test--global-teardown-count old-teardown-count))))


;;; Desktop notification escape sequences

(ert-deftest claudemacs-terminal-test-parses-osc-9-notification ()
  "An OSC 9 sequence yields its message with no title."
  :tags '(:unit :terminal-backend)
  (let ((result (claudemacs--terminal-parse-osc-notifications
                 "text\e]9;banana pancakes\amore text")))
    (should (equal (car result) '((nil . "banana pancakes"))))
    (should (equal (cdr result) ""))))

(ert-deftest claudemacs-terminal-test-parses-osc-777-notification ()
  "An OSC 777 sequence yields both its title and its message."
  :tags '(:unit :terminal-backend)
  (let ((result (claudemacs--terminal-parse-osc-notifications
                 "\e]777;notify;Codex;turn complete\e\\")))
    (should (equal (car result) '(("Codex" . "turn complete"))))))

(ert-deftest claudemacs-terminal-test-ignores-non-notification-sequences ()
  "Titles, progress reports, and empty messages are not notifications."
  :tags '(:unit :terminal-backend)
  (dolist (output '("\e]0;spinner | claudemacs\a"
                    "\e]9;1;500\a"
                    "\e]9;2;task\a"
                    "\e]9;3;value\a"
                    "\e]9;4;50\a"
                    "\e]9;5\a"
                    "\e]9;9;/tmp/project\e\\"
                    "\e]9;10\a"
                    "\e]9;10;0more\a"
                    "\e]9;10;3\a"
                    "\e]9;12anything\a"
                    "\e]9;\a"
                    "plain output with no escapes"))
    (should-not (car (claudemacs--terminal-parse-osc-notifications output)))))

(ert-deftest claudemacs-terminal-test-keeps-non-control-numeric-notifications ()
  "Numeric text outside ConEmu's OSC 9 grammar remains a notification."
  :tags '(:unit :terminal-backend)
  (dolist (body '("10;" "10;4" "10;abc" "42 tasks complete"))
    (should (equal (car (claudemacs--terminal-parse-osc-notifications
                         (concat "\e]9;" body "\a")))
                   (list (cons nil body))))))

(ert-deftest claudemacs-terminal-test-preserves-osc-777-title-and-body-shape ()
  "OSC 777 keeps title-only and body-only fields in their protocol positions."
  :tags '(:unit :terminal-backend)
  (should (equal (car (claudemacs--terminal-parse-osc-notifications
                       "\e]777;notify;Codex;\a"))
                 '(("Codex" . ""))))
  (should (equal (car (claudemacs--terminal-parse-osc-notifications
                       "\e]777;notify;;done\a"))
                 '(("" . "done"))))
  (should-not (car (claudemacs--terminal-parse-osc-notifications
                    "\e]777;notify;;\a"))))

(ert-deftest claudemacs-terminal-test-notification-survives-a-split-chunk ()
  "A sequence cut in half by a chunk boundary is carried into the next chunk."
  :tags '(:unit :terminal-backend)
  (let* ((first (claudemacs--terminal-parse-osc-notifications "out\e]9;banana "))
         (second (claudemacs--terminal-parse-osc-notifications
                  (concat (cdr first) "pancakes\a"))))
    (should-not (car first))
    (should (equal (car second) '((nil . "banana pancakes"))))
    (should (equal (cdr second) ""))))

(ert-deftest claudemacs-terminal-test-carry-does-not-grow-without-bound ()
  "Output that never terminates an OSC sequence is dropped, not buffered."
  :tags '(:unit :terminal-backend)
  (let ((result (claudemacs--terminal-parse-osc-notifications
                 (concat "\e]9;" (make-string 8192 ?x)))))
    (should (equal (cdr result) ""))))

(ert-deftest claudemacs-terminal-test-notification-setup-is-optional ()
  "Backends without a notification operation report that they have none."
  :tags '(:unit :terminal-backend)
  (claudemacs-terminal-test--with-buffer
    (should-not (claudemacs--terminal-setup-notifications #'ignore))))

(provide 'claudemacs-terminal-test)
;;; claudemacs-terminal-test.el ends here
