;;; claudemacs-session-list-test.el --- Tests for the session list -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;;
;; These tests describe the live-only session-list boundary.  They use
;; normalized rows rather than asserting private storage details: the list is
;; allowed to change its adapters while identity provenance, failure behavior,
;; and the user-visible contract remain stable.  History providers remain
;; covered where they support explicit resume selection, but history rows must
;; never enter the live-session table.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))
;; Load the working-tree module explicitly.  A stale ignored .elc can
;; otherwise hide concurrent implementation changes during this acceptance
;; suite.
(load (expand-file-name "../claudemacs-session-list.el"
                       (file-name-directory load-file-name))
      nil nil)
(require 'claudemacs)
;; `require' may resolve an older byte-compiled copy from a test harness
;; scratch directory.  Reload the working-tree lifecycle source explicitly so
;; startup/identity assertions exercise the same implementation as the other
;; session-list providers.
(load (expand-file-name "../claudemacs.el"
                       (file-name-directory load-file-name))
      nil nil t)

;;; Test helpers

(defun claudemacs-session-list-test--find-function (&rest names)
  "Return the first defined function in NAMES, or fail the current test.

The first name in each call is the intended session-list contract.  The
aliases keep these tests useful while the module is split/refactored during
the remediation and do not make a missing provider silently pass."
  (or (seq-find #'fboundp names)
      (ert-fail (format "Session-list contract is missing: %S" names))))

(defun claudemacs-session-list-test--call (names &rest args)
  "Call the first function in NAMES with ARGS."
  (apply (apply #'claudemacs-session-list-test--find-function names) args))

(defun claudemacs-session-list-test--rows (result)
  "Extract normalized rows from provider or refresh RESULT."
  (cond
   ((and (listp result) (plist-member result :rows))
    (plist-get result :rows))
   ((null result) nil)
   (t result)))

(defun claudemacs-session-list-test--warnings (result)
  "Extract provider warnings from RESULT."
  (or (and (listp result) (plist-get result :warnings))
      (and (boundp 'claudemacs-session-list-last-diagnostics)
           claudemacs-session-list-last-diagnostics)
      (and (boundp 'claudemacs--session-list-last-diagnostics)
           claudemacs--session-list-last-diagnostics)))

(defun claudemacs-session-list-test--reset-diagnostics ()
  "Clear provider diagnostics before an isolated provider assertion."
  (when (fboundp 'claudemacs--session-list--reset-diagnostics)
    (claudemacs--session-list--reset-diagnostics))
  (when (boundp 'claudemacs--session-list-last-diagnostics)
    (setq claudemacs--session-list-last-diagnostics nil))
  (when (boundp 'claudemacs-session-list-last-diagnostics)
    (setq claudemacs-session-list-last-diagnostics nil)))

(defun claudemacs-session-list-test--row-id (row)
  "Read the normalized identity value from ROW."
  (or (plist-get row :session-id)
      (plist-get row :id)
      (plist-get row :internal-id)))

(defun claudemacs-session-list-test--row-state (row)
  "Read ROW's normalized state as a lowercase string."
  (let ((state (plist-get row :state)))
    (cond
     ((symbolp state) (downcase (symbol-name state)))
     ((stringp state) (downcase state))
     (t state))))

(defun claudemacs-session-list-test--row-identity (row)
  "Read ROW's identity provenance."
  (let ((identity (plist-get row :identity)))
    (if (symbolp identity) identity (and identity (intern identity)))))

(defun claudemacs-session-list-test--make-row (&rest properties)
  "Make a normalized test row from PROPERTIES."
  (let ((row (list :tool 'claude
                   :session-id nil
                   :identity 'unknown
                   :workspace nil
                   :instance nil
                   :cwd "/tmp/project"
                   :updated-at nil
                   :description nil
                   :state 'history
                   :buffer nil
                   :source 'claude-history)))
    (while properties
      (setq row (plist-put row (pop properties) (pop properties))))
    (unless (plist-get row :key)
      (setq row
            (plist-put row :key
                       (if (and (plist-get row :tool)
                                (plist-get row :session-id))
                           (format "%s:%s" (plist-get row :tool)
                                   (plist-get row :session-id))
                         (format "row:%x" (sxhash row))))))
    row))

(defun claudemacs-session-list-test--make-live-row (&rest properties)
  "Make a normalized live-buffer test row from PROPERTIES."
  (apply #'claudemacs-session-list-test--make-row
         :state 'live :source 'live-buffer properties))

(defmacro claudemacs-session-list-test--with-temp-directory (var &rest body)
  "Bind VAR to a temporary directory while evaluating BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "claudemacs-session-list-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun claudemacs-session-list-test--write (file content)
  "Write CONTENT to FILE, creating its parent directory."
  (make-directory (file-name-directory file) t)
  (write-region content nil file nil 'silent)
  file)

(defun claudemacs-session-list-test--session-buffer
    (name cwd tool &optional id identity live)
  "Create a fake Claudemacs buffer with the normalized runtime fields."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (setq-local claudemacs--cwd cwd)
      (setq-local claudemacs--tool tool)
      ;; The old Claude-only variable remains set for compatibility tests;
      ;; the remediation must use the tool-neutral field when available.
      (setq-local claudemacs--claude-session-uuid
                    (and (eq tool 'claude) id))
      (setq-local claudemacs--session-uuid id)
      (setq-local claudemacs--authoritative-session-id id)
      (setq-local claudemacs--session-id id)
      (setq-local claudemacs--tracked-session-id id)
      (setq-local claudemacs--codex-session-id
                    (and (eq tool 'codex) id))
      (setq-local claudemacs--session-identity identity)
      (setq-local claudemacs--session-id-provenance identity)
      (setq-local claudemacs--session-live live)
      (setq-local claudemacs--terminal-backend 'test))
    buffer))

(defun claudemacs-session-list-test--kill-buffers (&rest buffers)
  "Kill live BUFFERS, ignoring already-dead buffers."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun claudemacs-session-list-test--result-id-list (result)
  "Return normalized IDs from RESULT, preserving unknown entries."
  (mapcar #'claudemacs-session-list-test--row-id
          (claudemacs-session-list-test--rows result)))

(ert-deftest claudemacs-session-list-test-json-parser-supports-emacs-26-fallback ()
  "Provider JSON adapters retain validation semantics without native JSON APIs."
  :tags '(:unit :session-list :compatibility)
  (cl-letf (((symbol-function 'json-parse-buffer) nil)
            ((symbol-function 'json-parse-string) nil))
    (let ((null-value (make-symbol "test-json-null")))
      (with-temp-buffer
        (insert "{\"entries\":[]}")
        (let ((object (claudemacs--session-list--parse-json-buffer
                       'hash-table 'list null-value nil)))
          (should (hash-table-p object))
          (should (claudemacs--session-list--json-member-p object 'entries))
          (should-not (gethash "entries" object))))
      (should
       (equal (claudemacs--session-list--parse-json-string
               "[{\"id\":\"legacy\"}]" 'alist 'list null-value nil)
              '(((id . "legacy"))))))))

;;; Identity and merge

(ert-deftest claudemacs-session-list-test-resume-and-branch-use-explicit-ids ()
  "Resume and branch argument builders never fall back to a bare picker."
  :tags '(:unit :session-list :identity :lifecycle)
  (should (equal (claudemacs--get-resume-args 'claude "claude-id")
                 '("--resume" "claude-id")))
  (should (equal (claudemacs--get-resume-args 'codex "codex-id")
                 '("resume" "codex-id")))
  (should-error (claudemacs--get-resume-args 'codex nil))
  (should (equal (claudemacs--get-branch-args
                  'claude "source-id" "destination-id")
                 '("--resume" "source-id" "--fork-session"
                   "--session-id" "destination-id")))
  (should (equal (claudemacs--get-branch-args 'codex "source-id")
                 '("fork" "source-id")))
  (should-not (claudemacs--get-branch-args 'codex nil)))

(ert-deftest claudemacs-session-list-test-start-identity-plans-are-fail-closed ()
  "Start modes establish exact or unknown provenance explicitly."
  :tags '(:unit :session-list :identity :lifecycle)
  (should (equal (claudemacs--identity-plan-for-start
                  'claude nil "generated-id")
                 '(:provenance exact :id "generated-id")))
  (should (equal (claudemacs--identity-plan-for-start
                  'claude '("--resume" "source-id") nil)
                 '(:provenance exact :id "source-id")))
  (should (equal (claudemacs--identity-plan-for-start
                  'codex '("resume" "source-id") nil)
                 '(:provenance exact :id "source-id")))
  (should (equal (claudemacs--identity-plan-for-start
                  'codex '("fork" "source-id") nil)
                 '(:provenance unknown)))
  (should (equal (claudemacs--identity-plan-for-start
                  'codex nil nil)
                 '(:provenance unknown)))
  (should (equal (claudemacs--identity-plan-for-start
                  'unknown nil nil)
                 '(:provenance unknown))))

(ert-deftest claudemacs-session-list-test-invalid-launch-ids-fail-before-buffer-creation ()
  "Unsafe explicit launch IDs fail before a Claudemacs buffer is left behind."
  :tags '(:unit :session-list :identity :lifecycle :errors)
  (claudemacs-session-list-test--with-temp-directory work-dir
    (let ((claudemacs-terminal-backend 'test-backend)
          (claudemacs-tool-registry
           '((claude :program "claude" :switches nil)
             (codex :program "codex" :switches nil)))
          (claudemacs-switch-to-buffer-on-create nil))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program) "/bin/true"))
                ((symbol-function 'display-buffer)
                 (lambda (_buffer) (selected-window)))
                ((symbol-function 'claudemacs--terminal-ensure-backend)
                 (lambda (_backend) t))
                ((symbol-function 'claudemacs--terminal-start)
                 (lambda (&rest _args) nil))
                ((symbol-function 'claudemacs--terminal-post-display)
                 (lambda (&rest _args) nil))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _args) nil)))
        (dolist (case '((claude "--session-id" "bad id")
                        (claude "--resume" "-leading-option")
                        (codex "--session-id" "bad id")
                        (codex "resume" "bad id")
                        (codex "fork" "bad id")
                        (codex "fork")))
          (let* ((tool (car case))
                 (args (cdr case))
                 (before (seq-filter
                          (lambda (buffer)
                            (string-prefix-p "*claudemacs:"
                                             (buffer-name buffer)))
                          (buffer-list)))
                 new-buffers)
            (unwind-protect
                (progn
                  (should-error (apply #'claudemacs--run-with-args
                                       tool work-dir args))
                  (setq new-buffers
                        (seq-filter
                         (lambda (buffer)
                           (and (string-prefix-p "*claudemacs:"
                                                 (buffer-name buffer))
                                (not (memq buffer before))))
                         (buffer-list)))
                  (should-not new-buffers))
              (apply #'claudemacs-session-list-test--kill-buffers
                     new-buffers))))))))

(ert-deftest claudemacs-session-list-test-start-uses-selected-directory-and-retains-generated-claude-id ()
  "A selected work directory controls naming and new Claude ID ownership."
  :tags '(:unit :session-list :identity :lifecycle :paths)
  (claudemacs-session-list-test--with-temp-directory selected
    (let ((claudemacs-terminal-backend 'test-backend)
          (claudemacs-tool-registry
           '((claude :program "claude" :switches nil)))
          (claudemacs-switch-to-buffer-on-create nil)
          (claudemacs--start-identity-plan nil)
          (generated-id "generated-claude-id")
          observed-switches
          buffer)
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program) "/bin/true"))
                ((symbol-function 'claudemacs--generate-uuid)
                 (lambda () generated-id))
                ((symbol-function 'display-buffer)
                 (lambda (_buffer) (selected-window)))
                ((symbol-function 'claudemacs--terminal-ensure-backend)
                 (lambda (_backend) t))
                ((symbol-function 'claudemacs--terminal-start)
                 (lambda (_buffer _backend _program switches)
                   (setq observed-switches switches)))
                ((symbol-function 'claudemacs--terminal-post-display)
                 (lambda (_buffer) nil))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _args) nil))
                ((symbol-function 'claudemacs--register-current-session-identity)
                 (lambda () nil)))
        (setq buffer (claudemacs--run-with-args 'claude selected))
        (unwind-protect
            (progn
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (should (equal (file-truename claudemacs--cwd)
                               (file-truename selected)))
                (should (equal claudemacs--session-id generated-id))
                (should (eq claudemacs--session-id-provenance 'exact)))
              ;; The explicit directory is used by the fallback session ID,
              ;; not the caller's current default-directory.  With no
              ;; workspace provider this is the visible buffer identity.
              (should (string-match-p
                       (regexp-quote (file-truename selected))
                       (buffer-name buffer)))
              (let ((info (claudemacs--get-session-info buffer)))
                (should (= (plist-get info :instance) 1)))
              (should (equal (member generated-id observed-switches)
                             (list generated-id))))
          (claudemacs-session-list-test--kill-buffers buffer))))))

(ert-deftest claudemacs-session-list-test-codex-start-stays-unknown-without-discovery ()
  "A new Codex start succeeds from a source buffer without history discovery.

The post-display callback is intentionally checked in the target buffer: a
startup implementation that accidentally runs it in the source buffer can
attach terminal state and identity to the wrong session."
  :tags '(:unit :session-list :identity :lifecycle :interaction)
  (claudemacs-session-list-test--with-temp-directory work-dir
    (let ((claudemacs-terminal-backend 'test-backend)
          (claudemacs-tool-registry
           '((codex :program "codex" :switches nil)))
          (claudemacs-switch-to-buffer-on-create nil)
          (claudemacs--start-identity-plan nil)
          (source (get-buffer-create "*codex-start-source*"))
          baseline-called
          discovery-called
          post-display-buffer
          buffer)
      (unwind-protect
          (with-current-buffer source
            (setq default-directory work-dir)
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (_program) "/bin/true"))
                      ((symbol-function 'display-buffer)
                       (lambda (_buffer) (selected-window)))
                      ((symbol-function 'claudemacs--terminal-ensure-backend)
                       (lambda (_backend) t))
                      ((symbol-function 'claudemacs--terminal-start)
                       (lambda (target _backend _program _switches)
                         (should (eq (current-buffer) target))))
                      ((symbol-function 'claudemacs--terminal-post-display)
                       (lambda (target)
                         (setq post-display-buffer target)
                         (should (eq (current-buffer) target))))
                      ((symbol-function 'claudemacs--codex-capture-baseline)
                       (lambda (&rest _args)
                         (setq baseline-called t)
                         nil))
                      ((symbol-function 'claudemacs--begin-codex-session-discovery)
                       (lambda (&rest _args)
                         (setq discovery-called t)
                         nil))
                      ((symbol-function 'claudemacs--session-list-history-rows)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup called unified history")))
                      ((symbol-function 'claudemacs--session-list-claude-history)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup called Claude history")))
                      ((symbol-function 'claudemacs--session-list-codex-history)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup called Codex history")))
                      ((symbol-function 'claudemacs--call-history-provider)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup called history adapter")))
                      ((symbol-function 'claudemacs--history-rows-for-tool)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup called focused history adapter")))
                      ((symbol-function 'claudemacs--history-rows-for-cwd)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup queried history by CWD")))
                      ((symbol-function 'claudemacs--select-history-session-id)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup selected a history ID")))
                      ((symbol-function 'claudemacs--codex-history-provider)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup called Codex provider alias")))
                      ((symbol-function 'claudemacs--session-list-history-provider)
                       (lambda (&rest _args)
                         (ert-fail "Codex startup called history provider")))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _args) nil)))
              (setq buffer (claudemacs--run-with-args 'codex work-dir))
              (should (buffer-live-p buffer))
              (should (eq post-display-buffer buffer))
              (should-not baseline-called)
              (should-not discovery-called)
              (with-current-buffer buffer
                (should-not claudemacs--session-id)
                (should (eq claudemacs--session-id-provenance 'unknown)))))
        (claudemacs-session-list-test--kill-buffers source buffer)))))

(ert-deftest claudemacs-session-list-test-explicit-directory-beats-projectile-root ()
  "An explicit launch directory B wins over the caller's Projectile root A."
  :tags '(:unit :session-list :lifecycle :paths :projectile)
  (claudemacs-session-list-test--with-temp-directory projectile-root
    (claudemacs-session-list-test--with-temp-directory explicit-directory
      (let ((claudemacs-terminal-backend 'test-backend)
            (claudemacs-tool-registry
             '((claude :program "claude" :switches nil)))
            (claudemacs-switch-to-buffer-on-create nil)
            (claudemacs-prefer-projectile-root t)
            (default-directory projectile-root)
            terminal-start-cwd
            buffer)
        (cl-letf (((symbol-function 'projectile-project-root)
                   (lambda () projectile-root))
                  ((symbol-function 'executable-find)
                   (lambda (_program) "/bin/true"))
                  ((symbol-function 'claudemacs--generate-uuid)
                   (lambda () "explicit-directory-id"))
                  ((symbol-function 'display-buffer)
                   (lambda (_buffer) (selected-window)))
                  ((symbol-function 'claudemacs--terminal-ensure-backend)
                   (lambda (_backend) t))
                  ((symbol-function 'claudemacs--terminal-start)
                   (lambda (target _backend _program _switches)
                     (setq terminal-start-cwd
                           (buffer-local-value 'default-directory target))))
                  ((symbol-function 'claudemacs--terminal-post-display)
                   (lambda (_buffer) nil))
                  ((symbol-function 'run-with-timer)
                   (lambda (&rest _args) nil)))
          (setq buffer (claudemacs--run-with-args
                        'claude explicit-directory))
          (unwind-protect
              (with-current-buffer buffer
                (should (equal (file-truename claudemacs--cwd)
                               (file-truename explicit-directory)))
                (should-not (equal (file-truename claudemacs--cwd)
                                   (file-truename projectile-root)))
                (should (equal (file-truename
                                (directory-file-name terminal-start-cwd))
                               (file-truename
                                (directory-file-name explicit-directory)))))
            (claudemacs-session-list-test--kill-buffers buffer)))))))

(ert-deftest claudemacs-session-list-test-live-identity-is-immutable-unless-forced ()
  "A later observation cannot remap an attached live buffer."
  :tags '(:unit :session-list :identity :lifecycle)
  (with-temp-buffer
    (setq-local claudemacs--tool 'codex)
    (should (equal (plist-get (claudemacs--set-session-identity
                               "first-id" 'exact)
                              :id)
                   "first-id"))
    (claudemacs--set-session-identity "second-id" 'exact)
    (should (equal claudemacs--session-id "first-id"))
    (claudemacs--set-session-identity "second-id" 'exact t)
    (should (equal claudemacs--session-id "second-id"))
    (should (eq claudemacs--session-id-provenance 'exact))))

(ert-deftest claudemacs-session-list-test-unknown-live-never-merges-by-cwd ()
  "A live unknown row is not associated with a same-CWD history row."
  :tags '(:unit :session-list :identity)
  (let ((history-called nil)
        (history-adapter-called nil)
        (registry-called nil)
        (live-buffer (claudemacs-session-list-test--session-buffer
                      "*claudemacs:codex:unknown-live*"
                      "/tmp/same-project" 'codex nil 'unknown t)))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs--session-list-live-buffer-rows)
                   (lambda ()
                     (list (claudemacs-session-list-test--make-live-row
                            :tool 'codex :cwd "/tmp/same-project"
                            :identity 'unknown :buffer live-buffer))))
                  ((symbol-function 'claudemacs--session-list-claude-history)
                   (lambda (&optional _cwd)
                     (setq history-called t)
                     (list (claudemacs-session-list-test--make-row
                            :tool 'codex :session-id "authoritative-id"
                            :identity 'exact :cwd "/tmp/same-project"))))
                  ((symbol-function 'claudemacs--session-list-codex-history)
                   (lambda (&optional _cwd)
                     (setq history-called t) nil))
                  ((symbol-function 'claudemacs--history-rows-for-tool)
                   (lambda (&rest _args)
                     (setq history-adapter-called t)
                     nil))
                  ((symbol-function 'claudemacs--session-list-read-registry)
                   (lambda () (setq registry-called t) nil))
                  ((symbol-function 'claudemacs--session-list-register-rows)
                   (lambda (_rows) nil)))
          (let ((rows (claudemacs--session-list-refresh-data)))
            (should (= (length rows) 1))
            (should-not (claudemacs-session-list-test--row-id (car rows)))
            (should (eq (claudemacs-session-list-test--row-identity (car rows))
                        'unknown))
            (should-not history-called)
            (should-not history-adapter-called)
            (should-not registry-called)))
      (claudemacs-session-list-test--kill-buffers live-buffer))))

(ert-deftest claudemacs-session-list-test-refresh-queries-live-provider-once ()
  "A refresh queries only the live-buffer provider once."
  :tags '(:unit :session-list :provider :interaction)
  (let ((live-calls 0) (claude-calls 0) (codex-calls 0)
        (history-adapter-calls 0)
        (registry-calls 0)
        (live-one (claudemacs-session-list-test--session-buffer
                   "*claudemacs:codex:live-one*" "/tmp/live-one"
                   'codex "live-one" 'exact t))
        (live-two (claudemacs-session-list-test--session-buffer
                   "*claudemacs:codex-2:live-two*" "/tmp/live-two"
                   'codex "live-two" 'exact t))
        result)
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs--session-list-live-buffer-rows)
                   (lambda ()
                     (setq live-calls (1+ live-calls))
                     (list (claudemacs-session-list-test--make-live-row
                            :tool 'codex :session-id "live-one"
                            :identity 'exact :buffer live-one)
                           (claudemacs-session-list-test--make-live-row
                            :tool 'codex :session-id "live-two"
                            :identity 'exact :buffer live-two))))
                  ((symbol-function 'claudemacs--session-list-claude-history)
                   (lambda (&optional _cwd)
                     (setq claude-calls (1+ claude-calls)) nil))
                  ((symbol-function 'claudemacs--session-list-codex-history)
                   (lambda (&optional _cwd)
                     (setq codex-calls (1+ codex-calls)) nil))
                  ((symbol-function 'claudemacs--history-rows-for-tool)
                   (lambda (&rest _args)
                     (setq history-adapter-calls (1+ history-adapter-calls))
                     nil))
                  ((symbol-function 'claudemacs--session-list-history-rows)
                   (lambda (&rest _args)
                     (setq history-adapter-calls (1+ history-adapter-calls))
                     nil))
                  ((symbol-function 'claudemacs--session-list-read-registry)
                   (lambda () (setq registry-calls (1+ registry-calls)) nil))
                  ((symbol-function 'claudemacs--session-list-register-rows)
                   (lambda (_rows) (ert-fail "refresh must not write registry"))))
          (setq result (claudemacs--session-list-refresh-data))
          (should (= live-calls 1))
          (should (= claude-calls 0))
          (should (= codex-calls 0))
          (should (= history-adapter-calls 0))
          (should (= registry-calls 0))
          (should (= (length result) 2))
          (dolist (row result)
            (should (eq (plist-get row :state) 'live))
            (should (eq (plist-get row :source) 'live-buffer))
            (should (buffer-live-p (plist-get row :buffer))))
      (claudemacs-session-list-test--kill-buffers live-one live-two)))))

(ert-deftest claudemacs-session-list-test-actual-live-provider-never-queries-history ()
  "The real live-buffer provider never calls history or registry adapters."
  :tags '(:unit :session-list :provider :interaction :liveness)
  (let ((buffer (claudemacs-session-list-test--session-buffer
                 "*claudemacs:claude:actual-live-provider*"
                 "/tmp/actual-live-provider" 'claude "live-id" 'exact t)))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs--list-all-sessions)
                   (lambda () (list buffer)))
                  ((symbol-function 'claudemacs--terminal-live-p)
                   (lambda () t))
                  ((symbol-function 'claudemacs--session-list-history-rows)
                   (lambda (&rest _args)
                     (ert-fail "live provider called unified history")))
                  ((symbol-function 'claudemacs--session-list-claude-history)
                   (lambda (&rest _args)
                     (ert-fail "live provider called Claude history")))
                  ((symbol-function 'claudemacs--session-list-codex-history)
                   (lambda (&rest _args)
                     (ert-fail "live provider called Codex history")))
                  ((symbol-function 'claudemacs--call-history-provider)
                   (lambda (&rest _args)
                     (ert-fail "live provider called history adapter")))
                  ((symbol-function 'claudemacs--history-rows-for-tool)
                   (lambda (&rest _args)
                     (ert-fail "live provider called focused history adapter")))
                  ((symbol-function 'claudemacs--session-list-read-registry)
                   (lambda (&rest _args)
                     (ert-fail "live provider read registry")))
                  ((symbol-function 'claudemacs--session-list-register-rows)
                   (lambda (&rest _args)
                     (ert-fail "live provider wrote registry")))
                  ((symbol-function 'claudemacs--session-list-history-provider)
                   (lambda (&rest _args)
                     (ert-fail "live provider called history provider"))))
          (let ((rows (claudemacs--session-list-refresh-data)))
            (should (= (length rows) 1))
            (should (eq (plist-get (car rows) :buffer) buffer))
            (should (eq (plist-get (car rows) :state) 'live))))
      (claudemacs-session-list-test--kill-buffers buffer))))

(ert-deftest claudemacs-session-list-test-refresh-removes-dead-rows ()
  "Refresh removes a row as soon as its terminal is no longer live."
  :tags '(:unit :session-list :provider :interaction :liveness)
  (let* ((states (list 'live nil))
         (live-calls 0)
         (row-id "changing-live-id")
         (buffer (claudemacs-session-list-test--session-buffer
                  "*claudemacs:codex:changing-live*" "/tmp/changing-live"
                  'codex row-id 'exact t)))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs--session-list-live-buffer-rows)
                   (lambda ()
                     (setq live-calls (1+ live-calls))
                     (if (pop states)
                         (list (claudemacs-session-list-test--make-live-row
                                :tool 'codex :session-id row-id
                                :identity 'exact :buffer buffer))
                       nil)))
                  ((symbol-function 'claudemacs--session-list-claude-history)
                   (lambda (&optional _cwd) nil))
                  ((symbol-function 'claudemacs--session-list-codex-history)
                   (lambda (&optional _cwd) nil))
                  ((symbol-function 'claudemacs--session-list-read-registry)
                   (lambda () nil))
                  ((symbol-function 'claudemacs--session-list-register-rows)
                   (lambda (_rows) nil)))
          (let* ((first (claudemacs--session-list-refresh-data))
                 (second (claudemacs--session-list-refresh-data)))
            (should (= live-calls 2))
            (should (= (length first) 1))
            (should (equal (claudemacs-session-list-test--row-id (car first))
                           row-id))
            (should (null second))))
      (claudemacs-session-list-test--kill-buffers buffer))))

(ert-deftest claudemacs-session-list-test-empty-live-table-ignores-history ()
  "No live buffers means an empty table, even when history has rows."
  :tags '(:unit :session-list :provider :liveness)
  (let ((history-called nil)
        (history-adapter-called nil)
        (registry-called nil))
    (cl-letf (((symbol-function 'claudemacs--session-list-live-buffer-rows)
               (lambda () nil))
              ((symbol-function 'claudemacs--session-list-claude-history)
               (lambda (&optional _cwd)
                 (setq history-called t)
                 (list (claudemacs-session-list-test--make-row
                        :tool 'claude :session-id "historical-id"
                        :identity 'exact))))
              ((symbol-function 'claudemacs--session-list-codex-history)
               (lambda (&optional _cwd)
                 (setq history-called t) nil))
              ((symbol-function 'claudemacs--history-rows-for-tool)
               (lambda (&rest _args)
                 (setq history-adapter-called t)
                 nil))
              ((symbol-function 'claudemacs--session-list-read-registry)
               (lambda () (setq registry-called t) nil))
              ((symbol-function 'claudemacs--session-list-register-rows)
               (lambda (_rows) nil)))
      (should-not (claudemacs--session-list-refresh-data))
      (should-not history-called)
      (should-not history-adapter-called)
      (should-not registry-called))))

(ert-deftest claudemacs-session-list-test-identity-does-not-follow-display-time ()
  "Changing buffer display recency never changes an attached exact ID."
  :tags '(:unit :session-list :identity)
  (let ((buffer (claudemacs-session-list-test--session-buffer
                "*claudemacs:codex:identity-test*"
                "/tmp/identity-project" 'codex "stable-id" 'exact t)))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs--list-all-sessions)
                   (lambda () (list buffer)))
                  ((symbol-function 'claudemacs--terminal-live-p)
                   (lambda () t)))
          (let ((first (claudemacs-session-list-test--rows
                        (claudemacs-session-list-test--call
                         '(claudemacs--session-list-live-rows
                           claudemacs--session-list-live-buffer-rows
                           claudemacs--live-buffer-provider
                           claudemacs--session-list-live-provider))))
                second)
            (with-current-buffer buffer
              (setq buffer-display-time (time-subtract (current-time)
                                                       (seconds-to-time 3600))))
            (setq second (claudemacs-session-list-test--rows
                          (claudemacs-session-list-test--call
                           '(claudemacs--session-list-live-rows
                             claudemacs--session-list-live-buffer-rows
                             claudemacs--live-buffer-provider
                             claudemacs--session-list-live-provider))))
            (should (equal (claudemacs-session-list-test--result-id-list first)
                           (claudemacs-session-list-test--result-id-list second)))
            (should (equal (car (claudemacs-session-list-test--result-id-list second))
                           "stable-id"))))
      (claudemacs-session-list-test--kill-buffers buffer))))

(ert-deftest claudemacs-session-list-test-unknown-start-row-stays-unknown ()
  "An unknown start row never manufactures a session identity."
  :tags '(:unit :session-list :identity)
  (let ((row (claudemacs-session-list-test--call
              '(claudemacs--session-list--row)
              (claudemacs-session-list-test--make-row
               :session-id nil :identity 'unknown :state 'live))))
    ;; A missing authoritative ID remains unknown; it must never be inferred
    ;; from a candidate row or from display metadata.
    (should-not (claudemacs-session-list-test--row-id row))
    (should (eq (claudemacs-session-list-test--row-identity row) 'unknown))))

(ert-deftest claudemacs-session-list-test-row-normalizer-fails-closed-on-raw-id ()
  "A row with an ID but no valid provenance is not resumable.

Provider adapters must state why an ID is authoritative.  A compatibility
caller that supplies only `:session-id' must not get an implicit exact
identity merely because the value is non-nil."
  :tags '(:unit :session-list :identity :errors)
  (let ((row (claudemacs-session-list-test--call
              '(claudemacs--session-list--row)
              (list :tool 'codex :session-id "unproven-id"
                    :state 'live :cwd "/tmp/project"))))
    (should-not (claudemacs-session-list-test--row-id row))
    (should (eq (claudemacs-session-list-test--row-identity row) 'unknown))))

(ert-deftest claudemacs-session-list-test-tool-neutral-id-without-provenance-stays-unknown ()
  "A non-legacy tool-neutral ID is not authoritative without provenance."
  :tags '(:unit :session-list :identity :liveness :errors)
  (let ((buffer (claudemacs-session-list-test--session-buffer
                 "*claudemacs:codex:unproven-tool-neutral*"
                 "/tmp/unproven-project" 'codex "tool-neutral-id" nil t)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Keep an ID in the new tool-neutral slot, but remove every
            ;; provenance marker and legacy Claude UUID.  A display name or a
            ;; non-nil ID alone is not evidence that this buffer owns a CLI
            ;; conversation.
            (setq-local claudemacs--authoritative-session-id "tool-neutral-id"
                        claudemacs--session-id "tool-neutral-id"
                        claudemacs--tracked-session-id nil
                        claudemacs--codex-session-id nil
                        claudemacs--claude-session-uuid nil
                        claudemacs--session-id-provenance nil
                        claudemacs--session-identity nil))
          (cl-letf (((symbol-function 'claudemacs--list-all-sessions)
                     (lambda () (list buffer)))
                    ((symbol-function 'claudemacs--terminal-live-p)
                     (lambda () t)))
            (let ((row (car (claudemacs-session-list-test--rows
                             (claudemacs--session-list-live-buffer-rows)))))
              (should-not (claudemacs-session-list-test--row-id row))
              (should (eq (claudemacs-session-list-test--row-identity row)
                          'unknown)))))
      (claudemacs-session-list-test--kill-buffers buffer))))

(ert-deftest claudemacs-session-list-test-mutated-live-exact-id-renders-unknown ()
  "An externally mutated exact ID is sanitized to a literal `unknown'."
  :tags '(:unit :session-list :identity :liveness :presentation :errors)
  (let ((buffer (claudemacs-session-list-test--session-buffer
                 "*claudemacs:codex:mutated-id*"
                 "/tmp/mutated-id" 'codex "unsafe id" 'exact t)))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs--list-all-sessions)
                   (lambda () (list buffer)))
                  ((symbol-function 'claudemacs--terminal-live-p)
                   (lambda () t)))
          (let ((row (car (claudemacs--session-list-live-buffer-rows))))
            (should row)
            (should-not (claudemacs-session-list-test--row-id row))
            (should (eq (claudemacs-session-list-test--row-identity row)
                        'unknown))
            (with-temp-buffer
              (claudemacs-session-list-mode)
              (setq-local claudemacs--session-list-rows (list row))
              (let* ((entry (car (claudemacs--session-list--entries)))
                     (cells (cadr entry)))
                (should (equal (substring-no-properties (aref cells 2))
                               "unknown"))))))
      (claudemacs-session-list-test--kill-buffers buffer))))

(ert-deftest claudemacs-session-list-test-resume-uses-prompted-project-directory ()
  "An explicitly selected resume directory is the launch CWD."
  :tags '(:unit :session-list :identity :lifecycle :paths)
  (claudemacs-session-list-test--with-temp-directory current
    (claudemacs-session-list-test--with-temp-directory selected
      (let (observed-directory selected-cwd)
        (let ((default-directory current)
              (claudemacs-tool-registry '((codex))))
          (cl-letf (((symbol-function 'transient-args)
                     (lambda (_prefix) '("--prompt-project-root")))
                    ((symbol-function 'read-directory-name)
                     (lambda (&rest _args) selected))
                    ((symbol-function 'claudemacs--select-history-session-id)
                     (lambda (_tool cwd)
                       (setq selected-cwd cwd)
                       "resume-id"))
                    ((symbol-function 'claudemacs--run-with-args)
                     (lambda (_tool arg &rest _args)
                       ;; The common launcher accepts a directory string in
                       ;; its optional argument.  Capturing that value tests
                       ;; the observable launch CWD without coupling this
                       ;; test to the launcher internals.
                       (setq observed-directory arg))))
            (claudemacs--resume-tool-by-index 0))
          (should (equal (file-truename selected)
                         (file-truename selected-cwd)))
          (should (equal (file-truename selected)
                         (file-truename observed-directory))))))))

(ert-deftest claudemacs-session-list-test-custom-command-args-are-expanded ()
  "A custom command entered through `-f' is passed as separate argv values."
  :tags '(:unit :session-list :identity :lifecycle :arguments)
  (let (observed-args)
    (let ((claudemacs-tool-registry '((codex))))
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (_prefix)
                   '("resume 01a05cf3-1b28-7dc0-aaf4-cbd1b2a2bfb3")))
                ((symbol-function 'claudemacs--run-with-args)
                 (lambda (_tool _directory &rest args)
                   (setq observed-args args))))
        (claudemacs--start-tool-by-index 0)))
    (should (equal observed-args
                   '("resume" "01a05cf3-1b28-7dc0-aaf4-cbd1b2a2bfb3")))
    (should-not (claudemacs--explicit-resume-command-p
                 'codex '("--model resume")))))

(ert-deftest claudemacs-session-list-test-explicit-custom-resume-replaces-picker ()
  "An explicit resume command from `-u' or `-f' is not sent as a Codex prompt."
  :tags '(:unit :session-list :identity :lifecycle :arguments)
  (let (observed-args)
    (let ((claudemacs-tool-registry '((codex))))
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (_prefix)
                   '("resume 01a05cf3-1b28-7dc0-aaf4-cbd1b2a2bfb3")))
                ((symbol-function 'claudemacs--select-history-session-id)
                 (lambda (&rest _args)
                   (ert-fail "An explicit custom resume should skip history selection")))
                ((symbol-function 'claudemacs--run-with-args)
                 (lambda (_tool _directory &rest args)
                   (setq observed-args args))))
        (claudemacs--resume-tool-by-index 0)))
    (should (equal observed-args
                   '("resume" "01a05cf3-1b28-7dc0-aaf4-cbd1b2a2bfb3")))))

(ert-deftest claudemacs-session-list-test-history-session-id-rejects-unsafe-values ()
  "History IDs reject option-like, whitespace, and control-character values."
  :tags '(:unit :session-list :identity :provider :errors)
  (dolist (id '(nil 42 "" " " "-leading-option" "id with spaces"
                "id\nwith-newline" "id;next"))
    (should-not (claudemacs--history-row-session-id
                 (list :session-id id))))
  (dolist (id '("thread_abc-123" "123e4567-e89b-12d3-a456-426614174000"))
    (should (equal (claudemacs--history-row-session-id
                    (list :session-id id))
                   id))))

(ert-deftest claudemacs-session-list-test-history-provider-ids-reject-unsafe-values ()
  "Claude and Codex history adapters reject unsafe session IDs."
  :tags '(:unit :session-list :identity :provider :errors)
  (dolist (id '("bad id" "bad\nline" "-bad" "bad;next"))
    (should-not
     (claudemacs--session-list--claude-entry-row
      `((sessionId . ,id) (projectPath . "/tmp/project"))
      "/tmp/index.json"))
    (should-not
     (claudemacs--session-list--codex-alist-row
      `((id . ,id) (cwd . "/tmp/project")))))
  (should
   (claudemacs--session-list--claude-entry-row
    '((sessionId . "safe_id-1") (projectPath . "/tmp/project"))
    "/tmp/index.json"))
  (should
   (claudemacs--session-list--codex-alist-row
    '((id . "safe_id-1") (cwd . "/tmp/project")))))

;;; Liveness and live-buffer provider

(ert-deftest claudemacs-session-list-test-dead-process-is-omitted ()
  "A Claudemacs buffer whose terminal process died is omitted from the list."
  :tags '(:unit :session-list :liveness)
  (let ((buffer (claudemacs-session-list-test--session-buffer
                "*claudemacs:claude:dead-process*"
                "/tmp/dead-project" 'claude "known-id" 'exact nil)))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs--list-all-sessions)
                   (lambda () (list buffer)))
                  ((symbol-function 'claudemacs--terminal-live-p)
                   (lambda () nil)))
          (should-not
           (claudemacs-session-list-test--rows
            (claudemacs-session-list-test--call
             '(claudemacs--session-list-live-rows
               claudemacs--session-list-live-buffer-rows
               claudemacs--live-buffer-provider
               claudemacs--session-list-live-provider)))))
      (claudemacs-session-list-test--kill-buffers buffer))))

(ert-deftest claudemacs-session-list-test-enumeration-error-is-diagnostic ()
  "A live-buffer enumeration error is reported instead of being silent."
  :tags '(:unit :session-list :provider :errors :liveness)
  (claudemacs-session-list-test--reset-diagnostics)
  (cl-letf (((symbol-function 'claudemacs--list-all-sessions)
             (lambda () (error "enumerator unavailable")))
            ((symbol-function 'buffer-list)
             (lambda () nil)))
    (should (listp (claudemacs--session-list-refresh-data)))
    (should
     (seq-some
      (lambda (warning)
        (string-match-p "Live session enumeration\\|enumerator unavailable"
                        warning))
      (claudemacs-session-list-test--warnings nil)))))

(ert-deftest claudemacs-session-list-test-liveness-error-preserves-healthy-rows ()
  "A liveness error for one buffer does not discard healthy live rows."
  :tags '(:unit :session-list :provider :errors :liveness)
  (let ((broken (claudemacs-session-list-test--session-buffer
                 "*claudemacs:codex:liveness-error*"
                 "/tmp/broken-liveness" 'codex "broken-id" 'exact t))
        (healthy (claudemacs-session-list-test--session-buffer
                  "*claudemacs:claude:healthy-liveness*"
                  "/tmp/healthy-liveness" 'claude "healthy-id" 'exact t)))
    (unwind-protect
        (progn
          (claudemacs-session-list-test--reset-diagnostics)
          (cl-letf (((symbol-function 'claudemacs--list-all-sessions)
                     (lambda () (list broken healthy)))
                    ((symbol-function 'claudemacs--terminal-live-p)
                     (lambda ()
                       (if (eq (current-buffer) broken)
                           (error "terminal liveness unavailable")
                         t)))
                    ((symbol-function 'claudemacs--history-rows-for-tool)
                     (lambda (&rest _args)
                       (ert-fail "live provider queried history"))))
            (let ((rows (claudemacs--session-list-refresh-data)))
              (should (= (length rows) 1))
              (should (eq (plist-get (car rows) :buffer) healthy))
              (should (eq (plist-get (car rows) :state) 'live))
              (should
               (seq-some
                (lambda (warning)
                  (string-match-p "liveness\\|Live session buffer skipped"
                                  warning))
                (claudemacs-session-list-test--warnings nil))))))
      (claudemacs-session-list-test--kill-buffers broken healthy))))

(ert-deftest claudemacs-session-list-test-history-row-is-not-displayed ()
  "History-only records are not valid rows for the live session list."
  :tags '(:unit :session-list :liveness)
  (let (history-called)
    (cl-letf (((symbol-function 'claudemacs--session-list-live-buffer-rows)
               (lambda nil))
              ((symbol-function 'claudemacs--session-list-claude-history)
               (lambda (&optional _cwd)
                 (setq history-called t)
                 (list (claudemacs-session-list-test--make-row
                        :state 'history :source 'codex-history
                        :session-id "history-id" :identity 'exact))))
              ((symbol-function 'claudemacs--session-list-codex-history)
               (lambda (&optional _cwd)
                 (setq history-called t) nil)))
      (should-not (claudemacs--session-list-refresh-data))
      (should-not history-called))))

(ert-deftest claudemacs-session-list-test-live-eat-and-ghostel-use-facade ()
  "Both terminal backends use the terminal facade for liveness."
  :tags '(:unit :session-list :liveness)
  (dolist (backend '(eat ghostel))
    (let ((buffer (claudemacs-session-list-test--session-buffer
                  (format "*claudemacs:%s:backend-test*" backend)
                  "/tmp/backend-project" 'claude
                  (format "%s-id" backend) 'exact t)))
      (unwind-protect
          (cl-letf (((symbol-function 'claudemacs--list-all-sessions)
                     (lambda () (list buffer)))
                    ((symbol-function 'claudemacs--terminal-live-p)
                     (lambda () t)))
            (with-current-buffer buffer
              (setq-local claudemacs--terminal-backend backend)
              (let ((row (car (claudemacs-session-list-test--rows
                               (claudemacs-session-list-test--call
                                '(claudemacs--session-list-live-rows
                                  claudemacs--session-list-live-buffer-rows
                                  claudemacs--live-buffer-provider
                                  claudemacs--session-list-live-provider))))))
                (should (equal (claudemacs-session-list-test--row-state row)
                               "live")))))
        (claudemacs-session-list-test--kill-buffers buffer)))))

;;; History providers, paths, and failure behavior

(ert-deftest claudemacs-session-list-test-claude-configured-home-is-honored ()
  "CLAUDE_CONFIG_DIR controls the Claude history root."
  :tags '(:unit :session-list :provider :paths)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "CLAUDE_CONFIG_DIR" home)
      (let ((resolved (claudemacs-session-list-test--call
                       '(claudemacs--session-list-resolve-claude-home
                         claudemacs--session-list-claude-config-dir
                         claudemacs--claude-history-root))))
        (should (equal (file-name-as-directory (expand-file-name home))
                       (file-name-as-directory (expand-file-name resolved))))))))

(ert-deftest claudemacs-session-list-test-codex-configured-home-precedes-default ()
  "A configured Codex home is used before ~/.codex."
  :tags '(:unit :session-list :provider :paths)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "CODEX_SQLITE_HOME" home)
      (setenv "CODEX_HOME" "/tmp/should-not-win")
      (let ((resolved (claudemacs-session-list-test--call
                       '(claudemacs--session-list-resolve-codex-home
                         claudemacs--session-list-codex-home
                         claudemacs--codex-history-root))))
        (should (equal (file-name-as-directory (expand-file-name home))
                       (file-name-as-directory (expand-file-name resolved))))))))

(ert-deftest claudemacs-session-list-test-missing-history-is-warning-not-row ()
  "A missing optional history store leaves rows empty and emits a warning."
  :tags '(:unit :session-list :provider :errors)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "CLAUDE_CONFIG_DIR" home)
      (claudemacs-session-list-test--reset-diagnostics)
      (let* ((result (claudemacs--session-list-claude-history nil))
             (rows (claudemacs-session-list-test--rows result)))
        (should-not rows)
        (should (claudemacs-session-list-test--warnings result))))))

(ert-deftest claudemacs-session-list-test-malformed-claude-index-does-not-hide-valid-index ()
  "One malformed Claude index does not discard valid project history."
  :tags '(:unit :session-list :provider :errors)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "CLAUDE_CONFIG_DIR" home)
      (claudemacs-session-list-test--reset-diagnostics)
      (claudemacs-session-list-test--write
       (expand-file-name "projects/valid/sessions-index.json" home)
       (json-encode
        '((entries . (((sessionId . "valid-id")
                       (projectPath . "/tmp/valid")
                       (modified . "2026-08-15T12:00:00Z")
                       (summary . "valid summary")))))))
      (claudemacs-session-list-test--write
       (expand-file-name "projects/broken/sessions-index.json" home)
       "{not valid json")
      (let* ((result (claudemacs--session-list-claude-history nil))
             (ids (claudemacs-session-list-test--result-id-list result)))
        (should (member "valid-id" ids))
        (should (eq (claudemacs-session-list-test--row-identity
                     (seq-find (lambda (row)
                                 (equal (claudemacs-session-list-test--row-id row)
                                        "valid-id"))
                               (claudemacs-session-list-test--rows result)))
                    'exact))
        (should (claudemacs-session-list-test--warnings result))))))

(ert-deftest claudemacs-session-list-test-claude-index-missing-or-null-entries-is-diagnosed ()
  "A valid JSON object without an entries array is not a silent empty store."
  :tags '(:unit :session-list :provider :errors)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "CLAUDE_CONFIG_DIR" home)
      (claudemacs-session-list-test--write
       (expand-file-name "projects/missing/sessions-index.json" home)
       "{}")
      (claudemacs-session-list-test--write
       (expand-file-name "projects/null/sessions-index.json" home)
       "{\"entries\":null}")
      (claudemacs-session-list-test--write
       (expand-file-name "projects/not-array/sessions-index.json" home)
       "{\"entries\":{}}")
      (claudemacs-session-list-test--reset-diagnostics)
      (let ((result (claudemacs--session-list-claude-history nil)))
        (should-not (claudemacs-session-list-test--rows result))
        (should (claudemacs-session-list-test--warnings result))))))

(ert-deftest claudemacs-session-list-test-claude-history-caps-entry-normalization ()
  "A per-provider history cap bounds entry normalization work."
  :tags '(:unit :session-list :provider :limits)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment))
          (claudemacs-session-list-max-history 1)
          (claudemacs-session-list-max-claude-entries-per-index 1)
          processed)
      (setenv "CLAUDE_CONFIG_DIR" home)
      (claudemacs-session-list-test--write
       (expand-file-name "projects/capped/sessions-index.json" home)
       (json-encode
        `((entries . (((sessionId . "newest")
                       (projectPath . "/tmp/capped")
                       (modified . "2026-08-15T12:00:00Z"))
                      ((sessionId . "older")
                       (projectPath . "/tmp/capped")
                       (modified . "2026-08-14T12:00:00Z"))
                      ((sessionId . "oldest")
                       (projectPath . "/tmp/capped")
                       (modified . "2026-08-13T12:00:00Z")))))))
      (let ((original (symbol-function
                       'claudemacs--session-list--claude-entry-row)))
        (cl-letf (((symbol-function 'claudemacs--session-list--claude-entry-row)
                   (lambda (entry index-file)
                     (setq processed (1+ (or processed 0)))
                     (funcall original entry index-file))))
          (let ((result (claudemacs--session-list-claude-history nil)))
            (should (= (length (claudemacs-session-list-test--rows result)) 1))
            (should (equal (car (claudemacs-session-list-test--result-id-list result))
                           "newest"))
            (should (<= processed 1))))))))

(ert-deftest claudemacs-session-list-test-claude-index-read-is-bounded ()
  "Claude index reads request only one overflow sentinel byte."
  :tags '(:unit :session-list :provider :limits :errors)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment))
          (claudemacs-session-list-max-claude-index-bytes 8)
          read-end)
      (setenv "CLAUDE_CONFIG_DIR" home)
      (claudemacs-session-list-test--write
       (expand-file-name "projects/overflow/sessions-index.json" home)
       (make-string 128 ?x))
      (claudemacs-session-list-test--reset-diagnostics)
      (let ((original (symbol-function 'insert-file-contents-literally)))
        (cl-letf (((symbol-function 'insert-file-contents-literally)
                   (lambda (file &optional visit beg end replace)
                     (setq read-end end)
                     (funcall original file visit beg end replace))))
          (should-not (claudemacs--session-list-claude-history nil))
          (should (= read-end 9))
          (should
           (seq-some
            (lambda (warning)
              (string-match-p "exceeds 8-byte limit" warning))
            (claudemacs-session-list-test--warnings nil))))))))

(ert-deftest claudemacs-session-list-test-claude-index-traversal-is-bounded ()
  "Claude history traversal stops after the configured file cap."
  :tags '(:unit :session-list :provider :limits :errors)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment))
          (claudemacs-session-list-max-history 10)
          (claudemacs-session-list-max-claude-index-files 1)
          read-count)
      (setenv "CLAUDE_CONFIG_DIR" home)
      (dolist (spec '( ("one" . "one-id")
                       ("two" . "two-id")
                       ("three" . "three-id")))
        (claudemacs-session-list-test--write
         (expand-file-name
          (format "projects/%s/sessions-index.json" (car spec)) home)
         (json-encode
          `((entries . (((sessionId . ,(cdr spec))
                         (projectPath . "/tmp/project"))))))))
      (claudemacs-session-list-test--reset-diagnostics)
      (let ((original (symbol-function 'insert-file-contents-literally)))
        (cl-letf (((symbol-function 'insert-file-contents-literally)
                   (lambda (&rest args)
                     (setq read-count (1+ (or read-count 0)))
                     (apply original args))))
          (let ((rows (claudemacs--session-list-claude-history nil)))
            (should (= (length rows) 1))
            (should (= read-count 1))
            (should
             (seq-some
              (lambda (warning)
                (string-match-p "index-file limit reached" warning))
              (claudemacs-session-list-test--warnings nil)))))))))

(ert-deftest claudemacs-session-list-test-claude-directory-traversal-is-bounded ()
  "Claude history traversal stops even when directories contain no indexes."
  :tags '(:unit :session-list :provider :limits :errors)
  (claudemacs-session-list-test--with-temp-directory home
    (let ((process-environment (copy-sequence process-environment))
          (claudemacs-session-list-max-claude-index-files 100)
          (claudemacs-session-list-max-claude-index-directories 2)
          (directory-reads 0))
      (setenv "CLAUDE_CONFIG_DIR" home)
      (dotimes (index 8)
        (make-directory
         (expand-file-name (format "projects/empty-%s/nested" index) home)
         t))
      (claudemacs-session-list-test--reset-diagnostics)
      (let ((original (symbol-function 'directory-files)))
        (cl-letf (((symbol-function 'directory-files)
                   (lambda (&rest args)
                     (setq directory-reads (1+ directory-reads))
                     (apply original args))))
          (should-not (claudemacs--session-list--claude-index-files))
          (should (<= directory-reads 2))
          (should
           (seq-some
            (lambda (warning)
              (string-match-p "directory limit reached" warning))
            (claudemacs-session-list-test--warnings nil))))))))

(ert-deftest claudemacs-session-list-test-queued-claude-unsafe-paths-are-skipped ()
  "Queued Claude indexes replaced by unsafe paths are diagnosed before read."
  :tags '(:unit :session-list :provider :errors :paths :limits)
  (claudemacs-session-list-test--with-temp-directory home
    (let* ((process-environment (copy-sequence process-environment))
           (projects (expand-file-name "projects" home))
           (symlink-path (expand-file-name
                          "replaced/sessions-index.json" projects))
           (directory-path (expand-file-name
                            "directory/sessions-index.json" projects))
           (outside-path (expand-file-name "outside-sessions-index.json" home))
           (original-file-symlink-p (symbol-function 'file-symlink-p)))
      (setenv "CLAUDE_CONFIG_DIR" home)
      (make-directory directory-path t)
      (claudemacs-session-list-test--write outside-path
                                           "{\"entries\":[]}")
      (claudemacs-session-list-test--reset-diagnostics)
      (cl-letf (((symbol-function
                  'claudemacs--session-list--claude-index-files)
                 (lambda ()
                   ;; Model the race deterministically: these paths were
                   ;; queued by discovery, then changed before consumption.
                   (list symlink-path directory-path outside-path)))
                ((symbol-function 'file-symlink-p)
                 (lambda (path)
                   (if (equal (expand-file-name path)
                              (expand-file-name symlink-path))
                       t
                     (funcall original-file-symlink-p path))))
                ((symbol-function 'insert-file-contents-literally)
                 (lambda (&rest _args)
                   (ert-fail "unsafe Claude index path was read")))
                ((symbol-function 'insert-file-contents)
                 (lambda (&rest _args)
                   (ert-fail "unsafe Claude index path was read"))))
        (should-not (claudemacs--session-list-claude-history nil))
        (let ((warnings (claudemacs-session-list-test--warnings nil)))
          (should (seq-some (lambda (warning)
                              (string-match-p "path is a symlink" warning))
                            warnings))
          (should (seq-some (lambda (warning)
                              (string-match-p "path is not a regular file"
                                              warning))
                            warnings))
          (should (seq-some (lambda (warning)
                              (string-match-p "path is outside the projects root"
                                              warning))
                            warnings)))))))

(ert-deftest claudemacs-session-list-test-codex-nonzero-status-cannot-become-row ()
  "A failed sqlite invocation yields a warning, never stderr as an ID."
  :tags '(:unit :session-list :provider :errors)
  (claudemacs-session-list-test--reset-diagnostics)
  (let (arguments)
    (cl-letf (((symbol-function 'executable-find) (lambda (_name) "/bin/sqlite3"))
            ((symbol-function 'call-process)
             (lambda (&rest args) (setq arguments args) 1)))
      (let ((result (claudemacs-session-list-test--call
                     '(claudemacs--session-list--sqlite3-json)
                     "/tmp/codex-state.sqlite" "SELECT 1;")))
        (should-not result)
        (should (member "-readonly" arguments))
        (should (claudemacs-session-list-test--warnings result))))))

(ert-deftest claudemacs-session-list-test-codex-provider-error-is-diagnostic-not-empty-history ()
  "A full Codex provider query failure returns no rows and records a warning."
  :tags '(:unit :session-list :provider :errors)
  (claudemacs-session-list-test--with-temp-directory dir
    (let ((database (expand-file-name "state_5.sqlite" dir)))
      (claudemacs-session-list-test--write database "readable placeholder")
      (claudemacs-session-list-test--reset-diagnostics)
      (cl-letf (((symbol-function 'claudemacs--session-list-codex-database-file)
                 (lambda () database))
                ((symbol-function 'claudemacs--session-list--sqlite-schema-columns)
                 (lambda (_database)
                   '("id" "created_at" "updated_at" "cwd")))
                ((symbol-function 'claudemacs--session-list--codex-built-in-records)
                 (lambda (&rest _args)
                   (claudemacs--session-list--diagnostic
                    "Codex SQLite query failed: database is locked")
                   nil)))
        (let ((result (claudemacs--session-list-codex-history nil)))
          (should-not (claudemacs-session-list-test--rows result))
          (should (seq-some (lambda (warning)
                              (string-match-p "Codex SQLite query failed"
                                              warning))
                            (claudemacs-session-list-test--warnings result))))))))

(ert-deftest claudemacs-session-list-test-codex-minimal-schema-raw-record-succeeds ()
  "An older Codex schema with only required columns still yields a row."
  :tags '(:unit :session-list :provider :compatibility)
  (skip-unless (fboundp 'sqlite-open))
  (claudemacs-session-list-test--with-temp-directory dir
    (let* ((database (expand-file-name "state_5.sqlite" dir))
           (columns '("id" "created_at" "updated_at" "cwd"))
           (record (list "minimal-id" 100 101 "/tmp/minimal-project")))
      (claudemacs-session-list-test--write database "readable placeholder")
      (let ((rows
             (cl-letf (((symbol-function 'sqlite-open)
                        (lambda (_database) 'fake-codex-db))
                       ((symbol-function 'sqlite-select)
                        (lambda (_db _sql) (list record)))
                       ((symbol-function 'sqlite-close)
                        (lambda (_db) nil)))
               (claudemacs--session-list--codex-built-in-records
                database "SELECT required columns" columns))))
        (should (= (length rows) 1))
        (let ((row (car rows)))
          (should (equal (plist-get row :session-id) "minimal-id"))
          ;; Provider paths are canonicalized, including macOS's /tmp alias
          ;; and directory separator.  Assert path identity rather than a
          ;; platform-specific spelling.
          (should (claudemacs--session-list--same-path-p
                   (plist-get row :cwd) "/tmp/minimal-project"))
          (should (eq (plist-get row :identity) 'exact))
          ;; Optional title/preview columns do not exist in the old schema;
          ;; absence is a valid row, not a provider error.
          (should-not (plist-get row :description)))))))

(ert-deftest claudemacs-session-list-test-codex-malformed-row-diagnoses-resolved-database ()
  "A malformed Codex raw row is discarded with the resolved DB path."
  :tags '(:unit :session-list :provider :errors :paths)
  (skip-unless (fboundp 'sqlite-open))
  (claudemacs-session-list-test--with-temp-directory dir
    (let ((database (expand-file-name "state_5.sqlite" dir)))
      (claudemacs-session-list-test--write database "readable placeholder")
      (claudemacs-session-list-test--reset-diagnostics)
      (cl-letf (((symbol-function 'claudemacs--session-list-codex-database-file)
                 (lambda () database))
                ((symbol-function 'claudemacs--session-list--sqlite-schema-columns)
                 (lambda (_database)
                   '("id" "created_at" "updated_at" "cwd")))
                ((symbol-function 'sqlite-available-p)
                 (lambda () t))
                ((symbol-function 'sqlite-open)
                 (lambda (_database) 'fake-codex-db))
                ((symbol-function 'sqlite-select)
                 (lambda (_db _sql)
                   ;; The required CWD field is null.  This is a malformed
                   ;; database record, not an empty database.
                   (list (list "malformed-id" 100 101 nil))))
                ((symbol-function 'sqlite-close)
                 (lambda (_db) nil)))
        (should-not (claudemacs--session-list-codex-history nil))
        (should (seq-some
                 (lambda (warning)
                   (string-match-p (regexp-quote database) warning))
                 (claudemacs-session-list-test--warnings nil)))))))

(ert-deftest claudemacs-session-list-test-codex-cwd-filter-precedes-query-limit ()
  "A matching older CWD row is not lost behind a newer unrelated row."
  :tags '(:unit :session-list :provider :paths :limits)
  (claudemacs-session-list-test--with-temp-directory dir
    (let ((database (expand-file-name "state_5.sqlite" dir))
          seen-sql
          (matching (list :tool 'codex :session-id "matching-id"
                         :identity 'exact :cwd "/tmp/selected-project"
                         :updated-at 10 :state 'history))
          (unrelated (list :tool 'codex :session-id "unrelated-id"
                          :identity 'exact :cwd "/tmp/other-project"
                          :updated-at 20 :state 'history)))
      (claudemacs-session-list-test--write database "readable placeholder")
      (let ((claudemacs-session-list-max-history 1))
        (cl-letf (((symbol-function 'claudemacs--session-list-codex-database-file)
                   (lambda () database))
                  ((symbol-function 'claudemacs--session-list--sqlite-schema-columns)
                   (lambda (_database)
                     '("id" "created_at" "updated_at" "cwd")))
                  ((symbol-function 'claudemacs--session-list--codex-built-in-records)
                   (lambda (_database sql _columns)
                     (setq seen-sql sql)
                     ;; Model the database applying its SQL LIMIT.  The
                     ;; implementation must put the CWD predicate in SQL,
                     ;; before that limit, or the unrelated newer row wins.
                     (if (string-match-p "cwd" sql)
                         (list matching)
                       (list unrelated)))))
          (let ((rows (claudemacs--session-list-codex-history
                       "/tmp/selected-project")))
            ;; The selected CWD must be a predicate, not merely the `cwd'
            ;; column in SELECT.  It must also run before LIMIT so an
            ;; unrelated newer row cannot hide the matching project.
            (let ((where (string-match "WHERE[[:space:]]+" seen-sql))
                  (cwd (string-match "cwd[[:space:]]+IN[[:space:]]*(" seen-sql))
                  (limit (string-match "LIMIT[[:space:]]+" seen-sql)))
              (should where)
              (should cwd)
              (should limit)
              (should (< where cwd))
              (should (< cwd limit)))
            (should (equal (claudemacs-session-list-test--result-id-list rows)
                           '("matching-id")))))))))

(ert-deftest claudemacs-session-list-test-codex-provider-diagnostic-includes-resolved-database ()
  "Codex schema failures identify the resolved database path."
  :tags '(:unit :session-list :provider :errors :paths)
  (claudemacs-session-list-test--with-temp-directory dir
    (let ((database (expand-file-name "state_5.sqlite" dir)))
      (claudemacs-session-list-test--write database "readable placeholder")
      (claudemacs-session-list-test--reset-diagnostics)
      (cl-letf (((symbol-function 'claudemacs--session-list-codex-database-file)
                 (lambda () database))
                ((symbol-function 'claudemacs--session-list--sqlite-schema-columns)
                 (lambda (_database) nil)))
        (should-not (claudemacs--session-list-codex-history nil))
        (should (seq-some (lambda (warning)
                            (string-match-p (regexp-quote database) warning))
                          (claudemacs-session-list-test--warnings nil)))))))

(ert-deftest claudemacs-session-list-test-codex-multiline-context-is-safe ()
  "Newlines and delimiters in Codex context stay data, not row separators."
  :tags '(:unit :session-list :provider :privacy)
  (let* ((row (claudemacs-session-list-test--make-row
               :tool 'codex :session-id "codex-id" :identity 'exact
               :description "first line\nsecond line\u001fthird line"))
         (clean (claudemacs-session-list-test--call
                 '(claudemacs--session-list-clean-description
                   claudemacs--clean-session-description)
                 (plist-get row :description))))
    (should (stringp clean))
    (should-not (string-match-p "[\n\r]" clean))
    (should (<= (length clean)
               (or (and (boundp 'claudemacs-session-list-description-max-length)
                        claudemacs-session-list-description-max-length)
                   240)))
    ;; JSON transport keeps control characters inside a value; they must not
    ;; be interpreted as row delimiters.  Display only promises a bounded,
    ;; single-line preview here.
    (should (string-match-p "third line" clean))))

(ert-deftest claudemacs-session-list-test-context-is-bounded ()
  "Displayed context is a bounded, single-line preview."
  :tags '(:unit :session-list :privacy)
  (let* ((long (concat (make-string 400 ?x) "\nmore"))
         (clean (claudemacs-session-list-test--call
                 '(claudemacs--session-list-clean-description
                   claudemacs--clean-session-description)
                 long)))
    (should (<= (length clean)
               (or (and (boundp 'claudemacs-session-list-description-max-length)
                        claudemacs-session-list-description-max-length)
                   240)))
    (should-not (string-match-p "[\n\r]" clean))))

;;; Presentation and interaction

(ert-deftest claudemacs-session-list-test-same-basename-projects-stay-distinct ()
  "Two directories with the same basename remain distinguishable."
  :tags '(:unit :session-list :presentation :paths)
  (let ((first (claudemacs-session-list-test--make-live-row
                :tool 'claude :session-id "one" :identity 'exact
                :cwd "/tmp/one/app"))
        (second (claudemacs-session-list-test--make-live-row
                 :tool 'claude :session-id "two" :identity 'exact
                 :cwd "/tmp/two/app")))
    (with-temp-buffer
      (claudemacs-session-list-mode)
      (setq-local claudemacs--session-list-rows (list first second))
      (let* ((entries (claudemacs-session-list-test--call
                        '(claudemacs--session-list--entries)))
             (one (cadr (assoc "claude:one" entries)))
             (two (cadr (assoc "claude:two" entries))))
        (should (not (equal one two)))
        (should (string-match-p "one/app" (format "%S" one)))
        (should (string-match-p "two/app" (format "%S" two)))))))

(ert-deftest claudemacs-session-list-test-rendered-unknown-id-is-literal ()
  "The unknown identity cell renders the literal text `unknown'."
  :tags '(:unit :session-list :presentation :identity)
  (let ((row (claudemacs-session-list-test--make-live-row
              :tool 'codex :session-id nil :identity 'unknown
              :cwd "/tmp/unknown-project")))
    (with-temp-buffer
      (claudemacs-session-list-mode)
      (setq-local claudemacs--session-list-rows (list row))
      (let* ((entry (car (claudemacs--session-list--entries)))
             (cells (cadr entry))
             (id-cell (aref cells 2)))
        (should (equal (substring-no-properties id-cell) "unknown"))))))

(ert-deftest claudemacs-session-list-test-path-help-echo-retains-full-cwd ()
  "A shortened project cell carries the complete path as help-echo."
  :tags '(:unit :session-list :presentation :paths)
  (let* ((cwd "/very/long/project/path/that/must/remain/available")
         (row (claudemacs-session-list-test--make-live-row
               :cwd cwd :session-id "id" :identity 'exact))
         (value nil))
    (with-temp-buffer
      (claudemacs-session-list-mode)
      (setq-local claudemacs--session-list-rows (list row))
      (setq value (car (claudemacs-session-list-test--call
                        '(claudemacs--session-list--entries))))
      (let* ((cells (cdr value))
             (columns (append tabulated-list-format nil))
             (project-index
              (seq-position (mapcar #'car columns) "Project" #'equal))
             (project-cell (aref (car cells) project-index)))
        (should (equal (get-text-property 0 'help-echo project-cell) cwd))))))

(ert-deftest claudemacs-session-list-test-ret-visits-only-live-buffer ()
  "RET visits live rows and rechecks terminal liveness before visiting."
  :tags '(:unit :session-list :interaction)
  (let ((live (get-buffer-create "*claudemacs:claude:visit-test*"))
        (terminal-live t))
    (unwind-protect
        (with-temp-buffer
          (claudemacs-session-list-mode)
          (let ((row (claudemacs-session-list-test--make-live-row
                      :buffer live :session-id "live-id" :identity 'exact)))
            (setq-local claudemacs--session-list-rows (list row))
            (setq-local tabulated-list-entries
                        (claudemacs-session-list-test--call
                         '(claudemacs--session-list--entries)))
            (cl-letf (((symbol-function 'tabulated-list-get-id)
                       (lambda () (caar tabulated-list-entries)))
                      ((symbol-function 'claudemacs--terminal-live-p)
                       (lambda () terminal-live))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (buffer) (should (eq buffer live)))))
              (claudemacs-session-list-test--call
               '(claudemacs-session-list-visit
                 claudemacs--session-list-visit-row))))
          ;; A row can become stale between refresh and RET.  It must not be
          ;; visited merely because the buffer object still exists.
          (setq terminal-live nil)
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () (caar tabulated-list-entries)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _args)
                       (ert-fail "RET visited a dead terminal buffer"))))
            (should-error
             (claudemacs-session-list-test--call
              '(claudemacs-session-list-visit
                claudemacs--session-list-visit-row)))))
      (claudemacs-session-list-test--kill-buffers live))))

(ert-deftest claudemacs-session-list-test-bound-g-refresh-replaces-and-clears-rows ()
  "The bound `g' command replaces live rows and clears them when gone."
  :tags '(:unit :session-list :interaction :liveness)
  (let* ((table (get-buffer-create "*Claudemacs Sessions*"))
         (live (claudemacs-session-list-test--session-buffer
                "*claudemacs:claude:g-refresh*" "/tmp/g-refresh"
                'claude "g-refresh-id" 'exact t))
         (first-row
          (claudemacs--session-list--row
           (list :tool 'claude :session-id "g-refresh-id"
                 :identity 'exact :state 'live :source 'live-buffer
                 :cwd "/tmp/g-refresh" :buffer live)))
        (refresh-results nil))
    (unwind-protect
        (progn
          (setq refresh-results (list (list first-row) nil))
          (with-current-buffer table
            (claudemacs-session-list-mode)
            (should (eq (local-key-binding (kbd "g"))
                        #'claudemacs-session-list-refresh))
            (cl-letf (((symbol-function 'claudemacs--session-list-refresh-data)
                       (lambda () (pop refresh-results))))
              (call-interactively (local-key-binding (kbd "g")))
              (should (= (length claudemacs--session-list-rows) 1))
              (should (= (length tabulated-list-entries) 1))
              (call-interactively (local-key-binding (kbd "g")))
              (should-not claudemacs--session-list-rows)
              (should-not tabulated-list-entries))))
      (claudemacs-session-list-test--kill-buffers table live))))

(ert-deftest claudemacs-session-list-test-duplicate-live-identities-remain-visitable ()
  "Two live buffers resuming one exact ID have distinct table selection keys."
  :tags '(:unit :session-list :ui :identity)
  (let ((first (get-buffer-create "*claudemacs:codex:duplicate-one*"))
        (second (get-buffer-create "*claudemacs:codex-2:duplicate-two*"))
        visited)
    (unwind-protect
        (let* ((first-row (claudemacs--session-list--row
                           (list :tool 'codex :session-id "shared-id"
                                 :identity 'exact :state 'live :buffer first)))
               (second-row (claudemacs--session-list--row
                            (list :tool 'codex :session-id "shared-id"
                                  :identity 'exact :state 'live :buffer second)))
               (claudemacs--session-list-rows (list first-row second-row))
               (keys (mapcar #'car (claudemacs--session-list--entries))))
          (should (= (length (delete-dups (copy-sequence keys))) 2))
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () (cadr keys)))
                    ((symbol-function 'claudemacs--terminal-live-p)
                     (lambda () t))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _args) (setq visited buffer))))
            (claudemacs-session-list-visit)
            (should (eq visited second))))
      (claudemacs-session-list-test--kill-buffers first second))))

(ert-deftest claudemacs-session-list-test-duplicate-unknown-live-rows-remain-visitable ()
  "Two unknown live buffers remain separate rows with distinct UI keys."
  :tags '(:unit :session-list :ui :identity)
  (let ((first (claudemacs-session-list-test--session-buffer
                "*claudemacs:codex:unknown-one*" "/tmp/unknown-one"
                'codex nil 'unknown t))
        (second (claudemacs-session-list-test--session-buffer
                 "*claudemacs:codex-2:unknown-two*" "/tmp/unknown-two"
                 'codex nil 'unknown t))
        visited)
    (unwind-protect
        (let* ((first-row (claudemacs--session-list--row
                           (list :tool 'codex :identity 'unknown
                                 :state 'live :source 'live-buffer
                                 :buffer first)))
               (second-row (claudemacs--session-list--row
                            (list :tool 'codex :identity 'unknown
                                  :state 'live :source 'live-buffer
                                  :buffer second)))
               (claudemacs--session-list-rows (list first-row second-row))
               (keys (mapcar #'car (claudemacs--session-list--entries))))
          (should (= (length keys) 2))
          (should (= (length (delete-dups (copy-sequence keys))) 2))
          (should (equal (mapcar #'claudemacs-session-list-test--row-id
                                 claudemacs--session-list-rows)
                         '(nil nil)))
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () (car keys)))
                    ((symbol-function 'claudemacs--terminal-live-p)
                     (lambda () t))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _args) (setq visited buffer))))
            (claudemacs-session-list-visit)
            (should (eq visited first))))
      (claudemacs-session-list-test--kill-buffers first second))))

(ert-deftest claudemacs-session-list-test-public-command-uses-tabulated-list ()
  "The public command creates a tabulated list with refresh and visit keys."
  :tags '(:unit :session-list :interaction)
  (cl-letf (((symbol-function 'claudemacs--session-list-refresh-data)
             (lambda () nil)))
    (claudemacs-session-list)
    (unwind-protect
        (with-current-buffer (get-buffer "*Claudemacs Sessions*")
          (should (derived-mode-p 'tabulated-list-mode))
          (should (equal (mapcar #'car (append tabulated-list-format nil))
                         '("Workspace" "Tool instance" "Session ID" "Project")))
          (should (eq (local-key-binding (kbd "g"))
                      #'claudemacs-session-list-refresh))
          (should (eq (local-key-binding (kbd "RET"))
                      #'claudemacs-session-list-visit)))
      (when (get-buffer "*Claudemacs Sessions*")
        (kill-buffer "*Claudemacs Sessions*")))))

;;; Shutdown and lifecycle behavior

(ert-deftest claudemacs-session-list-test-no-shutdown-persistence ()
  "Loading/setup does not register session persistence at Emacs shutdown."
  :tags '(:unit :session-list :lifecycle)
  (should-not (memq #'claudemacs--save-session-snapshot-on-exit kill-emacs-hook))
  (should-not (memq #'claudemacs--session-list-refresh kill-emacs-hook))
  (should-not (fboundp 'claudemacs--save-session-snapshot-on-exit)))

(ert-deftest claudemacs-session-list-test-no-legacy-snapshot-variable ()
  "The prompt-bearing snapshot customization is removed."
  :tags '(:unit :session-list :privacy :lifecycle)
  (should-not (boundp 'claudemacs-session-snapshot-file))
  (should-not (fboundp 'claudemacs--write-session-snapshot))
  (should-not (fboundp 'claudemacs--read-session-snapshot)))

(provide 'claudemacs-session-list-test)
;;; claudemacs-session-list-test.el ends here
