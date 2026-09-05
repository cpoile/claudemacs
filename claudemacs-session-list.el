;;; claudemacs-session-list.el --- Live Claudemacs session list -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christopher Poile
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module deliberately contains no session-start or terminal lifecycle
;; code.  The overview adapts live Claudemacs buffers to a small row model and
;; renders only those rows with `tabulated-list-mode'.  The Claude and Codex
;; history adapters below remain available to lifecycle commands that need to
;; select an authoritative resume/branch ID; they are never queried by the
;; overview refresh.  In particular, a row's CWD and display recency are never
;; used to invent a session ID.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

;; SQLite support is optional.  Emacs 29 and later normally provide it, while
;; Emacs 28 users can use the direct sqlite3 fallback below.
(require 'sqlite nil t)

(declare-function claudemacs--terminal-live-p "claudemacs-terminal")
(declare-function claudemacs--is-claudemacs-buffer-p "claudemacs")
(declare-function claudemacs--list-all-sessions "claudemacs")
(declare-function claudemacs--get-session-info "claudemacs")
(declare-function claudemacs--format-tool-instance-name "claudemacs")
(declare-function claudemacs--get-workspace-name "claudemacs")

;;;; Customization

(defgroup claudemacs-session-list nil
  "Live session overview for Claudemacs."
  :group 'claudemacs)

(defcustom claudemacs-session-list-max-history 200
  "Maximum number of rows read from each CLI history store per provider call.

The cap keeps a locked or unexpectedly large history database from making a
provider query unbounded.  History providers are lifecycle-only adapters; the
live session-list refresh never reads either store."
  :type 'integer
  :group 'claudemacs-session-list)

(defcustom claudemacs-session-list-max-claude-index-bytes (* 8 1024 1024)
  "Maximum size of one Claude sessions index read during a refresh.

The Claude index is parsed as one JSON document, so a byte guard must happen
before reading it.  An index larger than this limit is skipped with a
diagnostic; this keeps a corrupt or unexpectedly large file from making the
otherwise synchronous session-list refresh unbounded.  Set to nil to disable
the guard explicitly."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'claudemacs-session-list)

(defcustom claudemacs-session-list-max-claude-entries-per-index 10000
  "Maximum number of Claude index entries inspected in one refresh.

This is a second guard after the byte limit.  The first entries are
processed because Claude normally writes its index newest-first; when the
limit is reached the remainder is skipped and a diagnostic makes the partial
result visible.  Set to nil to inspect every entry in an accepted index."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'claudemacs-session-list)

(defcustom claudemacs-session-list-max-claude-index-files 256
  "Maximum number of Claude session-index files processed per provider call.

The directory scan may find more files than this limit, but only the first
bounded set is read and parsed.  A diagnostic makes the partial result
visible.  Set to nil to process every discovered index file."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'claudemacs-session-list)

(defcustom claudemacs-session-list-max-claude-index-directories 4096
  "Maximum number of Claude history directories visited per provider call.

The index walker stops before entering additional directories once this finite
budget is exhausted, and reports that the result may be partial.  Set to nil
to disable the directory budget explicitly."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'claudemacs-session-list)

(defcustom claudemacs-session-list-description-max-length 240
  "Maximum number of characters retained for a displayed session context.

The session list intentionally keeps only a bounded preview in memory.  It
does not put complete prompts into `help-echo' or into Claudemacs-owned
storage."
  :type 'integer
  :group 'claudemacs-session-list)

(defconst claudemacs--session-list-unknown-label "unknown")
(defconst claudemacs--session-list-json-null (make-symbol "json-null"))
(defconst claudemacs--session-list-json-false (make-symbol "json-false"))

(defun claudemacs--session-list--history-limit ()
  "Return a safe positive history row cap."
  (max 1 (if (numberp claudemacs-session-list-max-history)
             claudemacs-session-list-max-history
           200)))

(defvar claudemacs--session-list-last-diagnostics nil
  "Diagnostics collected by the most recent session-list refresh.

Each element is a short human-readable string.  Provider errors are kept
separate from rows so one unavailable history source cannot hide live rows or
rows from the other source.")

(defvar claudemacs-session-list-last-diagnostics nil
  "Public copy of diagnostics from the most recent session-list refresh.")

(defvar-local claudemacs--session-list-rows nil)

;;;; Small data helpers

(defun claudemacs--session-list--diagnostic (format-string &rest args)
  "Add a concise diagnostic formatted with FORMAT-STRING and ARGS."
  (let ((text (apply #'format format-string args)))
    (push text claudemacs--session-list-last-diagnostics)
    (setq claudemacs-session-list-last-diagnostics
          (delete-dups
           (cons text claudemacs-session-list-last-diagnostics)))
    text))

(defun claudemacs--session-list--reset-diagnostics ()
  "Reset diagnostics for a new refresh."
  (setq claudemacs--session-list-last-diagnostics nil
        claudemacs-session-list-last-diagnostics nil))

(defun claudemacs--session-list--finish-diagnostics ()
  "Publish diagnostics in stable order after a refresh."
  (setq claudemacs--session-list-last-diagnostics
        (delete-dups (nreverse claudemacs--session-list-last-diagnostics))
        claudemacs-session-list-last-diagnostics
        (copy-sequence claudemacs--session-list-last-diagnostics)))

(defun claudemacs--session-list--string (value)
  "Return VALUE when it is a non-empty string, otherwise nil."
  (when (and (stringp value) (not (string-empty-p (string-trim value))))
    value))

(defconst claudemacs--session-list--history-id-regexp
  "\\`[A-Za-z0-9_.][A-Za-z0-9_.:-]*\\'"
  "Regexp for the ordinary ASCII IDs accepted from history providers.

History IDs are later used as completion/display keys and can be passed to
provider CLIs during lifecycle discovery.  Keep this allow-list deliberately
small: it excludes leading hyphens, whitespace, controls, bidi text, and other
display/option-spoofing characters while accepting UUID-like IDs and the
ordinary provider prefixes used by Claude and Codex.")

(defun claudemacs--session-list--normalize-history-id (value)
  "Return safe canonical history ID VALUE, or nil.

This is the single history-ID validation boundary for Claude and Codex.  Do
not trim or otherwise repair malformed IDs: whitespace and punctuation that
could become a CLI option or spoof a completion key must be rejected."
  (when (and (stringp value)
             (string-match-p claudemacs--session-list--history-id-regexp value))
    value))

(defun claudemacs--clean-session-description (text)
  "Turn TEXT into a bounded, compact, single-line session description.

This function is kept under the historical helper name because it is useful to
other Claudemacs code and existing configurations.  It never returns prompt
text beyond `claudemacs-session-list-description-max-length'."
  (when (stringp text)
    (let* ((clean (string-trim
                   (replace-regexp-in-string "[[:space:]\n\r]+" " " text)))
           (limit (max 1 (if (numberp claudemacs-session-list-description-max-length)
                             claudemacs-session-list-description-max-length
                           240))))
      (unless (string-empty-p clean)
        (if (> (length clean) limit)
            (concat (substring clean 0 (max 0 (- limit 3))) "...")
          clean)))))

(defun claudemacs--session-list--description-from-values (&rest values)
  "Return the first non-empty cleaned description in VALUES."
  (seq-some #'claudemacs--clean-session-description values))

(defun claudemacs--session-list--canonical-path (path)
  "Return PATH as an absolute canonical-ish path, or nil.

`file-truename' is useful for matching paths but can fail for an inaccessible
or concurrently removed directory.  In that case the expanded absolute path
is still a safe display/filter value."
  (when (claudemacs--session-list--string path)
    (let ((expanded (expand-file-name path)))
      (condition-case nil
          (file-name-as-directory
           (directory-file-name (file-truename expanded)))
        (error (file-name-as-directory (directory-file-name expanded)))))))

(defun claudemacs--session-list--path-under-directory-p (path directory)
  "Return non-nil when canonical PATH is a strict child of DIRECTORY.

Both arguments are treated as filesystem paths, not display strings.  The
separator-aware prefix check avoids confusing sibling names such as
`projects-old' with a child of `projects'."
  (let* ((root (file-name-as-directory
                (directory-file-name (expand-file-name directory))))
         (candidate (file-name-as-directory
                     (directory-file-name (expand-file-name path)))))
    (and (not (equal root candidate))
         (string-prefix-p root candidate))))

(defun claudemacs--session-list--same-path-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same CWD."
  (or (and (null left) (null right))
      (and left right
           (equal (directory-file-name
                   (claudemacs--session-list--canonical-path left))
                  (directory-file-name
                   (claudemacs--session-list--canonical-path right))))))

(defun claudemacs--session-list--time (value)
  "Convert a supported VALUE to an Emacs time value, or nil.

Numeric values at millisecond precision are converted to seconds.  ISO-like
strings are parsed with `date-to-time'.  Invalid timestamps are ignored rather
than becoming misleading epoch dates."
  (condition-case nil
      (cond
       ((null value) nil)
       ((and (numberp value) (> (abs value) 100000000000))
        (seconds-to-time (/ (float value) 1000.0)))
       ((numberp value) (seconds-to-time value))
       ((stringp value)
        (if (string-match-p "\\`[ \\t]*[0-9]+\\(?:\\.[0-9]+\\)?[ \\t]*\\'"
                           value)
            (claudemacs--session-list--time (string-to-number value))
          (date-to-time value)))
       (t nil))
    (error nil)))

(defun claudemacs--session-list--time< (left right)
  "Return non-nil when time LEFT is older than time RIGHT."
  (and left right
       (condition-case nil
           (time-less-p left right)
         (error (< (float-time left) (float-time right))))))

(defun claudemacs--session-list--timestamp-value (&rest values)
  "Return the first valid timestamp value in VALUES.

Codex has used both numeric millisecond/second fields and ISO-like text
timestamps across schema versions.  Keep the original value so the row
normalizer can parse it once while accepting either representation."
  (seq-some
   (lambda (value)
     (let ((time (claudemacs--session-list--time value)))
       (when (and time (> (float-time time) 0))
         value)))
   values))

(defun claudemacs--session-list--newer-first-p (left right)
  "Return non-nil when timestamp LEFT should sort before RIGHT."
  (cond
   ((and left right)
    (not (claudemacs--session-list--time< left right)))
   (left t)
   (t nil)))

(defun claudemacs--session-list--tool-symbol (tool)
  "Normalize TOOL to a non-empty symbol, or nil."
  (cond
   ((symbolp tool) (unless (eq tool :null) tool))
   ((stringp tool)
    (let ((value (string-trim tool)))
      (unless (string-empty-p value) (intern (downcase value)))))
   (t nil)))

(defun claudemacs--session-list--json-value (alist key)
  "Read KEY from JSON ALIST, accepting symbol or string keys."
  (cond
   ((hash-table-p alist)
    (or (gethash key alist)
        (gethash (symbol-name key) alist)))
   (t
    (or (alist-get key alist)
        (alist-get (symbol-name key) alist nil nil #'equal)))))

(defun claudemacs--session-list--json-member-p (object key)
  "Return non-nil when JSON OBJECT contains KEY, even when its value is nil."
  (cond
   ((hash-table-p object)
    (let ((missing :claudemacs-json-missing))
      (or (not (eq (gethash key object missing) missing))
          (not (eq (gethash (symbol-name key) object missing) missing)))))
   ((listp object)
    (or (assq key object)
        (assoc key object)
        (assoc (symbol-name key) object)))
   (t nil)))

(defun claudemacs--session-list--json-object-p (value)
  "Return non-nil when VALUE looks like a JSON object alist."
  (or (hash-table-p value)
      (and (listp value)
           (or (null value)
               (consp (car value))))))

(defun claudemacs--session-list--parse-json-buffer
    (object-type array-type null-object false-object)
  "Parse JSON at point with types compatible with Emacs 26.1.

OBJECT-TYPE, ARRAY-TYPE, NULL-OBJECT, and FALSE-OBJECT have the same meaning
as the corresponding `json-parse-buffer' keyword arguments.  Emacs 27 and
newer use the native parser; Emacs 26 falls back to `json-read' with its
dynamically bound representation variables."
  (if (fboundp 'json-parse-buffer)
      (json-parse-buffer :object-type object-type
                         :array-type array-type
                         :null-object null-object
                         :false-object false-object)
    (let ((json-object-type object-type)
          (json-array-type array-type)
          (json-null null-object)
          (json-false false-object))
      (goto-char (point-min))
      (json-read))))

(defun claudemacs--session-list--parse-json-string
    (string object-type array-type null-object false-object)
  "Parse STRING as JSON using the Emacs 26-compatible representation contract."
  (if (fboundp 'json-parse-string)
      (json-parse-string string
                         :object-type object-type
                         :array-type array-type
                         :null-object null-object
                         :false-object false-object)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (claudemacs--session-list--parse-json-buffer
       object-type array-type null-object false-object))))

(defun claudemacs--session-list--row-key (row)
  "Return a stable authoritative identity key for ROW, or nil when unknown.

Unknown rows deliberately have no identity key.  The UI key is added
separately so two live buffers remain independently visitable."
  (let ((tool (claudemacs--session-list--tool-symbol (plist-get row :tool)))
        (id (claudemacs--session-list--string (plist-get row :session-id)))
        (identity (plist-get row :identity)))
    (when (and tool id (memq identity '(exact discovered)))
      (format "%s:%s" tool id))))

(defun claudemacs--session-list--display-instance (tool instance)
  "Return a displayable tool-instance name for TOOL and INSTANCE."
  (cond
   ((claudemacs--session-list--string instance) instance)
   ((and tool (fboundp 'claudemacs--format-tool-instance-name))
    (condition-case nil
        (claudemacs--format-tool-instance-name
         tool (if (numberp instance) instance 1))
      (error (symbol-name tool))))
   (tool (symbol-name tool))
   (t nil)))

(defun claudemacs--session-list--row (plist)
  "Normalize PLIST into the documented session-list row contract."
  (let* ((tool (claudemacs--session-list--tool-symbol (plist-get plist :tool)))
         (raw-id (claudemacs--session-list--string
                  (plist-get plist :session-id)))
         (raw-identity (if (memq (plist-get plist :identity)
                                  '(exact discovered unknown))
                           (plist-get plist :identity)
                         'unknown))
         ;; An ID with unknown provenance is not an identity.  The same safe
         ;; token boundary applies to live and historical rows, so a malformed
         ;; exact/discovered ID renders as unknown rather than becoming a
         ;; completion key or CLI argument.
         (id (and (not (eq raw-identity 'unknown))
                  (claudemacs--session-list--normalize-history-id raw-id)))
         (identity (if id raw-identity 'unknown))
         (buffer (and (bufferp (plist-get plist :buffer))
                      (plist-get plist :buffer)))
         (cwd (claudemacs--session-list--canonical-path
               (plist-get plist :cwd))))
    (list :tool tool
          :session-id id
          :identity identity
          :workspace (claudemacs--session-list--string
                      (plist-get plist :workspace))
          :instance (claudemacs--session-list--string
                     (plist-get plist :instance))
          :cwd cwd
          :updated-at (claudemacs--session-list--time
                       (plist-get plist :updated-at))
          :description (claudemacs--clean-session-description
                        (plist-get plist :description))
          :state (if (memq (plist-get plist :state) '(live exited history))
                     (plist-get plist :state)
                   'history)
          :buffer buffer
          :source (plist-get plist :source))))

;;;; Live-buffer provider

(defun claudemacs--session-list--buffer-local (buffer variable)
  "Return BUFFER's local VARIABLE value when available."
  (when (and (buffer-live-p buffer) (boundp variable))
    (condition-case nil
        (buffer-local-value variable buffer)
      (error nil))))

(defun claudemacs--session-list--buffer-identity (buffer)
  "Return BUFFER's authoritative ID and provenance as a cons cell.

The tool-neutral variables are preferred.  Legacy Claude UUID state remains
supported until all callers have migrated.  Buffer names and workspace names
are intentionally not considered IDs.  IDs that fail the shared safe-token
allow-list are treated as unknown."
  (let* ((tool-neutral-id (or (claudemacs--session-list--buffer-local
                               buffer 'claudemacs--authoritative-session-id)
                              (claudemacs--session-list--buffer-local
                               buffer 'claudemacs--session-id)
                              (claudemacs--session-list--buffer-local
                               buffer 'claudemacs--codex-session-id)
                              (claudemacs--session-list--buffer-local
                               buffer 'claudemacs--tracked-session-id)))
         (legacy-id (claudemacs--session-list--buffer-local
                     buffer 'claudemacs--claude-session-uuid))
         (tool (claudemacs--session-list--tool-symbol
                (claudemacs--session-list--buffer-local buffer 'claudemacs--tool)))
         (id (or tool-neutral-id legacy-id))
         (raw-provenance (or (claudemacs--session-list--buffer-local
                              buffer 'claudemacs--session-id-provenance)
                             (claudemacs--session-list--buffer-local
                              buffer 'claudemacs--session-identity)
                             (claudemacs--session-list--buffer-local
                              buffer 'claudemacs--identity-provenance)))
         (provenance (if (memq raw-provenance '(exact discovered unknown))
                         raw-provenance
                       'unknown))
         (safe-id (claudemacs--session-list--normalize-history-id id)))
    (when safe-id
      (cons safe-id
            (if (and legacy-id (null tool-neutral-id)
                     (or (null tool) (eq tool 'claude)))
                'exact
              provenance)))))

(defun claudemacs--session-list--buffer-name-info (buffer)
  "Return tool, instance, and workspace parsed from BUFFER's display name.

This is only metadata parsing.  The final name component is a workspace label
for the current Claudemacs naming convention and is never treated as a CLI
session ID."
  (when (and (buffer-live-p buffer)
             (string-match
              "^\\*claudemacs:\\([^:-]+\\)\\(?:-\\([0-9]+\\)\\)?:\\(.+\\)\\*$"
              (buffer-name buffer)))
    (list :tool (intern (match-string 1 (buffer-name buffer)))
          :instance (if (match-string 2 (buffer-name buffer))
                        (string-to-number (match-string 2 (buffer-name buffer)))
                      1)
          :workspace (match-string 3 (buffer-name buffer)))))

(defun claudemacs--session-list--buffer-tool-and-instance (buffer)
  "Return BUFFER's tool and display instance as a two-element list."
  (let* ((name-info (claudemacs--session-list--buffer-name-info buffer))
         (tool (or (claudemacs--session-list--buffer-local
                    buffer 'claudemacs--tool)
                   (plist-get name-info :tool)))
         (instance (or (claudemacs--session-list--buffer-local
                        buffer 'claudemacs--tool-instance)
                       (plist-get name-info :instance))))
    (when (fboundp 'claudemacs--get-session-info)
      (condition-case nil
          (let ((info (claudemacs--get-session-info buffer)))
            (when (listp info)
              (setq tool (or tool (plist-get info :tool))
                    instance (or instance (plist-get info :instance)))))
        (error nil)))
    (list (claudemacs--session-list--tool-symbol tool)
          (claudemacs--session-list--display-instance
           (claudemacs--session-list--tool-symbol tool) instance))))

(defun claudemacs--session-list--buffer-workspace (buffer)
  "Return BUFFER's workspace label without treating it as an identity."
  (or (claudemacs--session-list--buffer-local buffer 'claudemacs--workspace-name)
      (claudemacs--session-list--buffer-local buffer 'claudemacs--workspace)
      (claudemacs--session-list--buffer-local buffer 'claudemacs--session-workspace)
      (when (fboundp 'claudemacs--get-session-info)
        (condition-case nil
            (let ((info (claudemacs--get-session-info buffer)))
              (and (listp info) (plist-get info :workspace)))
          (error nil)))
      ;; Current Claudemacs versions expose this label as :session-id in the
      ;; existing info helper.  It is a workspace/project label, not a CLI ID.
      (when (fboundp 'claudemacs--get-session-info)
        (condition-case nil
            (let ((info (claudemacs--get-session-info buffer)))
              (and (listp info) (plist-get info :session-id)))
          (error nil)))
      (plist-get (claudemacs--session-list--buffer-name-info buffer)
                 :workspace)
      (when (and (buffer-live-p buffer)
                 (fboundp 'claudemacs--get-workspace-name))
        (with-current-buffer buffer
          (condition-case nil
              (claudemacs--get-workspace-name)
            (error nil))))))

(defun claudemacs--session-list--buffer-cwd (buffer)
  "Return BUFFER's absolute working directory, or nil."
  (or (claudemacs--session-list--canonical-path
       (claudemacs--session-list--buffer-local buffer 'claudemacs--cwd))
      (when (buffer-live-p buffer)
        (claudemacs--session-list--canonical-path
         (buffer-local-value 'default-directory buffer)))))

(defun claudemacs--session-list--buffer-live-p (buffer)
  "Return whether BUFFER's terminal backend is currently live."
  (when (buffer-live-p buffer)
    (condition-case error-data
        (with-current-buffer buffer
          (if (fboundp 'claudemacs--terminal-live-p)
              (and (claudemacs--terminal-live-p) t)
            (let ((process (or (and (boundp 'claudemacs--terminal-process)
                                    claudemacs--terminal-process)
                               (and (boundp 'claudemacs--process)
                                    claudemacs--process))))
              (and (processp process) (process-live-p process)))))
      (error
       (claudemacs--session-list--diagnostic
        "Live session liveness check failed for %s: %s"
        (or (buffer-name buffer) "<unnamed>")
        (error-message-string error-data))
       nil))))

(defun claudemacs--session-list--fallback-buffers ()
  "Return Claudemacs-named buffers when the primary enumerator is unavailable."
  (condition-case error-data
      (seq-filter
       (lambda (buffer)
         (and (buffer-live-p buffer)
              (string-match-p "^\\*claudemacs:" (buffer-name buffer))))
       (buffer-list))
    (error
     (claudemacs--session-list--diagnostic
      "Live session fallback enumeration failed: %s"
      (error-message-string error-data))
     nil)))

(defun claudemacs--session-list-live-buffer-rows ()
  "Return normalized rows for every live Claudemacs session buffer.

The provider does not query either history store.  Buffers whose terminal
backend is dead are omitted, even when the buffer itself remains alive.  The
terminal facade is the liveness authority for both Eat and Ghostel."
  (let* ((buffers
          (if (fboundp 'claudemacs--list-all-sessions)
              (condition-case error-data
                  (let ((listed (claudemacs--list-all-sessions)))
                    (if (listp listed)
                        listed
                      (claudemacs--session-list--diagnostic
                       "Live session enumeration returned invalid data")
                      (claudemacs--session-list--fallback-buffers)))
                (error
                 ;; A broken enumerator must not hide healthy buffers that the
                 ;; conservative fallback can still observe.
                 (claudemacs--session-list--diagnostic
                  "Live session enumeration failed: %s"
                  (error-message-string error-data))
                 (claudemacs--session-list--fallback-buffers)))
            (claudemacs--session-list--fallback-buffers)))
         (rows nil))
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (condition-case error-data
            (let* ((tool-and-instance
                    (claudemacs--session-list--buffer-tool-and-instance buffer))
                   (tool (nth 0 tool-and-instance))
                   (instance (nth 1 tool-and-instance))
                   (identity (claudemacs--session-list--buffer-identity buffer))
                   (live (claudemacs--session-list--buffer-live-p buffer)))
              (when live
                (push
                 (claudemacs--session-list--row
                  (list :tool tool
                        :session-id (car identity)
                        :identity (or (cdr identity) 'unknown)
                        :workspace
                        (claudemacs--session-list--buffer-workspace buffer)
                        :instance instance
                        :cwd (claudemacs--session-list--buffer-cwd buffer)
                        :state 'live
                        :buffer buffer
                        :source 'live-buffer))
                 rows)))
          (error
           ;; Metadata for one malformed/stale buffer must not discard rows
           ;; from other live sessions.
           (claudemacs--session-list--diagnostic
            "Live session buffer skipped (%s): %s"
            (or (buffer-name buffer) "<unnamed>")
            (error-message-string error-data))))))
    (nreverse rows)))

;;;; Claude history provider

(defun claudemacs--session-list-claude-config-dir ()
  "Resolve Claude's configured history root."
  (let ((configured (or (claudemacs--session-list--string
                        (getenv "CLAUDE_CONFIG_DIR"))
                        (and (boundp 'claudemacs-claude-config-dir)
                             (claudemacs--session-list--string
                              claudemacs-claude-config-dir)))))
    (file-name-as-directory
     (expand-file-name (or configured "~/.claude")))))

(defun claudemacs--session-list--claude-index-file-status
    (index-file projects)
  "Return t when INDEX-FILE is a safe regular child of PROJECTS.

The index discovery pass is intentionally not trusted at read time: a queued
path may have been replaced between traversal and consumption.  Return a
concise reason symbol for callers to diagnose and skip unsafe paths."
  (condition-case nil
      (cond
       ((file-symlink-p index-file) 'symlink)
       ((not (file-regular-p index-file)) 'not-regular)
       ((not
         (claudemacs--session-list--path-under-directory-p
          (file-truename index-file)
          (file-truename projects)))
        'outside-projects)
       (t t))
    (error 'unresolvable)))

(defun claudemacs--session-list--claude-index-status-message (status)
  "Return a concise diagnostic explanation for Claude index STATUS."
  (cond
   ((eq status 'symlink) "path is a symlink")
   ((eq status 'not-regular) "path is not a regular file")
   ((eq status 'outside-projects) "path is outside the projects root")
   ((eq status 'unresolvable) "path could not be verified")
   (t "path is unsafe")))

(defun claudemacs--session-list--claude-index-files ()
  "Return bounded Claude session index paths under the history root.

Walk one directory at a time instead of asking
`directory-files-recursively' to materialize the complete result.  Symlinked
directories and files are skipped, and canonical directory keys prevent
cycles when a filesystem presents an alias to an already visited directory."
  (let* ((root (claudemacs--session-list-claude-config-dir))
         (projects (expand-file-name "projects" root))
         (file-limit
          (when (numberp claudemacs-session-list-max-claude-index-files)
            (max 1 claudemacs-session-list-max-claude-index-files)))
         (directory-limit
          (when (numberp claudemacs-session-list-max-claude-index-directories)
            (max 1 claudemacs-session-list-max-claude-index-directories)))
         (directories (and (file-directory-p projects)
                           (not (file-symlink-p projects))
                           (list projects)))
         (visited (make-hash-table :test #'equal))
         (files nil)
         (directories-visited 0)
         (file-limit-reached nil)
         (directory-limit-reached nil))
    (cond
     ((not (file-directory-p projects))
      (claudemacs--session-list--diagnostic
       "Claude history unavailable: %s (resolved from CLAUDE_CONFIG_DIR or ~/.claude)"
       projects)
      nil)
     ((file-symlink-p projects)
      (claudemacs--session-list--diagnostic
       "Claude history unavailable: symlinked projects directory skipped: %s"
       projects)
      nil)
     (t
      (while (and directories
                  (not file-limit-reached)
                  (not directory-limit-reached)
                  (or (null file-limit) (< (length files) file-limit))
                  (or (null directory-limit)
                      (< directories-visited directory-limit)))
        (let ((directory (pop directories)))
          (unless (file-symlink-p directory)
            (condition-case error-data
                (let ((key
                       (condition-case nil
                           (file-truename directory)
                         (error (expand-file-name directory)))))
                  (unless (gethash key visited)
                    (puthash key t visited)
                    (setq directories-visited (1+ directories-visited))
                    (dolist
                        (entry (reverse
                                (directory-files
                                 directory t directory-files-no-dot-files-regexp)))
                      (unless (or file-limit-reached directory-limit-reached)
                        (cond
                         ((and (file-directory-p entry)
                               (not (file-symlink-p entry)))
                          (if (and file-limit (>= (length files) file-limit))
                              (setq file-limit-reached t)
                            (push entry directories)))
                         ((and (not (file-symlink-p entry))
                               (not (file-directory-p entry))
                               (string= (file-name-nondirectory entry)
                                        "sessions-index.json"))
                          (if (and file-limit (>= (length files) file-limit))
                              (setq file-limit-reached t)
                            (push entry files)))))
                    ;; If an earlier entry queued another directory, the file
                    ;; cap has unseen work even when this directory had no
                    ;; additional matching file.
                    (when (and file-limit
                               (>= (length files) file-limit)
                               directories)
                      (setq file-limit-reached t))
                    ;; With no file-cap truncation, the directory budget is
                    ;; the distinct reason for leaving pending directories.
                    (when (and (not file-limit-reached)
                               directory-limit
                               (>= directories-visited directory-limit)
                               directories)
                      (setq directory-limit-reached t)))))
              (error
               (claudemacs--session-list--diagnostic
                "Claude history scan failed at %s: %s"
                directory (error-message-string error-data)))))))
      ;; This also covers a stop at the loop condition after a directory scan
      ;; consumed the last budgeted visit.
      (when (and (not file-limit-reached)
                 directory-limit
                 (>= directories-visited directory-limit)
                 directories)
        (setq directory-limit-reached t))
      (when file-limit-reached
        (claudemacs--session-list--diagnostic
         "Claude history index-file limit reached; additional files were not scanned"))
      (when directory-limit-reached
        (claudemacs--session-list--diagnostic
         "Claude history directory limit reached; additional directories were not scanned"))
      (nreverse files)))))

(defun claudemacs--session-list--claude-project-path (entry index-file)
  "Get ENTRY's project path, with a conservative INDEX-FILE fallback."
  (or (claudemacs--session-list--string
       (claudemacs--session-list--json-value entry 'projectPath))
      (claudemacs--session-list--string
       (claudemacs--session-list--json-value entry 'cwd))
      ;; A project key is not reliably reversible (hyphens can be literal),
      ;; so do not manufacture a path from it.  Keep nil rather than showing a
      ;; misleading project.
      (ignore index-file nil)))

(defun claudemacs--session-list--claude-entry-row (entry index-file)
  "Convert one validated Claude history ENTRY to a normalized row."
  (let ((id (claudemacs--session-list--normalize-history-id
             (claudemacs--session-list--json-value entry 'sessionId)))
        (project (claudemacs--session-list--claude-project-path
                  entry index-file)))
    (when (and id (or (null project) (stringp project)))
      (claudemacs--session-list--row
       (list :tool 'claude
             :session-id id
             ;; The history store owns this ID, so the row's identity is
             ;; authoritative even though no live buffer is attached to it.
             :identity 'exact
             :cwd project
             :updated-at
             (or (claudemacs--session-list--json-value entry 'modified)
                 (claudemacs--session-list--json-value entry 'updatedAt))
             :description
             (claudemacs--session-list--description-from-values
              (claudemacs--session-list--json-value entry 'summary)
              (claudemacs--session-list--json-value entry 'firstPrompt))
             :state 'history
             :source 'claude-history)))))

(defun claudemacs--session-list--claude-index-byte-limit ()
  "Return a safe Claude index byte limit, or nil when disabled."
  (when (numberp claudemacs-session-list-max-claude-index-bytes)
    (max 1 claudemacs-session-list-max-claude-index-bytes)))

(defun claudemacs--session-list--claude-entry-limit ()
  "Return a safe Claude per-index entry limit, or nil when disabled."
  (when (numberp claudemacs-session-list-max-claude-entries-per-index)
    (max 1 claudemacs-session-list-max-claude-entries-per-index)))

(defun claudemacs--session-list--bounded-history-add (row rows)
  "Add ROW to newest-first ROWS without exceeding the history row cap.

The bounded list is sorted as it grows, so a large Claude index never builds a
second unbounded collection merely to sort it at the end of a refresh."
  (let ((sorted (sort (cons row rows)
                      (lambda (left right)
                        (claudemacs--session-list--newer-first-p
                         (plist-get left :updated-at)
                         (plist-get right :updated-at))))))
    (seq-take sorted (claudemacs--session-list--history-limit))))

(defun claudemacs--session-list--claude-index-entries (entries function)
  "Call FUNCTION for each bounded Claude ENTRIES value.

Return non-nil when the per-index entry limit truncated ENTRIES.  Claude
indexes normally arrive as lists; a vector remains accepted for callers that
provide already-decoded data."
  (let ((limit (claudemacs--session-list--claude-entry-limit))
        (count 0)
        (truncated nil))
    (cond
     ((vectorp entries)
      (let ((length (length entries))
            (end (if limit (min (length entries) limit)
                   (length entries))))
        (dotimes (index end)
          (funcall function (aref entries index))
          (setq count (1+ count)))
        (setq truncated (and limit (> length end)))))
     ((and (listp entries) entries)
      (let ((remaining entries))
        (while (and remaining (or (null limit) (< count limit)))
          (funcall function (car remaining))
          (setq count (1+ count)
                remaining (cdr remaining)))
        (setq truncated (and limit remaining))))
     (t
      ;; A nil list is a valid empty array.  The parser's private null
      ;; sentinel is rejected by the caller before this helper is reached.
      nil))
    truncated))

(defun claudemacs--session-list-claude-history (&optional cwd)
  "Read all valid Claude history rows once, optionally filtered to CWD.

Malformed index files are diagnosed independently; valid files continue to
contribute rows.  The returned IDs come directly from Claude's
`sessionId' fields and are never selected by recency."
  (let* ((projects (expand-file-name
                    "projects"
                    (claudemacs--session-list-claude-config-dir)))
         (rows nil))
    (dolist (index-file (claudemacs--session-list--claude-index-files))
      ;; Revalidate queued paths immediately before every read.  Discovery is
      ;; only a candidate list: a path can be replaced or moved while this
      ;; provider is processing an earlier index.
      (let ((status (claudemacs--session-list--claude-index-file-status
                     index-file projects)))
        (if (not (eq status t))
            (claudemacs--session-list--diagnostic
             "Claude index skipped (%s): %s"
             (claudemacs--session-list--claude-index-status-message status)
             index-file)
          (let ((byte-limit
                 (claudemacs--session-list--claude-index-byte-limit)))
            (condition-case error-data
                (with-temp-buffer
              ;; Read no more than LIMIT+1 bytes in a unibyte buffer.  The
              ;; extra byte is the overflow sentinel and avoids the racy
              ;; file-attributes-then-read size check.
              (if byte-limit
                  (progn
                    (set-buffer-multibyte nil)
                    (let ((coding-system-for-read 'binary))
                      (insert-file-contents-literally
                       index-file nil 0 (1+ byte-limit))))
                (insert-file-contents index-file))
              (if (and byte-limit (> (buffer-size) byte-limit))
                  (claudemacs--session-list--diagnostic
                   "Claude index skipped (exceeds %s-byte limit): %s"
                   byte-limit index-file)
                ;; Hash tables plus a private null sentinel retain the
                ;; distinction between an empty object/array and JSON null.
                ;; This is important here: {} and null are malformed indexes,
                ;; while an entries empty array is a valid empty index.
                (let* ((object
                        (claudemacs--session-list--parse-json-buffer
                         'hash-table 'list
                         claudemacs--session-list-json-null
                         claudemacs--session-list-json-false))
                       (has-entries
                        (and (claudemacs--session-list--json-object-p object)
                             (claudemacs--session-list--json-member-p
                              object 'entries)))
                       (entries (and has-entries
                                      (claudemacs--session-list--json-value
                                       object 'entries))))
                  (cond
                   ((not (hash-table-p object))
                    (claudemacs--session-list--diagnostic
                     "Claude index ignored (root is not an object): %s"
                     index-file))
                   ((not has-entries)
                    (claudemacs--session-list--diagnostic
                     "Claude index ignored (entries is missing): %s"
                     index-file))
                   ((not (or (vectorp entries)
                             (listp entries)))
                    (claudemacs--session-list--diagnostic
                     "Claude index ignored (entries is not an array): %s"
                     index-file))
                   (t
                    (when
                        (claudemacs--session-list--claude-index-entries
                         entries
                         (lambda (entry)
                           (if (not
                                (claudemacs--session-list--json-object-p entry))
                               (claudemacs--session-list--diagnostic
                                "Claude index entry ignored (not an object): %s"
                                index-file)
                             (let ((row
                                    (claudemacs--session-list--claude-entry-row
                                     entry index-file)))
                               (if (null row)
                                   (claudemacs--session-list--diagnostic
                                    "Claude index entry ignored (missing or unsafe sessionId): %s"
                                    index-file)
                                 (when (or (null cwd)
                                           (claudemacs--session-list--same-path-p
                                            cwd (plist-get row :cwd)))
                                   (setq rows
                                         (claudemacs--session-list--bounded-history-add
                                          row rows))))))))
                      (claudemacs--session-list--diagnostic
                       "Claude index entry limit reached; older entries skipped: %s"
                       index-file)))))))
          (error
           (claudemacs--session-list--diagnostic
            "Claude index ignored (%s): %s"
            index-file (error-message-string error-data))))))))
    ;; Duplicate authoritative records collapse to the newest valid record.
    (let ((sorted (sort rows (lambda (left right)
                               (claudemacs--session-list--newer-first-p
                                (plist-get left :updated-at)
                                (plist-get right :updated-at))))))
      (seq-take
       (claudemacs--session-list--dedupe-history-rows sorted)
       (claudemacs--session-list--history-limit)))))

(defun claudemacs--session-list-history-rows (&optional tool cwd)
  "Return authoritative history rows for TOOL and optional CWD.

This is the small lifecycle/provider boundary used by session startup's
bounded identity discovery.  The result is a plist so provider failures can
be represented without ever being mistaken for a history row:

  (:rows ROWS)

When TOOL is nil both supported providers are queried once.  Each focused
provider remains available separately for the overview refresh and tests."
  (let* ((diagnostics-before
          (copy-sequence
           (or claudemacs-session-list-last-diagnostics
               claudemacs--session-list-last-diagnostics)))
         ;; `--diagnostic' pushes every emission onto the private list even
         ;; when the public list de-duplicates equal text.  Keep that list's
         ;; length so a repeated provider warning still fails closed.
         (diagnostics-before-internal
          (copy-sequence claudemacs--session-list-last-diagnostics))
         (result
          (condition-case error-data
              (list :value
                    (cond
                     ((eq tool 'claude)
                      (claudemacs--session-list-claude-history cwd))
                     ((eq tool 'codex)
                      (claudemacs--session-list-codex-history cwd))
                     ((null tool)
                      (append (claudemacs--session-list-claude-history cwd)
                              (claudemacs--session-list-codex-history cwd)))
                     (t nil)))
            (error
             (list :error (error-message-string error-data)
                   :value nil))))
         (raw (plist-get result :value))
         (explicit-error (plist-get result :error))
         (raw-error (and (listp raw) (plist-get raw :error)))
         (rows
          (cond
           ((and (listp raw) (memq :rows raw))
            (plist-get raw :rows))
           ((and (listp raw) (memq :error raw))
            (plist-get raw :rows))
           ((listp raw) raw)
           (t nil)))
         (diagnostics-after
          (or claudemacs-session-list-last-diagnostics
              claudemacs--session-list-last-diagnostics))
         (diagnostics-after-internal
          claudemacs--session-list-last-diagnostics)
         (new-diagnostics
          (let ((before-length (length diagnostics-before-internal))
                (after-length (length diagnostics-after-internal)))
            (if (>= after-length before-length)
                (cl-subseq diagnostics-after-internal 0
                           (- after-length before-length))
              ;; A provider should not remove diagnostics, but retain a
              ;; useful fallback if a compatibility caller replaces the
              ;; diagnostic list while the provider runs.
              (seq-remove (lambda (diagnostic)
                            (member diagnostic diagnostics-before))
                          diagnostics-after))))
         (provider-error
          (or explicit-error
              raw-error
              (and new-diagnostics
                   (mapconcat #'identity new-diagnostics " | ")))))
    (if provider-error
        (list :error provider-error
              :rows rows
              :diagnostics new-diagnostics)
      (list :rows rows))))

;;;; Codex history provider

(defun claudemacs--session-list-codex-home ()
  "Resolve Codex's configured home/database root.

`CODEX_SQLITE_HOME' is accepted for installations that expose it, followed by
the documented `CODEX_HOME', then the default `~/.codex'."
  (file-name-as-directory
   (expand-file-name
    (or (claudemacs--session-list--string (getenv "CODEX_SQLITE_HOME"))
        (claudemacs--session-list--string (getenv "CODEX_HOME"))
        (and (boundp 'claudemacs-codex-home)
             (claudemacs--session-list--string claudemacs-codex-home))
        "~/.codex"))))

(defun claudemacs--session-list-codex-database-file ()
  "Resolve Codex's SQLite database path."
  (let ((configured (or (and (boundp 'claudemacs-codex-database-file)
                             claudemacs-codex-database-file)
                        (and (boundp 'claudemacs-codex-sqlite-file)
                             claudemacs-codex-sqlite-file))))
    (if configured
        (expand-file-name configured)
      (let ((home (claudemacs--session-list-codex-home)))
        (if (string-match-p "\\.sqlite\\(?:3\\)?\\'" home)
            (expand-file-name home)
          (expand-file-name "state_5.sqlite" home))))))

(defun claudemacs--session-list--sqlite3-json (database sql)
  "Run sqlite3 directly for DATABASE and SQL, returning parsed JSON or nil.

The executable is never invoked through a shell.  A non-zero status or
malformed result is a provider error, not a row containing the error text."
  (let ((executable (executable-find "sqlite3")))
    (cond
     ((not executable)
      (claudemacs--session-list--diagnostic
       "Codex history unavailable (resolved database %s): sqlite3 executable was not found"
       database)
      nil)
     (t
      (with-temp-buffer
        (let ((stderr-file (make-temp-file "claudemacs-session-list-sqlite3-stderr-"))
              status output error-output)
          (unwind-protect
              (progn
                (setq status
                      ;; `call-process' accepts a file name for stderr in the
                      ;; second element of a destination pair.  Keeping it
                      ;; separate avoids mixing diagnostics with JSON data.
                      (call-process executable nil (list (current-buffer) stderr-file)
                                    nil "-readonly" "-json" database sql)
                      output (buffer-string)
                      error-output (with-temp-buffer
                                     (insert-file-contents stderr-file)
                                     (buffer-string)))
                (if (not (and (integerp status) (= status 0)))
                    (progn
                      (claudemacs--session-list--diagnostic
                       "Codex sqlite3 query failed for %s (exit %s)%s"
                       database status
                       (if (string-empty-p (string-trim error-output))
                           ""
                         (format ": %s"
                                 (truncate-string-to-width
                                  (string-trim error-output) 200 nil nil t))))
                      nil)
                  (condition-case error-data
                      (let ((parsed
                             (claudemacs--session-list--parse-json-string
                              output 'alist 'list nil nil)))
                        (if (listp parsed)
                            parsed
                          (claudemacs--session-list--diagnostic
                           "Codex sqlite3 query returned a non-array result (%s)"
                           database)
                          nil))
                    (error
                     (claudemacs--session-list--diagnostic
                      "Codex sqlite3 returned malformed JSON for %s: %s"
                      database (error-message-string error-data))
                     nil))))
            (when (file-exists-p stderr-file)
              (delete-file stderr-file)))))))))

(defun claudemacs--session-list--sqlite-schema-columns (database)
  "Return available `threads' column names for DATABASE, or nil.

The first return value is a list of strings.  The second value is non-nil when
the schema query itself failed."
  (if (and (fboundp 'sqlite-open)
           (or (not (fboundp 'sqlite-available-p))
               (sqlite-available-p)))
      (condition-case error-data
          (let ((db (sqlite-open database)))
            (unwind-protect
                (delete-dups
                 (delq nil
                       (mapcar (lambda (row)
                                 (and (listp row)
                                      (stringp (nth 1 row))
                                      (nth 1 row)))
                               (sqlite-select db "PRAGMA table_info(threads)"))))
              (sqlite-close db)))
        (error
         (claudemacs--session-list--diagnostic
          "Codex SQLite schema unavailable for %s: %s"
          database (error-message-string error-data))
         nil))
    (let ((result (claudemacs--session-list--sqlite3-json
                   database "PRAGMA table_info(threads);")))
      (when result
        (delete-dups
         (delq nil
               (mapcar (lambda (row)
                         (claudemacs--session-list--json-value row 'name))
                       result)))))))

(defun claudemacs--session-list--sql-quote (value)
  "Return VALUE as a safely quoted SQL string literal."
  (concat "'"
          (replace-regexp-in-string "'" "''" (or value "") t t)
          "'"))

(defun claudemacs--session-list--description-sql-limit ()
  "Return the positive SQL-side context limit."
  (max 1 (if (numberp claudemacs-session-list-description-max-length)
             claudemacs-session-list-description-max-length
           240)))

(defun claudemacs--session-list--codex-query-sql (columns &optional cwd)
  "Construct a bounded Codex query from available COLUMNS and optional CWD.

The CWD predicate is deliberately part of the SQL query, before ORDER/LIMIT,
so a busy Codex database cannot return only unrelated rows and then appear
empty after the Lisp-side canonical-path check.  Values are quoted as SQL
literals because the built-in and sqlite3 fallback APIs use the same query."
  (let* ((base '("id" "created_at" "updated_at" "cwd"))
         (optional-timestamps '("created_at_ms" "updated_at_ms"))
         (optional-descriptions '("name" "title" "preview"
                                  "first_user_message"))
         (description-limit
          (claudemacs--session-list--description-sql-limit))
         (selected
          (append
           base
           (seq-filter (lambda (column)
                         (member column columns))
                       optional-timestamps)
           (mapcar
            (lambda (column)
              (format "substr(%s,1,%d) AS %s"
                      column description-limit column))
            (seq-filter (lambda (column)
                          (member column columns))
                        optional-descriptions))))
         (cwd-values
          (when (claudemacs--session-list--string cwd)
            (delete-dups
             (delq nil
                   (mapcar
                    (lambda (path)
                      (when (claudemacs--session-list--string path)
                        (directory-file-name path)))
                    (list cwd
                          (claudemacs--session-list--canonical-path cwd)))))))
         (where (append (when (member "archived" columns)
                          '("archived = 0"))
                        (when (member "source" columns)
                          '("source = 'cli'"))
                        (when cwd-values
                          (list
                           (format "cwd IN (%s)"
                                   (mapconcat
                                    #'claudemacs--session-list--sql-quote
                                    cwd-values ","))))))
         (order (if (member "updated_at_ms" columns)
                    "updated_at_ms DESC, updated_at DESC"
                  "updated_at DESC, created_at DESC")))
    (format "SELECT %s FROM threads%s ORDER BY %s LIMIT %d;"
            (mapconcat #'identity selected ",")
            (if where (concat " WHERE " (mapconcat #'identity where " AND ")) "")
            order
            (claudemacs--session-list--history-limit))))

(defun claudemacs--session-list--codex-record-alist (record columns)
  "Return RECORD as an alist using built-in SQLite COLUMNS."
  (cond
   ((and columns (or (listp record) (vectorp record)))
    (cl-mapcar #'cons columns (if (vectorp record)
                                  (append record nil)
                                record)))
   ((or (listp record) (hash-table-p record))
    record)
   (t nil)))

(defun claudemacs--session-list--codex-record-id-cwd-valid-p
    (record &optional columns)
  "Return non-nil when RECORD has a usable ID and CWD."
  (condition-case nil
      (let* ((values (claudemacs--session-list--codex-record-alist
                      record columns))
             (id (claudemacs--session-list--normalize-history-id
                  (claudemacs--session-list--json-value values 'id)))
             (cwd (claudemacs--session-list--json-value values 'cwd)))
        (and id (claudemacs--session-list--string cwd)))
    (error nil)))

(defun claudemacs--session-list--codex-diagnose-malformed-records
    (records columns database)
  "Diagnose malformed Codex RECORDS once for DATABASE."
  (let ((count (cl-count-if-not
                (lambda (record)
                  (claudemacs--session-list--codex-record-id-cwd-valid-p
                   record columns))
                records)))
    (when (> count 0)
      (claudemacs--session-list--diagnostic
       "Codex history ignored %d malformed row(s) with missing/invalid id or cwd in %s"
       count database))
    count))

(defun claudemacs--session-list--codex-record-row (record columns)
  "Convert a built-in SQLite RECORD and COLUMNS to a normalized row."
  (claudemacs--session-list--codex-alist-row
   (claudemacs--session-list--codex-record-alist record columns)))

(defun claudemacs--session-list--codex-alist-row (record)
  "Convert an alist RECORD from SQLite or sqlite3 JSON to a normalized row."
  (when (or (listp record) (hash-table-p record))
    (let ((id (claudemacs--session-list--normalize-history-id
               (claudemacs--session-list--json-value record 'id)))
          (cwd (claudemacs--session-list--json-value record 'cwd))
          (created (claudemacs--session-list--json-value record 'created_at))
          (updated (claudemacs--session-list--json-value record 'updated_at))
          (created-ms (claudemacs--session-list--json-value record 'created_at_ms))
          (updated-ms (claudemacs--session-list--json-value record 'updated_at_ms)))
      (when (and id (claudemacs--session-list--string cwd))
        (let ((description
               (claudemacs--session-list--description-from-values
                (claudemacs--session-list--json-value record 'name)
                (claudemacs--session-list--json-value record 'title)
                (claudemacs--session-list--json-value record 'preview)
                (claudemacs--session-list--json-value
                 record 'first_user_message)))
              (updated-value (claudemacs--session-list--timestamp-value
                              updated-ms updated created-ms created)))
          (claudemacs--session-list--row
           (list :tool 'codex
                 :session-id id
                 ;; The database is Codex's authoritative session index.
                 :identity 'exact
                 :cwd cwd
                 :updated-at updated-value
                 :description description
                 :state 'history
                 :source 'codex-history)))))))

(defun claudemacs--session-list--codex-built-in-records (database sql columns)
  "Query DATABASE with built-in SQLite, returning raw records or nil."
  (condition-case error-data
      (let ((db (sqlite-open database)))
        (unwind-protect
            (let ((records (sqlite-select db sql)))
              (claudemacs--session-list--codex-diagnose-malformed-records
               records columns database)
              (mapcar (lambda (record)
                        (claudemacs--session-list--codex-record-row
                         record columns))
                      records))
          (sqlite-close db)))
    (error
     (claudemacs--session-list--diagnostic
      "Codex SQLite query failed for %s: %s"
      database (error-message-string error-data))
     nil)))

(defun claudemacs--session-list-codex-history (&optional cwd)
  "Read Codex SQLite history once, optionally filtered to CWD.

Built-in SQLite is preferred.  Emacs builds without SQLite use a bounded JSON
query through a directly invoked `sqlite3'.  Schema capability is inspected
before optional columns are referenced, and every malformed/error result is
discarded rather than displayed as a session."
  (let ((database (claudemacs--session-list-codex-database-file)))
    (if (not (file-readable-p database))
        (progn
          (claudemacs--session-list--diagnostic
           "Codex history unavailable: %s (resolved from CODEX_SQLITE_HOME, CODEX_HOME, or ~/.codex)"
           database)
          nil)
      (let* ((columns (claudemacs--session-list--sqlite-schema-columns database))
             (required '("id" "created_at" "updated_at" "cwd")))
        (if (not (and columns (cl-every (lambda (column)
                                        (member column columns))
                                      required)))
            (progn
              (claudemacs--session-list--diagnostic
               "Codex history ignored for %s: threads schema lacks required columns"
               database)
              nil)
          (let* ((sql (claudemacs--session-list--codex-query-sql columns cwd))
                 (raw-rows
                  (if (and (fboundp 'sqlite-open)
                           (or (not (fboundp 'sqlite-available-p))
                               (sqlite-available-p)))
                      (claudemacs--session-list--codex-built-in-records
                       database sql
                       (let* ((base '("id" "created_at" "updated_at" "cwd"))
                              (optional '("created_at_ms" "updated_at_ms" "name"
                                          "title" "preview" "first_user_message")))
                         (append base
                                 (seq-filter (lambda (column)
                                               (member column columns))
                                             optional))))
                    (let ((records
                           (claudemacs--session-list--sqlite3-json
                            database sql)))
                      (claudemacs--session-list--codex-diagnose-malformed-records
                       records nil database)
                      (mapcar (lambda (record)
                                (claudemacs--session-list--codex-alist-row
                                 record))
                              records))))
                 (rows (delq nil raw-rows)))
            (let ((sorted
                   (sort (if cwd
                             (seq-filter
                              (lambda (row)
                                (claudemacs--session-list--same-path-p
                                 cwd (plist-get row :cwd)))
                              rows)
                           rows)
                         (lambda (left right)
                           (claudemacs--session-list--newer-first-p
                            (plist-get left :updated-at)
                            (plist-get right :updated-at))))))
              (claudemacs--session-list--dedupe-history-rows sorted))))))))

(defun claudemacs--session-list--dedupe-history-rows (rows)
  "Collapse duplicate history ROWS by authoritative tool/session key."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (row rows)
      (let ((id (plist-get row :session-id)))
        (when (and (plist-get row :tool) (stringp id)
                   (not (string-empty-p id)))
          (let* ((key (format "%s:%s" (plist-get row :tool) id))
                 (old (gethash key seen)))
            (when (or (null old)
                      (claudemacs--session-list--newer-first-p
                       (plist-get row :updated-at)
                       (plist-get old :updated-at)))
              (puthash key row seen))))))
    (let (result)
      (maphash (lambda (_key row) (push row result)) seen)
      (sort result
            (lambda (left right)
              (claudemacs--session-list--newer-first-p
               (plist-get left :updated-at)
               (plist-get right :updated-at)))))))

;;;; Live refresh

(defun claudemacs--session-list-refresh-data ()
  "Return the live Claudemacs rows observed by one provider pass.

The session list intentionally has no history or registry data source.  Keep
this call bounded to one live-buffer enumeration so `g' replaces stale rows
when a terminal process exits."
  (claudemacs--session-list--reset-diagnostics)
  (let ((rows (condition-case error-data
                  (claudemacs--session-list-live-buffer-rows)
                (error
                 (claudemacs--session-list--diagnostic
                  "Live session provider failed: %s"
                  (error-message-string error-data))
                 nil))))
    (claudemacs--session-list--finish-diagnostics)
    rows))

;; Short aliases make the adapter contracts easy to discover from *Help* and
;; are also useful for focused tests without coupling them to the UI.
(defalias 'claudemacs--live-buffer-provider
  #'claudemacs--session-list-live-buffer-rows)
(defalias 'claudemacs--claude-history-provider
  #'claudemacs--session-list-claude-history)
(defalias 'claudemacs--codex-history-provider
  #'claudemacs--session-list-codex-history)

;;;; Tabulated UI

(defun claudemacs--session-list--cell (visible &optional help)
  "Propertize VISIBLE with HELP while keeping both values single-line."
  (let ((text (if (stringp visible) visible "")))
    (propertize (replace-regexp-in-string "[\n\r\t]" " " text)
                'help-echo (or help text))))

(defun claudemacs--session-list--ui-key (row)
  "Return ROW's table-selection key.

Authoritative identity keys are not necessarily unique live-row keys: two
Emacs buffers can explicitly resume the same conversation.  Include the live
buffer name so every displayed live row remains independently visitable."
  (let ((identity-key (claudemacs--session-list--row-key row))
        (buffer (plist-get row :buffer)))
    (cond
     ((buffer-live-p buffer)
      (format "live:%s:%s"
              (or identity-key "unknown")
              (buffer-name buffer)))
     (identity-key identity-key)
     (t (format "row:%x" (sxhash row))))))

(defun claudemacs--session-list--entries ()
  "Return `tabulated-list-entries' for current buffer rows."
  (mapcar
   (lambda (row)
     (let* ((cwd (plist-get row :cwd))
            (project (and cwd (abbreviate-file-name cwd)))
            (workspace (or (plist-get row :workspace) "—"))
            (instance (or (plist-get row :instance)
                          (and (plist-get row :tool)
                               (symbol-name (plist-get row :tool)))
                          "—")))
       (list (claudemacs--session-list--ui-key row)
             (vector
              (claudemacs--session-list--cell workspace)
              (claudemacs--session-list--cell instance)
              (claudemacs--session-list--cell project cwd)))))
   claudemacs--session-list-rows))

(defun claudemacs--session-list--warning-header ()
  "Return a concise header line including current provider diagnostics."
  (if claudemacs--session-list-last-diagnostics
      (concat " Claudemacs sessions — warnings: "
              (mapconcat #'identity claudemacs--session-list-last-diagnostics
                         " | "))
    " Claudemacs sessions — g refreshes, RET visits live rows"))

(defun claudemacs-session-list-refresh ()
  "Refresh live Claudemacs rows and redraw the current table."
  (interactive)
  (when (derived-mode-p 'claudemacs-session-list-mode)
    (setq claudemacs--session-list-rows
          (claudemacs--session-list-refresh-data)
          tabulated-list-entries
          (claudemacs--session-list--entries)
          header-line-format
          (claudemacs--session-list--warning-header))
    (tabulated-list-print t)
    (when claudemacs--session-list-last-diagnostics
      (message "%s"
               (mapconcat #'identity claudemacs--session-list-last-diagnostics
                          " | ")))))

(defun claudemacs-session-list-visit ()
  "Visit the live Claudemacs buffer on the current session-list row.

Re-check the terminal process because a row can become stale between a
refresh and `RET'."
  (interactive)
  (let* ((key (tabulated-list-get-id))
         (row (seq-find (lambda (item)
                          (equal key (claudemacs--session-list--ui-key item)))
                        claudemacs--session-list-rows))
         (buffer (and row (plist-get row :buffer))))
    (cond
     ((null row) (user-error "No session row is selected"))
     ((and (eq (plist-get row :state) 'live)
           (buffer-live-p buffer)
           (claudemacs--session-list--buffer-live-p buffer))
      (pop-to-buffer buffer))
     (t (user-error "That session is no longer live; press g to refresh")))))

(define-derived-mode claudemacs-session-list-mode tabulated-list-mode
  "Claudemacs-Sessions"
  "Major mode for the live Claudemacs session list."
  (setq tabulated-list-format
        [
         ("Workspace" 20 t)
         ("Tool instance" 18 t)
         ("Project" 48 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Workspace" . t))
  (local-set-key (kbd "g") #'claudemacs-session-list-refresh)
  (local-set-key (kbd "RET") #'claudemacs-session-list-visit)
  (tabulated-list-init-header))

;;;###autoload
(defun claudemacs-session-list ()
  "Display all currently live Claudemacs sessions in a table.

`g' re-enumerates live buffers once.  `RET' re-checks terminal liveness before
visiting the selected buffer.  CLI history is intentionally not displayed."
  (interactive)
  (let ((buffer (get-buffer-create "*Claudemacs Sessions*")))
    (with-current-buffer buffer
      (claudemacs-session-list-mode)
      (claudemacs-session-list-refresh))
    (pop-to-buffer buffer)))

;; Keep the old command name as a compatibility entry point while callers
;; migrate to the public `claudemacs-session-list' command.  It has no legacy
;; snapshot behavior.
(defalias 'claudemacs-session-overview #'claudemacs-session-list)
(defalias 'claudemacs-session-overview-mode #'claudemacs-session-list-mode)
(defalias 'claudemacs-session-overview-visit #'claudemacs-session-list-visit)

(provide 'claudemacs-session-list)
;;; claudemacs-session-list.el ends here
