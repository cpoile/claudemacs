;;; claudemacs-ai-notes-todo.el --- Project TODOs and worktree management -*- lexical-binding: t; -*-
;; Author: Claude + Chad Crawford
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))
;; Keywords: claudecode ai emacs llm tools org-mode
;; URL: https://github.com/cpoile/claudemacs
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Project-based TODO management with optional worktree isolation for claudemacs.
;;
;; This provides a simple workflow:
;; 1. Create a TODO node for a project: `claudemacs-todo-capture' (C-c n p t)
;; 2. From the TODO node, choose how to execute:
;;    - `claudemacs-todo-send-to-main' (C-c c t) - Send to main session
;;    - `claudemacs-todo-create-worktree' (C-c c w) - Create worktree + new session
;; 3. View all TODOs for a project: `claudemacs-todo-list' (C-c n p l t)
;;
;; TODO nodes are stored in org-roam as: projects/{project}/todo-{slug}.org
;; with properties:
;;   :PROJECT_NAME: short project name
;;   :PROJECT_ROOT: full path to project
;;   :STATUS: draft | active | done | rejected
;;   :WORKTREE_PATH: (set when worktree is created)
;;   :WORKTREE_BRANCH: (set when worktree is created)

;;; Code:

(require 'org-roam)
(require 'cl-lib)
(require 'tabulated-list)
(require 'json)

;; Forward declarations
(declare-function projectile-project-root "projectile")
(declare-function projectile-known-projects "projectile")
(declare-function claudemacs-spawn-agent "claudemacs")
(declare-function eat-term-send-string "eat")

;;;; Customization

(defgroup claudemacs-todo nil
  "Project TODO management for claudemacs."
  :group 'claudemacs)

(defcustom claudemacs-todo-worktree-directory
  (expand-file-name "claudemacs-worktrees" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Base directory for storing worktrees.
Worktrees are created as {this-dir}/{project-name}/{branch-slug}/"
  :type 'directory
  :group 'claudemacs-todo)

;;;; Project Selection

(defun claudemacs-todo--worktree-main-repo (dir)
  "If DIR is a git worktree, return the main repository path.
Otherwise return nil."
  (let ((git-dir (expand-file-name ".git" dir)))
    (when (file-regular-p git-dir)
      ;; .git is a file, meaning this is a worktree
      (with-temp-buffer
        (insert-file-contents git-dir)
        (when (re-search-forward "gitdir: \\(.+\\)" nil t)
          (let ((gitdir (match-string 1)))
            ;; gitdir points to .git/worktrees/<name>
            ;; We want the parent repo's root
            (when (string-match "/\\.git/worktrees/" gitdir)
              (let ((main-git-dir (substring gitdir 0 (match-beginning 0))))
                (expand-file-name main-git-dir)))))))))

(defun claudemacs-todo--infer-project ()
  "Infer the current project from context.
Checks for worktrees and maps them back to the main repository."
  (let* ((current-dir (or (and (fboundp 'projectile-project-root)
                               (projectile-project-root))
                          default-directory))
         ;; Check if we're in a worktree and get main repo
         (main-repo (claudemacs-todo--worktree-main-repo current-dir)))
    (or main-repo current-dir)))

(defun claudemacs-todo--select-project ()
  "Prompt user to select a git project.
Returns the project root path. Defaults to inferred project from context."
  (let* ((inferred (claudemacs-todo--infer-project))
         (projects (if (and (fboundp 'projectile-known-projects)
                            (projectile-known-projects))
                       ;; Filter to only git repos
                       (seq-filter
                        (lambda (p)
                          (file-directory-p (expand-file-name ".git" p)))
                        (projectile-known-projects))
                     nil)))
    (if projects
        (completing-read "Project: " projects nil t nil nil inferred)
      (read-directory-name "Git project root: " inferred))))

(defun claudemacs-todo--project-name (project-root)
  "Get short project name from PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

;;;; Slug Helpers

(defun claudemacs-todo--slugify (text)
  "Convert TEXT to a branch-safe slug."
  (let* ((slug (downcase text))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
         (slug (replace-regexp-in-string "^-\\|-$" "" slug)))
    slug))

(defun claudemacs-todo--default-branch-name (title)
  "Generate default branch name from TITLE."
  (let ((slug (claudemacs-todo--slugify title)))
    (format "feature/%s" slug)))

;;;; Node Property Helpers

(defun claudemacs-todo--get-property (property)
  "Get PROPERTY from the current org-roam node."
  (org-entry-get (point-min) property))

(defun claudemacs-todo--set-property (property value)
  "Set PROPERTY to VALUE in the current org-roam node."
  (save-excursion
    (goto-char (point-min))
    (org-set-property property value)))

(defun claudemacs-todo--node-p ()
  "Return non-nil if current buffer is a claudemacs TODO node."
  (and (derived-mode-p 'org-mode)
       (buffer-file-name)
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "^:PROJECT_ROOT:" nil t))))

;;;; TODO Capture

;;;###autoload
(defun claudemacs-todo-capture ()
  "Capture a new TODO for a projectile project.
Prompts for project selection, then creates an org-roam node."
  (interactive)
  (unless (featurep 'org-roam)
    (user-error "org-roam is required"))
  (let* ((project-root (claudemacs-todo--select-project))
         (project-name (claudemacs-todo--project-name project-root))
         (project-dir (expand-file-name (concat "projects/" project-name) org-roam-directory))
         ;; Generate timestamps directly to avoid escaping issues
         (id-timestamp (format-time-string "%Y%m%dT%H%M%S"))
         (date-stamp (format-time-string "%Y-%m-%d")))
    ;; Ensure project directory exists
    (unless (file-directory-p project-dir)
      (make-directory project-dir t))
    ;; Set up capture template dynamically
    (let ((org-roam-capture-templates
           `(("t" "Project TODO" plain "%?"
              :target (file+head
                       ,(concat "projects/" project-name "/todo-${slug}.org")
                       ,(format ":PROPERTIES:
:ID: %s
:PROJECT_NAME: %s
:PROJECT_ROOT: %s
:STATUS: draft
:CREATED: %s
:END:
#+title: ${title}
#+filetags: :todo:%s:

** Task Description

** Acceptance Criteria
- [ ]

** Progress Log

" id-timestamp project-name project-root date-stamp project-name))
              :unnarrowed t))))
      (org-roam-capture))))

;;;; Git Worktree Operations

(defun claudemacs-todo--pre-trust-worktree (worktree-path)
  "Pre-trust WORKTREE-PATH in Claude's global config to skip trust dialog.
Calls the pretrust-directory.py script to add an entry to ~/.claude.json."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name
                                               (locate-library "claudemacs-ai-notes-todo"))))
         (script-path (expand-file-name "scripts/pretrust-directory.py" script-dir))
         (expanded-path (expand-file-name worktree-path)))
    (if (file-exists-p script-path)
        (let ((result (call-process "uv" nil nil nil
                                    "run" script-path expanded-path)))
          (if (= result 0)
              (message "Pre-trusted worktree: %s" expanded-path)
            (message "Warning: Failed to pre-trust worktree (exit %d)" result)))
      (message "Warning: pretrust-directory.py not found at %s" script-path))))

(defun claudemacs-todo--worktree-path (project-root branch-name)
  "Calculate worktree path for PROJECT-ROOT and BRANCH-NAME."
  (let* ((project-name (claudemacs-todo--project-name project-root))
         (branch-slug (claudemacs-todo--slugify branch-name)))
    (expand-file-name
     (concat project-name "/" branch-slug)
     claudemacs-todo-worktree-directory)))

(defun claudemacs-todo--worktree-exists-p (worktree-path)
  "Return non-nil if WORKTREE-PATH exists and is a git worktree."
  (and (file-directory-p worktree-path)
       (file-exists-p (expand-file-name ".git" worktree-path))))

(defun claudemacs-todo--branch-exists-p (project-root branch-name)
  "Return non-nil if BRANCH-NAME exists in PROJECT-ROOT."
  (let ((default-directory project-root))
    (= 0 (call-process "git" nil nil nil "rev-parse" "--verify" branch-name))))

(defun claudemacs-todo--create-worktree (project-root branch-name worktree-path)
  "Create a git worktree at WORKTREE-PATH for BRANCH-NAME from PROJECT-ROOT.
Creates the branch if it doesn't exist."
  (let ((default-directory project-root))
    ;; Ensure parent directory exists
    (make-directory (file-name-directory worktree-path) t)
    ;; Create worktree (with new branch if needed)
    (if (claudemacs-todo--branch-exists-p project-root branch-name)
        ;; Branch exists, just create worktree
        (let ((result (call-process "git" nil "*claudemacs-worktree-output*" nil
                                    "worktree" "add" worktree-path branch-name)))
          (unless (= 0 result)
            (error "Failed to create worktree: see *claudemacs-worktree-output*")))
      ;; Create new branch with worktree
      (let ((result (call-process "git" nil "*claudemacs-worktree-output*" nil
                                  "worktree" "add" "-b" branch-name worktree-path)))
        (unless (= 0 result)
          (error "Failed to create worktree with new branch: see *claudemacs-worktree-output*"))))))

;;;; Send to Main Session

(defun claudemacs-todo--find-main-session (project-root)
  "Find the main claudemacs buffer for PROJECT-ROOT."
  (let ((expanded-root (expand-file-name project-root)))
    (cl-find-if
     (lambda (buf)
       (and (string-match-p "^\\*claudemacs:" (buffer-name buf))
            ;; Exclude named agents (buffers with :agent-name suffix)
            (not (string-match-p "^\\*claudemacs:[^:]+:[^*]+\\*$" (buffer-name buf)))
            (with-current-buffer buf
              (and (boundp 'claudemacs--cwd)
                   (string= (expand-file-name claudemacs--cwd) expanded-root)))))
     (buffer-list))))

(defun claudemacs-todo--get-node-content ()
  "Get the content of the current TODO node for sending to Claude."
  (save-excursion
    (goto-char (point-min))
    ;; Skip to after filetags line
    (when (re-search-forward "^#\\+filetags:" nil t)
      (forward-line 1)
      (string-trim (buffer-substring-no-properties (point) (point-max))))))

;;;###autoload
(defun claudemacs-todo-send-to-main ()
  "Send the current TODO to the main claudemacs session for its project.
Use this for quick tasks that don't need worktree isolation."
  (interactive)
  (unless (claudemacs-todo--node-p)
    (user-error "Not in a claudemacs TODO node"))
  (require 'claudemacs)
  (let* ((project-root (claudemacs-todo--get-property "PROJECT_ROOT"))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                    (match-string 1))))
         (content (claudemacs-todo--get-node-content))
         (claude-buffer (claudemacs-todo--find-main-session project-root)))
    (unless project-root
      (user-error "No PROJECT_ROOT property found"))
    (unless claude-buffer
      (user-error "No claudemacs session found for project: %s\nStart one with M-x claudemacs-run in that project" project-root))
    ;; Send to Claude
    (with-current-buffer claude-buffer
      (when (and (boundp 'eat-terminal) eat-terminal)
        (let ((message (format "[TODO] %s\n\n%s" (or title "Task") content)))
          (eat-term-send-string eat-terminal "\C-u")
          (eat-term-send-string eat-terminal message)
          (sit-for 0.1)
          (eat-term-send-string eat-terminal "\r"))))
    ;; Update status
    (claudemacs-todo--set-property "STATUS" "active")
    (save-buffer)
    (message "Sent TODO to main claudemacs session")))

;;;; Create Worktree

(defun claudemacs-todo--send-task-to-buffer (buffer-name content worktree-path &optional delay)
  "Send task CONTENT to BUFFER-NAME after optional DELAY seconds.
WORKTREE-PATH is included in the message for context."
  (let ((send-fn (lambda (buf-name task-content wpath)
                   (message "Sending task to %s..." buf-name)
                   (let ((buffer (get-buffer buf-name)))
                     (if (not buffer)
                         (message "ERROR: Buffer %s not found" buf-name)
                       (with-current-buffer buffer
                         (if (not (and (boundp 'eat-terminal) eat-terminal))
                             (message "ERROR: eat-terminal not ready in %s" buf-name)
                           (let ((msg (format "[WORKTREE TASK]\n\n%s\n\nWorktree: %s\nPlease help me with this task."
                                              task-content wpath)))
                             (eat-term-send-string eat-terminal "\C-u")
                             (eat-term-send-string eat-terminal msg)
                             (sit-for 0.1)
                             (eat-term-send-string eat-terminal "\r")
                             (message "Task sent to %s" buf-name)))))))))
    (if delay
        (run-with-timer delay nil send-fn buffer-name content worktree-path)
      (funcall send-fn buffer-name content worktree-path))))

;;;###autoload
(defun claudemacs-todo-create-worktree ()
  "Create a worktree for the current TODO and spawn a claudemacs session.
Use this for feature work that benefits from isolation.
If the worktree and session already exist, sends the task to the existing session."
  (interactive)
  (unless (claudemacs-todo--node-p)
    (user-error "Not in a claudemacs TODO node"))
  (require 'claudemacs)
  (let* ((project-root (claudemacs-todo--get-property "PROJECT_ROOT"))
         (existing-worktree (claudemacs-todo--get-property "WORKTREE_PATH"))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                    (match-string 1))))
         (default-branch (claudemacs-todo--default-branch-name (or title "feature")))
         (branch-name (or (claudemacs-todo--get-property "WORKTREE_BRANCH")
                          (read-string "Branch name: " default-branch)))
         (worktree-path (or existing-worktree
                            (claudemacs-todo--worktree-path project-root branch-name)))
         (content (claudemacs-todo--get-node-content))
         ;; Calculate expected buffer name
         (expanded-path (expand-file-name worktree-path))
         (expected-buffer-name (format "*claudemacs:%s:%s*" expanded-path branch-name))
         (existing-buffer (get-buffer expected-buffer-name)))
    (unless project-root
      (user-error "No PROJECT_ROOT property found"))
    ;; Create worktree if needed
    (unless (claudemacs-todo--worktree-exists-p worktree-path)
      (message "Creating worktree at %s..." worktree-path)
      (claudemacs-todo--create-worktree project-root branch-name worktree-path)
      ;; Store worktree info in node
      (claudemacs-todo--set-property "WORKTREE_PATH" worktree-path)
      (claudemacs-todo--set-property "WORKTREE_BRANCH" branch-name))
    ;; Update status
    (claudemacs-todo--set-property "STATUS" "active")
    (save-buffer)
    ;; Check if session already exists
    (if existing-buffer
        (progn
          ;; Session exists - send task immediately (no delay needed)
          (claudemacs-todo--send-task-to-buffer expected-buffer-name content worktree-path)
          (pop-to-buffer existing-buffer)
          (message "Sent task to existing session: %s" expected-buffer-name))
      ;; New session - pre-trust and spawn
      (claudemacs-todo--pre-trust-worktree worktree-path)
      (let ((buffer-name (claudemacs-spawn-agent worktree-path branch-name)))
        ;; Store buffer name in node
        (claudemacs-todo--set-property "CLAUDEMACS_BUFFER" buffer-name)
        (save-buffer)
        ;; Wait for session to be ready (5 seconds for Claude to initialize)
        (claudemacs-todo--send-task-to-buffer buffer-name content worktree-path 5)
        (message "Created worktree and spawned claudemacs session: %s" buffer-name)))))

;;;; Resend Task

;;;###autoload
(defun claudemacs-todo-resend ()
  "Resend the current TODO content to its associated claudemacs session.
Works for both main session TODOs and worktree TODOs."
  (interactive)
  (unless (claudemacs-todo--node-p)
    (user-error "Not in a claudemacs TODO node"))
  (let* ((worktree-path (claudemacs-todo--get-property "WORKTREE_PATH"))
         (project-root (claudemacs-todo--get-property "PROJECT_ROOT"))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                    (match-string 1))))
         (content (claudemacs-todo--get-node-content))
         (claude-buffer (if worktree-path
                            ;; Find worktree session
                            (cl-find-if
                             (lambda (buf)
                               (and (string-match-p "^\\*claudemacs:" (buffer-name buf))
                                    (with-current-buffer buf
                                      (and (boundp 'claudemacs--cwd)
                                           (string= (expand-file-name claudemacs--cwd)
                                                    (expand-file-name worktree-path))))))
                             (buffer-list))
                          ;; Find main session
                          (claudemacs-todo--find-main-session project-root))))
    (unless claude-buffer
      (user-error "No claudemacs session found. Use C-c c t or C-c c w first"))
    (with-current-buffer claude-buffer
      (when (and (boundp 'eat-terminal) eat-terminal)
        (let ((message (if worktree-path
                           (format "[WORKTREE TASK]\n\n%s\n\nWorktree: %s\nPlease help me with this task."
                                   content worktree-path)
                         (format "[TODO] %s\n\n%s" (or title "Task") content))))
          (eat-term-send-string eat-terminal "\C-u")
          (eat-term-send-string eat-terminal message)
          (sit-for 0.1)
          (eat-term-send-string eat-terminal "\r"))))
    (message "Resent TODO to claudemacs session")))

;;;; TODO List Buffer

(defconst claudemacs-todo-status-order
  '("draft" "active" "done" "rejected")
  "Order of TODO statuses for sorting.")

(defun claudemacs-todo-list-buffer-name (&optional project)
  "Generate buffer name for TODO list, optionally for PROJECT."
  (if project
      (format "*todo-list:%s*" project)
    "*todo-list*"))

;;;; Faces for TODO List

(defface claudemacs-todo-status-draft
  '((t :foreground "#888888" :weight normal))
  "Face for draft status."
  :group 'claudemacs-todo)

(defface claudemacs-todo-status-active
  '((t :foreground "#ffaa00" :weight bold))
  "Face for active status."
  :group 'claudemacs-todo)

(defface claudemacs-todo-status-done
  '((t :foreground "#00ff00" :weight bold))
  "Face for done status."
  :group 'claudemacs-todo)

(defface claudemacs-todo-status-rejected
  '((t :foreground "#ff4444" :weight normal :strike-through t))
  "Face for rejected status."
  :group 'claudemacs-todo)

(defface claudemacs-todo-title
  '((t :foreground "#aaccff"))
  "Face for TODO title."
  :group 'claudemacs-todo)

(defface claudemacs-todo-project
  '((t :foreground "#88aaff"))
  "Face for project name."
  :group 'claudemacs-todo)

(defun claudemacs-todo--status-face (status)
  "Return the face for STATUS."
  (pcase status
    ("draft" 'claudemacs-todo-status-draft)
    ("active" 'claudemacs-todo-status-active)
    ("done" 'claudemacs-todo-status-done)
    ("rejected" 'claudemacs-todo-status-rejected)
    (_ 'default)))

(defun claudemacs-todo--format-status (status)
  "Format STATUS with appropriate face."
  (propertize (or status "draft") 'face (claudemacs-todo--status-face status)))

(defun claudemacs-todo--status-sort-key (status)
  "Return sort key for STATUS (lower = first)."
  (or (cl-position (or status "draft") claudemacs-todo-status-order :test #'string=) 99))

(defun claudemacs-todo--query-todos (&optional project-filter)
  "Query all TODO nodes from org-roam, optionally filtered by PROJECT-FILTER.
Returns a list of plists with :id, :title, :project, :status, :file, :created."
  (let* ((todos '())
         ;; Query org-roam for file-level nodes only (level = 0)
         ;; This excludes sub-nodes within the file like "Progress Log"
         (nodes (org-roam-db-query
                 [:select [nodes:id nodes:file nodes:title]
                  :from nodes
                  :where (and (like nodes:file "%/todo-%.org")
                              (= nodes:level 0))])))
    (dolist (row nodes)
      (let* ((id (nth 0 row))
             (file (nth 1 row))
             (title (nth 2 row)))
        ;; Read properties from the file
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file nil 0 2000) ; Just read header
            (let ((project (when (re-search-forward "^:PROJECT_NAME:\\s-*\\(.+\\)$" nil t)
                             (match-string 1)))
                  (project-root (progn
                                  (goto-char (point-min))
                                  (when (re-search-forward "^:PROJECT_ROOT:\\s-*\\(.+\\)$" nil t)
                                    (match-string 1))))
                  (status (progn
                            (goto-char (point-min))
                            (when (re-search-forward "^:STATUS:\\s-*\\(.+\\)$" nil t)
                              (match-string 1))))
                  (created (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^:CREATED:\\s-*\\(.+\\)$" nil t)
                               (match-string 1))))
                  (worktree-path (progn
                                   (goto-char (point-min))
                                   (when (re-search-forward "^:WORKTREE_PATH:\\s-*\\(.+\\)$" nil t)
                                     (match-string 1)))))
              (when (and project
                         (or (null project-filter)
                             (string= project project-filter)
                             ;; Normalize paths: expand ~ and remove trailing slashes
                             (string= (directory-file-name (expand-file-name project-root))
                                      (directory-file-name (expand-file-name project-filter)))))
                (push (list :id id
                            :title title
                            :project project
                            :project-root project-root
                            :status (or status "draft")
                            :file file
                            :created (or created "")
                            :worktree-path worktree-path)
                      todos)))))))
    ;; Sort by status order, then by created date (newest first)
    (sort todos
          (lambda (a b)
            (let ((status-a (claudemacs-todo--status-sort-key (plist-get a :status)))
                  (status-b (claudemacs-todo--status-sort-key (plist-get b :status))))
              (if (= status-a status-b)
                  (string> (plist-get a :created) (plist-get b :created))
                (< status-a status-b)))))))

(defvar-local claudemacs-todo-list--project-filter nil
  "Current project filter for the TODO list buffer.")

(defun claudemacs-todo-list--get-entries ()
  "Get tabulated list entries for TODOs."
  (mapcar
   (lambda (todo)
     (let ((id (plist-get todo :id))
           (title (plist-get todo :title))
           (project (plist-get todo :project))
           (status (plist-get todo :status))
           (created (plist-get todo :created))
           (file (plist-get todo :file)))
       (list file
             (vector
              (claudemacs-todo--format-status status)
              (propertize (or title "Untitled") 'face 'claudemacs-todo-title)
              (propertize (or project "") 'face 'claudemacs-todo-project)
              (or created "")))))
   (claudemacs-todo--query-todos claudemacs-todo-list--project-filter)))

(defun claudemacs-todo-list-refresh ()
  "Refresh the TODO list buffer."
  (interactive)
  (tabulated-list-revert))

(defun claudemacs-todo-list-open ()
  "Open the TODO at point."
  (interactive)
  (when-let ((file (tabulated-list-get-id)))
    (find-file file)))

(defun claudemacs-todo-list-set-status (new-status)
  "Set the status of the TODO at point to NEW-STATUS."
  (when-let ((file (tabulated-list-get-id)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^:STATUS:\\s-*.+$" nil t)
            (replace-match (format ":STATUS: %s" new-status))
          ;; Add STATUS property if it doesn't exist
          (when (re-search-forward "^:PROPERTIES:" nil t)
            (forward-line 1)
            (insert (format ":STATUS: %s\n" new-status))))
        (save-buffer)))
    (claudemacs-todo-list-refresh)
    (message "Set status to: %s" new-status)))

(defun claudemacs-todo-list-mark-done ()
  "Mark the TODO at point as done."
  (interactive)
  (claudemacs-todo-list-set-status "done"))

(defun claudemacs-todo-list-mark-rejected ()
  "Mark the TODO at point as rejected."
  (interactive)
  (claudemacs-todo-list-set-status "rejected"))

(defun claudemacs-todo-list-mark-active ()
  "Mark the TODO at point as active."
  (interactive)
  (claudemacs-todo-list-set-status "active"))

(defun claudemacs-todo-list-mark-draft ()
  "Mark the TODO at point as draft."
  (interactive)
  (claudemacs-todo-list-set-status "draft"))

(defun claudemacs-todo-list-cycle-status ()
  "Cycle the status of the TODO at point."
  (interactive)
  (when-let ((file (tabulated-list-get-id)))
    (let* ((current-status
            (with-temp-buffer
              (insert-file-contents file nil 0 1000)
              (when (re-search-forward "^:STATUS:\\s-*\\(.+\\)$" nil t)
                (match-string 1))))
           (current-idx (or (cl-position (or current-status "draft")
                                         claudemacs-todo-status-order :test #'string=) 0))
           (next-idx (mod (1+ current-idx) (length claudemacs-todo-status-order)))
           (next-status (nth next-idx claudemacs-todo-status-order)))
      (claudemacs-todo-list-set-status next-status))))

(defvar claudemacs-todo-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claudemacs-todo-list-open)
    (define-key map (kbd "o") #'claudemacs-todo-list-open)
    (define-key map (kbd "g") #'claudemacs-todo-list-refresh)
    (define-key map (kbd "d") #'claudemacs-todo-list-mark-done)
    (define-key map (kbd "r") #'claudemacs-todo-list-mark-rejected)
    (define-key map (kbd "a") #'claudemacs-todo-list-mark-active)
    (define-key map (kbd "u") #'claudemacs-todo-list-mark-draft)
    (define-key map (kbd "TAB") #'claudemacs-todo-list-cycle-status)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `claudemacs-todo-list-mode'.")

;; Evil mode support
(with-eval-after-load 'evil
  (evil-define-key 'normal claudemacs-todo-list-mode-map
    (kbd "RET") #'claudemacs-todo-list-open
    (kbd "o") #'claudemacs-todo-list-open
    (kbd "gr") #'claudemacs-todo-list-refresh
    (kbd "d") #'claudemacs-todo-list-mark-done
    (kbd "r") #'claudemacs-todo-list-mark-rejected
    (kbd "a") #'claudemacs-todo-list-mark-active
    (kbd "u") #'claudemacs-todo-list-mark-draft
    (kbd "TAB") #'claudemacs-todo-list-cycle-status
    (kbd "q") #'quit-window))

(define-derived-mode claudemacs-todo-list-mode tabulated-list-mode "Claudemacs-TODOs"
  "Major mode for viewing and managing claudemacs project TODOs.

\\{claudemacs-todo-list-mode-map}"
  (setq tabulated-list-format
        [("Status" 12 (lambda (a b)
                        (< (claudemacs-todo--status-sort-key (aref (cadr a) 0))
                           (claudemacs-todo--status-sort-key (aref (cadr b) 0)))))
         ("Title" 50 t)
         ("Project" 20 t)
         ("Created" 12 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Status" . nil))
  (setq tabulated-list-entries #'claudemacs-todo-list--get-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun claudemacs-todo-list ()
  "Display a buffer listing all project TODOs."
  (interactive)
  (let ((buffer (get-buffer-create (claudemacs-todo-list-buffer-name))))
    (with-current-buffer buffer
      (claudemacs-todo-list-mode)
      (setq-local claudemacs-todo-list--project-filter nil)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

;;;###autoload
(defun claudemacs-todo-list-project (&optional prompt)
  "Display a buffer listing TODOs for the current project.
Auto-infers project from context (including worktree detection).
With prefix arg PROMPT, prompts for project selection."
  (interactive "P")
  (let* ((project (if prompt
                      (claudemacs-todo--select-project)
                    (claudemacs-todo--infer-project)))
         (project-name (claudemacs-todo--project-name project))
         (buffer (get-buffer-create (claudemacs-todo-list-buffer-name project-name))))
    (with-current-buffer buffer
      (claudemacs-todo-list-mode)
      (setq-local claudemacs-todo-list--project-filter project)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

;;;; Minor Mode

(defvar claudemacs-todo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c t") #'claudemacs-todo-send-to-main)
    (define-key map (kbd "C-c c w") #'claudemacs-todo-create-worktree)
    (define-key map (kbd "C-c c s") #'claudemacs-todo-resend)
    map)
  "Keymap for `claudemacs-todo-mode'.

Key bindings:
  C-c c t   Send TODO to main claudemacs session
  C-c c w   Create worktree and spawn new claudemacs session
  C-c c s   Resend TODO content to associated session")

;;;###autoload
(define-minor-mode claudemacs-todo-mode
  "Minor mode for claudemacs TODO nodes.

\\{claudemacs-todo-mode-map}"
  :lighter " ClaudeTODO"
  :keymap claudemacs-todo-mode-map)

(defun claudemacs-todo--maybe-enable-mode ()
  "Enable `claudemacs-todo-mode' if this is a claudemacs TODO node."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             ;; Check if filename matches todo pattern in org-roam projects dir
             (string-match-p "/projects/[^/]+/todo-" (buffer-file-name)))
    ;; Double-check by reading the PROJECT_ROOT property
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^:PROJECT_ROOT:" nil t)
        (claudemacs-todo-mode 1)))))

;; Auto-enable in TODO nodes
(add-hook 'find-file-hook #'claudemacs-todo--maybe-enable-mode)

;;;; MCP Tool Functions

(defun claudemacs-ai-todo-list-all (&optional project-filter)
  "List all project TODOs for MCP.
Returns JSON with all TODOs, optionally filtered by PROJECT-FILTER."
  (let ((todos (claudemacs-todo--query-todos project-filter)))
    (json-encode
     (mapcar (lambda (todo)
               `((id . ,(plist-get todo :id))
                 (title . ,(plist-get todo :title))
                 (project . ,(plist-get todo :project))
                 (project_root . ,(plist-get todo :project-root))
                 (status . ,(plist-get todo :status))
                 (file . ,(plist-get todo :file))
                 (created . ,(plist-get todo :created))))
             todos))))

(defun claudemacs-ai-todo-get-assigned ()
  "Get the TODO assigned to the current worktree session.
Returns JSON with the TODO details or null if not in a worktree."
  (let* ((cwd (or (bound-and-true-p claudemacs-session-cwd)
                  (bound-and-true-p claudemacs--cwd)
                  default-directory))
         (expanded-cwd (expand-file-name cwd)))
    ;; Find a TODO with this worktree path
    (let ((todos (claudemacs-todo--query-todos)))
      (let ((assigned (cl-find-if
                       (lambda (todo)
                         (let ((wpath (plist-get todo :worktree-path)))
                           (and wpath
                                (string= (expand-file-name wpath) expanded-cwd))))
                       todos)))
        (if assigned
            (json-encode
             `((id . ,(plist-get assigned :id))
               (title . ,(plist-get assigned :title))
               (project . ,(plist-get assigned :project))
               (project_root . ,(plist-get assigned :project-root))
               (status . ,(plist-get assigned :status))
               (file . ,(plist-get assigned :file))
               (created . ,(plist-get assigned :created))
               (content . ,(claudemacs-ai-todo--get-full-content (plist-get assigned :file)))))
          "null")))))

(defun claudemacs-ai-todo--get-full-content (file)
  "Get the full content of a TODO FILE."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; Skip to after filetags line
      (when (re-search-forward "^#\\+filetags:" nil t)
        (forward-line 1)
        (string-trim (buffer-substring-no-properties (point) (point-max)))))))

(defun claudemacs-ai-todo-add-progress (file-or-title message)
  "Add a timestamped progress entry to a TODO.
FILE-OR-TITLE can be a file path or a TODO title.
MESSAGE is the progress text to add."
  (let ((file (if (and (stringp file-or-title)
                       (file-exists-p file-or-title))
                  file-or-title
                ;; Search for file by title
                (let ((todos (claudemacs-todo--query-todos)))
                  (plist-get
                   (cl-find-if (lambda (t) (string= (plist-get t :title) file-or-title))
                               todos)
                   :file)))))
    (unless file
      (error "TODO not found: %s" file-or-title))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Find the Progress Log section
        (if (re-search-forward "^\\*\\* Progress Log" nil t)
            (progn
              (forward-line 1)
              ;; Skip any property drawer
              (when (looking-at ":PROPERTIES:")
                (re-search-forward ":END:" nil t)
                (forward-line 1))
              ;; Insert the progress entry
              (insert (format "\n- [%s] %s\n"
                              (format-time-string "%Y-%m-%d %H:%M")
                              message)))
          ;; No Progress Log section, create one at the end
          (goto-char (point-max))
          (insert (format "\n** Progress Log\n\n- [%s] %s\n"
                          (format-time-string "%Y-%m-%d %H:%M")
                          message)))
        (save-buffer)))
    (format "Added progress entry to %s" (file-name-nondirectory file))))

(defun claudemacs-ai-todo-update-status (file-or-title new-status)
  "Update the status of a TODO.
FILE-OR-TITLE can be a file path or a TODO title.
NEW-STATUS should be one of: draft, active, done, rejected."
  (let ((file (if (and (stringp file-or-title)
                       (file-exists-p file-or-title))
                  file-or-title
                ;; Search for file by title
                (let ((todos (claudemacs-todo--query-todos)))
                  (plist-get
                   (cl-find-if (lambda (t) (string= (plist-get t :title) file-or-title))
                               todos)
                   :file)))))
    (unless file
      (error "TODO not found: %s" file-or-title))
    (unless (member new-status claudemacs-todo-status-order)
      (error "Invalid status: %s. Must be one of: %s"
             new-status (string-join claudemacs-todo-status-order ", ")))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^:STATUS:\\s-*.+$" nil t)
            (replace-match (format ":STATUS: %s" new-status))
          ;; Add STATUS property if it doesn't exist
          (when (re-search-forward "^:PROPERTIES:" nil t)
            (forward-line 1)
            (insert (format ":STATUS: %s\n" new-status))))
        (save-buffer)))
    (format "Updated status to: %s" new-status)))

(defun claudemacs-ai-todo-check-acceptance (file-or-title item-text &optional checked)
  "Check or uncheck an acceptance criteria item in a TODO.
FILE-OR-TITLE can be a file path or a TODO title.
ITEM-TEXT is the text of the checkbox item to find.
CHECKED if non-nil, check the item; otherwise uncheck."
  (let ((file (if (and (stringp file-or-title)
                       (file-exists-p file-or-title))
                  file-or-title
                ;; Search for file by title
                (let ((todos (claudemacs-todo--query-todos)))
                  (plist-get
                   (cl-find-if (lambda (t) (string= (plist-get t :title) file-or-title))
                               todos)
                   :file)))))
    (unless file
      (error "TODO not found: %s" file-or-title))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Find the Acceptance Criteria section
        (unless (re-search-forward "^\\*\\* Acceptance Criteria" nil t)
          (error "No Acceptance Criteria section found"))
        ;; Find the matching item
        (let ((section-end (save-excursion
                             (if (re-search-forward "^\\*\\* " nil t)
                                 (point)
                               (point-max))))
              (found nil))
          (while (and (not found)
                      (re-search-forward "^- \\[[ X]\\] \\(.+\\)$" section-end t))
            (when (string-match-p (regexp-quote item-text) (match-string 1))
              (setq found t)
              (goto-char (match-beginning 0))
              (if checked
                  (progn
                    (re-search-forward "\\[ \\]" (line-end-position) t)
                    (replace-match "[X]"))
                (re-search-forward "\\[X\\]" (line-end-position) t)
                (replace-match "[ ]"))))
          (unless found
            (error "Acceptance criteria item not found: %s" item-text))
          (save-buffer))))
    (format "%s: %s" (if checked "Checked" "Unchecked") item-text)))

(defun claudemacs-ai-todo-get-acceptance-criteria (file-or-title)
  "Get all acceptance criteria items from a TODO.
FILE-OR-TITLE can be a file path or a TODO title.
Returns JSON array of {text, checked} objects."
  (let ((file (if (and (stringp file-or-title)
                       (file-exists-p file-or-title))
                  file-or-title
                ;; Search for file by title
                (let ((todos (claudemacs-todo--query-todos)))
                  (plist-get
                   (cl-find-if (lambda (t) (string= (plist-get t :title) file-or-title))
                               todos)
                   :file)))))
    (unless file
      (error "TODO not found: %s" file-or-title))
    (let ((criteria '()))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Find the Acceptance Criteria section
        (when (re-search-forward "^\\*\\* Acceptance Criteria" nil t)
          (let ((section-end (save-excursion
                               (if (re-search-forward "^\\*\\* " nil t)
                                   (point)
                                 (point-max)))))
            (while (re-search-forward "^- \\[\\([ X]\\)\\] \\(.+\\)$" section-end t)
              (push `((text . ,(match-string 2))
                      (checked . ,(if (string= (match-string 1) "X") t :json-false)))
                    criteria)))))
      (json-encode (nreverse criteria)))))

;;;; Global Keybindings

;; Define prefix keymaps for notes commands
(defvar claudemacs-notes-project-map (make-sparse-keymap)
  "Keymap for notes project commands (C-c n p).")

(defvar claudemacs-notes-project-list-map (make-sparse-keymap)
  "Keymap for notes project list commands (C-c n p l).")

(defvar claudemacs-notes-list-map (make-sparse-keymap)
  "Keymap for notes list commands (C-c n l).")

;; Set up the nested keymap structure:
;; C-c n p t   -> capture TODO for project
;; C-c n p l t -> list TODOs for project (filtered)
;; C-c n l t   -> list all TODOs (unfiltered)
(define-key claudemacs-notes-project-map (kbd "t") #'claudemacs-todo-capture)
(define-key claudemacs-notes-project-map (kbd "l") claudemacs-notes-project-list-map)
(define-key claudemacs-notes-project-list-map (kbd "t") #'claudemacs-todo-list-project)
(define-key claudemacs-notes-list-map (kbd "t") #'claudemacs-todo-list)

;;;###autoload
(defun claudemacs-todo-setup-global-keybindings ()
  "Set up global keybindings for TODO management.
Binds:
  C-c n p t   - Capture a new TODO for a project
  C-c n p l t - List TODOs for a project (filtered)
  C-c n l t   - List all TODOs (unfiltered)"
  (interactive)
  ;; Create C-c n prefix if it doesn't exist
  (unless (keymapp (lookup-key global-map (kbd "C-c n")))
    (define-key global-map (kbd "C-c n") (make-sparse-keymap)))
  ;; Bind C-c n p to project map
  (define-key global-map (kbd "C-c n p") claudemacs-notes-project-map)
  ;; Bind C-c n l to list map
  (define-key global-map (kbd "C-c n l") claudemacs-notes-list-map)
  (message "TODO keybindings: C-c n p t (capture), C-c n p l t (project list), C-c n l t (all)"))

;; Auto-setup keybindings when loaded
(with-eval-after-load 'claudemacs-ai-notes-todo
  (claudemacs-todo-setup-global-keybindings))

(provide 'claudemacs-ai-notes-todo)
;;; claudemacs-ai-notes-todo.el ends here
