;;; claudemacs-ai-notes.el --- Structured org-mode notes for Claude AI -*- lexical-binding: t; -*-
;; Author: Christopher Poile <cpoile@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: claudecode ai emacs llm tools org-mode
;; URL: https://github.com/cpoile/claudemacs
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Structured org-mode notes system for Claude AI.
;; Notes are stored in .claude/claudemacs-notes.org and organized into sections:
;;
;; * SUMMARIES - Session summaries and work history
;; * TODOS - Tasks and work items
;; * CONCEPTS - Key concepts, patterns, and architecture understanding
;; * TOOLS - Tool usage notes, common commands, project-specific helpers
;; * DOCUMENTATION - File/directory documentation, API notes
;; * ARCHIVE - Completed/archived items
;;
;; The design principle is database-like querying rather than full-buffer
;; reads/writes. Each section can be queried independently, with automatic
;; timestamps and tagging support.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-id)

;;;; Org-roam Integration (optional)

(defvar claudemacs-notes-use-org-roam t
  "When non-nil and org-roam is available, create notes as org-roam nodes.")

(defun claudemacs-notes--roam-available-p ()
  "Return non-nil if org-roam is available and enabled."
  (and claudemacs-notes-use-org-roam
       (featurep 'org-roam)))

(defun claudemacs-notes--setup-roam-capture-templates ()
  "Add claudemacs-specific capture templates to org-roam."
  (when (claudemacs-notes--roam-available-p)
    (require 'org-roam)
    (let ((project-name (claudemacs-notes--project-name)))
      ;; Add claudemacs templates if not already present
      (unless (assoc "cc" org-roam-capture-templates)
        (setq org-roam-capture-templates
              (append org-roam-capture-templates
                      `(("cc" "Claudemacs Concept" plain "%?"
                         :target (file+head
                                  ,(concat "projects/" project-name "/concept-${slug}.org")
                                  ":PROPERTIES:
:ID: %(format-time-string \"%%Y%%m%%dT%%H%%M%%S\")
:ADDED: %(format-time-string \"%%Y-%%m-%%d\")
:PROJECT: ${project}
:CATEGORY: CONCEPTS
:END:
#+title: ${title}
#+filetags: :ai_generated:claudemacs:concepts:${project}:
")
                         :unnarrowed t)
                        ("cd" "Claudemacs Documentation" plain "%?"
                         :target (file+head
                                  ,(concat "projects/" project-name "/doc-${slug}.org")
                                  ":PROPERTIES:
:ID: %(format-time-string \"%%Y%%m%%dT%%H%%M%%S\")
:ADDED: %(format-time-string \"%%Y-%%m-%%d\")
:PROJECT: ${project}
:CATEGORY: DOCUMENTATION
:END:
#+title: ${title}
#+filetags: :ai_generated:claudemacs:documentation:${project}:
")
                         :unnarrowed t)
                        ("ct" "Claudemacs Tool" plain "%?"
                         :target (file+head
                                  ,(concat "projects/" project-name "/tool-${slug}.org")
                                  ":PROPERTIES:
:ID: %(format-time-string \"%%Y%%m%%dT%%H%%M%%S\")
:ADDED: %(format-time-string \"%%Y-%%m-%%d\")
:PROJECT: ${project}
:CATEGORY: TOOLS
:END:
#+title: ${title}
#+filetags: :ai_generated:claudemacs:tools:${project}:
")
                         :unnarrowed t)
                        ("cs" "Claudemacs Summary" plain "%?"
                         :target (file+head
                                  ,(concat "projects/" project-name "/summary-${slug}.org")
                                  ":PROPERTIES:
:ID: %(format-time-string \"%%Y%%m%%dT%%H%%M%%S\")
:ADDED: %(format-time-string \"%%Y-%%m-%%d\")
:PROJECT: ${project}
:CATEGORY: SUMMARIES
:END:
#+title: ${title}
#+filetags: :ai_generated:claudemacs:summaries:${project}:
")
                         :unnarrowed t))))))))

;; Interactive capture functions for keybindings
(defun claudemacs-notes-capture-concept ()
  "Capture a new concept using org-roam."
  (interactive)
  (claudemacs-notes--setup-roam-capture-templates)
  (if (claudemacs-notes--roam-available-p)
      (let ((org-roam-capture-templates
             (list (assoc "cc" org-roam-capture-templates))))
        (org-roam-capture))
    (call-interactively #'claudemacs-notes-add-concept-interactive)))

(defun claudemacs-notes-capture-documentation ()
  "Capture new documentation using org-roam."
  (interactive)
  (claudemacs-notes--setup-roam-capture-templates)
  (if (claudemacs-notes--roam-available-p)
      (let ((org-roam-capture-templates
             (list (assoc "cd" org-roam-capture-templates))))
        (org-roam-capture))
    (call-interactively #'claudemacs-notes-add-documentation-interactive)))

(defun claudemacs-notes-capture-tool ()
  "Capture a new tool note using org-roam."
  (interactive)
  (claudemacs-notes--setup-roam-capture-templates)
  (if (claudemacs-notes--roam-available-p)
      (let ((org-roam-capture-templates
             (list (assoc "ct" org-roam-capture-templates))))
        (org-roam-capture))
    (call-interactively #'claudemacs-notes-add-tool-interactive)))

(defun claudemacs-notes-capture-summary ()
  "Capture a new summary using org-roam."
  (interactive)
  (claudemacs-notes--setup-roam-capture-templates)
  (if (claudemacs-notes--roam-available-p)
      (let ((org-roam-capture-templates
             (list (assoc "cs" org-roam-capture-templates))))
        (org-roam-capture))
    (call-interactively #'claudemacs-notes-add-summary-interactive)))

;;;; TODO Capture with Claudemacs Integration

(defvar claudemacs-notes--todo-target-project nil
  "The projectile project root for the current TODO capture.
Set during capture and used by the finalize hook to route to claudemacs.")

(defun claudemacs-notes--select-project ()
  "Prompt user to select a projectile project.
Returns the project root path."
  (if (and (fboundp 'projectile-known-projects)
           (projectile-known-projects))
      (completing-read "Project: " (projectile-known-projects)
                       nil t nil nil (projectile-project-root))
    (read-directory-name "Project directory: ")))

(defun claudemacs-notes--get-claudemacs-buffer-for-project (project-root)
  "Find the claudemacs buffer for PROJECT-ROOT.
Returns the buffer or nil if not found."
  (let ((expanded-root (expand-file-name project-root)))
    (cl-find-if
     (lambda (buf)
       (and (string-match-p "^\\*claudemacs:" (buffer-name buf))
            (with-current-buffer buf
              (and (boundp 'claudemacs--cwd)
                   (string= (expand-file-name claudemacs--cwd) expanded-root)))))
     (buffer-list))))

(defvar claudemacs-notes--todo-title nil
  "Cached title from the TODO capture buffer.")

(defvar claudemacs-notes--todo-body nil
  "Cached body from the TODO capture buffer.")

(defun claudemacs-notes--cache-todo-content ()
  "Cache the TODO content before the capture buffer is killed.
Called from org-capture-prepare-finalize-hook."
  (when claudemacs-notes--todo-target-project
    (setq claudemacs-notes--todo-title
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
              (match-string 1))))
    (setq claudemacs-notes--todo-body
          (save-excursion
            (goto-char (point-min))
            ;; Skip to after filetags line
            (when (re-search-forward "^#\\+filetags:" nil t)
              (forward-line 1)
              (string-trim (buffer-substring-no-properties (point) (point-max))))))))

(defun claudemacs-notes--send-todo-to-claudemacs ()
  "Send the captured TODO to the appropriate claudemacs session.
Called from org-capture-after-finalize-hook."
  (when claudemacs-notes--todo-target-project
    (let* ((project-root claudemacs-notes--todo-target-project)
           (claude-buffer (claudemacs-notes--get-claudemacs-buffer-for-project project-root))
           (title claudemacs-notes--todo-title)
           (body claudemacs-notes--todo-body))
      (when (and claude-buffer title)
        (let ((message-text (format "[TODO] %s\n\n%s" title (or body ""))))
          (with-current-buffer claude-buffer
            (when (and (boundp 'eat-terminal) eat-terminal)
              (eat-term-send-string eat-terminal "\C-u")
              (eat-term-send-string eat-terminal message-text)
              (sit-for 0.1)
              (eat-term-send-string eat-terminal "\r")))
          (message "Sent TODO to claudemacs: %s" title)))
      ;; Clear state
      (setq claudemacs-notes--todo-target-project nil
            claudemacs-notes--todo-title nil
            claudemacs-notes--todo-body nil))))

(defun claudemacs-notes-capture-todo ()
  "Capture a new TODO and optionally send to claudemacs.
Prompts for a projectile project to associate with the TODO.
On C-c C-c, the TODO is sent to the claudemacs session for that project."
  (interactive)
  (unless (claudemacs-notes--roam-available-p)
    (user-error "org-roam is required for TODO capture"))
  ;; Select project first
  (let* ((project-root (claudemacs-notes--select-project))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (project-dir (expand-file-name (concat "projects/" project-name) org-roam-directory)))
    ;; Store for the finalize hook
    (setq claudemacs-notes--todo-target-project project-root)
    ;; Ensure project directory exists
    (unless (file-directory-p project-dir)
      (make-directory project-dir t))
    ;; Add hooks for this capture (removed after finalize)
    (add-hook 'org-capture-prepare-finalize-hook #'claudemacs-notes--cache-todo-content)
    (add-hook 'org-capture-after-finalize-hook #'claudemacs-notes--send-todo-to-claudemacs)
    (add-hook 'org-capture-after-finalize-hook #'claudemacs-notes--cleanup-todo-hooks)
    ;; Set up capture template dynamically
    (let ((org-roam-capture-templates
           `(("t" "TODO for Claude" plain "%?"
              :target (file+head
                       ,(concat "projects/" project-name "/todo-${slug}.org")
                       ,(format ":PROPERTIES:
:ID: %%(format-time-string \"%%%%Y%%%%m%%%%dT%%%%H%%%%M%%%%S\")
:ADDED: %%(format-time-string \"%%%%Y-%%%%m-%%%%d\")
:PROJECT: %s
:PROJECT_ROOT: %s
:CATEGORY: TODOS
:END:
#+title: ${title}
#+filetags: :todo:%s:
" project-name project-root project-name))
              :unnarrowed t))))
      (org-roam-capture))))

(defun claudemacs-notes--cleanup-todo-hooks ()
  "Remove the TODO capture hooks after finalize."
  (remove-hook 'org-capture-prepare-finalize-hook #'claudemacs-notes--cache-todo-content)
  (remove-hook 'org-capture-after-finalize-hook #'claudemacs-notes--send-todo-to-claudemacs)
  (remove-hook 'org-capture-after-finalize-hook #'claudemacs-notes--cleanup-todo-hooks))

(defun claudemacs-notes--project-name ()
  "Get a short project name from the current work directory."
  (let ((work-dir (claudemacs-ai-notes--get-work-dir)))
    (file-name-nondirectory (directory-file-name work-dir))))

(defun claudemacs-notes--roam-project-dir ()
  "Get the org-roam project directory for the current claudemacs session."
  (when (claudemacs-notes--roam-available-p)
    (let ((project-name (claudemacs-notes--project-name)))
      (expand-file-name (concat "projects/" project-name) org-roam-directory))))

(defun claudemacs-notes--ensure-roam-project-dir ()
  "Ensure the org-roam project directory exists."
  (when-let ((dir (claudemacs-notes--roam-project-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun claudemacs-notes--roam-node-file (title category)
  "Generate a roam node filename for TITLE in CATEGORY.
Returns full path like ~/org-roam/projects/claudemacs/doc-filename.org"
  (when-let ((project-dir (claudemacs-notes--ensure-roam-project-dir)))
    (let* ((slug (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" (downcase title)))
           (slug (replace-regexp-in-string "^-\\|-$" "" slug))
           (prefix (pcase category
                     ("DOCUMENTATION" "doc")
                     ("CONCEPTS" "concept")
                     ("TOOLS" "tool")
                     ("TODOS" "todo")
                     ("SUMMARIES" "summary")
                     (_ "note")))
           (filename (format "%s-%s.org" prefix slug)))
      (expand-file-name filename project-dir))))

(defun claudemacs-notes--create-roam-node (title body &optional category tags)
  "Create an org-roam node with TITLE and BODY.
CATEGORY is the type (DOCUMENTATION, CONCEPTS, etc).
TAGS are optional org tags.
Returns the file path of the created node."
  (when (claudemacs-notes--roam-available-p)
    (let* ((file-path (claudemacs-notes--roam-node-file title category))
           ;; Use timestamp-based ID for readability
           (id (format-time-string "%Y%m%dT%H%M%S"))
           (date-stamp (format-time-string "%Y-%m-%d"))
           (project-name (claudemacs-notes--project-name))
           (tag-string (if tags
                          (concat " :" (mapconcat #'identity
                                                  (if (listp tags) tags (list tags))
                                                  ":") ":")
                        "")))
      (with-temp-file file-path
        (insert (format ":PROPERTIES:
:ID: %s
:ADDED: %s
:PROJECT: %s
:CATEGORY: %s
:END:
#+title: %s
#+filetags: :ai_generated:claudemacs:%s:%s%s

%s
"
                        id
                        date-stamp
                        project-name
                        (or category "NOTE")
                        title
                        (downcase (or category "note"))
                        (downcase project-name)
                        tag-string
                        (or body ""))))
      ;; Update org-roam db
      (org-roam-db-update-file file-path)
      file-path)))

;;;; Constants - Section Schema

(defconst claudemacs-notes-sections
  '(("SUMMARIES" . "Session summaries and work history")
    ("TODOS" . "Tasks and work items")
    ("CONCEPTS" . "Key concepts, patterns, and architecture")
    ("TOOLS" . "Tool usage, commands, project helpers")
    ("DOCUMENTATION" . "File/directory docs, API notes")
    ("ARCHIVE" . "Completed and archived items"))
  "Standard sections for the notes file.
Each section is a (NAME . DESCRIPTION) pair.")

(defconst claudemacs-notes-default-template
  "#+TITLE: Claudemacs Session Notes
#+STARTUP: overview

* SUMMARIES
:PROPERTIES:
:DESCRIPTION: Session summaries and work history
:END:

* TODOS
:PROPERTIES:
:DESCRIPTION: Tasks and work items
:END:

* CONCEPTS
:PROPERTIES:
:DESCRIPTION: Key concepts, patterns, and architecture
:END:

* TOOLS
:PROPERTIES:
:DESCRIPTION: Tool usage, commands, project helpers
:END:

* DOCUMENTATION
:PROPERTIES:
:DESCRIPTION: File/directory docs, API notes
:END:

* ARCHIVE                                                          :ARCHIVE:
:PROPERTIES:
:DESCRIPTION: Completed and archived items
:END:
"
  "Default template for a new notes file.")

;;;; Buffer Management (imported from claudemacs-ai.el)

;; Dynamic variable for session context (set by MCP server via let binding)
(defvar claudemacs-session-cwd nil
  "The working directory for the current claudemacs session.
Set by the MCP server via a let binding to provide session context.
This must be defvar'd to be dynamically scoped in lexical-binding mode.")

(defun claudemacs-ai-notes--get-work-dir ()
  "Get the working directory for the notes file.
Checks for `claudemacs-session-cwd' (set by MCP server via let binding),
then falls back to other methods."
  (cond
   ;; First priority: MCP server provides cwd via let binding
   ((and (boundp 'claudemacs-session-cwd) claudemacs-session-cwd)
    claudemacs-session-cwd)
   ;; If in a claudemacs buffer, use its cwd
   ((and (boundp 'claudemacs--cwd) claudemacs--cwd)
    claudemacs--cwd)
   ;; Extract from buffer name pattern *claudemacs:/path/to/dir/*
   ((and (buffer-name)
         (string-match "^\\*claudemacs:\\(.*\\)\\*$" (buffer-name)))
    (match-string 1 (buffer-name)))
   ;; Fallback to default-directory
   (t default-directory)))

(defun claudemacs-ai-notes--get-file-path ()
  "Get the file path for the notes file.
Returns path to .claude/claudemacs-notes.org in the project directory."
  (let ((work-dir (claudemacs-ai-notes--get-work-dir)))
    (expand-file-name ".claude/claudemacs-notes.org" work-dir)))

(defun claudemacs-ai-notes--get-buffer-name ()
  "Generate a nice buffer name for the notes file.
Uses the full path for consistency with other claudemacs buffers."
  (let ((work-dir (claudemacs-ai-notes--get-work-dir)))
    (format "*claudemacs-notes:%s*" work-dir)))

(defun claudemacs-ai-notes--ensure-buffer ()
  "Ensure the notes file exists and return it.
The buffer is backed by .claude/claudemacs-notes.org for persistence.
Uses `org-mode' for structured note-taking.
Creates the standard section structure if the file is new."
  (let* ((file-path (claudemacs-ai-notes--get-file-path))
         (file-path-expanded (expand-file-name file-path))
         (dir (file-name-directory file-path-expanded))
         (nice-name (claudemacs-ai-notes--get-buffer-name)))
    ;; First check if buffer already exists by nice-name
    (or (get-buffer nice-name)
        ;; Then check if file is already being visited
        (find-buffer-visiting file-path-expanded)
        ;; Otherwise create it
        (progn
          ;; Ensure .claude directory exists
          (unless (file-directory-p dir)
            (make-directory dir t))
          ;; Open or create the file
          (let ((buf (find-file-noselect file-path-expanded)))
            (with-current-buffer buf
              (unless (derived-mode-p 'org-mode)
                (org-mode))
              (setq-local buffer-read-only nil)
              ;; Initialize with template if empty
              (when (= (buffer-size) 0)
                (insert claudemacs-notes-default-template)
                (save-buffer))
              ;; Rename to a nicer name (still saves to file-path)
              (rename-buffer nice-name t)  ; t = make unique if needed
              ;; Auto-save when buffer is modified
              (add-hook 'after-change-functions
                        (lambda (&rest _)
                          (when (buffer-modified-p)
                            (save-buffer)))
                        nil t))
            buf)))))

;;;; Timestamp Helpers

(defun claudemacs-ai-notes--inactive-timestamp ()
  "Return an inactive org timestamp for now."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun claudemacs-ai-notes--active-timestamp ()
  "Return an active org timestamp for now."
  (format-time-string "<%Y-%m-%d %a %H:%M>"))

(defun claudemacs-ai-notes--date-stamp ()
  "Return a simple date stamp for ADDED properties."
  (format-time-string "%Y-%m-%d"))

;;;; Section Navigation

(defun claudemacs-ai-notes--goto-section (section-name)
  "Go to the beginning of SECTION-NAME in the notes buffer.
Returns the position if found, nil otherwise."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (goto-char (point-min))
    ;; Match section name, allowing for optional tags at end of line
    (when (re-search-forward (format "^\\* %s\\(\\s-\\|$\\)" (regexp-quote section-name)) nil t)
      (beginning-of-line)
      (point))))

(defun claudemacs-ai-notes--section-bounds (section-name)
  "Return (START . END) bounds for SECTION-NAME, or nil if not found."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (save-excursion
      (when (claudemacs-ai-notes--goto-section section-name)
        (let ((start (point)))
          (org-end-of-subtree t t)
          (cons start (point)))))))

;;;; Schema Query Tools - Section Overview

(defun claudemacs-ai-notes-list-sections ()
  "List all top-level sections in the notes file.
Returns a list of section names with their descriptions and item counts.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let (sections)
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) 1)
           (let* ((title (org-get-heading t t t t))
                  (desc (org-entry-get (point) "DESCRIPTION"))
                  (child-count 0))
             ;; Count direct children
             (save-excursion
               (let ((end (save-excursion (org-end-of-subtree t) (point))))
                 (forward-line 1)
                 (while (< (point) end)
                   (when (and (org-at-heading-p)
                              (= (org-current-level) 2))
                     (setq child-count (1+ child-count)))
                   (forward-line 1))))
             (push (list :section title
                         :description (or desc "")
                         :items child-count)
                   sections))))
       nil nil)
      (nreverse sections))))

;;;; Section-Specific Query Tools

(defun claudemacs-ai-notes-get-section-headings (section-name &optional include-body)
  "Get all headings within SECTION-NAME.
Returns a list of heading info for each item in the section.
If INCLUDE-BODY is non-nil, also include the body text (first ~200 chars).
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((bounds (claudemacs-ai-notes--section-bounds section-name))
          headings)
      (when bounds
        (save-excursion
          (goto-char (car bounds))
          (let ((end (cdr bounds)))
            (forward-line 1)  ; Skip section heading
            (while (< (point) end)
              (when (and (org-at-heading-p)
                         (= (org-current-level) 2))
                (let* ((title (org-get-heading t t t t))
                       (todo (org-get-todo-state))
                       (tags (org-get-tags))
                       (added (org-entry-get (point) "ADDED"))
                       (priority (org-get-priority (thing-at-point 'line t)))
                       (body nil))
                  ;; Optionally get body preview
                  (when include-body
                    (save-excursion
                      (org-end-of-meta-data t)
                      (let ((body-start (point))
                            (body-end (save-excursion
                                       (if (re-search-forward "^\\*" end t)
                                           (1- (point))
                                         end))))
                        (setq body (string-trim
                                   (buffer-substring-no-properties
                                    body-start
                                    (min body-end (+ body-start 200))))))))
                  (push (list :title title
                              :todo todo
                              :tags tags
                              :added added
                              :priority priority
                              :body body
                              :pos (point))
                        headings)))
              (forward-line 1)))))
      (or (nreverse headings)
          (format "No items in section: %s" section-name)))))

(defun claudemacs-ai-notes-get-todos (&optional include-body)
  "Get all TODO items from the TODOS section.
If INCLUDE-BODY is non-nil, include body text preview.
Designed to be called via emacsclient by Claude AI."
  (claudemacs-ai-notes-get-section-headings "TODOS" include-body))

(defun claudemacs-ai-notes-get-concepts (&optional include-body)
  "Get all items from the CONCEPTS section.
If INCLUDE-BODY is non-nil, include body text preview.
Designed to be called via emacsclient by Claude AI."
  (claudemacs-ai-notes-get-section-headings "CONCEPTS" include-body))

(defun claudemacs-ai-notes-get-tools (&optional include-body)
  "Get all items from the TOOLS section.
If INCLUDE-BODY is non-nil, include body text preview.
Designed to be called via emacsclient by Claude AI."
  (claudemacs-ai-notes-get-section-headings "TOOLS" include-body))

(defun claudemacs-ai-notes-get-documentation (&optional include-body)
  "Get all items from the DOCUMENTATION section.
If INCLUDE-BODY is non-nil, include body text preview.
Designed to be called via emacsclient by Claude AI."
  (claudemacs-ai-notes-get-section-headings "DOCUMENTATION" include-body))

(defun claudemacs-ai-notes-get-summaries (&optional include-body)
  "Get all items from the SUMMARIES section.
If INCLUDE-BODY is non-nil, include body text preview.
Designed to be called via emacsclient by Claude AI."
  (claudemacs-ai-notes-get-section-headings "SUMMARIES" include-body))

;;;; Section-Specific Add Tools

(defun claudemacs-ai-notes--add-to-section (section-name level title &optional body tags properties todo)
  "Add an entry to SECTION-NAME with LEVEL, TITLE, and optional BODY, TAGS, PROPERTIES, TODO.
Automatically adds ADDED property with current date.
Returns confirmation message."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((section-pos (claudemacs-ai-notes--goto-section section-name)))
      (unless section-pos
        (error "Section '%s' not found" section-name))
      ;; Now at section heading - go to the heading first
      (org-back-to-heading t)
      ;; Move past the heading line and any property drawer
      (org-end-of-meta-data t)
      ;; Ensure we have a blank line before inserting
      (unless (bolp)
        (insert "\n"))
      ;; Build the entry text
      (let ((entry-text
             (concat
              ;; Heading line
              (make-string (1+ level) ?*) " "
              (when todo (concat todo " "))
              title
              (when tags
                ;; Org-mode requires at least 2 spaces before tags for them to be recognized
                (concat "  " (if (string-prefix-p ":" tags) tags (concat ":" tags ":"))))
              "\n"
              ;; Properties
              ":PROPERTIES:\n"
              (format ":ADDED: %s\n" (claudemacs-ai-notes--date-stamp))
              (mapconcat (lambda (prop)
                           (format ":%s: %s\n" (car prop) (cdr prop)))
                         properties "")
              ":END:\n"
              ;; Body
              (when body (concat body "\n"))
              "\n")))
        (insert entry-text))
      (save-buffer)
      (format "Added to %s: %s" section-name title))))

(defun claudemacs-ai-notes-add-todo-item (title &optional body tags priority)
  "Add a TODO item to the TODOS section.
TITLE is the heading text.
BODY is optional body text.
TAGS is optional tags string (e.g., ':urgent:bug:').
PRIORITY is optional A, B, or C.
Designed to be called via emacsclient by Claude AI."
  (let ((full-title (if priority
                        (format "[#%s] %s" (upcase priority) title)
                      title)))
    (claudemacs-ai-notes--add-to-section "TODOS" 1 full-title body tags nil "TODO")))

(defun claudemacs-ai-notes-add-concept (title &optional body tags)
  "Add a concept to the CONCEPTS section.
TITLE is the concept name.
BODY is the explanation/notes.
TAGS is optional tags string.
Also creates an org-roam node if org-roam is available.
Designed to be called via emacsclient by Claude AI."
  (claudemacs-ai-notes--add-to-section "CONCEPTS" 1 title body tags nil nil)
  (when-let ((roam-file (claudemacs-notes--create-roam-node title body "CONCEPTS"
                                                            (when tags (split-string tags ":" t)))))
    (message "Created org-roam node: %s" roam-file)))

(defun claudemacs-ai-notes-add-tool (title &optional body tags)
  "Add a tool/command note to the TOOLS section.
TITLE is the tool/command name.
BODY is usage notes.
TAGS is optional tags string.
Also creates an org-roam node if org-roam is available.
Designed to be called via emacsclient by Claude AI."
  (claudemacs-ai-notes--add-to-section "TOOLS" 1 title body tags nil nil)
  (when-let ((roam-file (claudemacs-notes--create-roam-node title body "TOOLS"
                                                            (when tags (split-string tags ":" t)))))
    (message "Created org-roam node: %s" roam-file)))

(defun claudemacs-ai-notes-add-documentation (title &optional body tags file-path)
  "Add documentation to the DOCUMENTATION section.
TITLE is the file/directory/API name.
BODY is the documentation text.
TAGS is optional tags string.
FILE-PATH is optional associated file path (stored as FILE property).
Also creates an org-roam node if org-roam is available.
Designed to be called via emacsclient by Claude AI."
  (let ((properties (when file-path (list (cons "FILE" file-path)))))
    ;; Add to local notes file
    (claudemacs-ai-notes--add-to-section "DOCUMENTATION" 1 title body tags properties nil)
    ;; Also create org-roam node if available
    (when-let ((roam-file (claudemacs-notes--create-roam-node title body "DOCUMENTATION"
                                                               (when tags (split-string tags ":" t)))))
      (message "Created org-roam node: %s" roam-file))))

(defun claudemacs-ai-notes-add-summary (title &optional body)
  "Add a session summary to the SUMMARIES section.
TITLE is a brief summary title.
BODY is the detailed summary.
Also creates an org-roam node if org-roam is available.
Designed to be called via emacsclient by Claude AI."
  (claudemacs-ai-notes--add-to-section "SUMMARIES" 1 title body nil nil nil)
  (when-let ((roam-file (claudemacs-notes--create-roam-node title body "SUMMARIES" nil)))
    (message "Created org-roam node: %s" roam-file)))

;;;; Query by Tag

(defun claudemacs-ai-notes-query-by-tag (tag &optional section-name)
  "Find all entries with TAG, optionally limited to SECTION-NAME.
Returns list of matching entries with their section, title, and position.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((matches nil)
          (tag-pattern (if (string-prefix-p ":" tag)
                          tag
                        (concat ":" tag ":"))))
      (org-map-entries
       (lambda ()
         (when (>= (org-current-level) 2)
           (let ((tags (org-get-tags)))
             (when (and tags (member (replace-regexp-in-string ":" "" tag) tags))
               (let ((parent-section
                      (save-excursion
                        (while (and (org-up-heading-safe)
                                    (> (org-current-level) 1)))
                        (when (= (org-current-level) 1)
                          (org-get-heading t t t t)))))
                 (when (or (null section-name)
                           (string= parent-section section-name))
                   (push (list :section parent-section
                               :title (org-get-heading t t t t)
                               :todo (org-get-todo-state)
                               :pos (point))
                         matches)))))))
       nil nil)
      (or (nreverse matches)
          (format "No entries found with tag: %s" tag)))))

;;;; Get Entry by Title

(defun claudemacs-ai-notes-get-entry (title &optional section-name)
  "Get full content of entry with TITLE, optionally in SECTION-NAME.
Returns the complete entry including body and properties.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (save-excursion
      (goto-char (point-min))
      (let ((search-bounds (if section-name
                               (claudemacs-ai-notes--section-bounds section-name)
                             (cons (point-min) (point-max)))))
        (when search-bounds
          (goto-char (car search-bounds))
          (if (re-search-forward
               (format org-complex-heading-regexp-format (regexp-quote title))
               (cdr search-bounds) t)
              (progn
                (beginning-of-line)
                (let* ((start (point))
                       (end (save-excursion (org-end-of-subtree t) (point)))
                       (content (buffer-substring-no-properties start end))
                       (props (org-entry-properties (point)))
                       (tags (org-get-tags))
                       (todo (org-get-todo-state)))
                  (list :title title
                        :todo todo
                        :tags tags
                        :properties props
                        :content content)))
            (format "Entry not found: %s" title)))))))

;;;; Update Entry

(defun claudemacs-ai-notes-update-entry (title new-body &optional section-name)
  "Update the body of entry with TITLE, optionally in SECTION-NAME.
Replaces existing body content with NEW-BODY.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (save-excursion
      (goto-char (point-min))
      (let ((search-bounds (if section-name
                               (claudemacs-ai-notes--section-bounds section-name)
                             (cons (point-min) (point-max)))))
        (when search-bounds
          (goto-char (car search-bounds))
          (if (re-search-forward
               (format org-complex-heading-regexp-format (regexp-quote title))
               (cdr search-bounds) t)
              (progn
                (beginning-of-line)
                (org-end-of-meta-data t)
                (let ((body-start (point))
                      (body-end (save-excursion
                                 (if (re-search-forward "^\\*" nil t)
                                     (1- (line-beginning-position))
                                   (point-max)))))
                  (delete-region body-start body-end)
                  (insert new-body "\n")
                  ;; Update UPDATED property
                  (org-back-to-heading t)
                  (org-set-property "UPDATED" (claudemacs-ai-notes--date-stamp))
                  (save-buffer)
                  (format "Updated: %s" title)))
            (format "Entry not found: %s" title)))))))

;;;; Complete TODO

(defun claudemacs-ai-notes-complete-todo (title &optional archive)
  "Mark TODO with TITLE as DONE and optionally ARCHIVE it.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (save-excursion
      (let ((bounds (claudemacs-ai-notes--section-bounds "TODOS")))
        (when bounds
          (goto-char (car bounds))
          (if (re-search-forward
               (format org-complex-heading-regexp-format (regexp-quote title))
               (cdr bounds) t)
              (progn
                (beginning-of-line)
                (org-todo "DONE")
                (org-set-property "COMPLETED" (claudemacs-ai-notes--date-stamp))
                (when archive
                  ;; Move to ARCHIVE section using cut/paste
                  (org-cut-subtree)
                  (claudemacs-ai-notes--goto-section "ARCHIVE")
                  (org-back-to-heading t)
                  (org-end-of-meta-data t)
                  (org-paste-subtree 2))  ; Level 2 = child of ARCHIVE
                (save-buffer)
                (format "Completed%s: %s" (if archive " and archived" "") title))
            (format "TODO not found: %s" title)))))))

;;;; Recent Items

(defun claudemacs-ai-notes-get-recent (&optional days section-name)
  "Get entries added within DAYS (default 7), optionally in SECTION-NAME.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let* ((days (or days 7))
           (cutoff (time-subtract (current-time) (days-to-time days)))
           (cutoff-str (format-time-string "%Y-%m-%d" cutoff))
           matches)
      (org-map-entries
       (lambda ()
         (when (>= (org-current-level) 2)
           (let ((added (org-entry-get (point) "ADDED")))
             (when (and added (string>= added cutoff-str))
               (let ((parent-section
                      (save-excursion
                        (while (and (org-up-heading-safe)
                                    (> (org-current-level) 1)))
                        (when (= (org-current-level) 1)
                          (org-get-heading t t t t)))))
                 (when (or (null section-name)
                           (string= parent-section section-name))
                   (push (list :section parent-section
                               :title (org-get-heading t t t t)
                               :added added
                               :todo (org-get-todo-state)
                               :pos (point))
                         matches)))))))
       nil nil)
      (or (sort (nreverse matches)
                (lambda (a b)
                  (string> (plist-get a :added) (plist-get b :added))))
          (format "No entries in the last %d days" days)))))

;;;; Legacy API - For backward compatibility with existing tools

;; These functions maintain the original API from claudemacs-ai.el

(defun claudemacs-ai-get-notes ()
  "Get the content of the notes file for this session.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun claudemacs-ai-set-notes (content)
  "Set the notes file content to CONTENT, replacing any existing content.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (erase-buffer)
    (insert content)
    (format "Notes updated: %d characters" (length content))))

(defun claudemacs-ai-append-notes (content)
  "Append CONTENT to the notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert content)
    (format "Appended %d characters to notes" (length content))))

(defun claudemacs-ai-clear-notes ()
  "Clear the notes file for this session.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (erase-buffer)
    "Notes cleared"))

;;;; All the original org-mode operations from claudemacs-ai.el
;; Re-exported here for organization

(defun claudemacs-ai-notes-add-heading (level title)
  "Add a heading with LEVEL stars and TITLE to the notes file.
LEVEL should be 1-6. Adds at point-max.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert (make-string level ?*) " " title "\n")
    (format "Added level %d heading: %s" level title)))

(defun claudemacs-ai-notes-add-todo (level title &optional priority)
  "Add a TODO heading with LEVEL stars and TITLE to notes file.
Optional PRIORITY should be A, B, or C.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert (make-string level ?*) " TODO ")
    (when priority
      (insert (format "[#%s] " (upcase priority))))
    (insert title "\n")
    (format "Added TODO: %s" title)))

(defun claudemacs-ai-notes-toggle-todo ()
  "Toggle TODO state of the heading at point in notes file.
Cycles through: unmarked -> TODO -> DONE -> unmarked.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-todo)
    (let ((state (org-get-todo-state)))
      (format "TODO state: %s" (or state "none")))))

(defun claudemacs-ai-notes-set-todo-state (state)
  "Set the TODO STATE of heading at point in notes file.
STATE should be TODO, DONE, or empty string to clear.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-todo (if (string-empty-p state) 'none state))
    (format "Set TODO state to: %s" (or state "none"))))

(defun claudemacs-ai-notes-add-timestamp (&optional inactive)
  "Add current timestamp at point-max in notes file.
If INACTIVE is non-nil, use inactive timestamp [date] instead of <date>.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (goto-char (point-max))
    (let ((ts (if inactive
                  (claudemacs-ai-notes--inactive-timestamp)
                (claudemacs-ai-notes--active-timestamp))))
      (insert ts)
      (format "Added timestamp: %s" ts))))

(defun claudemacs-ai-notes-get-headings ()
  "Get all headings from the notes file as a structured list.
Returns list of (level title todo-state) for each heading.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-map-entries
     (lambda ()
       (list (org-current-level)
             (org-get-heading t t t t)
             (org-get-todo-state)))
     nil nil)))

(defun claudemacs-ai-notes-goto-heading (title)
  "Go to the first heading matching TITLE in notes file.
Returns the position or nil if not found.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (goto-char (point-min))
    (if (re-search-forward (format org-complex-heading-regexp-format (regexp-quote title)) nil t)
        (progn
          (org-beginning-of-line)
          (format "Moved to heading: %s at position %d" title (point)))
      (format "Heading not found: %s" title))))

(defun claudemacs-ai-notes-add-list-item (text &optional checkbox)
  "Add a list item with TEXT to notes file at point-max.
If CHECKBOX is non-nil, add a checkbox.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    (insert "- ")
    (when checkbox
      (insert "[ ] "))
    (insert text "\n")
    (format "Added list item: %s" text)))

(defun claudemacs-ai-notes-toggle-checkbox ()
  "Toggle checkbox at current line in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-toggle-checkbox)
    "Toggled checkbox"))

(defun claudemacs-ai-notes-schedule (timestamp)
  "Add SCHEDULED timestamp to heading at point in notes file.
TIMESTAMP should be org-compatible like '<2024-01-15 Mon>' or '+1d'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-schedule nil timestamp)
    (format "Scheduled: %s" timestamp)))

(defun claudemacs-ai-notes-deadline (timestamp)
  "Add DEADLINE timestamp to heading at point in notes file.
TIMESTAMP should be org-compatible like '<2024-01-15 Mon>' or '+1d'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-deadline nil timestamp)
    (format "Deadline: %s" timestamp)))

(defun claudemacs-ai-notes-set-property (property value)
  "Set PROPERTY to VALUE on heading at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-set-property property value)
    (format "Set property %s = %s" property value)))

(defun claudemacs-ai-notes-set-tags (tags)
  "Set TAGS on heading at point in notes file.
TAGS should be a colon-separated string like ':tag1:tag2:'.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-set-tags tags)
    (format "Set tags: %s" tags)))

(defun claudemacs-ai-notes-clock-in ()
  "Start clocking time on heading at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-clock-in)
    "Clock started"))

(defun claudemacs-ai-notes-clock-out ()
  "Stop clocking time in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-clock-out)
    "Clock stopped"))

(defun claudemacs-ai-notes-add-note ()
  "Add a note to heading at point in notes file (with timestamp).
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-add-note)
    "Note drawer added - ready for input"))

(defun claudemacs-ai-notes-set-effort (effort)
  "Set effort estimate EFFORT on heading at point.
EFFORT should be like '1:30' for 1 hour 30 minutes.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-set-effort nil effort)
    (format "Effort set: %s" effort)))

(defun claudemacs-ai-notes-archive-subtree ()
  "Archive the subtree at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-archive-subtree)
    "Subtree archived"))

(defun claudemacs-ai-notes-insert-link (url &optional description)
  "Insert an org link to URL with optional DESCRIPTION at point-max.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (goto-char (point-max))
    (if description
        (insert (format "[[%s][%s]]" url description))
      (insert (format "[[%s]]" url)))
    (format "Inserted link: %s" url)))

(defun claudemacs-ai-notes-sparse-tree (query)
  "Create a sparse tree in notes file matching QUERY.
QUERY can be a tag match like '+work-urgent' or a property match.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-match-sparse-tree nil query)
    (format "Sparse tree created for: %s" query)))

(defun claudemacs-ai-notes-get-property (property)
  "Get the value of PROPERTY from heading at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (or (org-entry-get (point) property)
        "nil")))

(defun claudemacs-ai-notes-promote ()
  "Promote heading at point (decrease level) in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-promote-subtree)
    "Heading promoted"))

(defun claudemacs-ai-notes-demote ()
  "Demote heading at point (increase level) in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-demote-subtree)
    "Heading demoted"))

(defun claudemacs-ai-notes-move-up ()
  "Move subtree at point up in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-move-subtree-up)
    "Subtree moved up"))

(defun claudemacs-ai-notes-move-down ()
  "Move subtree at point down in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-move-subtree-down)
    "Subtree moved down"))

(defun claudemacs-ai-notes-get-all-properties ()
  "Get all properties for heading at point in notes file.
Returns an alist of (property . value) pairs.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((props (org-entry-properties (point))))
      (or props "No properties at point"))))

(defun claudemacs-ai-notes-delete-property (property)
  "Delete PROPERTY from heading at point in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-entry-delete (point) property)
    (format "Deleted property: %s" property)))

(defun claudemacs-ai-notes-refile (target-heading)
  "Refile (move) subtree at point to TARGET-HEADING in notes file.
TARGET-HEADING should be the heading title to refile under.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let* ((target-pos (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward
                                (format org-complex-heading-regexp-format
                                        (regexp-quote target-heading))
                                nil t)
                           (point-at-bol)))))
      (if target-pos
          (let ((org-refile-targets `((nil . (:regexp . ,(regexp-quote target-heading))))))
            (org-refile nil nil (list target-heading (buffer-file-name) nil target-pos))
            (format "Refiled to: %s" target-heading))
        (format "Target heading not found: %s" target-heading)))))

(defun claudemacs-ai-notes-sort (&optional sorting-type)
  "Sort children of current heading in notes file.
SORTING-TYPE can be: alpha, num, time, func, priority, todo.
Defaults to alpha (alphabetical).
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((type (pcase (or sorting-type "alpha")
                  ("alpha" ?a)
                  ("num" ?n)
                  ("time" ?t)
                  ("func" ?f)
                  ("priority" ?p)
                  ("todo" ?o)
                  (_ ?a))))
      (org-sort-entries nil type)
      (format "Sorted entries by: %s" (or sorting-type "alpha")))))

(defun claudemacs-ai-notes-up-heading ()
  "Move to parent heading in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (if (org-up-heading-safe)
        (format "Moved to parent: %s at position %d"
                (org-get-heading t t t t) (point))
      "Already at top level")))

(defun claudemacs-ai-notes-next-heading ()
  "Move to next heading at same level in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (if (org-forward-heading-same-level 1)
        "No more headings at this level"
      (format "Moved to: %s at position %d"
              (org-get-heading t t t t) (point)))))

(defun claudemacs-ai-notes-prev-heading ()
  "Move to previous heading at same level in notes file.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (if (org-backward-heading-same-level 1)
        "No previous headings at this level"
      (format "Moved to: %s at position %d"
              (org-get-heading t t t t) (point)))))

(defun claudemacs-ai-notes-occur (regexp)
  "Search for REGEXP in notes file and return matches.
Returns a list of matching lines with positions.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (save-excursion
      (goto-char (point-min))
      (let (matches)
        (while (re-search-forward regexp nil t)
          (push (list :pos (match-beginning 0)
                      :line (line-number-at-pos)
                      :match (match-string 0)
                      :context (string-trim (thing-at-point 'line t)))
                matches))
        (or (nreverse matches)
            (format "No matches for: %s" regexp))))))

(defun claudemacs-ai-notes-get-tags ()
  "Get tags for heading at point in notes file.
Returns the tags as a list.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((tags (org-get-tags)))
      (or tags "No tags"))))

(defun claudemacs-ai-notes-cut-subtree ()
  "Cut (kill) subtree at point in notes file.
The subtree is stored in the kill ring for later pasting.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((heading (org-get-heading t t t t)))
      (org-cut-subtree)
      (format "Cut subtree: %s" heading))))

(defun claudemacs-ai-notes-copy-subtree ()
  "Copy subtree at point in notes file.
The subtree is stored in the kill ring for later pasting.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((heading (org-get-heading t t t t)))
      (org-copy-subtree)
      (format "Copied subtree: %s" heading))))

(defun claudemacs-ai-notes-paste-subtree (&optional level)
  "Paste subtree from kill ring in notes file.
Optional LEVEL specifies the heading level for pasted tree.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (org-paste-subtree level)
    "Pasted subtree"))

(defun claudemacs-ai-notes-get-heading-at-point ()
  "Get detailed info about heading at point in notes file.
Returns plist with level, title, todo-state, tags, priority, and position.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (if (org-at-heading-p)
        (list :level (org-current-level)
              :title (org-get-heading t t t t)
              :todo (org-get-todo-state)
              :tags (org-get-tags)
              :priority (org-get-priority (thing-at-point 'line t))
              :pos (point))
      "Not at a heading")))

(defun claudemacs-ai-notes-get-subtree ()
  "Get the content of subtree at point in notes file.
Returns the full text of the subtree including the heading.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (if (org-at-heading-p)
        (org-copy-subtree)
      (org-back-to-heading t)
      (org-copy-subtree))
    (current-kill 0 t)))

(defun claudemacs-ai-notes-get-children ()
  "Get direct children headings of current heading in notes file.
Returns list of (level, title, todo-state) for immediate children only.
Designed to be called via emacsclient by Claude AI."
  (with-current-buffer (claudemacs-ai-notes--ensure-buffer)
    (let ((parent-level (org-current-level))
          children)
      (when parent-level
        (save-excursion
          (org-back-to-heading t)
          (let ((end (save-excursion (org-end-of-subtree t) (point))))
            (forward-line 1)
            (while (< (point) end)
              (when (and (org-at-heading-p)
                         (= (org-current-level) (1+ parent-level)))
                (push (list :level (org-current-level)
                            :title (org-get-heading t t t t)
                            :todo (org-get-todo-state))
                      children))
              (forward-line 1)))))
      (or (nreverse children) "No children"))))

;;;; Minor Mode for Notes Buffer Interaction

(defvar claudemacs-notes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Send to Claude - only bindings that make sense in notes buffer
    (define-key map (kbd "C-c c c") #'claudemacs-notes-send-section)
    (define-key map (kbd "C-c c s") #'claudemacs-notes-send-subtree)
    (define-key map (kbd "C-c c r") #'claudemacs-notes-send-region)
    map)
  "Keymap for `claudemacs-notes-mode'.

Keybindings:
  C-c c c  Send current section to claudemacs
  C-c c s  Send subtree to claudemacs
  C-c c r  Send region to claudemacs

For org-roam, use the standard C-c n prefix (globally available):
  C-c n f  Find/create node
  C-c n i  Insert link to node
  C-c n l  Toggle backlinks buffer")

(defun claudemacs-notes--get-work-dir-from-buffer ()
  "Get the work directory from the current notes buffer name.
Notes buffer: *claudemacs-notes:/path/to/dir*
Returns the path portion."
  (let ((buf-name (buffer-name)))
    (when (string-match "^\\*claudemacs-notes:\\(.*\\)\\*$" buf-name)
      (match-string 1 buf-name))))

(defconst claudemacs-notes-send-prefix
  "[FROM NOTES] "
  "Prefix added to messages sent from notes buffer to claudemacs.
This helps Claude identify messages sent via the notes integration.")

(defun claudemacs-notes--send-to-claudemacs (text)
  "Send TEXT to the associated claudemacs session with prefix and auto-submit."
  (let ((work-dir (claudemacs-notes--get-work-dir-from-buffer)))
    (if work-dir
        (let* ((prefixed-text (concat claudemacs-notes-send-prefix text))
               (buf (get-buffer (format "*claudemacs:%s/*" work-dir))))
          (if buf
              (progn
                (with-current-buffer buf
                  (eat-term-send-string eat-terminal "\C-u")
                  (eat-term-send-string eat-terminal prefixed-text)
                  ;; Need sit-for to let the terminal process before sending return
                  ;; (this is what claudemacs--send-message-when-ready does)
                  (sit-for 0.1)
                  (eat-term-send-string eat-terminal "\r"))
                (message "Sent to claudemacs: %s..." (truncate-string-to-width text 50)))
            (user-error "Claudemacs buffer not found for %s" work-dir)))
      (user-error "Not in a claudemacs notes buffer"))))

(defun claudemacs-notes--get-claudemacs-buffer ()
  "Get the claudemacs buffer associated with this notes buffer.
Notes buffer: *claudemacs-notes:/path/to/dir*
Claudemacs buffer: *claudemacs:/path/to/dir/*"
  (when-let ((path (claudemacs-notes--get-work-dir-from-buffer)))
    (get-buffer (format "*claudemacs:%s/*" path))))

(defun claudemacs-notes-send-section ()
  "Send the current org section content to the associated claudemacs session.
This sends just the body content of the current heading (not the heading itself)."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((heading (org-get-heading t t t t))
         (todo-state (org-get-todo-state))
         (tags (org-get-tags))
         ;; Get body content
         (body-start (save-excursion
                      (org-end-of-meta-data t)
                      (point)))
         (body-end (save-excursion
                    (org-end-of-subtree t t)
                    (skip-chars-backward " \t\n")
                    (point)))
         (body (string-trim (buffer-substring-no-properties body-start body-end)))
         ;; Build the message
         (message-text
          (concat
           (when todo-state (format "[%s] " todo-state))
           heading
           (when tags (format " %s" (mapconcat (lambda (t) (concat ":" t ":")) tags "")))
           (when (not (string-empty-p body))
             (concat "\n\n" body)))))
    (claudemacs-notes--send-to-claudemacs message-text)))

(defun claudemacs-notes-send-subtree ()
  "Send the entire subtree (heading + body + children) to claudemacs."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((start (point))
         (end (save-excursion
               (org-end-of-subtree t t)
               (point)))
         (content (buffer-substring-no-properties start end)))
    (claudemacs-notes--send-to-claudemacs (string-trim content))))

(defun claudemacs-notes-send-region (start end)
  "Send the selected region to claudemacs."
  (interactive "r")
  (let ((content (buffer-substring-no-properties start end)))
    (claudemacs-notes--send-to-claudemacs (string-trim content))))

;;;###autoload
(define-minor-mode claudemacs-notes-mode
  "Minor mode for interacting with claudemacs from notes buffers.

\\{claudemacs-notes-mode-map}

Key bindings:
  Send to Claude:
    C-c c c     Send current section to claudemacs
    C-c c s     Send entire subtree to claudemacs
    C-c c r     Send selected region to claudemacs

  Capture new notes (org-roam):
    C-c c n c   New concept
    C-c c n d   New documentation
    C-c c n t   New tool note
    C-c c n s   New summary

  Org-roam:
    C-c c f     Find/create node
    C-c c i     Insert link to node
    C-c c l     Toggle backlinks buffer"
  :lighter " ClaudeNotes"
  :keymap claudemacs-notes-mode-map)

(defun claudemacs-notes--maybe-enable-mode ()
  "Enable `claudemacs-notes-mode' if this is a claudemacs notes buffer."
  (when (string-match-p "^\\*claudemacs-notes:" (buffer-name))
    (claudemacs-notes-mode 1)))

;; Auto-enable in claudemacs notes buffers
(add-hook 'org-mode-hook #'claudemacs-notes--maybe-enable-mode)

;; Note: Worktree/TODO management has been moved to claudemacs-ai-notes-todo.el
;; Use C-c n p t to create project TODOs
;; From TODO nodes: C-c c t (main session) or C-c c w (worktree)

(provide 'claudemacs-ai-notes)
;;; claudemacs-ai-notes.el ends here
