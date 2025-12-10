;;; claudemacs-sessions.el --- Session monitoring for claudemacs -*- lexical-binding: t; -*-

;; This file is part of claudemacs.

;;; Commentary:

;; This module provides a session monitoring buffer for claudemacs, allowing:
;; - View all running claudemacs sessions in a table
;; - See session status (alive, thinking, waiting for input)
;; - Open/switch to sessions with RET
;; - Kill or restart sessions from the list
;; - Automatic refresh every 2 seconds when buffer is active

;;; Code:

(require 'tabulated-list)
(require 'cl-lib)

;;;; Customization

(defgroup claudemacs-sessions nil
  "Session monitoring for claudemacs."
  :group 'claudemacs)

(defcustom claudemacs-sessions-refresh-interval 2.0
  "Interval in seconds between automatic refreshes of the sessions buffer."
  :type 'number
  :group 'claudemacs-sessions)

(defcustom claudemacs-sessions-buffer-name "*claudemacs-sessions*"
  "Name of the sessions monitoring buffer."
  :type 'string
  :group 'claudemacs-sessions)

;;;; Faces

(defface claudemacs-sessions-status-ready
  '((t :foreground "#00ff00" :weight bold))
  "Face for sessions that are ready for input."
  :group 'claudemacs-sessions)

(defface claudemacs-sessions-status-thinking
  '((t :foreground "#ffaa00" :weight bold))
  "Face for sessions that are thinking/processing."
  :group 'claudemacs-sessions)

(defface claudemacs-sessions-status-typing
  '((t :foreground "#66ccff" :weight bold))
  "Face for sessions where user is typing."
  :group 'claudemacs-sessions)

(defface claudemacs-sessions-status-waiting
  '((t :foreground "#ff66ff" :weight bold))
  "Face for sessions waiting for user input."
  :group 'claudemacs-sessions)

(defface claudemacs-sessions-status-dead
  '((t :foreground "#ff0000" :weight bold))
  "Face for sessions that are dead/not running."
  :group 'claudemacs-sessions)

(defface claudemacs-sessions-project
  '((t :foreground "#88aaff"))
  "Face for project directory column."
  :group 'claudemacs-sessions)

(defface claudemacs-sessions-buffer
  '((t :foreground "#aaaaff"))
  "Face for buffer name column."
  :group 'claudemacs-sessions)

(defface claudemacs-sessions-label
  '((t :foreground "#aaffaa"))
  "Face for session label column."
  :group 'claudemacs-sessions)

;;;; Internal Variables

(defvar claudemacs-sessions--refresh-timer nil
  "Timer for automatic refresh of the sessions buffer.")

;;;; Status Detection Functions

(defun claudemacs-sessions--get-session-status (buffer)
  "Get the status of a claudemacs session BUFFER.
Returns one of: `ready', `thinking', `waiting', or `dead'."
  (unless (buffer-live-p buffer)
    (cl-return-from claudemacs-sessions--get-session-status 'dead))

  (with-current-buffer buffer
    (if (not (and (boundp 'eat-terminal) eat-terminal))
        'dead
      (let* ((process (eat-term-parameter eat-terminal 'eat--process)))
        (if (not (and process (memq (process-status process) '(run open listen connect))))
            'dead
          ;; Process is running, check status
          ;; Look at the last ~1000 chars which should contain the prompt area
          (let* ((tail-start (max (point-min) (- (point-max) 1000)))
                 (tail-content (buffer-substring-no-properties tail-start (point-max))))
            (cond
             ;; Thinking: Claude is actively processing
             ((string-match-p "esc to interrupt" tail-content)
              'thinking)
             ;; Typing: Has "> " prompt with text after it (user is composing)
             ((string-match-p "─\n>..+.\n─" tail-content)
              'typing)
             ;; Ready: Has empty "> " prompt between horizontal lines
             ((string-match-p "─\n>..\n─" tail-content)
              'ready)
             ;; Otherwise waiting for user input (edit approval, multi-select, etc.)
             (t 'waiting))))))))

(defun claudemacs-sessions--format-status (status)
  "Format STATUS symbol into a display string with appropriate face."
  (pcase status
    ('ready (propertize "Ready" 'face 'claudemacs-sessions-status-ready))
    ('thinking (propertize "Thinking" 'face 'claudemacs-sessions-status-thinking))
    ('typing (propertize "Typing" 'face 'claudemacs-sessions-status-typing))
    ('waiting (propertize "Waiting" 'face 'claudemacs-sessions-status-waiting))
    ('dead (propertize "Dead" 'face 'claudemacs-sessions-status-dead))
    (_ (propertize "Unknown" 'face 'font-lock-comment-face))))

(defun claudemacs-sessions--parse-buffer-name (buffer-name)
  "Parse claudemacs buffer name into components.
Returns plist with :directory, :label, and :project."
  (when (string-match "^\\*claudemacs:\\([^:*]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name)
    (let* ((directory (match-string 1 buffer-name))
           (label (match-string 2 buffer-name))
           (project (file-name-nondirectory (directory-file-name directory))))
      (list :directory directory
            :label (or label "main")
            :project project))))

(defun claudemacs-sessions--get-all-sessions ()
  "Get list of all claudemacs session buffers with their info.
Returns a list of plists with session information."
  (let (sessions)
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (string-match-p "^\\*claudemacs:" name)
          (let* ((parsed (claudemacs-sessions--parse-buffer-name name))
                 (status (claudemacs-sessions--get-session-status buffer)))
            (when parsed
              (push (append parsed
                           (list :buffer-name name
                                 :buffer buffer
                                 :status status))
                    sessions))))))
    (nreverse sessions)))

;;;; Tabulated List Mode Implementation

(defun claudemacs-sessions--get-entries ()
  "Get tabulated list entries for all claudemacs sessions."
  (mapcar
   (lambda (session)
     (let* ((buffer-name (plist-get session :buffer-name))
            (project (plist-get session :project))
            (directory (plist-get session :directory))
            (label (plist-get session :label))
            (status (plist-get session :status)))
       (list buffer-name
             (vector
              (propertize project 'face 'claudemacs-sessions-project)
              (propertize buffer-name 'face 'claudemacs-sessions-buffer)
              (propertize label 'face 'claudemacs-sessions-label)
              (claudemacs-sessions--format-status status)
              directory))))
   (claudemacs-sessions--get-all-sessions)))

(defun claudemacs-sessions--get-marked-ids ()
  "Get list of buffer IDs that are marked with D."
  (let (marked)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (push (tabulated-list-get-id) marked))
        (forward-line 1)))
    marked))

(defun claudemacs-sessions--restore-marks (marked-ids)
  "Restore D marks on entries with IDs in MARKED-IDS."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (member (tabulated-list-get-id) marked-ids)
        (tabulated-list-put-tag "D"))
      (forward-line 1))))

(defun claudemacs-sessions-refresh ()
  "Refresh the sessions buffer content, preserving marks."
  (interactive)
  (when-let ((buffer (get-buffer claudemacs-sessions-buffer-name)))
    (with-current-buffer buffer
      (let ((pos (point))
            (marked (claudemacs-sessions--get-marked-ids)))
        (tabulated-list-revert)
        (claudemacs-sessions--restore-marks marked)
        (goto-char (min pos (point-max)))))))

(defun claudemacs-sessions--start-auto-refresh ()
  "Start the auto-refresh timer for the sessions buffer."
  (claudemacs-sessions--stop-auto-refresh)
  (setq claudemacs-sessions--refresh-timer
        (run-with-timer claudemacs-sessions-refresh-interval
                        claudemacs-sessions-refresh-interval
                        #'claudemacs-sessions--auto-refresh)))

(defun claudemacs-sessions--stop-auto-refresh ()
  "Stop the auto-refresh timer."
  (when claudemacs-sessions--refresh-timer
    (cancel-timer claudemacs-sessions--refresh-timer)
    (setq claudemacs-sessions--refresh-timer nil)))

(defun claudemacs-sessions--auto-refresh ()
  "Auto-refresh callback that only refreshes if buffer is visible."
  (let ((buffer (get-buffer claudemacs-sessions-buffer-name)))
    (if (and buffer (get-buffer-window buffer 'visible))
        (claudemacs-sessions-refresh)
      ;; Buffer not visible, stop the timer
      (claudemacs-sessions--stop-auto-refresh))))

;;;; Interactive Commands

(defun claudemacs-sessions-open-session ()
  "Open the session at point."
  (interactive)
  (when-let ((buffer-name (tabulated-list-get-id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (progn
          (display-buffer buffer)
          (select-window (get-buffer-window buffer)))
      (message "Buffer %s no longer exists" buffer-name))))

(defun claudemacs-sessions-kill-session ()
  "Kill the session at point."
  (interactive)
  (when-let ((buffer-name (tabulated-list-get-id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (when (yes-or-no-p (format "Kill session %s? " buffer-name))
          (with-current-buffer buffer
            (when (and (boundp 'eat-terminal) eat-terminal)
              (let ((process (eat-term-parameter eat-terminal 'eat--process)))
                (when (and process (process-live-p process))
                  (kill-process process)))))
          (kill-buffer buffer)
          (claudemacs-sessions-refresh)
          (message "Killed session %s" buffer-name))
      (message "Buffer %s no longer exists" buffer-name))))

(defun claudemacs-sessions-restart-session ()
  "Restart the session at point."
  (interactive)
  (when-let ((buffer-name (tabulated-list-get-id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (when (yes-or-no-p (format "Restart session %s? " buffer-name))
          (require 'claudemacs)
          ;; Get work-dir before killing buffer
          (let ((work-dir (with-current-buffer buffer
                           (or claudemacs--cwd
                               (when (string-match "^\\*claudemacs:\\([^:*]+\\)" buffer-name)
                                 (match-string 1 buffer-name))))))
            ;; Use claudemacs-restart to handle the restart properly
            (claudemacs-restart work-dir buffer-name)
            (message "Restarting session %s..." buffer-name)))
      (message "Buffer %s no longer exists" buffer-name))))

(defun claudemacs-sessions-mark-for-kill ()
  "Mark the session at point for killing."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun claudemacs-sessions-unmark ()
  "Remove mark from the session at point."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun claudemacs-sessions-execute-marks ()
  "Execute the marked operations."
  (interactive)
  (let ((kill-list '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (push (tabulated-list-get-id) kill-list))
        (forward-line 1)))
    (when kill-list
      (when (yes-or-no-p (format "Kill %d marked session(s)? " (length kill-list)))
        (dolist (buffer-name kill-list)
          (when-let ((buffer (get-buffer buffer-name)))
            (with-current-buffer buffer
              (when (and (boundp 'eat-terminal) eat-terminal)
                (let ((process (eat-term-parameter eat-terminal 'eat--process)))
                  (when (and process (process-live-p process))
                    (kill-process process)))))
            (kill-buffer buffer)))
        (claudemacs-sessions-refresh)
        (message "Killed %d session(s)" (length kill-list))))))

(defun claudemacs-sessions-display-buffer ()
  "Display the session at point in another window without selecting it."
  (interactive)
  (when-let ((buffer-name (tabulated-list-get-id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (display-buffer buffer)
      (message "Buffer %s no longer exists" buffer-name))))

;;;; Mode Definition

(defvar claudemacs-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claudemacs-sessions-open-session)
    (define-key map (kbd "o") #'claudemacs-sessions-display-buffer)
    (define-key map (kbd "R") #'claudemacs-sessions-restart-session)
    (define-key map (kbd "g") #'claudemacs-sessions-refresh)
    (define-key map (kbd "d") #'claudemacs-sessions-mark-for-kill)
    (define-key map (kbd "u") #'claudemacs-sessions-unmark)
    (define-key map (kbd "x") #'claudemacs-sessions-execute-marks)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `claudemacs-sessions-mode'.")

;; Evil mode support
(with-eval-after-load 'evil
  (evil-define-key 'normal claudemacs-sessions-mode-map
    (kbd "RET") #'claudemacs-sessions-open-session
    (kbd "o") #'claudemacs-sessions-display-buffer
    (kbd "R") #'claudemacs-sessions-restart-session
    (kbd "gr") #'claudemacs-sessions-refresh
    (kbd "d") #'claudemacs-sessions-mark-for-kill
    (kbd "u") #'claudemacs-sessions-unmark
    (kbd "x") #'claudemacs-sessions-execute-marks
    (kbd "q") #'quit-window)
  (evil-define-key 'motion claudemacs-sessions-mode-map
    (kbd "RET") #'claudemacs-sessions-open-session
    (kbd "o") #'claudemacs-sessions-display-buffer
    (kbd "q") #'quit-window))

(define-derived-mode claudemacs-sessions-mode tabulated-list-mode "Claudemacs-Sessions"
  "Major mode for viewing and managing claudemacs sessions.

\\{claudemacs-sessions-mode-map}"
  (setq tabulated-list-format
        [("Project" 20 t)
         ("Buffer" 50 t)
         ("Label" 15 t)
         ("Status" 10 t)
         ("Directory" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Project" . nil))
  (setq tabulated-list-entries #'claudemacs-sessions--get-entries)
  (tabulated-list-init-header)
  ;; Start auto-refresh when entering mode
  (add-hook 'kill-buffer-hook #'claudemacs-sessions--stop-auto-refresh nil t)
  ;; Start/stop timer based on window visibility
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (if (get-buffer-window (current-buffer) 'visible)
                  (claudemacs-sessions--start-auto-refresh)
                (claudemacs-sessions--stop-auto-refresh)))
            nil t))

;;;; Entry Point

;;;###autoload
(defun claudemacs-list-sessions ()
  "Display a buffer listing all claudemacs sessions.
The buffer updates automatically every 2 seconds while visible."
  (interactive)
  (let ((buffer (get-buffer-create claudemacs-sessions-buffer-name)))
    (with-current-buffer buffer
      (claudemacs-sessions-mode)
      (tabulated-list-print))
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))
    ;; Start auto-refresh
    (claudemacs-sessions--start-auto-refresh)))

(provide 'claudemacs-sessions)
;;; claudemacs-sessions.el ends here
