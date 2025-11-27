;;; claudemacs-ai-messaging.el --- Multi-agent messaging system for claudemacs -*- lexical-binding: t; -*-

;; This file is part of claudemacs.

;;; Commentary:

;; This module provides multi-agent support for claudemacs, allowing:
;; - Spawning multiple claudemacs agents per project
;; - Listing all running agents
;; - Sending messages between agents
;;
;; Agents are identified by unique buffer names:
;; - Primary agent: *claudemacs:<directory>*
;; - Additional agents: *claudemacs:<directory>:<agent-name>*

;;; Code:

(require 'cl-lib)

(defun claudemacs-ai-spawn-agent (directory &optional agent-name initial-prompt)
  "Spawn a new claudemacs agent in DIRECTORY.
AGENT-NAME is an optional identifier for the agent (auto-generated if not provided).
INITIAL-PROMPT is an optional message to send after the agent starts.
Returns the buffer name of the new agent.
Designed to be called via MCP by Claude AI."
  (require 'claudemacs)
  (let* ((expanded-dir (expand-file-name directory))
         ;; Auto-generate agent name if not provided
         (agent-id (or agent-name
                      (let ((existing-agents (claudemacs-ai-list-agents)))
                        ;; Find all agents for this directory
                        ;; existing-agents is a vector of vectors
                        (let* ((same-dir-agents (seq-filter
                                                 (lambda (agent)
                                                   (equal (aref agent 1) expanded-dir))
                                                 existing-agents))
                               (count (length same-dir-agents)))
                          (if (zerop count)
                              nil  ; First agent, no suffix
                            (format "agent-%d" count))))))
         ;; Generate buffer name
         (buffer-name (if agent-id
                         (format "*claudemacs:%s:%s*" expanded-dir agent-id)
                       (format "*claudemacs:%s*" expanded-dir)))
         (claudemacs-switch-to-buffer-on-create nil))

    ;; Check if buffer already exists
    (when (get-buffer buffer-name)
      (error "Agent already exists with buffer name: %s" buffer-name))

    ;; Check directory exists
    (unless (file-directory-p expanded-dir)
      (error "Directory does not exist: %s" expanded-dir))

    ;; Spawn the agent
    (let ((default-directory expanded-dir))
      ;; Temporarily override claudemacs--get-buffer-name to return our custom name
      (cl-letf (((symbol-function 'claudemacs--get-buffer-name)
                 (lambda () buffer-name)))
        (claudemacs--start expanded-dir)))

    ;; Send initial prompt if provided
    (when initial-prompt
      (claudemacs-ai-message-agent buffer-name initial-prompt))

    buffer-name))

(defun claudemacs-ai-list-agents ()
  "List all running claudemacs agent sessions.
Returns a list of (buffer-name directory) tuples.
Designed to be called via MCP by Claude AI."
  (let ((agents '()))
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (string-match "^\\*claudemacs:\\([^:]+\\)\\(?::\\(.+\\)\\)?\\*$" name)
          (let ((directory (match-string 1 name)))
            ;; Use vectors so json-encode treats them as arrays, not objects
            (push (vector name directory) agents)))))
    (vconcat (nreverse agents))))

(defun claudemacs-ai-message-agent (buffer-name message &optional from-buffer)
  "Send MESSAGE to the claudemacs agent in BUFFER-NAME.
FROM-BUFFER is the sender's buffer name (defaults to current buffer if it's a claudemacs buffer).
The message is prefixed with sender context and logged to the message board.
Designed to be called via MCP by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))

  ;; Determine sender
  (let* ((sender (or from-buffer
                    (when (and (buffer-name)
                              (string-match-p "^\\*claudemacs:" (buffer-name)))
                      (buffer-name))
                    "unknown"))
         ;; Format message with metadata
         (formatted-message (format "[Message from %s]\n%s" sender message)))

    (with-current-buffer buffer-name
      (unless (and (boundp 'eat-terminal) eat-terminal)
        (error "Buffer '%s' is not a claudemacs buffer (no eat-terminal)" buffer-name))

      (let ((process (eat-term-parameter eat-terminal 'eat--process)))
        (unless (and process (process-live-p process))
          (error "Claudemacs agent in '%s' is not running" buffer-name))

        ;; Log to message board
        (claudemacs-ai-message-board-log sender buffer-name message)

        ;; Send the message using eat's input mechanism
        ;; This properly simulates user input and triggers submission
        (dolist (char (string-to-list formatted-message))
          (eat-self-input 1 char))
        ;; Send carriage return to submit
        (eat-self-input 1 ?\r)))

    (format "Message sent to %s from %s" buffer-name sender)))

;;;; Message Board (Org-Mode Buffer)

(defconst claudemacs-ai-message-board-buffer "*claudemacs:message-board*"
  "Buffer name for the message board.")

(defun claudemacs-ai-message-board-get-buffer ()
  "Get or create the message board buffer."
  (let ((buf (get-buffer-create claudemacs-ai-message-board-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'org-mode)
        (org-mode)
        (insert "#+TITLE: Claudemacs Message Board\n")
        (insert "#+STARTUP: overview\n\n")
        (insert "* Log\n\n")))
    buf))

(defun claudemacs-ai-message-board-log (sender recipient message)
  "Log a message from SENDER to RECIPIENT on the message board.
Creates an org hierarchy: * Log -> ** Recipient -> *** Sender -> **** Message."
  (with-current-buffer (claudemacs-ai-message-board-get-buffer)
    (let ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
      ;; Find or create recipient heading
      (goto-char (point-min))
      (unless (re-search-forward (concat "^\\*\\* " (regexp-quote recipient) "$") nil t)
        (goto-char (point-max))
        (insert (format "** %s\n" recipient)))

      ;; Find or create sender heading under recipient
      (let ((recipient-end (save-excursion
                            (or (and (re-search-forward "^\\*\\* " nil t)
                                     (match-beginning 0))
                                (point-max)))))
        (goto-char (point-min))
        (re-search-forward (concat "^\\*\\* " (regexp-quote recipient) "$"))
        (unless (re-search-forward (concat "^\\*\\*\\* " (regexp-quote sender) "$") recipient-end t)
          (goto-char recipient-end)
          (insert (format "*** %s\n" sender))))

      ;; Add message under sender
      (let ((sender-end (save-excursion
                         (or (and (re-search-forward "^\\*\\*\\* " nil t)
                                  (match-beginning 0))
                             (and (re-search-forward "^\\*\\* " nil t)
                                  (match-beginning 0))
                             (point-max)))))
        (goto-char sender-end)
        (insert (format "**** %s\n%s\n\n" timestamp message))))))

(defun claudemacs-ai-message-board-get ()
  "Get the message board buffer content as a string."
  (with-current-buffer (claudemacs-ai-message-board-get-buffer)
    (buffer-string)))

(defun claudemacs-ai-message-board-clear ()
  "Clear the message board."
  (when-let ((buf (get-buffer claudemacs-ai-message-board-buffer)))
    (kill-buffer buf)))

(defun claudemacs-ai-message-board-summary ()
  "Get a human-readable summary of the message board.
Counts messages by sender/recipient pairs."
  (let ((counts (make-hash-table :test 'equal)))
    (with-current-buffer (claudemacs-ai-message-board-get-buffer)
      (save-excursion
        (goto-char (point-min))
        ;; Parse org hierarchy: ** recipient, *** sender, **** message
        (while (re-search-forward "^\\*\\* \\(.+\\)$" nil t)
          (let ((recipient (match-string 1))
                (recipient-end (save-excursion
                                (or (and (re-search-forward "^\\*\\* " nil t)
                                         (match-beginning 0))
                                    (point-max)))))
            ;; Find all senders under this recipient
            (while (re-search-forward "^\\*\\*\\* \\(.+\\)$" recipient-end t)
              (let* ((sender (match-string 1))
                     (sender-end (save-excursion
                                  (or (and (re-search-forward "^\\*\\*\\* " nil t)
                                           (match-beginning 0))
                                      (and (re-search-forward "^\\*\\* " nil t)
                                           (match-beginning 0))
                                      (point-max))))
                     ;; Count messages (level 4 headings)
                     (message-count 0))
                (save-excursion
                  (while (re-search-forward "^\\*\\*\\*\\* " sender-end t)
                    (setq message-count (1+ message-count))))
                (puthash (cons sender recipient) message-count counts)))))))

    ;; Format summary
    (if (zerop (hash-table-count counts))
        "No messages logged yet."
      (with-temp-buffer
        (insert "Message Board Summary:\n")
        (insert "=====================\n\n")
        ;; Group by sender
        (let ((by-sender (make-hash-table :test 'equal)))
          (maphash (lambda (key count)
                     (let ((sender (car key))
                           (recipient (cdr key)))
                       (push (cons recipient count)
                             (gethash sender by-sender))))
                   counts)
          (maphash (lambda (sender recipients)
                     (insert (format "From: %s\n" sender))
                     (dolist (entry recipients)
                       (let ((recipient (car entry))
                             (count (cdr entry)))
                         (insert (format "  → %s: %d message%s\n"
                                       recipient count (if (> count 1) "s" "")))))
                     (insert "\n"))
                   by-sender))
        (buffer-string)))))

(provide 'claudemacs-ai-messaging)
;;; claudemacs-ai-messaging.el ends here
