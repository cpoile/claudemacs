;;; claudemacs-ai-messaging.el --- Multi-agent messaging system for claudemacs -*- lexical-binding: t; -*-

;; This file is part of claudemacs.

;;; Commentary:

;; This module provides multi-agent support for claudemacs, allowing:
;; - Spawning multiple claudemacs agents per project
;; - Listing all running agents
;; - Sending messages between agents
;; - Queuing messages for agents that are busy (thinking/waiting for permissions)
;;
;; Agents are identified by unique buffer names:
;; - Primary agent: *claudemacs:<directory>*
;; - Additional agents: *claudemacs:<directory>:<agent-name>*

;;; Code:

(require 'cl-lib)

;;;; Message Queue System

(defvar claudemacs-ai-message-queues (make-hash-table :test 'equal)
  "Hash table mapping buffer names to message queues.
Each queue is a list of plists with keys: :message, :sender, :timestamp.")

(defun claudemacs-ai-message-queue-add (buffer-name message sender)
  "Add MESSAGE from SENDER to the queue for BUFFER-NAME.
Returns the number of messages now in the queue."
  (let* ((queue (gethash buffer-name claudemacs-ai-message-queues '()))
         (entry (list :message message
                      :sender sender
                      :timestamp (current-time))))
    (puthash buffer-name (append queue (list entry)) claudemacs-ai-message-queues)
    (length (gethash buffer-name claudemacs-ai-message-queues))))

(defun claudemacs-ai-message-queue-get (buffer-name &optional clear)
  "Get all queued messages for BUFFER-NAME.
Returns a list of plists with keys: :message, :sender, :timestamp.
If CLEAR is non-nil, removes the messages from the queue after retrieving them."
  (let ((queue (gethash buffer-name claudemacs-ai-message-queues '())))
    (when clear
      (remhash buffer-name claudemacs-ai-message-queues))
    queue))

(defun claudemacs-ai-message-queue-peek (buffer-name)
  "Check if there are queued messages for BUFFER-NAME.
Returns the count of messages without removing them."
  (length (gethash buffer-name claudemacs-ai-message-queues '())))

(defun claudemacs-ai-message-queue-clear (buffer-name)
  "Clear all queued messages for BUFFER-NAME."
  (remhash buffer-name claudemacs-ai-message-queues))

(defun claudemacs-ai-message-queue-format (buffer-name)
  "Format queued messages for BUFFER-NAME as a human-readable string.
Designed to be shown to Claude AI agents."
  (let ((queue (gethash buffer-name claudemacs-ai-message-queues '())))
    (if (null queue)
        "No queued messages."
      (with-temp-buffer
        (insert (format "You have %d queued message%s:\n\n"
                       (length queue)
                       (if (> (length queue) 1) "s" "")))
        (dolist (entry queue)
          (let ((message (plist-get entry :message))
                (sender (plist-get entry :sender))
                (timestamp (plist-get entry :timestamp)))
            (insert (format "═══════════════════════════════════════════════════════\n"))
            (insert (format "From: %s\n" sender))
            (insert (format "Time: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))
            (insert (format "─────────────────────────────────────────────────────\n"))
            (insert message)
            (insert "\n\n")))
        (insert (format "═══════════════════════════════════════════════════════\n\n"))
        (insert "To respond to a message, use the MCP tool:\n")
        (insert "  mcp__claudemacs__message_agent\n")
        (insert "with parameters:\n")
        (insert "  buffer_name: <sender's buffer name>\n")
        (insert "  message: <your response>\n\n")
        (insert "To clear these messages after reading, use:\n")
        (insert "  mcp__claudemacs__check_messages with clear=true\n")
        (buffer-string)))))

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

(defun claudemacs-ai-find-agent-by-cwd (cwd)
  "Find the claudemacs agent buffer for CWD.
Returns the buffer name, or nil if not found.
Prefers the primary agent (without agent-name suffix) if multiple exist.
Designed to be called via MCP by Claude AI."
  (let* ((expanded-cwd (expand-file-name cwd))
         (primary-buffer (format "*claudemacs:%s*" expanded-cwd))
         (agents '()))
    ;; First check if primary agent exists
    (if (get-buffer primary-buffer)
        primary-buffer
      ;; Otherwise find any agent for this directory
      (dolist (buffer (buffer-list))
        (let ((name (buffer-name buffer)))
          (when (string-match (concat "^\\*claudemacs:"
                                     (regexp-quote expanded-cwd)
                                     "\\(?::\\(.+\\)\\)?\\*$")
                             name)
            (push name agents))))
      ;; Return the first match, or nil
      (car agents))))

(defun claudemacs-ai-check-messages (buffer-name &optional clear)
  "Check queued messages for BUFFER-NAME.
If CLEAR is non-nil, messages are removed from the queue after retrieval.
Returns a formatted string with all queued messages and instructions on how to respond.
Designed to be called via MCP by Claude AI agents to check their inbox."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (claudemacs-ai-message-queue-format buffer-name)
  (when clear
    (claudemacs-ai-message-queue-clear buffer-name))
  (claudemacs-ai-message-queue-format buffer-name))

(defun claudemacs-ai-agent-ready-p (buffer-name)
  "Check if the claudemacs agent in BUFFER-NAME is ready to receive messages.
Returns t if the agent has a prompt visible and is not busy thinking.
Returns nil if the agent is busy, waiting for input, or not ready.
Designed to be called via MCP by Claude AI."
  (when-let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (when (and (boundp 'eat-terminal) eat-terminal)
        (let* ((process (eat-term-parameter eat-terminal 'eat--process))
               ;; Get last ~1000 chars of buffer for checking
               (tail-start (max (point-min) (- (point-max) 1000)))
               (tail-content (buffer-substring-no-properties tail-start (point-max))))
          (and process
               (memq (process-status process) '(run open listen connect))
               ;; Check for the "? for shortcuts" line which appears when ready for input
               (string-match-p "? for shortcuts" tail-content)
               ;; Not showing thinking indicator
               (not (string-match-p "Thought for\\|Thinking on\\|Slithering\\|Flummoxing" tail-content))))))))


(defun claudemacs-ai-send-message-now (buffer-name message &optional from-buffer)
  "Send MESSAGE directly to the claudemacs agent in BUFFER-NAME.
FROM-BUFFER is the sender's buffer name for logging purposes.
This function sends immediately via process-send-string, bypassing any readiness checks.
The message will be queued by Claude Code if it's currently busy.
Designed to be called via MCP by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))

  (let* ((sender (or from-buffer "unknown"))
         (formatted-message (format "[Message from %s]\n%s" sender message)))

    (with-current-buffer buffer-name
      (unless (and (boundp 'eat-terminal) eat-terminal)
        (error "Buffer '%s' is not a claudemacs buffer (no eat-terminal)" buffer-name))

      (let ((process (eat-term-parameter eat-terminal 'eat--process)))
        (unless (and process (process-live-p process))
          (error "Claudemacs agent in '%s' is not running" buffer-name))

        ;; Log to message board
        (claudemacs-ai-message-board-log sender buffer-name message)

        ;; Send the message directly to the process, then submit with just carriage return
        (process-send-string process formatted-message)
        (sit-for 0.1)  ; Small delay to ensure message is processed
        (process-send-string process "\r")))

    (format "Message sent to %s from %s" buffer-name sender)))

(defun claudemacs-ai-message-agent (buffer-name message &optional from-buffer)
  "Queue MESSAGE for the claudemacs agent in BUFFER-NAME.
FROM-BUFFER is the sender's buffer name (defaults to current buffer if it's a claudemacs buffer).
The message is added to the recipient's queue and logged to the message board.
The recipient can check their messages using mcp__claudemacs__check_messages.
Designed to be called via MCP by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))

  ;; Determine sender
  (let ((sender (or from-buffer
                    (when (and (buffer-name)
                              (string-match-p "^\\*claudemacs:" (buffer-name)))
                      (buffer-name))
                    "unknown")))

    ;; Verify it's a claudemacs buffer
    (with-current-buffer buffer-name
      (unless (and (boundp 'eat-terminal) eat-terminal)
        (error "Buffer '%s' is not a claudemacs buffer (no eat-terminal)" buffer-name))

      (let ((process (eat-term-parameter eat-terminal 'eat--process)))
        (unless (and process (process-live-p process))
          (error "Claudemacs agent in '%s' is not running" buffer-name))))

    ;; Add to queue
    (let ((queue-size (claudemacs-ai-message-queue-add buffer-name message sender)))
      ;; Log to message board
      (claudemacs-ai-message-board-log sender buffer-name message)

      (format "Message queued for %s from %s (%d message%s in queue). The recipient can check messages using mcp__claudemacs__check_messages."
              buffer-name sender queue-size (if (> queue-size 1) "s" "")))))

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
