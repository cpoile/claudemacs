;;; test-server-info.el --- Check Emacs server configuration -*- lexical-binding: t; -*-

(message "=== Emacs Server Information ===")
(message "server-socket-dir: %s" (if (boundp 'server-socket-dir) server-socket-dir "NOT BOUND"))
(message "server-name: %s" (if (boundp 'server-name) server-name "NOT BOUND"))
(message "server-running-p: %s" (if (fboundp 'server-running-p) (server-running-p) "FUNCTION NOT DEFINED"))
(message "server-socket-dir exists?: %s" (if (and (boundp 'server-socket-dir) server-socket-dir)
                                               (file-exists-p server-socket-dir)
                                             "N/A"))

(when (and (boundp 'server-socket-dir) server-socket-dir)
  (let ((socket-file (expand-file-name "server" server-socket-dir)))
    (message "Expected socket file: %s" socket-file)
    (message "Socket file exists?: %s" (file-exists-p socket-file))))

(message "\n=== Testing ai-setup-claude-environment ===")
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'claudemacs-ai)
(claudemacs-ai-setup-claude-environment)
(message "CLAUDEMACS_SOCKET after setup: %s" (getenv "CLAUDEMACS_SOCKET"))
