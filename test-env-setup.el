;;; test-env-setup.el --- Test claudemacs environment setup -*- lexical-binding: t; -*-

;; Load required files
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'claudemacs-ai)

;; Mock server setup
(setq server-socket-dir "/tmp/emacs501")
(defun server-running-p () t)

;; Test 1: Check claudemacs-ai-get-cli-path
(message "Test 1: CLI Path")
(message "  CLI path: %s" (claudemacs-ai-get-cli-path))

;; Test 2: Check claudemacs-ai-setup-claude-environment
(message "\nTest 2: Global Environment Setup")
(claudemacs-ai-setup-claude-environment)
(message "  PATH (via getenv): %s" (getenv "PATH"))
(message "  CLAUDEMACS_SOCKET (via getenv): %s" (getenv "CLAUDEMACS_SOCKET"))

;; Test 3: Check process-environment construction
(message "\nTest 3: Process Environment Construction")
(let* ((cli-dir (file-name-directory (claudemacs-ai-get-cli-path)))
       (claudemacs-socket (when (and (boundp 'server-socket-dir)
                                      server-socket-dir
                                      (server-running-p))
                            (expand-file-name "server" server-socket-dir)))
       (process-environment
        (append (list (format "PATH=%s:%s" cli-dir (getenv "PATH"))
                      "TERM=xterm-256color")
                (when claudemacs-socket
                  (list (format "CLAUDEMACS_SOCKET=%s" claudemacs-socket)))
                process-environment)))
  (message "  cli-dir: %s" cli-dir)
  (message "  claudemacs-socket: %s" claudemacs-socket)
  (message "  PATH in process-environment: %s"
           (car (seq-filter (lambda (s) (string-prefix-p "PATH=" s))
                           process-environment)))
  (message "  CLAUDEMACS_SOCKET in process-environment: %s"
           (car (seq-filter (lambda (s) (string-prefix-p "CLAUDEMACS_SOCKET=" s))
                           process-environment))))

;; Test 4: Check shell command construction (when claudemacs-use-shell-env is t)
(message "\nTest 4: Shell Command Construction")
(let* ((cli-dir (file-name-directory (claudemacs-ai-get-cli-path)))
       (claudemacs-socket (when (and (boundp 'server-socket-dir)
                                      server-socket-dir
                                      (server-running-p))
                            (expand-file-name "server" server-socket-dir)))
       (env-vars (format "PATH=%s:$PATH%s"
                        cli-dir
                        (if claudemacs-socket
                            (format " CLAUDEMACS_SOCKET=%s" claudemacs-socket)
                          "")))
       (claudemacs-program "claude")
       (switches '("--verbose")))
  (message "  env-vars string: %s" env-vars)
  (message "  Full command would be: %s %s %s"
           env-vars
           claudemacs-program
           (mapconcat 'shell-quote-argument switches " ")))

(message "\nAll tests complete!")
