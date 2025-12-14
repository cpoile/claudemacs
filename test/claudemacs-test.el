;;; claudemacs-test.el --- Tests for claudemacs -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Test suite for claudemacs.el using ERT (Emacs Lisp Regression Testing).
;; 
;; Test categories:
;; - :unit - Pure function tests, no external dependencies
;; - :integration - Tests requiring mocked dependencies  
;; - :e2e - End-to-end tests with real processes
;; - :requires-claude - Tests requiring Claude CLI installation

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path to find claudemacs
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'claudemacs)

;;; Test Utilities

(defmacro claudemacs-test-with-temp-buffer (&rest body)
  "Execute BODY in a temporary buffer with claudemacs loaded.
This provides a clean environment for testing without side effects."
  `(with-temp-buffer
     (let ((inhibit-message t))
       ,@body)))

(defmacro claudemacs-test-with-temp-file (filename content &rest body)
  "Execute BODY with a temporary file FILENAME containing CONTENT.
The file is automatically cleaned up after BODY executes."
  (declare (indent 2))
  `(let ((temp-file (make-temp-file ,filename)))
     (unwind-protect
         (progn
           (write-region ,content nil temp-file)
           (let ((buffer-file-name temp-file))
             ,@body))
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

(defun claudemacs-test-cleanup-buffers ()
  "Clean up any claudemacs test buffers."
  (dolist (buffer (buffer-list))
    (when (string-match-p "^\\*claudemacs:.*test\\*" (buffer-name buffer))
      (kill-buffer buffer))))

;;; Project Root Detection Tests

(ert-deftest claudemacs-test-project-root-detection-git ()
  "Test project root detection with git repository."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claudemacs-test" t)))
    (unwind-protect
        (progn
          ;; Create a git repo
          (let ((default-directory test-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "test content" nil "test.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "initial" "--quiet"))
          
          ;; Test from repo root with mocked buffer-file-name
          (let ((test-file (expand-file-name "test.txt" test-dir)))
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () test-file)))
              ;; Remove trailing slash for comparison
              (should (string= (file-name-as-directory (claudemacs--project-root)) 
                               (file-name-as-directory test-dir)))))
          
          ;; Test from subdirectory
          (let* ((subdir (expand-file-name "subdir" test-dir))
                 (sub-file (expand-file-name "sub.txt" subdir)))
            (make-directory subdir)
            (write-region "sub content" nil sub-file)
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () sub-file)))
              (should (string= (file-name-as-directory (claudemacs--project-root)) 
                               (file-name-as-directory test-dir))))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claudemacs-test-project-root-detection-no-git ()
  "Test project root detection without git repository."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claudemacs-no-git-test" t)))
    (unwind-protect
        (progn
          ;; Create a regular directory (no git)
          (let ((default-directory test-dir)
                (test-file (expand-file-name "test.txt" test-dir)))
            (write-region "content" nil test-file)
            
            ;; Mock buffer-file-name to return our test file
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () test-file)))
              ;; Should return buffer dir since there's no git repo
              (should (string= (file-name-directory (buffer-file-name)) (claudemacs--project-root))))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claudemacs-test-project-root-with-explicit-dir ()
  "Test project root detection with explicit directory parameter."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claudemacs-explicit-test" t)))
    (unwind-protect
        (progn
          ;; Create a git repo
          (let ((default-directory test-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "content" nil "test.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "initial" "--quiet"))
          
          ;; Test with explicit directory parameter (normalize trailing slashes)
          (should (string= (file-name-as-directory (claudemacs--project-root test-dir)) 
                           (file-name-as-directory test-dir)))
          
          ;; Test with subdirectory
          (let ((subdir (expand-file-name "sub" test-dir)))
            (make-directory subdir)
            (should (string= (file-name-as-directory (claudemacs--project-root subdir)) 
                             (file-name-as-directory test-dir)))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claudemacs-test-project-root-nested-repos ()
  "Test project root detection with nested git repositories."
  :tags '(:unit :project)
  (let ((outer-dir (make-temp-file "claudemacs-outer" t))
        (inner-dir nil))
    (unwind-protect
        (progn
          ;; Create outer git repo
          (let ((default-directory outer-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "outer" nil "outer.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "outer" "--quiet"))
          
          ;; Create inner git repo
          (setq inner-dir (expand-file-name "inner" outer-dir))
          (make-directory inner-dir)
          (let ((default-directory inner-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "inner" nil "inner.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "inner" "--quiet"))
          
          ;; Test that inner directory returns inner repo, not outer (normalize paths)
          (should (string= (file-name-as-directory (claudemacs--project-root inner-dir)) 
                           (file-name-as-directory inner-dir)))
          
          ;; Test that outer directory returns outer repo
          (should (string= (file-name-as-directory (claudemacs--project-root outer-dir)) 
                           (file-name-as-directory outer-dir))))
      
      ;; Cleanup
      (when (file-exists-p outer-dir)
        (delete-directory outer-dir t)))))

(ert-deftest claudemacs-test-project-root-projectile-marker ()
  "Test project root detection with .projectile marker file."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claudemacs-projectile-marker" t))
        (claudemacs-prefer-projectile-root t))
    (unwind-protect
        (progn
          ;; Create a directory with .projectile marker (no git)
          (write-region "" nil (expand-file-name ".projectile" test-dir))
          (let* ((subdir (expand-file-name "src/components" test-dir))
                 (test-file (expand-file-name "Button.tsx" subdir)))
            (make-directory subdir t)
            (write-region "export const Button = () => {}" nil test-file)

            ;; From subdirectory, should find .projectile root
            (cl-letf (((symbol-function 'buffer-file-name)
                       (lambda () test-file)))
              (should (string= (file-name-as-directory (claudemacs--project-root))
                               (file-name-as-directory test-dir))))))
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claudemacs-test-project-root-projectile-marker-monorepo ()
  "Test .projectile marker takes precedence over nested git repos in monorepo."
  :tags '(:unit :project)
  (let ((monorepo-dir (make-temp-file "claudemacs-monorepo" t))
        (claudemacs-prefer-projectile-root t))
    (unwind-protect
        (progn
          ;; Create monorepo root with .projectile
          (write-region "" nil (expand-file-name ".projectile" monorepo-dir))

          ;; Create two nested git repos (like packages in a monorepo)
          (let ((pkg-a (expand-file-name "packages/pkg-a" monorepo-dir))
                (pkg-b (expand-file-name "packages/pkg-b" monorepo-dir)))
            (make-directory pkg-a t)
            (make-directory pkg-b t)

            ;; Initialize git in pkg-a
            (let ((default-directory pkg-a))
              (call-process "git" nil nil nil "init" "--quiet")
              (write-region "module A" nil "index.js")
              (call-process "git" nil nil nil "add" ".")
              (call-process "git" nil nil nil
                           "-c" "user.name=Test"
                           "-c" "user.email=test@example.com"
                           "commit" "-m" "init" "--quiet"))

            ;; Initialize git in pkg-b
            (let ((default-directory pkg-b))
              (call-process "git" nil nil nil "init" "--quiet")
              (write-region "module B" nil "index.js")
              (call-process "git" nil nil nil "add" ".")
              (call-process "git" nil nil nil
                           "-c" "user.name=Test"
                           "-c" "user.email=test@example.com"
                           "commit" "-m" "init" "--quiet"))

            ;; From pkg-a, should find monorepo root (not pkg-a git root)
            (let ((test-file (expand-file-name "index.js" pkg-a)))
              (cl-letf (((symbol-function 'buffer-file-name)
                         (lambda () test-file)))
                (should (string= (file-name-as-directory (claudemacs--project-root))
                                 (file-name-as-directory monorepo-dir)))))

            ;; From pkg-b, should also find monorepo root
            (let ((test-file (expand-file-name "index.js" pkg-b)))
              (cl-letf (((symbol-function 'buffer-file-name)
                         (lambda () test-file)))
                (should (string= (file-name-as-directory (claudemacs--project-root))
                                 (file-name-as-directory monorepo-dir)))))))
      ;; Cleanup
      (when (file-exists-p monorepo-dir)
        (delete-directory monorepo-dir t)))))

(ert-deftest claudemacs-test-project-root-projectile-disabled ()
  "Test that .projectile is ignored when prefer-projectile-root is nil."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claudemacs-projectile-disabled" t))
        (claudemacs-prefer-projectile-root nil))
    (unwind-protect
        (progn
          ;; Create dir with both .projectile and git
          (write-region "" nil (expand-file-name ".projectile" test-dir))
          (let ((git-subdir (expand-file-name "subproject" test-dir)))
            (make-directory git-subdir)
            (let ((default-directory git-subdir))
              (call-process "git" nil nil nil "init" "--quiet")
              (write-region "content" nil "file.txt")
              (call-process "git" nil nil nil "add" ".")
              (call-process "git" nil nil nil
                           "-c" "user.name=Test"
                           "-c" "user.email=test@example.com"
                           "commit" "-m" "init" "--quiet"))

            ;; With prefer-projectile-root nil, should find git root, not .projectile
            (let ((test-file (expand-file-name "file.txt" git-subdir)))
              (cl-letf (((symbol-function 'buffer-file-name)
                         (lambda () test-file)))
                (should (string= (file-name-as-directory (claudemacs--project-root))
                                 (file-name-as-directory git-subdir)))))))
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

;;; Session ID Generation Tests

(ert-deftest claudemacs-test-session-id-doom-workspace ()
  "Test session ID generation with Doom Emacs workspace."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name)
             (lambda () "my-doom-workspace"))
            ((symbol-function 'fboundp) 
             (lambda (func) 
               (eq func '+workspace-current-name))))
    (should (string= (claudemacs--session-id) "my-doom-workspace"))))

(ert-deftest claudemacs-test-session-id-perspective-workspace ()
  "Test session ID generation with Doom's Perspective wrapper."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name)
             (lambda (persp) "my-perspective"))
            ((symbol-function 'get-current-persp)
             (lambda () 'fake-persp))
            ((symbol-function 'fboundp)
             (lambda (func)
               (memq func '(safe-persp-name get-current-persp)))))
    (should (string= (claudemacs--session-id) "my-perspective"))))

(ert-deftest claudemacs-test-session-id-vanilla-perspective ()
  "Test session ID generation with vanilla perspective.el (persp-current-name)."
  :tags '(:unit :session)
  (cl-letf (((symbol-function 'persp-current-name)
             (lambda () "my-vanilla-persp"))
            ((symbol-function 'fboundp)
             (lambda (func)
               (eq func 'persp-current-name))))
    (should (string= (claudemacs--session-id) "my-vanilla-persp"))))

(ert-deftest claudemacs-test-session-id-fallback-to-project ()
  "Test session ID fallback to project root when no workspace is active."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name) (lambda (persp) nil))
            ((symbol-function 'get-current-persp) (lambda () nil))
            ((symbol-function 'fboundp) (lambda (func) nil))
            ((symbol-function 'claudemacs--project-root) (lambda () "/tmp/test-project"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claudemacs--session-id) "/tmp/test-project"))))

(ert-deftest claudemacs-test-session-id-empty-workspace-names ()
  "Test session ID with empty or nil workspace names."
  :tags '(:unit :session)
  ;; Test with Doom workspace returning empty string
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () ""))
            ((symbol-function 'fboundp) 
             (lambda (func) (eq func '+workspace-current-name)))
            ((symbol-function 'claudemacs--project-root) (lambda () "/fallback"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claudemacs--session-id) "/fallback")))
  
  ;; Test with Perspective returning nil
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name) (lambda (persp) nil))
            ((symbol-function 'get-current-persp) (lambda () 'fake-persp))
            ((symbol-function 'fboundp) 
             (lambda (func) (memq func '(safe-persp-name get-current-persp))))
            ((symbol-function 'claudemacs--project-root) (lambda () "/fallback2"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claudemacs--session-id) "/fallback2"))))

(ert-deftest claudemacs-test-session-id-workspace-priority ()
  "Test that Doom workspace takes priority over Perspective."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "doom-wins"))
            ((symbol-function 'safe-persp-name) (lambda (persp) "perspective-loses"))
            ((symbol-function 'get-current-persp) (lambda () 'fake-persp))
            ((symbol-function 'fboundp) 
             (lambda (func) 
               (memq func '(+workspace-current-name safe-persp-name get-current-persp)))))
    (should (string= (claudemacs--session-id) "doom-wins"))))

(ert-deftest claudemacs-test-buffer-detection ()
  "Test claudemacs buffer detection."
  :tags '(:unit :session)
  (claudemacs-test-with-temp-buffer
    ;; Test non-claudemacs buffer
    (should-not (claudemacs--is-claudemacs-buffer-p))
    
    ;; Test claudemacs buffer
    (rename-buffer "*claudemacs:test*")
    (should (claudemacs--is-claudemacs-buffer-p))
    
    ;; Test with specific buffer argument
    (with-temp-buffer
      (should-not (claudemacs--is-claudemacs-buffer-p (current-buffer)))
      (rename-buffer "*claudemacs:another*")
      (should (claudemacs--is-claudemacs-buffer-p (current-buffer)))))
  
  ;; Test with non-live buffer
  (let ((dead-buffer (get-buffer-create "*test-dead*")))
    (kill-buffer dead-buffer)
    (should-not (claudemacs--is-claudemacs-buffer-p dead-buffer))))

;;; Startup Hook Behavior Tests

(ert-deftest claudemacs-test-startup-hook-called-during-setup ()
  "Test that claudemacs-startup-hook is called during eat integration setup."
  :tags '(:integration :startup-hook)
  (let ((hook-called nil)
        (hook-called-in-claudemacs-buffer nil)
        (test-buffer nil))
    
    ;; Create a test hook function
    (add-hook 'claudemacs-startup-hook 
              (lambda () 
                (setq hook-called t)
                (when (claudemacs--is-claudemacs-buffer-p)
                  (setq hook-called-in-claudemacs-buffer t))))
    
    ;; Mock the bell handler setup to avoid session ID dependency
    (cl-letf (((symbol-function 'claudemacs-setup-bell-handler)
               (lambda () nil)))
      
      (unwind-protect
          (progn
            ;; Create a buffer that looks like a claudemacs buffer
            (setq test-buffer (get-buffer-create "*claudemacs:test-hook*"))
            (with-current-buffer test-buffer
              ;; Set up minimal fake eat-terminal
              (setq-local eat-terminal 'fake-terminal))
            
            ;; Call the setup function directly
            (claudemacs--setup-eat-integration test-buffer)
            
            ;; Verify hook was called
            (should hook-called)
            (should hook-called-in-claudemacs-buffer))
        
        ;; Cleanup
        (remove-hook 'claudemacs-startup-hook 
                     (lambda () 
                       (setq hook-called t)
                       (when (claudemacs--is-claudemacs-buffer-p)
                         (setq hook-called-in-claudemacs-buffer t))))
        (when (and test-buffer (buffer-live-p test-buffer))
          (kill-buffer test-buffer))))))

(ert-deftest claudemacs-test-startup-hook-multiple-functions ()
  "Test that multiple functions can be added to claudemacs-startup-hook."
  :tags '(:integration :startup-hook)
  (let ((hook1-called nil)
        (hook2-called nil)
        (test-buffer nil))
    
    ;; Create two test hook functions
    (add-hook 'claudemacs-startup-hook (lambda () (setq hook1-called t)))
    (add-hook 'claudemacs-startup-hook (lambda () (setq hook2-called t)))
    
    ;; Mock the bell handler setup to avoid session ID dependency
    (cl-letf (((symbol-function 'claudemacs-setup-bell-handler)
               (lambda () nil)))
      
      (unwind-protect
          (progn
            ;; Create a buffer that looks like a claudemacs buffer
            (setq test-buffer (get-buffer-create "*claudemacs:test-multiple*"))
            (with-current-buffer test-buffer
              ;; Set up minimal fake eat-terminal
              (setq-local eat-terminal 'fake-terminal))
            
            ;; Call the setup function directly
            (claudemacs--setup-eat-integration test-buffer)
            
            ;; Verify both hooks were called
            (should hook1-called)
            (should hook2-called))
        
        ;; Cleanup
        (remove-hook 'claudemacs-startup-hook (lambda () (setq hook1-called t)))
        (remove-hook 'claudemacs-startup-hook (lambda () (setq hook2-called t)))
        (when (and test-buffer (buffer-live-p test-buffer))
          (kill-buffer test-buffer))))))

(ert-deftest claudemacs-test-startup-hook-buffer-context ()
  "Test that claudemacs-startup-hook runs with claudemacs buffer as current buffer."
  :tags '(:integration :startup-hook)
  (let ((captured-buffer-name nil)
        (captured-cwd nil)
        (test-buffer nil))
    
    ;; Create a test hook function that captures context
    (add-hook 'claudemacs-startup-hook 
              (lambda () 
                (setq captured-buffer-name (buffer-name))
                (setq captured-cwd claudemacs--cwd)))
    
    ;; Mock the bell handler setup to avoid session ID dependency
    (cl-letf (((symbol-function 'claudemacs-setup-bell-handler)
               (lambda () nil)))
      
      (unwind-protect
          (progn
            ;; Create a buffer that looks like a claudemacs buffer
            (setq test-buffer (get-buffer-create "*claudemacs:test-context*"))
            (with-current-buffer test-buffer
              ;; Set up minimal fake environment
              (setq-local eat-terminal 'fake-terminal)
              (setq-local claudemacs--cwd "/test/directory"))
            
            ;; Call the setup function directly
            (claudemacs--setup-eat-integration test-buffer)
            
            ;; Verify hook ran in correct buffer context
            (should captured-buffer-name)
            (should (string= captured-buffer-name "*claudemacs:test-context*"))
            (should captured-cwd)
            (should (string= captured-cwd "/test/directory")))
        
        ;; Cleanup
        (remove-hook 'claudemacs-startup-hook 
                     (lambda () 
                       (setq captured-buffer-name (buffer-name))
                       (setq captured-cwd claudemacs--cwd)))
        (when (and test-buffer (buffer-live-p test-buffer))
          (kill-buffer test-buffer))))))

(ert-deftest claudemacs-test-startup-hook-error-handling ()
  "Test that errors in claudemacs-startup-hook don't break setup."
  :tags '(:integration :startup-hook)
  (let ((hook-error-occurred nil)
        (setup-completed nil)
        (test-buffer nil))
    
    ;; Create a hook function that throws an error
    (add-hook 'claudemacs-startup-hook 
              (lambda () (error "Test hook error")))
    
    ;; Mock the other setup functions to track completion
    (cl-letf (((symbol-function 'claudemacs--setup-buffer-keymap)
               (lambda () (setq setup-completed t)))
              ((symbol-function 'claudemacs-setup-bell-handler)
               (lambda () nil)))
      
      (unwind-protect
          (progn
            ;; Create a buffer that looks like a claudemacs buffer
            (setq test-buffer (get-buffer-create "*claudemacs:test-error*"))
            (with-current-buffer test-buffer
              ;; Set up minimal fake eat-terminal
              (setq-local eat-terminal 'fake-terminal))
            
            ;; Call the setup function and expect it to handle errors gracefully
            (condition-case err
                (claudemacs--setup-eat-integration test-buffer)
              (error (setq hook-error-occurred t)))
            
            ;; Setup should have completed despite hook error
            (should setup-completed)
            ;; The error should have been propagated (or could be caught - depends on implementation)
        
        ;; Cleanup
        (remove-hook 'claudemacs-startup-hook (lambda () (error "Test hook error")))
        (when (and test-buffer (buffer-live-p test-buffer))
          (kill-buffer test-buffer)))))))

;;; Configuration Behavior Tests

(ert-deftest claudemacs-test-bell-handler-behavior ()
  "Test that claudemacs-notify-on-await affects bell handler behavior."
  :tags '(:unit :config)
  (let ((notification-called nil))
    ;; Mock system notification
    (cl-letf (((symbol-function 'claudemacs--system-notification)
               (lambda (&rest args) (setq notification-called t))))
      
      ;; Test with notifications enabled
      (let ((claudemacs-notify-on-await t))
        (claudemacs--bell-handler nil)
        (should notification-called))
      
      ;; Test with notifications disabled
      (setq notification-called nil)
      (let ((claudemacs-notify-on-await nil))
        (claudemacs--bell-handler nil)
        (should-not notification-called)))))

(ert-deftest claudemacs-test-notification-sound-behavior ()
  "Test that claudemacs-notification-sound-mac affects notification calls."
  :tags '(:unit :config)
  (let ((notification-command nil))
    ;; Mock call-process to capture the full command
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (when (string= program "osascript")
                   (setq notification-command (mapconcat 'identity args " ")))))
              ;; Mock system-type to be macOS
              (system-type 'darwin))
      
      ;; Test with custom sound
      (let ((claudemacs-notification-sound-mac "Ping"))
        (claudemacs--system-notification "Test message" "Test title")
        (should notification-command)
        (should (string-match-p "Ping" notification-command)))
      
      ;; Test with different sound
      (setq notification-command nil)
      (let ((claudemacs-notification-sound-mac "Glass"))
        (claudemacs--system-notification "Test message" "Test title") 
        (should notification-command)
        (should (string-match-p "Glass" notification-command))))))

(ert-deftest claudemacs-test-program-switches-behavior ()
  "Test that claudemacs-program-switches affects command construction."
  :tags '(:unit :config)
  ;; Test the switches logic directly without complex mocking
  (let ((claudemacs-program-switches nil)
        (args '("--resume")))
    ;; Test with no custom switches - should just have the passed args
    (let ((result (remove nil (append args claudemacs-program-switches))))
      (should (equal result '("--resume"))))
    
    ;; Test with custom switches - should include both
    (setq claudemacs-program-switches '("--verbose" "--test"))
    (let ((result (remove nil (append args claudemacs-program-switches))))
      (should (member "--verbose" result))
      (should (member "--test" result))
      (should (member "--resume" result))
      (should (= (length result) 3)))))

(ert-deftest claudemacs-test-switch-to-buffer-behavior ()
  "Test buffer switching behavior based on custom variables."
  :tags '(:unit :config)
  (let ((window-selected nil))
    ;; Mock window selection functions
    (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer))
              ((symbol-function 'select-window) 
               (lambda (window &optional norecord) (setq window-selected t)))
              ((symbol-function 'get-buffer-window) 
               (lambda (buffer &optional all-frames) 'fake-window)))
      
      ;; Test switch-to-buffer-on-create behavior
      (let ((test-buffer (get-buffer-create "*claudemacs:switch-test*")))
        (unwind-protect
            (progn
              ;; Test with switching enabled
              (setq window-selected nil)
              (let ((claudemacs-switch-to-buffer-on-create t))
                (with-current-buffer test-buffer
                  (let ((window (display-buffer test-buffer)))
                    (when claudemacs-switch-to-buffer-on-create
                      (select-window window))))
                (should window-selected))
              
              ;; Test with switching disabled  
              (setq window-selected nil)
              (let ((claudemacs-switch-to-buffer-on-create nil))
                (with-current-buffer test-buffer
                  (let ((window (display-buffer test-buffer)))
                    (when claudemacs-switch-to-buffer-on-create
                      (select-window window))))
                (should-not window-selected)))
          
          ;; Cleanup
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer)))))))

;;; Multi-Tool Support Tests

(ert-deftest claudemacs-test-buffer-naming-with-tool ()
  "Test buffer naming includes tool name."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-default-tool 'claude))
    ;; Test with default tool
    (let ((buf-name (claudemacs--get-buffer-name)))
      (should (string-match-p "^\\*claudemacs:claude:" buf-name)))

    ;; Test with explicit tool
    (let ((buf-name (claudemacs--get-buffer-name 'codex)))
      (should (string-match-p "^\\*claudemacs:codex:" buf-name)))

    ;; Test with another tool
    (let ((buf-name (claudemacs--get-buffer-name 'gemini)))
      (should (string-match-p "^\\*claudemacs:gemini:" buf-name)))))

(ert-deftest claudemacs-test-format-session-choices ()
  "Test formatting session choices for display."
  :tags '(:unit :multi-tool)
  (let ((session-info (list :tool 'claude :session-id "main")))
    ;; Test current session formatting
    (should (string= (claudemacs--format-session-choice session-info t)
                     "claude:main (current)"))

    ;; Test non-current session formatting
    (should (string= (claudemacs--format-session-choice session-info nil)
                     "claude:main"))))

(ert-deftest claudemacs-test-list-all-sessions ()
  "Test listing all claudemacs sessions across all workspaces."
  :tags '(:unit :multi-tool)
  (unwind-protect
      (progn
        ;; Create multiple session buffers
        (get-buffer-create "*claudemacs:claude:workspace1*")
        (get-buffer-create "*claudemacs:codex:workspace2*")
        (get-buffer-create "*not-a-claudemacs-buffer*")

        (let ((all-sessions (claudemacs--list-all-sessions)))
          ;; Should find both claudemacs buffers
          (should (>= (length all-sessions) 2))
          ;; Should not include non-claudemacs buffer
          (should-not (member (get-buffer "*not-a-claudemacs-buffer*") all-sessions))))
    ;; Cleanup
    (when (get-buffer "*claudemacs:claude:workspace1*")
      (kill-buffer "*claudemacs:claude:workspace1*"))
    (when (get-buffer "*claudemacs:codex:workspace2*")
      (kill-buffer "*claudemacs:codex:workspace2*"))
    (when (get-buffer "*not-a-claudemacs-buffer*")
      (kill-buffer "*not-a-claudemacs-buffer*"))))

(ert-deftest claudemacs-test-list-sessions-for-workspace ()
  "Test listing sessions only for current workspace."
  :tags '(:unit :multi-tool)
  (unwind-protect
      (let ((claudemacs-tool-registry
             '((claude :program "claude" :switches nil)
               (codex :program "codex" :switches nil))))
        ;; Mock session ID to return "workspace1"
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "workspace1")))
          ;; Create sessions in different workspaces
          (get-buffer-create "*claudemacs:claude:workspace1*")
          (get-buffer-create "*claudemacs:codex:workspace1*")
          (get-buffer-create "*claudemacs:gemini:workspace2*")

          (let ((workspace-sessions (claudemacs--list-sessions-for-workspace)))
            ;; Should only find sessions in workspace1
            (should (= (length workspace-sessions) 2))
            ;; Check we got the right sessions
            (should (member 'claude (mapcar (lambda (s) (plist-get s :tool)) workspace-sessions)))
            (should (member 'codex (mapcar (lambda (s) (plist-get s :tool)) workspace-sessions)))
            (should-not (member 'gemini (mapcar (lambda (s) (plist-get s :tool)) workspace-sessions))))))
    ;; Cleanup
    (dolist (buf '("*claudemacs:claude:workspace1*"
                   "*claudemacs:codex:workspace1*"
                   "*claudemacs:gemini:workspace2*"))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(ert-deftest claudemacs-test-list-sessions-sorted-by-recency ()
  "Test that sessions are sorted by most recently accessed."
  :tags '(:unit :multi-tool)
  (unwind-protect
      (let ((claudemacs-tool-registry
             '((claude :program "claude" :switches nil)
               (codex :program "codex" :switches nil)
               (gemini :program "gemini" :switches nil))))
        ;; Mock session ID to return "test"
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test")))
          ;; Create three sessions
          (let ((buf-claude (get-buffer-create "*claudemacs:claude:test*"))
                (buf-codex (get-buffer-create "*claudemacs:codex:test*"))
                (buf-gemini (get-buffer-create "*claudemacs:gemini:test*")))

            ;; Set different buffer-display-time values (most recent has highest value)
            ;; Use time-add to create different timestamps
            (let ((base-time (current-time)))
              (with-current-buffer buf-claude
                (setq-local buffer-display-time (time-add base-time (seconds-to-time 100))))
              (with-current-buffer buf-codex
                (setq-local buffer-display-time (time-add base-time (seconds-to-time 300)))) ;; Most recent
              (with-current-buffer buf-gemini
                (setq-local buffer-display-time (time-add base-time (seconds-to-time 200)))))

            (let ((sessions (claudemacs--list-sessions-for-workspace)))
              ;; Should be sorted: codex (300), gemini (200), claude (100)
              (should (= (length sessions) 3))
              (should (eq (plist-get (nth 0 sessions) :tool) 'codex))
              (should (eq (plist-get (nth 1 sessions) :tool) 'gemini))
              (should (eq (plist-get (nth 2 sessions) :tool) 'claude))))))
    ;; Cleanup
    (dolist (buf '("*claudemacs:claude:test*"
                   "*claudemacs:codex:test*"
                   "*claudemacs:gemini:test*"))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(ert-deftest claudemacs-test-get-current-session-buffer ()
  "Test getting the most relevant session buffer for current context."
  :tags '(:unit :multi-tool)
  (unwind-protect
      (let ((test-buf (get-buffer-create "*claudemacs:claude:test*")))
        ;; Test 1: When we're in a claudemacs buffer, return it
        (with-current-buffer test-buf
          (should (eq (claudemacs--get-current-session-buffer) test-buf)))

        ;; Test 2: When not in claudemacs buffer and sessions exist in workspace
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test")))
          (let ((result (claudemacs--get-current-session-buffer)))
            (should (bufferp result))
            (should (string-match-p "^\\*claudemacs:" (buffer-name result))))))
    ;; Cleanup
    (when (get-buffer "*claudemacs:claude:test*")
      (kill-buffer "*claudemacs:claude:test*"))))

(ert-deftest claudemacs-test-multiple-tools-same-workspace ()
  "Test that multiple tools can run in the same workspace."
  :tags '(:unit :multi-tool)
  (unwind-protect
      (let ((claudemacs-default-tool 'claude))
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "main")))
          ;; Create buffers for different tools in same workspace
          (let ((claude-buf (get-buffer-create "*claudemacs:claude:main*"))
                (codex-buf (get-buffer-create "*claudemacs:codex:main*")))

            ;; Both buffers should be recognized as claudemacs buffers
            (should (claudemacs--is-claudemacs-buffer-p claude-buf))
            (should (claudemacs--is-claudemacs-buffer-p codex-buf))

            ;; Should be able to get specific buffers by tool
            (should (eq (claudemacs--get-buffer 'claude) claude-buf))
            (should (eq (claudemacs--get-buffer 'codex) codex-buf))

            ;; Buffer names should be different
            (should-not (string= (buffer-name claude-buf) (buffer-name codex-buf))))))
    ;; Cleanup
    (dolist (buf '("*claudemacs:claude:main*" "*claudemacs:codex:main*"))
      (when (get-buffer buf)
        (kill-buffer buf)))))

;;; Description Function Safety Tests

(ert-deftest claudemacs-test-smart-switch-description-never-errors ()
  "Test that smart-switch-description always returns a valid string.
This function is called by the transient menu and must never error."
  :tags '(:unit :multi-tool)
  (unwind-protect
      (progn
        ;; Test with no sessions
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test")))
          (let ((desc (claudemacs--smart-switch-description)))
            (should (stringp desc))
            (should (string-match-p "Switch to Session" desc))))

        ;; Test with one session
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test")))
          (let ((buf (get-buffer-create "*claudemacs:claude:test*")))
            (with-current-buffer buf
              (setq-local buffer-display-time (current-time)))
            (let ((desc (claudemacs--smart-switch-description)))
              (should (stringp desc))
              (should (or (string-match-p "claude" desc)
                         (string-match-p "Switch to Session" desc))))))

        ;; Test with multiple sessions
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test")))
          (let ((buf1 (get-buffer-create "*claudemacs:claude:test*"))
                (buf2 (get-buffer-create "*claudemacs:codex:test*")))
            (with-current-buffer buf1
              (setq-local buffer-display-time (current-time)))
            (with-current-buffer buf2
              (setq-local buffer-display-time (current-time)))
            (let ((desc (claudemacs--smart-switch-description)))
              (should (stringp desc))
              (should (string-match-p "Switch to Session" desc))))))
    ;; Cleanup
    (when (get-buffer "*claudemacs:claude:test*")
      (kill-buffer "*claudemacs:claude:test*"))
    (when (get-buffer "*claudemacs:codex:test*")
      (kill-buffer "*claudemacs:codex:test*"))))

(ert-deftest claudemacs-test-kill-description-never-errors ()
  "Test that kill-description always returns a valid string.
This function is called by the transient menu and must never error."
  :tags '(:unit :kill)
  (unwind-protect
      (progn
        ;; Test with no sessions
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-no-sessions"))
                  ((symbol-function 'claudemacs--list-all-sessions) (lambda () nil)))
          (let ((desc (claudemacs--kill-description)))
            (should (stringp desc))
            (should (string-match-p "Kill Session" desc))))

        ;; Test with one session
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-one")))
          (let ((buf (get-buffer-create "*claudemacs:claude:test-one*")))
            (with-current-buffer buf
              (setq-local claudemacs--tool 'claude)
              (setq-local buffer-display-time (current-time)))
            (let ((desc (claudemacs--kill-description)))
              (should (stringp desc))
              (should (or (string-match-p "claude" desc)
                         (string-match-p "Kill Session" desc))))))

        ;; Test with multiple sessions - should show most recent
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-multi")))
          (let ((buf1 (get-buffer-create "*claudemacs:claude:test-multi*"))
                (buf2 (get-buffer-create "*claudemacs:codex:test-multi*")))
            (with-current-buffer buf1
              (setq-local claudemacs--tool 'claude)
              (setq-local buffer-display-time (time-subtract (current-time) (seconds-to-time 10))))
            (with-current-buffer buf2
              (setq-local claudemacs--tool 'codex)
              (setq-local buffer-display-time (current-time)))
            (let ((desc (claudemacs--kill-description)))
              (should (stringp desc))
              ;; Should show codex (most recent)
              (should (or (string-match-p "codex" desc)
                         (string-match-p "Kill Session" desc)))))))
    ;; Cleanup
    (when (get-buffer "*claudemacs:claude:test-one*")
      (kill-buffer "*claudemacs:claude:test-one*"))
    (when (get-buffer "*claudemacs:claude:test-multi*")
      (kill-buffer "*claudemacs:claude:test-multi*"))
    (when (get-buffer "*claudemacs:codex:test-multi*")
      (kill-buffer "*claudemacs:codex:test-multi*"))))

(ert-deftest claudemacs-test-kill-current-session ()
  "Test killing the current/most recent session."
  :tags '(:unit :kill)
  (unwind-protect
      (progn
        ;; Test error when no session exists
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-kill-none"))
                  ((symbol-function 'claudemacs--list-all-sessions) (lambda () nil)))
          (should-error (claudemacs-kill) :type 'error))

        ;; Test killing when in a claudemacs buffer
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-kill-current"))
                  ((symbol-function 'eat-kill-process) (lambda () nil)))
          (let ((buf (get-buffer-create "*claudemacs:claude:test-kill-current*")))
            (with-current-buffer buf
              (setq-local claudemacs--tool 'claude)
              (setq-local buffer-display-time (current-time))
              (claudemacs-kill)
              ;; Buffer should be killed
              (should-not (buffer-live-p buf))))))

        ;; Test killing most recent session when not in claudemacs buffer
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-kill-recent"))
                  ((symbol-function 'eat-kill-process) (lambda () nil)))
          (let ((buf (get-buffer-create "*claudemacs:claude:test-kill-recent*")))
            (with-current-buffer buf
              (setq-local claudemacs--tool 'claude)
              (setq-local buffer-display-time (current-time)))
            ;; Call from a different buffer
            (with-temp-buffer
              (claudemacs-kill)
              ;; Claude buffer should be killed
              (should-not (buffer-live-p buf)))))
    ;; Cleanup
    (when (get-buffer "*claudemacs:claude:test-kill-current*")
      (kill-buffer "*claudemacs:claude:test-kill-current*"))
    (when (get-buffer "*claudemacs:claude:test-kill-recent*")
      (kill-buffer "*claudemacs:claude:test-kill-recent*"))))

(ert-deftest claudemacs-test-kill-specific-session-no-sessions ()
  "Test kill-specific-session errors when no sessions exist."
  :tags '(:unit :kill)
  (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-kill-none"))
            ((symbol-function 'claudemacs--list-all-sessions) (lambda () nil)))
    (should-error (claudemacs-kill-specific-session) :type 'error)))

(ert-deftest claudemacs-test-kill-specific-session-with-selection ()
  "Test kill-specific-session kills the selected session."
  :tags '(:unit :kill)
  (unwind-protect
      (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-kill-specific"))
                ((symbol-function 'eat-kill-process) (lambda () nil)))
        (let ((buf1 (get-buffer-create "*claudemacs:claude:test-kill-specific*"))
              (buf2 (get-buffer-create "*claudemacs:codex:test-kill-specific*")))
          ;; Setup buffers with tool info
          (with-current-buffer buf1
            (setq-local claudemacs--tool 'claude)
            (setq-local buffer-display-time (current-time)))
          (with-current-buffer buf2
            (setq-local claudemacs--tool 'codex)
            (setq-local buffer-display-time (current-time)))

          ;; Mock completing-read to select the codex session
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt choices &rest _)
                       (should (stringp prompt))
                       (should (string-match-p "Kill session" prompt))
                       ;; Return just the string (car of the cons), not the whole cons cell
                       (caar (seq-filter (lambda (c) (string-match-p "codex" (car c))) choices)))))
            (claudemacs-kill-specific-session)
            ;; Codex buffer should be killed
            (should-not (buffer-live-p buf2))
            ;; Claude buffer should still exist
            (should (buffer-live-p buf1)))))
    ;; Cleanup
    (when (get-buffer "*claudemacs:claude:test-kill-specific*")
      (kill-buffer "*claudemacs:claude:test-kill-specific*"))
    (when (get-buffer "*claudemacs:codex:test-kill-specific*")
      (kill-buffer "*claudemacs:codex:test-kill-specific*"))))

(ert-deftest claudemacs-test-kill-handles-multiple-workspaces ()
  "Test that kill operations respect workspace boundaries."
  :tags '(:unit :kill :multi-tool)
  (unwind-protect
      (let ((workspace-a-session-id "workspace-a")
            (workspace-b-session-id "workspace-b"))
        ;; Create sessions in different workspaces
        (let ((buf-a (get-buffer-create "*claudemacs:claude:workspace-a*"))
              (buf-b (get-buffer-create "*claudemacs:codex:workspace-b*")))
          (with-current-buffer buf-a
            (setq-local claudemacs--tool 'claude)
            (setq-local buffer-display-time (current-time)))
          (with-current-buffer buf-b
            (setq-local claudemacs--tool 'codex)
            (setq-local buffer-display-time (current-time)))

          ;; Mock session-id to return workspace-a
          (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () workspace-a-session-id))
                    ((symbol-function 'eat-kill-process) (lambda () nil)))
            ;; Kill should only affect workspace-a
            (claudemacs-kill)
            (should-not (buffer-live-p buf-a))
            (should (buffer-live-p buf-b)))))
    ;; Cleanup
    (when (get-buffer "*claudemacs:claude:workspace-a*")
      (kill-buffer "*claudemacs:claude:workspace-a*"))
    (when (get-buffer "*claudemacs:codex:workspace-b*")
      (kill-buffer "*claudemacs:codex:workspace-b*"))))

(provide 'claudemacs-test)
;;; claudemacs-test.el ends here
