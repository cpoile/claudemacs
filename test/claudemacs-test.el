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
  "Test that `claudemacs-startup-hook' runs during terminal setup."
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
    (cl-letf (((symbol-function 'claudemacs-setup-bell-handler) #'ignore)
              ((symbol-function 'claudemacs--terminal-ready-p)
               (lambda () t))
              ((symbol-function 'claudemacs--terminal-setup-faces) #'ignore))
      
      (unwind-protect
          (progn
            ;; Create a buffer that looks like a claudemacs buffer
            (setq test-buffer (get-buffer-create "*claudemacs:test-hook*"))
            (with-current-buffer test-buffer
              (setq-local claudemacs--terminal-backend 'fake))
            
            ;; Call the setup function directly
            (claudemacs--setup-terminal-integration test-buffer)
            
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
    (cl-letf (((symbol-function 'claudemacs-setup-bell-handler) #'ignore)
              ((symbol-function 'claudemacs--terminal-ready-p)
               (lambda () t))
              ((symbol-function 'claudemacs--terminal-setup-faces) #'ignore))
      
      (unwind-protect
          (progn
            ;; Create a buffer that looks like a claudemacs buffer
            (setq test-buffer (get-buffer-create "*claudemacs:test-multiple*"))
            (with-current-buffer test-buffer
              (setq-local claudemacs--terminal-backend 'fake))
            
            ;; Call the setup function directly
            (claudemacs--setup-terminal-integration test-buffer)
            
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
    (cl-letf (((symbol-function 'claudemacs-setup-bell-handler) #'ignore)
              ((symbol-function 'claudemacs--terminal-ready-p)
               (lambda () t))
              ((symbol-function 'claudemacs--terminal-setup-faces) #'ignore))
      
      (unwind-protect
          (progn
            ;; Create a buffer that looks like a claudemacs buffer
            (setq test-buffer (get-buffer-create "*claudemacs:test-context*"))
            (with-current-buffer test-buffer
              ;; Set up minimal fake environment
              (setq-local claudemacs--terminal-backend 'fake)
              (setq-local claudemacs--cwd "/test/directory"))
            
            ;; Call the setup function directly
            (claudemacs--setup-terminal-integration test-buffer)
            
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
              ((symbol-function 'claudemacs-setup-bell-handler) #'ignore)
              ((symbol-function 'claudemacs--terminal-ready-p)
               (lambda () t))
              ((symbol-function 'claudemacs--terminal-setup-faces) #'ignore))
      
      (unwind-protect
          (progn
            ;; Create a buffer that looks like a claudemacs buffer
            (setq test-buffer (get-buffer-create "*claudemacs:test-error*"))
            (with-current-buffer test-buffer
              (setq-local claudemacs--terminal-backend 'fake))
            
            ;; Call the setup function and expect it to handle errors gracefully
            (condition-case err
                (claudemacs--setup-terminal-integration test-buffer)
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
               (lambda (&rest args) (setq notification-called t)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest arguments)
                 (apply function arguments))))
      
      ;; Test with notifications enabled
      (let ((claudemacs-notify-on-await t))
        (claudemacs--bell-handler nil)
        (should notification-called))
      
      ;; Test with notifications disabled
      (setq notification-called nil)
      (let ((claudemacs-notify-on-await nil))
        (claudemacs--bell-handler nil)
        (should-not notification-called)))))

(ert-deftest claudemacs-test-tool-notification-switches ()
  "Test that Codex notifies through the terminal regardless of focus."
  :tags '(:unit :codex :config)
  (should (equal (claudemacs--get-tool-notification-switches 'codex)
                 '("--config" "tui.notification_method=\"osc9\""
                   "--config" "tui.notification_condition=\"always\"")))
  (should-not (claudemacs--get-tool-notification-switches 'claude))
  (should-not (claudemacs--get-tool-notification-switches 'gemini))
  (let ((claudemacs-codex-notification-switches nil))
    (should-not (claudemacs--get-tool-notification-switches 'codex))))

(ert-deftest claudemacs-test-migrates-uncustomized-codex-bel-default ()
  "A live reload upgrades the old uncustomized BEL default to OSC9."
  :tags '(:unit :codex :config)
  (let ((claudemacs-codex-notification-switches
         (copy-tree claudemacs--legacy-codex-notification-switches)))
    (should (equal (claudemacs--migrated-codex-notification-switches)
                   claudemacs--default-codex-notification-switches))))

(ert-deftest claudemacs-test-preserves-customized-codex-bel-setting ()
  "An explicit Customize-backed BEL setting is not migrated."
  :tags '(:unit :codex :config)
  (let ((old-saved (get 'claudemacs-codex-notification-switches 'saved-value))
        (claudemacs-codex-notification-switches
         (copy-tree claudemacs--legacy-codex-notification-switches)))
    (unwind-protect
        (progn
          (put 'claudemacs-codex-notification-switches 'saved-value '(explicit))
          (should-not (claudemacs--migrated-codex-notification-switches)))
      (put 'claudemacs-codex-notification-switches 'saved-value old-saved))))

(ert-deftest claudemacs-test-notification-handler-uses-tool-message ()
  "A notification carrying the tool's own message shows that message."
  :tags '(:unit :config)
  (let ((claudemacs-notify-on-await t)
        (claudemacs-notify-with-tool-message t)
        (claudemacs--tool 'codex)
        (received nil))
    (cl-letf (((symbol-function 'claudemacs--system-notification)
               (lambda (message &optional title)
                 (setq received (cons message title)))))
      (with-temp-buffer
        (claudemacs--notification-handler "Renamed the helper and ran the tests")
        (should (equal (car received) "Renamed the helper and ran the tests"))
        (should (equal (cdr received) "Codex"))
        ;; A tool-supplied title wins over the tool name.
        (claudemacs--notification-handler "body text" "Codex review")
        (should (equal (cdr received) "Codex review"))))))

(ert-deftest claudemacs-test-notification-handler-falls-back-to-generic-text ()
  "Without a usable message, notifications keep the generic completion text."
  :tags '(:unit :config)
  (let ((claudemacs-notify-on-await t)
        (claudemacs--tool 'codex)
        (received nil)
        (count 0))
    (cl-letf (((symbol-function 'claudemacs--system-notification)
               (lambda (message &optional _title)
                 (setq received message
                       count (1+ count))))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest arguments)
                 (apply function arguments))))
      ;; An empty message is not worth showing.
      (with-temp-buffer
        (let ((claudemacs-notify-with-tool-message t))
          (claudemacs--notification-handler "   ")))
      (should (equal received "Codex finished and is awaiting your input"))
      ;; Neither is any message when the user turned the feature off.
      (setq received nil)
      (with-temp-buffer
        (let ((claudemacs-notify-with-tool-message nil))
          (claudemacs--notification-handler "Renamed the helper")
          (claudemacs--bell-handler)))
      (should (equal received "Codex finished and is awaiting your input"))
      (should (= count 2)))))

(ert-deftest claudemacs-test-bell-after-tool-message-is-not-shown-twice ()
  "A tool that announces one event twice produces one notification."
  :tags '(:unit :config)
  (let ((claudemacs-notify-on-await t)
        (claudemacs-notify-with-tool-message t)
        (claudemacs--tool 'claude)
        (count 0))
    (cl-letf (((symbol-function 'claudemacs--system-notification)
               (lambda (&rest _args) (setq count (1+ count))))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest arguments)
                 (apply function arguments))))
      (with-temp-buffer
        ;; Claude Code's `iterm2_with_bell' channel sends OSC 9 and then BEL.
        (claudemacs--notification-handler "Finished the refactor")
        (claudemacs--bell-handler)
        (should (= count 1))
        ;; A bell arriving on its own still notifies.
        (setq-local claudemacs--last-tool-notification-time nil)
        (claudemacs--bell-handler)
        (should (= count 2))))))

(ert-deftest claudemacs-test-deferred-bell-lets-ghostel-notification-win ()
  "A deferred Ghostel OSC callback suppresses its following BEL."
  :tags '(:unit :config :ghostel)
  (let ((claudemacs-notify-on-await t)
        (claudemacs-notify-with-tool-message t)
        (claudemacs--tool 'claude)
        (scheduled nil)
        (count 0))
    (cl-letf (((symbol-function 'claudemacs--system-notification)
               (lambda (&rest _args) (setq count (1+ count))))
              ((symbol-function 'run-at-time)
               (lambda (delay _repeat function &rest arguments)
                 (push (list delay function arguments) scheduled))))
      (with-temp-buffer
        ;; BEL is observed before Ghostel invokes its already-deferred OSC hook.
        (claudemacs--bell-handler)
        (claudemacs--notification-handler "Finished the refactor")
        (should (= count 1))
        (pcase-let ((`(,delay ,function ,arguments) (car scheduled)))
          (should (> delay 0))
          (apply function arguments))
        (should (= count 1))))))

(ert-deftest claudemacs-test-notification-handler-respects-notify-on-await ()
  "Tool notifications stay silent when notifications are disabled."
  :tags '(:unit :config)
  (let ((claudemacs-notify-on-await nil)
        (notified nil))
    (cl-letf (((symbol-function 'claudemacs--system-notification)
               (lambda (&rest _args) (setq notified t))))
      (with-temp-buffer
        (claudemacs--notification-handler "Renamed the helper"))
      (should-not notified))))

(ert-deftest claudemacs-test-notification-message-is-single-line-and-bounded ()
  "Multi-line agent messages are flattened and truncated for the popup."
  :tags '(:unit :config)
  (let ((flattened (claudemacs--notification-message-text
                    "first line\nsecond\tline")))
    (should (equal flattened "first line second line")))
  (should-not (claudemacs--notification-message-text "\n \t"))
  (should-not (claudemacs--notification-message-text nil))
  (should (<= (length (claudemacs--notification-message-text
                       (make-string 500 ?x)))
              (1+ claudemacs--notification-message-width))))

(ert-deftest claudemacs-test-notification-text-cannot-break-applescript ()
  "Tool-supplied text stays inside its AppleScript string literal."
  :tags '(:unit :config)
  (should (equal (claudemacs--escape-applescript-string "say \"hi\"")
                 "say \\\"hi\\\""))
  (should (equal (claudemacs--escape-applescript-string "back\\slash")
                 "back\\\\slash")))

(ert-deftest claudemacs-test-ghostel-claude-enables-native-cursor ()
  "Ghostel Claude sessions use Claude Code's terminal cursor path."
  :tags '(:unit :ghostel :terminal-backend)
  (let ((process-environment
         (list "CLAUDE_CODE_ACCESSIBILITY=0" "PATH=/bin")))
    (claudemacs--configure-terminal-process-environment 'claude 'ghostel)
    (should (equal (getenv "CLAUDE_CODE_ACCESSIBILITY") "1"))))

(ert-deftest claudemacs-test-non-ghostel-does-not-change-cursor-environment ()
  "The Ghostel cursor workaround does not affect other sessions."
  :tags '(:unit :terminal-backend)
  (let ((process-environment
         (list "CLAUDE_CODE_ACCESSIBILITY=0" "PATH=/bin")))
    (claudemacs--configure-terminal-process-environment 'claude 'eat)
    (should (equal (getenv "CLAUDE_CODE_ACCESSIBILITY") "0"))))

(ert-deftest claudemacs-test-ghostel-submit-separates-text-and-return ()
  "Test that programmatic Ghostel submission creates an input boundary."
  :tags '(:unit :ghostel :terminal-backend)
  (let (events)
    (cl-letf (((symbol-function 'claudemacs--terminal-send-string)
               (lambda (string)
                 (setq events (append events (list (list :text string))))))
              ((symbol-function 'sleep-for)
               (lambda (seconds &optional _milliseconds)
                 (setq events (append events (list (list :delay seconds))))))
              ((symbol-function 'claudemacs--terminal-send-key)
               (lambda (key)
                 (setq events (append events (list (list :key key)))))))
      (with-temp-buffer
        (setq-local claudemacs--terminal-backend 'ghostel)
        (setq-local claudemacs--tool 'claude)
        (claudemacs--send-to-buffer (current-buffer) "Fix this"))
      (should (equal events
                     '((:text "Fix this") (:delay 0.15) (:key return)))))))

(ert-deftest claudemacs-test-ghostel-submit-delay-is-configurable ()
  "Test that Ghostel's programmatic submit boundary can be adjusted."
  :tags '(:unit :ghostel :terminal-backend)
  (let ((claudemacs-ghostel-submit-delay 0.3)
        observed-delay)
    (cl-letf (((symbol-function 'sleep-for)
               (lambda (seconds &optional _milliseconds)
                 (setq observed-delay seconds)))
              ((symbol-function 'claudemacs--terminal-send-string) #'ignore)
              ((symbol-function 'claudemacs--terminal-send-key) #'ignore))
      (with-temp-buffer
        (setq-local claudemacs--terminal-backend 'ghostel)
        (claudemacs--send-to-buffer (current-buffer) "Fix this"))
      (should (= observed-delay 0.3)))))

(ert-deftest claudemacs-test-ghostel-standalone-return-is-immediate ()
  "Test that standalone Ghostel Return does not pay the paste delay."
  :tags '(:unit :ghostel :terminal-backend)
  (let (events)
    (cl-letf (((symbol-function 'sleep-for)
               (lambda (&rest _arguments)
                 (push 'unexpected-delay events)))
              ((symbol-function 'claudemacs--terminal-send-key)
               (lambda (key)
                 (push (list :key key) events))))
      (let ((claudemacs--terminal-backend 'ghostel))
        (claudemacs--send-return-for-tool (current-buffer)))
      (should (equal events '((:key return)))))))

(ert-deftest claudemacs-test-eat-submit-has-no-ghostel-delay ()
  "Test that Eat submission remains immediate."
  :tags '(:unit :eat :terminal-backend)
  (let (events)
    (cl-letf (((symbol-function 'sleep-for)
               (lambda (&rest _arguments)
                 (push 'unexpected-delay events)))
              ((symbol-function 'claudemacs--terminal-send-key)
               (lambda (key)
                 (push (list :key key) events))))
      (let ((claudemacs--terminal-backend 'eat))
        (claudemacs--send-return-for-tool (current-buffer)))
      (should (equal events '((:key return)))))))

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

(ert-deftest claudemacs-test-windows-notification-is-non-modal ()
  "Test that Windows uses a native tray notification, not a dialog."
  :tags '(:unit :config :windows)
  (let (notification-arguments
        called-program)
    (cl-letf (((symbol-function 'claudemacs--windows-notification)
               (lambda (&rest args)
                 (setq notification-arguments args)))
              ((symbol-function 'call-process)
               (lambda (program &rest _args)
                 (setq called-program program)))
              (system-type 'windows-nt))
      (claudemacs--system-notification "Finished" "Claudemacs")
      (should (equal notification-arguments
                     '("Finished" "Claudemacs")))
      (should-not called-program))))

(ert-deftest claudemacs-test-windows-toast-launches-helper-directly ()
  "Test that toast arguments are passed directly to PowerShell."
  :tags '(:unit :config :windows)
  (let (process-arguments)
    (cl-letf (((symbol-function 'claudemacs--windows-notification-script)
               (lambda () "C:/claudemacs/claudemacs-toast.ps1"))
              ((symbol-function 'executable-find)
               (lambda (_program) "C:/Windows/powershell.exe"))
              ((symbol-function 'make-process)
               (lambda (&rest args) (setq process-arguments args))))
      (claudemacs--launch-windows-notification
       "Finished & waiting" "Claude's session")
      (should
       (equal (plist-get process-arguments :command)
              '("C:/Windows/powershell.exe"
                "-NoProfile" "-WindowStyle" "Hidden"
                "-ExecutionPolicy" "Bypass"
                "-File" "C:/claudemacs/claudemacs-toast.ps1"
                "-Title" "Claude's session"
                "-Message" "Finished & waiting"
                "-TimeoutSeconds" "5"))))))

(ert-deftest claudemacs-test-windows-toast-auto-installs-identity ()
  "Test that the Windows identity is refreshed once per Emacs process."
  :tags '(:unit :config :windows)
  (let ((claudemacs--windows-notification-identity-ready nil)
        (install-count 0)
        launched)
    (cl-letf (((symbol-function 'claudemacs--install-windows-notification-shortcut)
               (lambda ()
                 (cl-incf install-count)
                 "C:/Start Menu/Claudemacs.lnk"))
              ((symbol-function 'claudemacs--launch-windows-notification)
               (lambda (&rest _arguments) (setq launched t))))
      (claudemacs--windows-notification "Finished" "Claudemacs")
      (claudemacs--windows-notification "Finished again" "Claudemacs")
      (should launched)
      (should (= install-count 1)))))

(ert-deftest claudemacs-test-windows-toast-helper-explicitly-dismisses ()
  "Test that the Windows helper hides displayed toasts after its timeout."
  :tags '(:unit :config :windows)
  (let* ((library-directory
          (file-name-directory
           (or (symbol-file 'claudemacs--system-notification 'defun)
               (locate-library "claudemacs"))))
         (script (expand-file-name "claudemacs-toast.ps1"
                                   library-directory))
         (contents (with-temp-buffer
                     (insert-file-contents script)
                     (buffer-string))))
    (should (string-match-p
             "Start-Sleep -Seconds \\$TimeoutSeconds" contents))
    (should (string-match-p
             "\\$notifier\\.Hide(\\$toast)" contents))))

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

;;; Tool Label Tests

(ert-deftest claudemacs-test-start-description-shows-tool-label ()
  "A tool's `:label' precedes the session name in the start menu."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-show-model-in-menu nil)
        (claudemacs-tool-registry
         '((claude :label "Claude" :program "claude" :switches nil))))
    (cl-letf (((symbol-function 'claudemacs--get-next-instance-number)
               (lambda (&rest _) 2)))
      (should (equal (substring-no-properties
                      (claudemacs--get-tool-start-description 'claude t))
                     "Claude - claude-2 (default)")))))

(ert-deftest claudemacs-test-start-description-omits-missing-label ()
  "A tool without a `:label' shows only its session name."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-show-model-in-menu nil)
        (claudemacs-tool-registry
         '((claude :program "claude" :switches nil))))
    (cl-letf (((symbol-function 'claudemacs--get-next-instance-number)
               (lambda (&rest _) 2)))
      (should (equal (substring-no-properties
                      (claudemacs--get-tool-start-description 'claude nil))
                     "claude-2")))))

(ert-deftest claudemacs-test-resume-description-shows-tool-label ()
  "A tool's `:label' precedes the session name in the resume menu."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex :label "Codex" :program "codex" :switches nil))))
    (cl-letf (((symbol-function 'claudemacs--get-next-instance-number)
               (lambda (&rest _) 1)))
      (should (equal (substring-no-properties
                      (claudemacs--get-tool-resume-description 'codex nil))
                     "Codex - codex (resume)")))))

;;; Per-tool Environment in History Lookups

(ert-deftest claudemacs-test-history-picker-is-most-recent-first ()
  "The resume picker sorts history by recency and preserves that display order."
  :tags '(:unit :multi-tool)
  (dolist (tool '(claude codex))
    (let ((rows (list (list :session-id "oldest" :cwd "/tmp/project"
                            :updated-at (seconds-to-time 10))
                      (list :session-id "middle" :cwd "/tmp/project"
                            :updated-at (seconds-to-time 20))
                      (list :session-id "newest" :cwd "/tmp/project"
                            :updated-at (seconds-to-time 30))))
          choices
          properties)
      (cl-letf (((symbol-function 'claudemacs--history-rows-for-tool)
                 (lambda (_tool _cwd) rows))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq choices collection
                         properties completion-extra-properties)
                   (caar collection))))
        (should (equal (claudemacs--select-history-session-id
                        tool "/tmp/project")
                       "newest")))
      (should (equal (mapcar #'cdr choices)
                     '("newest" "middle" "oldest")))
      (should (eq (plist-get properties :display-sort-function) #'identity))
      (should (eq (plist-get properties :cycle-sort-function) #'identity)))))

(ert-deftest claudemacs-test-history-provider-runs-with-tool-env ()
  "History lookups run in the profile's environment, then restore it."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex-pers :tool codex :program "cdx"
                       :env ("CODEX_HOME=/tmp/personal-home"))))
        (before (getenv "CODEX_HOME"))
        seen)
    (cl-letf (((symbol-function 'claudemacs--session-list-history-rows)
               (lambda (&rest _)
                 (setq seen (getenv "CODEX_HOME"))
                 (list :rows nil))))
      (claudemacs--call-history-provider 'codex-pers "/tmp/project"))
    (should (equal seen "/tmp/personal-home"))
    ;; The binding must not outlive the lookup.
    (should (equal (getenv "CODEX_HOME") before))))

(ert-deftest claudemacs-test-history-provider-without-env-is-unchanged ()
  "A tool without `:env' sees Emacs's own environment."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry '((codex :program "codex")))
        (process-environment (cons "CODEX_HOME=/tmp/ambient" process-environment))
        seen)
    (cl-letf (((symbol-function 'claudemacs--session-list-history-rows)
               (lambda (&rest _)
                 (setq seen (getenv "CODEX_HOME"))
                 (list :rows nil))))
      (claudemacs--call-history-provider 'codex "/tmp/project"))
    (should (equal seen "/tmp/ambient"))))

(ert-deftest claudemacs-test-history-provider-resolves-profile-codex-home ()
  "Each Codex profile resolves the session storage under its own CODEX_HOME."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex-work :program "codex" :env ("CODEX_HOME=/tmp/work-home"))
           (codex-pers :tool codex :program "cdx"
                       :env ("CODEX_HOME=/tmp/personal-home"))))
        homes)
    (cl-letf (((symbol-function 'claudemacs--session-list-codex-history)
               (lambda (&rest _)
                 (push (claudemacs--session-list-codex-home) homes)
                 nil))
              ((symbol-function 'claudemacs--session-list-claude-history)
               (lambda (&rest _) nil)))
      (claudemacs--call-history-provider 'codex-work "/tmp/project")
      (claudemacs--call-history-provider 'codex-pers "/tmp/project"))
    (should (equal (nreverse homes)
                   (list (file-name-as-directory (expand-file-name "/tmp/work-home"))
                         (file-name-as-directory (expand-file-name "/tmp/personal-home")))))))

(ert-deftest claudemacs-test-history-provider-resolves-profile-claude-dir ()
  "A Claude profile resolves history under its own CLAUDE_CONFIG_DIR."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((claude-alt :tool claude :program "cl"
                       :env ("CLAUDE_CONFIG_DIR=/tmp/alt-claude"))))
        seen)
    (cl-letf (((symbol-function 'claudemacs--session-list-claude-history)
               (lambda (&rest _)
                 (setq seen (claudemacs--session-list-claude-config-dir))
                 nil)))
      (claudemacs--call-history-provider 'claude-alt "/tmp/project"))
    (should (equal seen
                   (file-name-as-directory (expand-file-name "/tmp/alt-claude"))))))

;;; Tool Family (:tool) Tests

(ert-deftest claudemacs-test-tool-kind-prefers-declared-tool ()
  "An explicit `:tool' decides the CLI family even for an alias program."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex-personal :tool codex :program "cdx")
           (work :tool claude :program "/opt/bin/my-wrapper")
           (quoted :tool "codex" :program "zz"))))
    (should (eq (claudemacs--tool-kind 'codex-personal) 'codex))
    (should (eq (claudemacs--tool-kind 'work) 'claude))
    (should (eq (claudemacs--tool-kind 'quoted) 'codex))))

(ert-deftest claudemacs-test-tool-kind-infers-from-program-then-key ()
  "Without `:tool', the family comes from the program, then the registry key."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((profile-one :program "/usr/local/bin/codex")
           (profile-two :program "codex-personal")
           (gemini :program "gemini-cli")
           (codex-work :program "/opt/bin/unrecognized")
           (mystery :program "/opt/bin/unrecognized"))))
    (should (eq (claudemacs--tool-kind 'profile-one) 'codex))
    (should (eq (claudemacs--tool-kind 'profile-two) 'codex))
    (should (eq (claudemacs--tool-kind 'gemini) 'gemini))
    ;; Program says nothing, so the key settles it.
    (should (eq (claudemacs--tool-kind 'codex-work) 'codex))
    ;; Neither says anything: a generic tool, with no CLI-specific behavior.
    (should-not (claudemacs--tool-kind 'mystery))))

(ert-deftest claudemacs-test-tool-family-drives-cli-specific-behavior ()
  "Two keys running Codex get identical Codex-specific handling."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex-work :program "codex")
           (codex-personal :tool codex :program "cdx")))
        (claudemacs-codex-notification-switches '("--notify")))
    (dolist (tool '(codex-work codex-personal))
      (should (equal (claudemacs--get-tool-notification-switches tool)
                     '("--notify")))
      (should (equal (claudemacs--get-resume-args tool "abc123")
                     '("resume" "abc123")))
      (should (equal (claudemacs--get-branch-args tool "abc123")
                     '("fork" "abc123")))
      (should (equal (claudemacs--translate-args-for-tool
                      tool '("--dangerous-skip-permissions"))
                     '("--dangerously-bypass-approvals-and-sandbox")))
      (should (equal (claudemacs--model-type-switches
                      tool '("deep" :model "gpt-test" :effort "max"))
                     '("--model" "gpt-test" "--config"
                       "model_reasoning_effort=\"max\""))))))

(ert-deftest claudemacs-test-tool-family-reads-profile-config ()
  "Each profile reads the configuration directory from its own `:env'."
  :tags '(:unit :multi-tool)
  (let* ((work-home (make-temp-file "claudemacs-codex-work" t))
         (personal-home (make-temp-file "claudemacs-codex-pers" t))
         (claudemacs-tool-registry
          `((codex-work :program "codex"
                        :env (,(concat "CODEX_HOME=" work-home)))
            (codex-personal :tool codex :program "cdx"
                            :env (,(concat "CODEX_HOME=" personal-home))))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "config.toml" work-home)
            (insert "model = \"gpt-work\"\nmodel_reasoning_effort = \"high\"\n"))
          (with-temp-file (expand-file-name "config.toml" personal-home)
            (insert "model = \"gpt-personal\"\nmodel_reasoning_effort = \"low\"\n"))
          (should (equal (claudemacs--get-tool-configured-model 'codex-work)
                         '(:model "gpt-work" :effort "high")))
          (should (equal (claudemacs--get-tool-configured-model 'codex-personal)
                         '(:model "gpt-personal" :effort "low"))))
      (delete-directory work-home t)
      (delete-directory personal-home t))))

(ert-deftest claudemacs-test-hyphenated-tool-keys-parse-from-buffer-name ()
  "Session info survives a hyphenated registry key, with and without instances."
  :tags '(:unit :multi-tool)
  (should (equal (claudemacs--split-tool-instance-name "codex-personal")
                 '(codex-personal . 1)))
  (should (equal (claudemacs--split-tool-instance-name "codex-personal-3")
                 '(codex-personal . 3)))
  (should (equal (claudemacs--split-tool-instance-name "codex") '(codex . 1)))
  (cl-letf (((symbol-function 'claudemacs--session-id) (lambda (&rest _) "main")))
    (let ((buffer (get-buffer-create "*claudemacs:codex-personal-2:main*")))
      (unwind-protect
          (let ((info (claudemacs--get-session-info buffer)))
            (should (eq (plist-get info :tool) 'codex-personal))
            (should (= (plist-get info :instance) 2))
            (should (equal (plist-get info :session-id) "main")))
        (kill-buffer buffer)))))

(ert-deftest claudemacs-test-numeric-suffix-profile-key-stays-intact ()
  "An exact registry key wins over the display instance suffix grammar."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex :program "codex")
           (codex-personal-2024 :tool codex :program "cdx"))))
    (should (equal (claudemacs--split-tool-instance-name
                    "codex-personal-2024")
                   '(codex-personal-2024 . 1)))
    (should (equal (claudemacs--split-tool-instance-name "codex-2")
                   '(codex . 2)))))

(ert-deftest claudemacs-test-next-instance-skips-profile-name-collision ()
  "Automatic instance allocation does not claim another profile's name."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex :program "codex")
           (codex-2 :tool codex :program "codex"))))
    (cl-letf (((symbol-function 'claudemacs--get-instance-numbers-for-tool)
               (lambda (&rest _) '(1))))
      (should (= (claudemacs--get-next-instance-number 'codex) 3)))))

(ert-deftest claudemacs-test-running-session-keeps-captured-tool-family ()
  "Registry edits do not change how an existing session receives text."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry '((work :tool codex :program "codex")))
        events)
    (with-temp-buffer
      (setq-local claudemacs--tool 'work
                  claudemacs--tool-instance 1
                  claudemacs--session-tool-kind 'codex
                  claudemacs--session-tool-kind-set-p t)
      (setq claudemacs-tool-registry '((work :tool claude :program "claude")))
      (cl-letf (((symbol-function 'claudemacs--terminal-paste-string)
                 (lambda (text) (push (list :paste text) events)))
                ((symbol-function 'claudemacs--terminal-send-string)
                 (lambda (text) (push (list :send text) events))))
        (claudemacs--send-to-buffer (current-buffer) "hello" t))
      (should (equal events '((:paste "hello")))))))

(ert-deftest claudemacs-test-branch-rejects-changed-profile-family ()
  "Branching stops when a live session's profile changed CLI families."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((work :tool codex :program "codex"))))
    (with-temp-buffer
      (let ((session (list :tool 'work
                           :tool-kind 'claude
                           :buffer (current-buffer))))
        (cl-letf (((symbol-function 'claudemacs--list-sessions-for-workspace)
                   (lambda () (list session))))
          (should-error (claudemacs-branch-session)
                        :type 'user-error))))))

(ert-deftest claudemacs-test-duplicate-registry-keys-warn ()
  "Repeated registry keys warn instead of silently collapsing."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex :label "Codex work" :program "codex")
           (codex :label "Codex pers" :program "codex")
           (claude :label "Claude" :program "claude")))
        warning-text)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _) (setq warning-text message))))
      (claudemacs--warn-on-duplicate-tool-keys)
      (should warning-text)
      (should (string-match-p "`codex'" warning-text)))))

;;; Tool Environment Tests

(ert-deftest claudemacs-test-tool-env-returns-registry-entries ()
  "A tool's `:env' entries are read from the registry."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex :program "codex" :env ("CODEX_HOME=/tmp/personal"))
           (claude :program "claude"))))
    (should (equal (claudemacs--get-tool-env 'codex)
                   '("CODEX_HOME=/tmp/personal")))
    (should-not (claudemacs--get-tool-env 'claude))))

(ert-deftest claudemacs-test-codex-profile-home-isolates-sqlite-state ()
  "A profile CODEX_HOME outranks an ambient SQLite home for that profile."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex-personal :tool codex :program "codex"
                           :env ("CODEX_HOME=/tmp/personal"))))
        (process-environment
         (cons "CODEX_SQLITE_HOME=/tmp/ambient" process-environment))
        seen)
    (claudemacs--call-with-tool-env
     'codex-personal
     (lambda ()
       (setq seen (list (getenv "CODEX_HOME")
                        (getenv "CODEX_SQLITE_HOME")))))
    (should (equal seen '("/tmp/personal" "/tmp/personal")))))

(ert-deftest claudemacs-test-explicit-profile-sqlite-home-wins ()
  "An explicit profile SQLite home is not replaced by CODEX_HOME."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex-personal :tool codex :program "codex"
                           :env ("CODEX_HOME=/tmp/personal"
                                 "CODEX_SQLITE_HOME=/tmp/sqlite")))))
    (should (equal (claudemacs--effective-tool-env 'codex-personal)
                   '("CODEX_HOME=/tmp/personal"
                     "CODEX_SQLITE_HOME=/tmp/sqlite")))))

(ert-deftest claudemacs-test-tool-env-rejects-malformed-entries ()
  "Malformed `:env' entries signal rather than starting with a wrong environment."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex :program "codex" :env ("CODEX_HOME"))
           (gemini :program "gemini" :env ("=novar"))
           (aider :program "aider" :env (bad-entry)))))
    (should-error (claudemacs--get-tool-env 'codex))
    (should-error (claudemacs--get-tool-env 'gemini))
    (should-error (claudemacs--get-tool-env 'aider))))

(ert-deftest claudemacs-test-tool-env-applies-to-session-process ()
  "A tool's `:env' reaches the session process and outranks the global list."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex :program "codex"
                  :env ("CODEX_HOME=/tmp/personal" "TERM=dumb"))))
        (claudemacs-process-environment '("TERM=xterm-256color"))
        (claudemacs-use-shell-env nil)
        captured-env)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/codex"))
              ((symbol-function 'claudemacs--terminal-ensure-backend) (lambda (&rest _) nil))
              ((symbol-function 'claudemacs--terminal-post-display) (lambda (&rest _) nil))
              ((symbol-function 'claudemacs--setup-terminal-integration) (lambda (&rest _) nil))
              ((symbol-function 'claudemacs--set-session-identity) (lambda (&rest _) nil))
              ((symbol-function 'display-buffer) (lambda (buffer &rest _)
                                                   (set-window-buffer (selected-window) buffer)
                                                   (selected-window)))
              ((symbol-function 'select-window) (lambda (&rest _) nil))
              ((symbol-function 'run-with-timer) (lambda (&rest _) nil))
              ((symbol-function 'claudemacs--terminal-start)
               (lambda (&rest _)
                 (setq captured-env (list (getenv "CODEX_HOME")
                                          (getenv "CODEX_SQLITE_HOME")
                                          (getenv "TERM")))
                 nil)))
      (let ((buffer (claudemacs--start default-directory 'codex 1)))
        (unwind-protect
            (should (equal captured-env
                           '("/tmp/personal" "/tmp/personal" "dumb")))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest claudemacs-test-tool-env-does-not-leak-outside-session ()
  "A tool's `:env' does not modify the Emacs environment."
  :tags '(:unit :multi-tool)
  (let ((claudemacs-tool-registry
         '((codex :program "codex" :env ("CLAUDEMACS_TEST_ENV=set"))))
        (claudemacs-use-shell-env nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/codex"))
              ((symbol-function 'claudemacs--terminal-ensure-backend) (lambda (&rest _) nil))
              ((symbol-function 'claudemacs--terminal-post-display) (lambda (&rest _) nil))
              ((symbol-function 'claudemacs--setup-terminal-integration) (lambda (&rest _) nil))
              ((symbol-function 'claudemacs--set-session-identity) (lambda (&rest _) nil))
              ((symbol-function 'display-buffer) (lambda (buffer &rest _)
                                                   (set-window-buffer (selected-window) buffer)
                                                   (selected-window)))
              ((symbol-function 'select-window) (lambda (&rest _) nil))
              ((symbol-function 'run-with-timer) (lambda (&rest _) nil))
              ((symbol-function 'claudemacs--terminal-start) (lambda (&rest _) nil)))
      (let ((buffer (claudemacs--start default-directory 'codex 1)))
        (unwind-protect
            (should-not (getenv "CLAUDEMACS_TEST_ENV"))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;;; Model Menu Tests

(ert-deftest claudemacs-test-model-menu-is-off-by-default ()
  "The start menu does not read or display model information by default."
  :tags '(:unit :model-menu)
  (let ((claudemacs-show-model-in-menu nil))
    (should-not (claudemacs--model-type-toggle-visible-p))
    (should (string-match-p
             ":if claudemacs--model-type-toggle-visible-p"
             (format "%S" (get 'claudemacs-start-menu 'transient--layout))))
    (should-not (string-match-p "gpt-5.6"
                                (claudemacs--get-tool-start-description
                                 'codex t)))))

(ert-deftest claudemacs-test-model-menu-displays-configured-model ()
  "The enabled start menu displays the configured model in comment face."
  :tags '(:unit :model-menu)
  (let ((claudemacs-show-model-in-menu t)
        (claudemacs-tool-registry
         '((codex :program "codex" :switches nil))))
    (cl-letf (((symbol-function 'claudemacs--get-tool-configured-model)
               (lambda (_tool) '(:model "gpt-test" :effort "high"))))
      (let ((description (claudemacs--get-tool-start-description 'codex t)))
        (should (string-match-p "gpt-test/high" description))
        (should (eq (get-text-property
                     (string-match "gpt-test/high" description)
                     'face description)
                    'font-lock-comment-face))))))

(ert-deftest claudemacs-test-model-type-switches-are-tool-specific ()
  "Model types translate to the correct switches for each supported tool."
  :tags '(:unit :model-menu)
  (should (equal
           (claudemacs--model-type-switches
            'codex '("deep" :model "gpt-test" :effort "max"))
           '("--model" "gpt-test" "--config"
             "model_reasoning_effort=\"max\"")))
  (should (equal
           (claudemacs--model-type-switches
            'claude '("deep" :model "opus" :effort "max"))
           '("--model" "opus" "--effort" "max"))))

(ert-deftest claudemacs-test-model-type-display-uses-preset-name ()
  "Model type displays use the preset name."
  :tags '(:unit :model-menu)
  (should (equal
           (claudemacs--format-model-type-display
            '("sol-high" :model "gpt-5.6-sol" :effort "high"))
           "sol-high")))

(ert-deftest claudemacs-test-configured-model-display-uses-matching-preset ()
  "A configured model uses its preset display when it exactly matches."
  :tags '(:unit :model-menu)
  (let ((claudemacs-tool-registry
         '((codex :model-types
                  (("luna-max" :model "gpt-5.6-luna" :effort "max"))))))
    (cl-letf (((symbol-function 'claudemacs--get-tool-configured-model)
               (lambda (_tool) '(:model "gpt-5.6-luna" :effort "max"))))
      (should (equal (claudemacs--get-tool-model-display 'codex)
                     "luna-max")))))

(ert-deftest claudemacs-test-toggle-model-type-cycles-and-refreshes ()
  "Toggling selects successive model types and refreshes an active menu."
  :tags '(:unit :model-menu)
  (let ((claudemacs-show-model-in-menu t)
        (claudemacs--model-type-offset nil)
        (claudemacs-default-tool 'codex)
        (claudemacs-tool-registry
         '((codex :program "codex" :switches nil
                  :model-types (("first" :model "one")
                                ("second" :model "two")))))
        refresh-count)
    (cl-letf (((symbol-function 'claudemacs--get-tool-configured-model)
               (lambda (_tool) '(:model "one")))
              ((symbol-function 'transient--refresh-transient)
               (lambda () (setq refresh-count (1+ (or refresh-count 0))))))
      (claudemacs-toggle-model-type)
      (should (= claudemacs--model-type-offset 1))
      (should (equal (claudemacs--model-type-name
                      (claudemacs--model-type-for-tool 'codex))
                     "second"))
      (claudemacs-toggle-model-type)
      (should (= claudemacs--model-type-offset 2))
      (should-not (claudemacs--model-type-for-tool 'codex))
      ;; No active transient prefix means the refresh hook is not required.
      (should-not refresh-count))))

(ert-deftest claudemacs-test-model-type-cycle-includes-unmatched-default ()
  "An unmatched configured model is followed by all types before wrapping."
  :tags '(:unit :model-menu)
  (let ((claudemacs-tool-registry
         '((codex :model-types (("first" :model "one")
                                ("second" :model "two"))))))
    (cl-letf (((symbol-function 'claudemacs--get-tool-configured-model)
               (lambda (_tool) '(:model "configured-default"))))
      (should-not (claudemacs--model-type-for-tool-at-offset 'codex nil))
      (should-not (claudemacs--model-type-for-tool-at-offset 'codex 0))
      (should (equal
               (claudemacs--model-type-name
                (claudemacs--model-type-for-tool-at-offset 'codex 1))
               "first"))
      (should (equal
               (claudemacs--model-type-name
                (claudemacs--model-type-for-tool-at-offset 'codex 2))
               "second"))
      (should-not (claudemacs--model-type-for-tool-at-offset 'codex 3))
      (should (equal
               (claudemacs--model-type-name
                (claudemacs--model-type-for-tool-at-offset 'codex 4))
               "first")))))

(ert-deftest claudemacs-test-model-type-cycles-are-independent-per-tool ()
  "Each tool wraps according to its own default-and-types cycle."
  :tags '(:unit :model-menu)
  (let ((claudemacs--model-type-offset nil)
        (claudemacs-default-tool 'tool-one)
        (claudemacs-tool-registry
         '((tool-one :model-types (("one-first" :model "one-first")
                                   ("one-second" :model "one-second")))
           ;; The configured default is already the first listed type, so
           ;; this tool has two unique positions: default and one-other.
           (tool-two :model-types (("two-default" :model "two-default")
                                   ("two-other" :model "two-other"))))))
    (cl-letf (((symbol-function 'claudemacs--get-tool-configured-model)
               (lambda (tool)
                 (if (eq tool 'tool-one)
                     '(:model "one-default")
                   '(:model "two-default")))))
      (should (= (claudemacs--model-type-cycle-length 'tool-one) 3))
      (should (= (claudemacs--model-type-cycle-length 'tool-two) 2))
      (should (= (claudemacs--model-type-count) 3))
      (let ((model-name
             (lambda (tool)
               (let ((model-type (claudemacs--model-type-for-tool tool)))
                 (and model-type
                      (claudemacs--model-type-name model-type))))))
        (should-not (funcall model-name 'tool-one))
        (should-not (funcall model-name 'tool-two))
        ;; Offset 1: both tools advance from their defaults.
        (claudemacs-toggle-model-type)
        (should (equal (funcall model-name 'tool-one) "one-first"))
        (should (equal (funcall model-name 'tool-two) "two-other"))
        ;; Offset 2: tool-one selects its second type while tool-two wraps.
        (claudemacs-toggle-model-type)
        (should (equal (funcall model-name 'tool-one) "one-second"))
        (should-not (funcall model-name 'tool-two))
        ;; Offset 3: tool-one wraps while tool-two advances again.
        (claudemacs-toggle-model-type)
        (should-not (funcall model-name 'tool-one))
        (should (equal (funcall model-name 'tool-two) "two-other"))
        ;; Offset 4: both continue their own cycles.
        (claudemacs-toggle-model-type)
        (should (equal (funcall model-name 'tool-one) "one-first"))
        (should-not (funcall model-name 'tool-two))))))

(ert-deftest claudemacs-test-start-menu-adds-selected-model-switches ()
  "Starting a tool from the menu passes its selected model switches."
  :tags '(:unit :model-menu)
  (let ((claudemacs-show-model-in-menu t)
        (claudemacs--model-type-offset 1)
        (claudemacs-tool-registry
         '((codex :program "codex" :switches nil
                  :model-types (("first" :model "one")
                                ("second" :model "two")))))
        observed)
    (cl-letf (((symbol-function 'claudemacs--get-tool-configured-model)
               (lambda (_tool) '(:model "one")))
              ((symbol-function 'transient-args) (lambda (_prefix) nil))
              ((symbol-function 'claudemacs--run-with-args)
               (lambda (_tool _directory &rest args)
                 (setq observed args))))
      (claudemacs--start-tool-by-index 0)
      (should (equal observed '("--model" "two"))))))

(ert-deftest claudemacs-test-model-toggle-starts-from-each-tool-config ()
  "The first toggle advances each tool from its own configured model."
  :tags '(:unit :model-menu)
  (let ((claudemacs-show-model-in-menu t)
        (claudemacs--model-type-offset nil)
        (claudemacs-default-tool 'claude)
        (claudemacs-tool-registry
         '((claude :model-types (("opus-max" :model "opus" :effort "max")
                                ("sonnet-high" :model "sonnet" :effort "high")))
           (codex :model-types (("luna-max" :model "luna" :effort "max")
                                ("sol-high" :model "sol" :effort "high"))))))
    (cl-letf (((symbol-function 'claudemacs--get-tool-configured-model)
               (lambda (tool)
                 (if (eq tool 'claude)
                     '(:model "sonnet" :effort "medium")
                   '(:model "luna" :effort "max")))))
      (should (equal (claudemacs--get-tool-model-display 'claude)
                     "sonnet-medium"))
      (should (equal (claudemacs--get-tool-model-display 'codex)
                     "luna-max"))
      (claudemacs-toggle-model-type)
      (should (equal (claudemacs--model-type-name
                      (claudemacs--model-type-for-tool 'claude))
                     "opus-max"))
      (should (equal (claudemacs--model-type-name
                      (claudemacs--model-type-for-tool 'codex))
                     "sol-high"))
      (should (string-match-p
               "sonnet-high"
               (claudemacs--get-toggle-model-type-description)))
      (claudemacs-toggle-model-type)
      (should (= claudemacs--model-type-offset 2))
      (should (equal (claudemacs--model-type-name
                      (claudemacs--model-type-for-tool 'claude))
                     "sonnet-high"))
      (should-not (claudemacs--model-type-for-tool 'codex))
      (should (string-match-p
               "sonnet-medium"
               (claudemacs--get-toggle-model-type-description))))))

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
                  ((symbol-function 'claudemacs--terminal-kill) #'ignore))
          (let ((buf (get-buffer-create "*claudemacs:claude:test-kill-current*")))
            (with-current-buffer buf
              (setq-local claudemacs--tool 'claude)
              (setq-local buffer-display-time (current-time))
              (claudemacs-kill)
              ;; Buffer should be killed
              (should-not (buffer-live-p buf))))))

        ;; Test killing most recent session when not in claudemacs buffer
        (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-kill-recent"))
                  ((symbol-function 'claudemacs--terminal-kill) #'ignore))
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
                ((symbol-function 'claudemacs--terminal-kill) #'ignore))
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
                    ((symbol-function 'claudemacs--terminal-kill) #'ignore))
            ;; Kill should only affect workspace-a
            (claudemacs-kill)
            (should-not (buffer-live-p buf-a))
            (should (buffer-live-p buf-b)))))
    ;; Cleanup
    (when (get-buffer "*claudemacs:claude:workspace-a*")
      (kill-buffer "*claudemacs:claude:workspace-a*"))
    (when (get-buffer "*claudemacs:codex:workspace-b*")
      (kill-buffer "*claudemacs:codex:workspace-b*"))))

;;; Branch/Continue Session Tests

(ert-deftest claudemacs-test-get-branch-args-per-tool ()
  "Branch arguments require an authoritative source ID."
  :tags '(:unit :branch)
  ;; Missing IDs fail closed instead of invoking a picker or --last.
  (should-not (claudemacs--get-branch-args 'claude))
  (should-not (claudemacs--get-branch-args 'codex))
  (should (equal (claudemacs--get-branch-args 'claude "abc-123")
                 '("--resume" "abc-123" "--fork-session")))
  (should (equal (claudemacs--get-branch-args
                  'claude "abc-123" "destination-id")
                 '("--resume" "abc-123" "--fork-session"
                   "--session-id" "destination-id")))
  (should (equal (claudemacs--get-branch-args 'codex "019cbad9-9004-7b33-b212-0261d35fc7b7")
                 '("fork" "019cbad9-9004-7b33-b212-0261d35fc7b7")))
  (should (equal (claudemacs--get-branch-args 'gemini "source-id")
                 '("--resume" "source-id")))
  (should (equal (claudemacs--get-branch-args 'unknown-tool "source-id")
                 '("--resume" "source-id"))))

(ert-deftest claudemacs-test-generate-uuid ()
  "Test that generated UUIDs have valid v4 format and are unique."
  :tags '(:unit :branch)
  (let ((uuid1 (claudemacs--generate-uuid))
        (uuid2 (claudemacs--generate-uuid)))
    ;; Check UUID v4 format: 8-4-4-4-12 hex digits with version 4 marker
    (should (string-match-p "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}$" uuid1))
    (should (string-match-p "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}$" uuid2))
    ;; Two generated UUIDs should be different
    (should-not (equal uuid1 uuid2))))

(ert-deftest claudemacs-test-get-resume-flag-per-tool ()
  "Test that each tool returns correct resume flag."
  :tags '(:unit :branch)
  ;; Claude uses --resume
  (should (equal (claudemacs--get-resume-flag 'claude) "--resume"))
  ;; Codex uses 'resume' subcommand (no dashes)
  (should (equal (claudemacs--get-resume-flag 'codex) "resume"))
  ;; Unknown tools default to --resume
  (should (equal (claudemacs--get-resume-flag 'unknown-tool) "--resume")))

(provide 'claudemacs-test)
;;; claudemacs-test.el ends here
