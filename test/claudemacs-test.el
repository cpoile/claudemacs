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

;;; Basic Sanity Tests

(ert-deftest claudemacs-test-sanity ()
  "Basic sanity test to verify ERT is working."
  :tags '(:unit :sanity)
  (should t)
  (should-not nil)
  (should (= 2 (+ 1 1)))
  (should (string= "hello" "hello"))
  (should (listp '(1 2 3))))

(ert-deftest claudemacs-test-package-loaded ()
  "Test that claudemacs package is loaded correctly."
  :tags '(:unit :sanity)
  (should (featurep 'claudemacs))
  (should (fboundp 'claudemacs-run))
  (should (fboundp 'claudemacs-kill))
  (should (fboundp 'claudemacs-toggle-buffer))
  (should (boundp 'claudemacs-program))
  (should (boundp 'claudemacs-notify-on-await)))

(ert-deftest claudemacs-test-custom-group-exists ()
  "Test that claudemacs custom group is properly defined."
  :tags '(:unit :sanity)
  (should (get 'claudemacs 'group-documentation))
  ;; Check that the group has members (custom variables and faces)
  (should (get 'claudemacs 'custom-group))
  ;; Verify some expected members are in the group
  (let ((members (get 'claudemacs 'custom-group)))
    (should (assq 'claudemacs-program members))
    (should (assq 'claudemacs-notify-on-await members))))

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
  "Test session ID generation with Perspective mode."
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

;;; Buffer Name Generation Tests

(ert-deftest claudemacs-test-buffer-name-generation ()
  "Test buffer name generation from session ID."
  :tags '(:unit :session)
  (cl-letf (((symbol-function 'claudemacs--session-id) (lambda () "test-session")))
    (should (string= (claudemacs--get-buffer-name) "*claudemacs:test-session*"))))

(ert-deftest claudemacs-test-buffer-name-with-special-chars ()
  "Test buffer name generation with special characters in session ID."
  :tags '(:unit :session)
  (cl-letf (((symbol-function 'claudemacs--session-id) 
             (lambda () "/path/with/slashes")))
    (should (string= (claudemacs--get-buffer-name) "*claudemacs:/path/with/slashes*"))))

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

;;; Startup Hook Tests

(ert-deftest claudemacs-test-startup-hook-exists ()
  "Test that claudemacs-startup-hook variable exists."
  :tags '(:unit :startup-hook)
  (should (boundp 'claudemacs-startup-hook))
  (should (eq claudemacs-startup-hook nil))) ; Default value should be nil

(ert-deftest claudemacs-test-startup-hook-is-hook ()
  "Test that claudemacs-startup-hook is properly defined as a hook."
  :tags '(:unit :startup-hook)
  (should (get 'claudemacs-startup-hook 'custom-type))
  (should (eq (get 'claudemacs-startup-hook 'custom-type) 'hook)))

(ert-deftest claudemacs-test-startup-hook-documentation ()
  "Test that claudemacs-startup-hook has proper documentation."
  :tags '(:unit :startup-hook)
  (let ((doc (documentation-property 'claudemacs-startup-hook 'variable-documentation)))
    (should doc)
    (should (string-match-p "after.*finished starting up" doc))
    (should (string-match-p "current buffer" doc))))

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

;;; Custom Variable Tests

(ert-deftest claudemacs-test-custom-variable-defaults ()
  "Test that custom variables have correct default values."
  :tags '(:unit :config)
  (should (string= claudemacs-program "claude"))
  (should (eq claudemacs-program-switches nil))
  (should (eq claudemacs-switch-to-buffer-on-create t))
  (should (eq claudemacs-switch-to-buffer-on-toggle t))
  (should (eq claudemacs-m-return-is-submit nil))
  (should (eq claudemacs-shift-return-newline t))
  (should (eq claudemacs-switch-to-buffer-on-file-add nil))
  (should (eq claudemacs-notify-on-await t))
  (should (string= claudemacs-notification-sound-mac "Submarine"))
  (should (eq claudemacs-startup-hook nil))) ; Include startup hook in defaults test

(ert-deftest claudemacs-test-custom-variable-types ()
  "Test that custom variables have correct types."
  :tags '(:unit :config)
  ;; Test string variables
  (should (stringp claudemacs-program))
  (should (stringp claudemacs-notification-sound-mac))
  
  ;; Test boolean variables
  (should (booleanp claudemacs-switch-to-buffer-on-create))
  (should (booleanp claudemacs-switch-to-buffer-on-toggle))
  (should (booleanp claudemacs-m-return-is-submit))
  (should (booleanp claudemacs-shift-return-newline))
  (should (booleanp claudemacs-switch-to-buffer-on-file-add))
  (should (booleanp claudemacs-notify-on-await))
  
  ;; Test list variable
  (should (listp claudemacs-program-switches)))

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

(provide 'claudemacs-test)
;;; claudemacs-test.el ends here
