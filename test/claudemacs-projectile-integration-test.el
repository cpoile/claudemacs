;;; claudemacs-projectile-integration-test.el --- Real projectile integration tests -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0") (projectile "2.0"))

;;; Commentary:
;; Real integration tests for claudemacs projectile support.
;; 
;; REQUIREMENTS:
;; - These tests require the 'projectile' package to be installed
;; - Tests will fail if projectile is not available
;; - This is intentional - we test real projectile integration, not fallbacks
;;
;; These tests verify that claudemacs correctly integrates with projectile
;; by creating real project structures and comparing our results with
;; projectile's actual detection.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path to find claudemacs
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'claudemacs)

;; Smart projectile loading - try to find projectile in user's installation
(defun claudemacs-test--try-load-projectile ()
  "Try to load projectile from common installation locations.
Returns t if successful, nil otherwise."
  (or
   ;; Try normal require first (in case it's in load-path)
   (ignore-errors (require 'projectile) t)
   
   ;; Try straight.el installation
   (let ((straight-projectile-dir "~/.config/emacs/.local/straight/repos/projectile"))
     (when (file-exists-p (expand-file-name straight-projectile-dir))
       (add-to-list 'load-path (expand-file-name straight-projectile-dir))
       (ignore-errors (require 'projectile) t)))
   
   ;; Try package.el installation locations
   (let ((package-dirs '("~/.emacs.d/elpa" "~/.config/emacs/elpa")))
     (cl-some (lambda (dir)
                (when (file-exists-p dir)
                  (let ((projectile-dirs (directory-files dir t "projectile-[0-9]")))
                    (when projectile-dirs
                      (add-to-list 'load-path (car projectile-dirs))
                      (ignore-errors (require 'projectile) t)))))
              package-dirs))
   
   ;; Try Doom Emacs location
   (let ((doom-projectile-dir "~/.doom.d/.local/straight/repos/projectile"))
     (when (file-exists-p (expand-file-name doom-projectile-dir))
       (add-to-list 'load-path (expand-file-name doom-projectile-dir))
       (ignore-errors (require 'projectile) t)))
   
   ;; Try spacemacs location
   (let ((spacemacs-projectile-dir "~/.emacs.d/.cache/projectile"))
     (when (file-exists-p (expand-file-name spacemacs-projectile-dir))
       (add-to-list 'load-path (expand-file-name spacemacs-projectile-dir))
       (ignore-errors (require 'projectile) t)))))

;; Try to load projectile
(if (claudemacs-test--try-load-projectile)
    (message "✓ Projectile loaded successfully - projectile integration tests will run")
  (progn
    (message "WARNING: Projectile not found. Projectile integration tests will be skipped.")
    (message "To run these tests, install projectile or ensure it's in your load-path.")))

;;; Test Utilities

(defmacro claudemacs-test-require-projectile (&rest body)
  "Execute BODY only if projectile is available, otherwise skip test."
  `(if (featurep 'projectile)
       (progn ,@body)
     (ert-skip "Projectile not available - skipping test")))

(defmacro claudemacs-test-with-projectile-preference (enabled &rest body)
  "Execute BODY with `claudemacs-prefer-projectile-root' set to ENABLED."
  (declare (indent 1))
  `(let ((original-value claudemacs-prefer-projectile-root))
     (unwind-protect
         (progn
           (setq claudemacs-prefer-projectile-root ,enabled)
           ,@body)
       (setq claudemacs-prefer-projectile-root original-value))))

(defun claudemacs-test-create-directory-structure (base-dir structure)
  "Create directory STRUCTURE under BASE-DIR."
  (dolist (item structure)
    (let ((path (expand-file-name (car item) base-dir))
          (content (cdr item)))
      (if (string-suffix-p "/" (car item))
          (make-directory path t)
        (let ((dir (file-name-directory path)))
          (when dir (make-directory dir t))
          (write-region (or content "") nil path))))))

(defun claudemacs-test-init-git-repo (dir)
  "Initialize a git repository in DIR with a basic commit."
  (let ((default-directory dir))
    (call-process "git" nil nil nil "init" "--quiet")
    (write-region "# Test repo\n" nil "README.md")
    (call-process "git" nil nil nil "add" ".")
    (call-process "git" nil nil nil
                  "-c" "user.name=Test User"
                  "-c" "user.email=test@example.com"
                  "commit" "-m" "Initial commit" "--quiet")))

;;; Core Integration Tests

(ert-deftest claudemacs-test-projectile-basic-integration ()
  "Test basic projectile integration with .projectile marker.

Directory structure:
project/
├── .projectile
└── src/
    └── file.el"
  :tags '(:integration :projectile)
  (claudemacs-test-require-projectile
   (let ((project-dir (make-temp-file "claudemacs-basic-projectile" t)))
    (unwind-protect
        (progn
          (claudemacs-test-create-directory-structure project-dir
            '((".projectile" . "")
              ("src/" . nil)
              ("src/file.el" . "(message \"test\")")))
          
          (claudemacs-test-with-projectile-preference t
            (let ((test-file (expand-file-name "src/file.el" project-dir))
                  (default-directory (expand-file-name "src" project-dir)))
              (cl-letf (((symbol-function 'buffer-file-name) 
                         (lambda () test-file)))
                ;; Get projectile's detection and verify our function matches exactly
                (let ((projectile-root (projectile-project-root)))
                  (should (string= (file-name-as-directory (claudemacs--project-root))
                                   (file-name-as-directory projectile-root))))))))
      (when (file-exists-p project-dir)
        (delete-directory project-dir t))))))

(ert-deftest claudemacs-test-projectile-vs-git-preference ()
  "Test that projectile preference controls which detection is used.

Directory structure:
parent/
├── .projectile
└── child/
    ├── .git/
    └── file.txt"
  :tags '(:integration :projectile)
  (let ((parent-dir (make-temp-file "claudemacs-preference-test" t)))
    (unwind-protect
        (let ((child-dir (expand-file-name "child" parent-dir)))
          (claudemacs-test-create-directory-structure parent-dir
            '((".projectile" . "")
              ("child/" . nil)
              ("child/.git/" . nil)
              ("child/file.txt" . "test content")))
          (claudemacs-test-init-git-repo child-dir)
          
          (let ((test-file (expand-file-name "child/file.txt" parent-dir))
                (default-directory child-dir))
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () test-file)))
              
              ;; Test with projectile preference DISABLED
              (claudemacs-test-with-projectile-preference nil
                ;; Should use git detection (child dir)
                (should (string= (file-name-as-directory (claudemacs--project-root))
                                 (file-name-as-directory child-dir))))
              
              ;; Test with projectile preference ENABLED
              (claudemacs-test-with-projectile-preference t
                ;; Should match whatever projectile detects
                (let ((projectile-root (projectile-project-root)))
                  (should (string= (file-name-as-directory (claudemacs--project-root))
                                   (file-name-as-directory projectile-root))))))))
      (when (file-exists-p parent-dir)
        (delete-directory parent-dir t)))))

(ert-deftest claudemacs-test-projectile-package-json ()
  "Test projectile integration with package.json project marker.

Directory structure:
project/
├── package.json
└── src/
    └── app.js"
  :tags '(:integration :projectile)
  (let ((project-dir (make-temp-file "claudemacs-package-json" t)))
    (unwind-protect
        (progn
          (claudemacs-test-create-directory-structure project-dir
            '(("package.json" . "{\"name\": \"test-project\", \"version\": \"1.0.0\"}")
              ("src/" . nil)
              ("src/app.js" . "console.log('Hello World');")))
          
          (claudemacs-test-with-projectile-preference t
            (let ((test-file (expand-file-name "src/app.js" project-dir))
                  (default-directory (expand-file-name "src" project-dir)))
              (cl-letf (((symbol-function 'buffer-file-name) 
                         (lambda () test-file)))
                ;; Projectile should detect the package.json
                (let ((projectile-root (projectile-project-root)))
                  ;; Our function should match projectile's detection exactly
                  (should (string= (file-name-as-directory (claudemacs--project-root))
                                   (file-name-as-directory projectile-root))))))))
      (when (file-exists-p project-dir)
        (delete-directory project-dir t)))))

(ert-deftest claudemacs-test-projectile-monorepo ()
  "Test projectile with monorepo structure.

Directory structure:
monorepo/
├── .projectile
├── backend/
│   ├── .git/
│   └── api/
│       └── server.py
└── frontend/
    ├── .git/
    └── src/
        └── app.tsx"
  :tags '(:integration :projectile)
  (let ((monorepo-dir (make-temp-file "claudemacs-monorepo" t)))
    (unwind-protect
        (let ((backend-dir (expand-file-name "backend" monorepo-dir))
              (frontend-dir (expand-file-name "frontend" monorepo-dir)))
          (claudemacs-test-create-directory-structure monorepo-dir
            '((".projectile" . "")
              ("backend/" . nil)
              ("backend/.git/" . nil)
              ("backend/api/" . nil)
              ("backend/api/server.py" . "from flask import Flask")
              ("frontend/" . nil)
              ("frontend/.git/" . nil)
              ("frontend/src/" . nil)
              ("frontend/src/app.tsx" . "export default function App() {}")))
          
          (claudemacs-test-init-git-repo backend-dir)
          (claudemacs-test-init-git-repo frontend-dir)
          
          (claudemacs-test-with-projectile-preference t
            ;; Test from backend
            (let ((backend-file (expand-file-name "backend/api/server.py" monorepo-dir))
                  (default-directory (expand-file-name "backend/api" monorepo-dir)))
              (cl-letf (((symbol-function 'buffer-file-name) 
                         (lambda () backend-file)))
                (let ((projectile-root (projectile-project-root)))
                  (should (string= (file-name-as-directory (claudemacs--project-root))
                                   (file-name-as-directory projectile-root))))))
            
            ;; Test from frontend
            (let ((frontend-file (expand-file-name "frontend/src/app.tsx" monorepo-dir))
                  (default-directory (expand-file-name "frontend/src" monorepo-dir)))
              (cl-letf (((symbol-function 'buffer-file-name) 
                         (lambda () frontend-file)))
                (let ((projectile-root (projectile-project-root)))
                  (should (string= (file-name-as-directory (claudemacs--project-root))
                                   (file-name-as-directory projectile-root))))))))
      (when (file-exists-p monorepo-dir)
        (delete-directory monorepo-dir t)))))

(ert-deftest claudemacs-test-projectile-nested-projects ()
  "Test projectile with nested project structure.

Directory structure:
outer/
├── setup.py
└── inner/
    ├── .projectile
    └── code/
        └── file.py"
  :tags '(:integration :projectile)
  (let ((outer-dir (make-temp-file "claudemacs-nested" t)))
    (unwind-protect
        (let ((inner-dir (expand-file-name "inner" outer-dir)))
          (claudemacs-test-create-directory-structure outer-dir
            '(("setup.py" . "from setuptools import setup")
              ("inner/" . nil)
              ("inner/.projectile" . "")
              ("inner/code/" . nil)
              ("inner/code/file.py" . "print('nested')")))
          
          (claudemacs-test-with-projectile-preference t
            (let ((test-file (expand-file-name "inner/code/file.py" outer-dir))
                  (default-directory (expand-file-name "inner/code" outer-dir)))
              (cl-letf (((symbol-function 'buffer-file-name) 
                         (lambda () test-file)))
                ;; Projectile should detect the nearest project marker
                (let ((projectile-root (projectile-project-root)))
                  (should (string= (file-name-as-directory (claudemacs--project-root))
                                   (file-name-as-directory projectile-root))))))))
      (when (file-exists-p outer-dir)
        (delete-directory outer-dir t)))))

(ert-deftest claudemacs-test-projectile-explicit-dir-parameter ()
  "Test that explicit DIR parameter works with projectile.

Directory structure:
project/
├── .projectile
└── subdir/
    └── file.txt"
  :tags '(:integration :projectile)
  (let ((project-dir (make-temp-file "claudemacs-explicit" t)))
    (unwind-protect
        (let ((subdir (expand-file-name "subdir" project-dir)))
          (claudemacs-test-create-directory-structure project-dir
            '((".projectile" . "")
              ("subdir/" . nil)
              ("subdir/file.txt" . "content")))
          
          (claudemacs-test-with-projectile-preference t
            ;; Test with explicit DIR parameter
            (let ((default-directory subdir))
              ;; When we pass a specific directory, projectile should detect from there
              (let ((projectile-root (projectile-project-root))
                    (our-result (claudemacs--project-root subdir)))
                (should (string= (file-name-as-directory our-result)
                                 (file-name-as-directory projectile-root)))))))
      (when (file-exists-p project-dir)
        (delete-directory project-dir t)))))

;;; Error Handling Tests (minimal mocking for unavoidable cases)

(ert-deftest claudemacs-test-projectile-function-not-available ()
  "Test fallback when projectile function is unavailable."
  :tags '(:integration :projectile)
  (let ((test-dir (make-temp-file "claudemacs-no-projectile" t)))
    (unwind-protect
        (progn
          (claudemacs-test-create-directory-structure test-dir
            '((".git/" . nil)
              ("file.txt" . "test content")))
          (claudemacs-test-init-git-repo test-dir)
          
          ;; Mock unavailable projectile function
          (cl-letf (((symbol-function 'projectile-project-root) nil))
            (claudemacs-test-with-projectile-preference t
              (let ((test-file (expand-file-name "file.txt" test-dir)))
                (cl-letf (((symbol-function 'buffer-file-name) 
                           (lambda () test-file)))
                  ;; Should fall back to git detection
                  (should (string= (file-name-as-directory (claudemacs--project-root))
                                   (file-name-as-directory test-dir))))))))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claudemacs-test-projectile-throws-error ()
  "Test graceful fallback when projectile throws an error."
  :tags '(:integration :projectile)
  (let ((test-dir (make-temp-file "claudemacs-projectile-error" t)))
    (unwind-protect
        (progn
          (claudemacs-test-create-directory-structure test-dir
            '((".git/" . nil)
              ("file.txt" . "test content")))
          (claudemacs-test-init-git-repo test-dir)
          
          ;; Mock projectile to throw an error
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (error "Projectile error"))))
            (claudemacs-test-with-projectile-preference t
              (let ((test-file (expand-file-name "file.txt" test-dir)))
                (cl-letf (((symbol-function 'buffer-file-name) 
                           (lambda () test-file)))
                  ;; Should gracefully fall back to git detection
                  (should (string= (file-name-as-directory (claudemacs--project-root))
                                   (file-name-as-directory test-dir))))))))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(provide 'claudemacs-projectile-integration-test)
;;; claudemacs-projectile-integration-test.el ends here
