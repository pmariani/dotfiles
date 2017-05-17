;; Start the daemon with /Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9 --daemon

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Backup and Temporary files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Custom file in separate location
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

(column-number-mode t)
(desktop-save-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-cursor-color "magenta")
(setq confirm-kill-emacs 'yes-or-no-p)
(setq sentence-end-double-space nil) ; Sentences end with one space
(setq visible-bell t)
(show-paren-mode 1)
(tool-bar-mode -1)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "s-w"))  ;; AppleKey-w: delete-frame
(global-unset-key (kbd "s-p"))  ;; AppleKey-p: ns-print-buffer
(global-unset-key (kbd "s-t"))  ;; AppleKey-t: ns-popup-font-panel
(global-unset-key (kbd "C-t"))  ;; Transpose chars

;; Transparency
;; (add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ido
  :init (setq ido-enable-flex-matching t) (setq ido-everywhere t)
  :config (ido-mode 1))

(use-package tramp
  :init (setq tramp-default-method "ssh"))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

(use-package flycheck
  :ensure t)

(use-package flycheck-pyflakes
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package zenburn-theme
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package dired
  :init
  (add-hook 'after-init-hook '(lambda () (require 'dired-x)))
  (customize-set-variable 'dired-omit-files "^\\.?#\\|^\\.$\\|^__pycache__$")
  (setq dired-omit-mode t))

(use-package json-mode
  :ensure t)

;; python:
;; brew install python3
;; virtualenv --python python3 ~/.emacs.d/python_venv
;; source ~/.emacs.d/python_venv/bin/activate
;; pip install --upgrade pip
;; pip install jedi flake8 importmagic
(use-package elpy
  :ensure t
  :init
  (setq elpy-modules '(elpy-module-company
		       elpy-module-eldoc
		       elpy-module-highlight-indentation
		       elpy-module-pyvenv
		       elpy-module-flymake
		       elpy-module-sane-defaults))
  (pyvenv-activate "~/.emacs.d/python_venv")
  
  :config
  (add-hook 'elpy-mode-hook (lambda ()
			      (hs-minor-mode t)
			      (set-fill-column 100)))
  (elpy-enable))
(customize-set-variable 'elpy-disable-backend-error-display nil)


(defun copy-region-to-os-pasteboard ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "pbcopy"))

(global-set-key (kbd "<f8>") 'copy-region-to-os-pasteboard)

;; Make Tramp use the PATH of the remote user
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; History of input
(defvar history-of-vagrant-directories nil)
(eval-after-load "savehist"
  '(add-to-list 'savehist-additional-variables 'history-of-vagrant-directories))

;; Function to run tox
(defun run-tox-on-dir (directory-name)
  (interactive
   (list (read-from-minibuffer "run tox on " (car history-of-vagrant-directories) nil nil 'history-of-vagrant-directories)))
  (let ((default-directory (concat "/ssh:vagrant:/vagrant/" directory-name)))
    (shell-command "tox &")))

;; Keybinding for tox
(global-set-key (kbd "C-t") 'run-tox-on-dir)

(server-start)

;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(customize-set-variable 'org-default-notes-file "~/org/notes.org")
(customize-set-variable 'org-capture-templates
			'(
			  ("n" "Captured Note" entry (file+headline org-default-notes-file "Captured Notes") "")))
(customize-save-variable 'epg-gpg-program "/usr/local/bin/gpg2") ;; add pinentry-mode loopback to ~/.gnupg/gpg.conf
(customize-set-variable 'org-crypt-key "")
(customize-set-variable 'org-tags-exclude-from-inheritance '("crypt"))

(use-package org-crypt
  :config (org-crypt-use-before-save-magic))

(customize-set-variable 'safe-local-variable-values '((buffer-auto-save-file-name)))
