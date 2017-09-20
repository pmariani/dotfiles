(set-language-environment "UTF-8")
(customize-set-variable 'exec-path (append exec-path '("/usr/local/bin")))
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


;;;;;;;;;;
;; MU4E ;;
;;;;;;;;;;
;; Had to manually edit offlineimap to force it to use python2.7

;;; Helper functions
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(defun get-email-address (one-account-definition)
  (cdr (assoc :email-address one-account-definition)))

(defun get-context-name (one-account-definition)
  (cdr (assoc :name one-account-definition)))

(defun get-folders (one-account-definition)
  (cdr (assoc :folders one-account-definition)))

(defun get-maildir (folder)
  (cdr (assoc :maildir folder)))

(defun get-name (folder)
  (cdr (assoc :name folder)))

(defun get-shortcut (folder)
  (cdr (assoc :shortcut folder)))

(defun build-maildir-clause (email-address maildir)
  (concat "maildir:/" email-address maildir))

(defun unread-messages-query (account-definitions)
  (let* ((email-addresses (mapcar #'get-email-address account-definitions))
	 (maildirs (mapcar (lambda (email-address) (build-maildir-clause email-address "/INBOX")) email-addresses))
	 (joined-maildirs (mapconcat 'identity maildirs " OR ")))
    (concat "flag:unread AND (" joined-maildirs ")")))

(defun ~make-context-maildir-shortcuts (folders email-address)
  (let* ((inbox-shortcut `(,(concat "/" email-address "/INBOX") . ?i))
	 (dynamic-shortcuts (mapcar (lambda (folder)
				      `(,(concat "/" email-address (get-maildir folder)) . ,(get-shortcut folder)))
				    folders)))
    (append `(,inbox-shortcut)
	    dynamic-shortcuts)))

(defun ~make-context-vars (folders email-address)
  `((mu4e-drafts-folder . ,(concat "/" email-address "/[Gmail].Drafts"))
    (mu4e-refile-folder . ,(concat "/" email-address "/[Gmail].Archive"))
    (mu4e-sent-folder   . ,(concat "/" email-address "/[Gmail].Sent Mail"))
    (mu4e-trash-folder  . ,(concat "/" email-address "/[Gmail].Trash"))
    (smtpmail-smtp-user . ,email-address)
    (user-mail-address  . ,email-address)
    (mu4e-maildir-shortcuts . ,(~make-context-maildir-shortcuts folders email-address))))

(defun ~make-context (one-account-definition)
  (let* ((context-name (get-context-name one-account-definition))
	 (folders (get-folders one-account-definition)))
    (lexical-let ((email-address (get-email-address one-account-definition)))
      (make-mu4e-context
       :name context-name
       :match-func (lambda (message) (when message
				       (string-prefix-p
					(concat "/" email-address "/")
					(mu4e-message-field message :maildir))))
       :vars (~make-context-vars folders email-address)))))

(defun make-contexts (account-definitions)
  (mapcar #'~make-context account-definitions))

(defun ~make-bookmark-for-folder (folder email-address)
  (make-mu4e-bookmark
   :name (get-name folder)
   :query (build-maildir-clause email-address (get-maildir folder))
   :key (get-shortcut folder)))

(defun ~make-bookmarks-for-account (one-account-definition)
  (let ((email-address (get-email-address one-account-definition))
	(folders (get-folders one-account-definition)))
    (mapcar (lambda (folder) (~make-bookmark-for-folder folder email-address))
	    folders)))

(defun make-bookmarks (account-definitions)
  (let* ((unread-messages-bookmark (make-mu4e-bookmark
				    :name  "Unread messages"
				    :query (unread-messages-query account-definitions)
				    :key ?u))
	 (nested-bookmarks (mapcar #'~make-bookmarks-for-account account-definitions))
	 (bookmarks (apply #'append nested-bookmarks)))
    (append `(,unread-messages-bookmark) bookmarks)))

;;; Definitions
(setq *ACCOUNT-DEFINITIONS*
      '(((:email-address . "foo")
	 (:name . "Personal")
	 (:folders . (((:name . "BJJ log")
		       (:maildir . "/[Gmail].bjj_log")
		       (:shortcut . ?b))
		      ((:name . "Workout log")
		       (:maildir . "/[Gmail].workout_log")
		       (:shortcut . ?w))
		      ((:name . "House remodel")
		       (:maildir . "/[Gmail].house_remodel")
		       (:shortcut . ?h)))))
	((:email-address . "bar")
	 (:name . "Work")
	 (:folders . (((:name . "SOC 2")
		       (:maildir . "/[Gmail].soc")
		       (:shortcut . ?s))
		      ((:name . "Usage Service")
		       (:maildir . "/[Gmail].usage_service")
		       (:shortcut . ?U)))))))

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :config
  (customize-set-variable 'mu4e-maildir                 "~/.Mail")
  (customize-set-variable 'mu4e-get-mail-command        "/usr/local/bin/offlineimap")
  (customize-set-variable 'mu4e-sent-messages-behavior 'delete)
  (customize-set-variable 'mu4e-show-images             t)
  (customize-set-variable 'mu4e-update-interval         300)
  (customize-set-variable 'smtpmail-default-smtp-server "smtp.gmail.com")
  (customize-set-variable 'smtpmail-smtp-server         "smtp.gmail.com")
  (customize-set-variable 'user-full-name               "Pierre Mariani")
  (customize-set-variable 'smtpmail-smtp-service        587)
  (customize-set-variable 'smtpmail-stream-type         'starttls)
  (customize-set-variable 'message-kill-buffer-on-exit  t)
  (customize-set-variable 'mu4e-bookmarks (make-bookmarks *ACCOUNT-DEFINITIONS*))
  (customize-set-variable 'mu4e-contexts (make-contexts *ACCOUNT-DEFINITIONS*))
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (customize-set-variable 'mu4e-alert-interesting-mail-query (unread-messages-query *ACCOUNT-DEFINITIONS*))
  (mu4e-alert-set-default-style 'notifier)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
