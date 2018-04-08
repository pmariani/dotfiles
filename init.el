(set-language-environment "UTF-8")

(require 'cl-lib)
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap use-package
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
(show-paren-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 75))  ;; transparency
;; (add-to-list 'default-frame-alist '(alpha . (90 . 75)))  ;; transparency
(customize-set-variable 'confirm-kill-emacs 'yes-or-no-p)
(customize-set-variable 'sentence-end-double-space nil) ; Sentences end with one space
(customize-set-variable 'visible-bell t)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "s-w"))  ;; AppleKey-w: delete-frame
(global-unset-key (kbd "s-p"))  ;; AppleKey-p: ns-print-buffer
(global-unset-key (kbd "s-t"))  ;; AppleKey-t: ns-popup-font-panel
(global-unset-key (kbd "C-t"))  ;; Transpose chars

(global-set-key (kbd "C-x C-e") 'eval-print-last-sexp)

(set-fill-column 130)
(global-prettify-symbols-mode t)

(use-package dracula-theme
  :ensure t
  :config (set-cursor-color "magenta"))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ido
  :init (progn
          (customize-set-variable 'ido-enable-flex-matching t)
          (customize-set-variable 'ido-everywhere t))
  :config (ido-mode 1))

(use-package tramp
  :init (customize-set-variable 'tramp-default-method "ssh"))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package dired
  :init (progn
          (add-hook 'after-init-hook (lambda () (require 'dired-x)))
          (customize-set-variable 'dired-omit-files "^\\.?#\\|^\\.$\\|^__pycache__$")
          (setq dired-omit-mode t)))

(use-package json-mode
  :ensure t)

(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package paredit
  :ensure t
  :config (progn
            (autoload 'enable-paredit-mode "paredit")
            (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
            (add-hook 'emacs-lisp-mode-hook (lambda ()
                                              (customize-set-variable 'indent-tabs-mode nil)))))

(use-package mode-icons
  :ensure t
  :config (mode-icons-mode t))

(use-package smex
  :ensure t
  :config (progn
            (global-set-key (kbd "M-x") 'smex)
            (global-set-key (kbd "M-X") 'smex-major-mode-commands)
            ;; This is your old M-x.
            (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(use-package js2-mode
  :ensure t
  :config (progn
            (customize-set-variable 'js-indent-level 2)
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)))

(use-package nyan-mode
  :ensure t
  :config (progn (nyan-mode 1)
                 (customize-set-variable 'nyan-animate-nyancat t)
                 (customize-set-variable 'nyan-wavy-trail t)))

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode t))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

;; python:
;; brew install python3
;; python3.6 -m venv ~/.emacs.d/python_venv
;; source ~/.emacs.d/python_venv/bin/activate
;; pip install --upgrade pip
;; pip install jedi flake8 importmagic
;;
;; in ~/Code/.dir-locals.el:
;; ((python-mode (pyvenv-activate . "~/.emacs.d/python_venv")))

(use-package elpy
  :ensure t
  :init (progn
          (setq elpy-modules '(elpy-module-company
                               elpy-module-eldoc
                               elpy-module-highlight-indentation
                               elpy-module-pyvenv
                               elpy-module-flymake
                               elpy-module-sane-defaults)))

  :config (progn
            (add-hook 'elpy-mode-hook (lambda ()
                                        (pyvenv-mode t)
                                        (hs-minor-mode t)
                                        (font-lock-add-keywords nil
                                                                '(("\\<\\(\\(FIXME\\)\\|\\(TODO\\)\\|\\(NOTE\\)\\):" 1 font-lock-warning-face prepend)))))
            (customize-set-variable 'elpy-disable-backend-error-display nil)
            (elpy-enable)))

;; Org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(customize-set-variable 'org-default-notes-file "~/org/notes.org")
(customize-set-variable 'org-capture-templates '(
                                                 ("n" "Captured Note" entry (file+headline org-default-notes-file "Captured Notes") "")))
(customize-set-variable 'org-tags-exclude-from-inheritance '("crypt"))
(customize-set-variable 'safe-local-variable-values '((buffer-auto-save-file-name)))

;;;;;;;;;;
;; MU4E ;;
;;;;;;;;;;

;; brew install mu offlineimap

;; Update offlineimap to use python2 regardless of the current python interpreter in env.
;; setup ~/.authinfo for smtp credentials
;;
;; head `which offlineimap`
;; #!/bin/bash
;; PYTHONPATH="/usr/local/Cellar/offlineimap/7.1.5/libexec/vendor/lib/python2.7/site-packages" exec "/usr/local/Cellar/offlineimap/7.1.5/libexec/offlineimap.py" "$@"
;;
;; head /usr/local/Cellar/offlineimap/7.1.5/libexec/offlineimap.py
;; #!/usr/bin/python  # This points to python 2 whereas the old line uses the env python

;;; Helper functions
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(defun mac-open-perso (url &optional _)
  (start-process (concat "opening in firefox " url) nil "open" "-a" "Firefox" url))

(defun mac-open-chrome (url &optional _)
  (start-process (concat "opening in chrome" url) nil "open" "-a" "Google Chrome" url))

(defun unread-messages-query (account-definitions)
  (let* ((inboxes (mapcar (lambda (one-account) (concat "maildir:" (account-inbox one-account))) account-definitions))
         (joined-maildirs (mapconcat 'identity inboxes " OR ")))
    (concat "flag:unread AND (" joined-maildirs ")")))

(defun make-account-context (one-account)
  (let* ((context-name (account-name one-account)))
    (lexical-let ((email-address (account-email-address one-account))
                  (browser-func (account-browser-func one-account))
                  (inbox (account-inbox one-account)))
      (make-mu4e-context
       :name context-name
       :enter-func (lambda () (customize-set-variable 'browse-url-browser-function  browser-func))
       :match-func (lambda (message) (when message
                                       (string-prefix-p
                                        (concat "/" email-address "/")
                                        (mu4e-message-field message :maildir))))
       :vars   (list (cons 'mu4e-drafts-folder (account-drafts one-account))
                     (cons 'mu4e-refile-folder (account-refile one-account))
                     (cons 'mu4e-sent-folder   (account-sent one-account))
                     (cons 'mu4e-trash-folder  (account-trash one-account))
                     (cons 'smtpmail-smtp-user (account-email-address one-account))
                     (cons 'user-mail-address  (account-email-address one-account))
                     (cons 'mu4e-maildir-shortcuts `((,inbox . ?i))))))))

(defun make-bookmarks (account-definitions)
  `(,(make-mu4e-bookmark
      :name  "Unread messages"
      :query (unread-messages-query account-definitions)
      :key ?u)))

;; Structures
(cl-defstruct folder name maildir shortcut full-maildir)
(cl-defstruct account email-address type name inbox drafts refile sent trash browser-func)

(setq provider-to-folders '((:gmail .  ((:inbox . "/INBOX")
                                        (:draft . "/[Gmail].Drafts")
                                        (:archive . "/[Gmail].Archive")
                                        (:sent . "/[Gmail].Sent Mail")
                                        (:trash . "/[Gmail].Trash")))
                            (:yandex . ((:inbox . "/INBOX")
                                        (:draft . "/Drafts")
                                        (:archive . "/Archive")
                                        (:sent . "/Sent")
                                        (:trash . "/Trash")))))

(defun compute-account-definitions-fields (account-definitions)
  (seq-do (lambda (one-account)
            (let* ((email-address (account-email-address one-account))
                  (type (account-type one-account))
                  (folder-map (alist-get type provider-to-folders)))
              (progn
                (setf (account-inbox one-account) (concat "/" email-address (alist-get :inbox folder-map)))
                (setf (account-drafts one-account) (concat "/" email-address (alist-get :draft folder-map)))
                (setf (account-refile one-account) (concat "/" email-address (alist-get :archive folder-map)))
                (setf (account-sent one-account) (concat "/" email-address (alist-get :sent folder-map)))
                (setf (account-trash one-account) (concat "/" email-address (alist-get :trash folder-map))))))
          account-definitions))

(load-file  "~/.emacs.d/email-addresses.el")

;; Definitions
(setq *ACCOUNT-DEFINITIONS*
      (list (make-account :email-address (alist-get :perso email-addresses)
                          :type          :gmail
                          :name          "Personal"
                          :browser-func  'mac-open-perso)
            (make-account :email-address (alist-get :torrents email-addresses)
                          :type          :yandex
                          :name          "Torrents"
                          :browser-func  'mac-open-perso)
            (make-account :email-address (alist-get :work email-addresses)
                          :type          :gmail
                          :name          "Work"
                          :browser-func  'mac-open-chrome)))

(compute-account-definitions-fields *ACCOUNT-DEFINITIONS*)

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :config (progn
            (customize-set-variable 'message-kill-buffer-on-exit     t)
            (customize-set-variable 'mu4e-bookmarks                  (make-bookmarks *ACCOUNT-DEFINITIONS*))
            (customize-set-variable 'mu4e-contexts                   (mapcar #'make-account-context *ACCOUNT-DEFINITIONS*))
            (customize-set-variable 'mu4e-compose-dont-reply-to-self t)
            (customize-set-variable 'mu4e-user-mail-address-list     `(,(alist-get :perso email-addresses)
                                                                       ,(alist-get :torrents email-addresses)
                                                                       ,(alist-get :work email-addresses)))
            (customize-set-variable 'mu4e-get-mail-command           "true")
            (customize-set-variable 'mu4e-headers-include-related    t)
            (customize-set-variable 'mu4e-html2text-command          "/usr/local/bin/w3m -T text/html")
            (customize-set-variable 'mu4e-index-update-in-background t)
            (customize-set-variable 'mu4e-maildir                    "~/.Mail2")
            (customize-set-variable 'mu4e-sent-messages-behavior     'delete)
            (customize-set-variable 'mu4e-show-images                t)
            (customize-set-variable 'mu4e-update-interval            120)
            (customize-set-variable 'mu4e-headers-visible-lines      20)
            (customize-set-variable 'mu4e-use-fancy-chars            t)
            (customize-set-variable 'send-mail-function              'smtpmail-send-it)
            (customize-set-variable 'smtpmail-default-smtp-server    "smtp.gmail.com")
            (customize-set-variable 'smtpmail-smtp-server            "smtp.gmail.com")
            (customize-set-variable 'smtpmail-smtp-service           587)
            (customize-set-variable 'smtpmail-stream-type            'starttls)
            (customize-set-variable 'user-full-name                  "Pierre Mariani")
            (add-hook 'mu4e-compose-mode-hook (lambda () (auto-fill-mode -1)))
            (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init (progn
          (customize-set-variable 'mu4e-alert-interesting-mail-query (unread-messages-query *ACCOUNT-DEFINITIONS*))
          (mu4e-alert-set-default-style 'notifier)
          (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
          (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)))
