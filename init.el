;;; package --- Summary

;;; Commentary:

;;; Code:
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
(setq create-lockfiles nil)  ;; Prevent annoying elpy errors
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
(setq-default global-visual-line-mode t)
(set-frame-parameter (selected-frame) 'alpha '(95 . 75))  ;; transparency
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))  ;; transparency
(customize-set-variable 'confirm-kill-emacs 'yes-or-no-p)
(customize-set-variable 'sort-fold-case t)
(customize-set-variable 'sentence-end-double-space nil) ; Sentences end with one space
(customize-set-variable 'visible-bell nil)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "s-w"))  ;; AppleKey-w: delete-frame
(global-unset-key (kbd "s-p"))  ;; AppleKey-p: ns-print-buffer
(global-unset-key (kbd "s-t"))  ;; AppleKey-t: ns-popup-font-panel
(global-unset-key (kbd "C-t"))  ;; Transpose chars
(set-face-attribute 'default nil :font "Monaco-11")

(set-fill-column 130)
(global-prettify-symbols-mode t)

(font-lock-add-keywords nil '(("\\<\\(\\(FIXME\\)\\|\\(TODO\\)\\|\\(NOTE\\)\\):" 1 font-lock-warning-face prepend)))

(use-package rjsx-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package web-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(eval-and-compile
  (defvar-local beautiful-themes '(dracula-theme
                                   creamsody-theme
                                   warm-night-theme
                                   purple-haze-theme
                                   ample-theme
                                   darktooth-theme))

  (defvar-local selected-theme (nth
                                (random (length beautiful-themes))
                              beautiful-themes))
  (message (concat "Selecting " (symbol-name selected-theme)))

  (defmacro enable-selected-theme ()
    "Yeah yeah."
    `(use-package ,selected-theme
       :ensure t)))

(enable-selected-theme)

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
  :ensure t
  :config
  (add-hook 'markdown-mode-hook (lambda () (set-fill-column 100))))

(use-package yaml-mode
  :ensure t)

(use-package company
  :ensure t
  :config (global-company-mode))

(use-package flycheck
  :ensure t
  :config (progn
            (customize-set-variable 'flycheck-emacs-lisp-load-path 'inherit)
            (global-flycheck-mode)))

(use-package flycheck-cask
  :ensure t
  :config (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (progn
            (setq magit-commit-show-diff nil)
            (put 'magit-clean 'disabled nil)
            (add-hook 'magit-status-sections-hook 'magit-insert-worktrees)))

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package dired
  :init (progn
          (add-hook 'after-init-hook (lambda () (require 'dired-x)))
          (customize-set-variable 'dired-omit-files "^\\.?#\\|^\\.$\\|^__pycache__$")
          (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))))

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

'(use-package nyan-mode
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

'(use-package moody
  :ensure t
  :config (progn
            (setq x-underline-at-descent-line t)
            (moody-replace-mode-line-buffer-identification)
            (moody-replace-vc-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package org
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb))
  :config (progn
            (org-babel-do-load-languages 'org-babel-load-languages
                                         '((shell . t)))
            (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "WON'T-DO")))
            (customize-set-variable 'org-default-notes-file "~/org/notes.org")
            (customize-set-variable 'org-capture-templates '(("n"
                                                              "Captured Note"
                                                              entry
                                                              (file+headline org-default-notes-file "Captured Notes")
                                                              "")))))

(server-start)

(provide 'init)
;;; init.el ends here
