;;; package --- Summary
;;; Commentary:
;; C-u M-x align-regexp \(\s-*\)\s-
;; M-;   to add comment end of line
;; M-x occur is cool!
;; M-x highlight-phrase then unhighlight-xx
;; C-c C-/ comment-or-uncomment-region
;; C-x C-= to change font size
;; C-c . org mode insert date
;; <f1> open-init.el
;; <f2> hs-toggle-hiding
;; <f3> hs-hide-all
;; <f4> toggle-truncate-lines
;; <f5> start-debugging
;; <f6> my-whitespace-cleanup
;; <f12> imenu
;; M-m compile
;; In ibuffer use comma (',') to rotate through different sorting
;; shr-render-buffer to render HTML
;; M-x list-face-display and look for font-lock-X-face
;; hide-ifdef-toggle-shadowing
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

;; This is to benchmark startup time.
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Backup and Temporary files
(setq create-lockfiles nil)  ;; Prevent annoying elpy errors
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Custom file in separate location
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)
(setq gc-cons-threshold 100000000)

(column-number-mode t)
(show-paren-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Allows to switch buffers in case-insensitive fashion.
(setq read-buffer-completion-ignore-case t)
;; (desktop-save-mode 1)
;; (savehist-mode 1)
;; (setq-default global-visual-line-mode t)
(customize-set-variable 'confirm-kill-emacs 'yes-or-no-p)
(customize-set-variable 'sort-fold-case t)
(customize-set-variable 'sentence-end-double-space nil) ; Sentences end with one
                                                        ; space
(customize-set-variable 'visible-bell nil)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-t"))  ;; Transpose chars
(global-unset-key (kbd "<f1>"))

(add-to-list 'auto-mode-alist '("\\.razor\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.ts" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.shader" . c++-mode))
(setq js-jsx-detect-syntax nil)

(use-package dired
  :init (progn
          (add-hook 'after-init-hook (lambda () (require 'dired-x)))
          ;; (customize-set-variable 'dired-omit-files "^\\.?#\\|^\\.$\\|^__pycache__$")
          ;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
          ))

(setq whitespace-line-column 80)
(setq-default fill-column 80)
;; (global-display-fill-column-indicator-mode)
;; (global-whitespace-mode) ;; show whitespace characters
(setq sgml-basic-offset 2) ;; For HTML+ indentation
(setq js-indent-level 2)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(add-to-list 'default-frame-alist '(font . "consolas-11" ))
(set-face-attribute 'default t :font "consolas-11" )

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package whitespace-cleanup-mode
  :ensure t
  ;; Enable whitespace-cleanup-mode on save
  :config (global-whitespace-cleanup-mode t))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

(defun open-init.el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; (defun set-screen-size ()
;;   (interactive)
;;   (when window-system
;;     (set-frame-size (selected-frame) 150 40)
;;     (set-frame-position (selected-frame) 20 0)))

(defun my-whitespace-cleanup ()
  (interactive)
  (whitespace-cleanup)
  (message "cleaned whitespace.."))

(defun start-debugging ()
  (interactive)
  (message "start debugging..")
  (shell-command ".\\start-debugging"))

(defun pierre-compile ()
  (interactive)
  (let ((build-script-directory (locate-dominating-file buffer-file-name "build.bat")))
    (if (null build-script-directory) (message "%s: BUILD SCRIPT NOT FOUND! Starting from file %s" real-this-command buffer-file-name)
      (let ((my-compile-command (file-name-concat build-script-directory "build.bat")))
        (message "%s: COMPILING %s" real-this-command my-compile-command)
        (compile my-compile-command)))))

(use-package cc-mode :init (progn
                             (customize-set-variable 'c-default-style "bsd")
                             (global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
                             (global-set-key (kbd "<f1>") 'open-init.el)
                             (global-set-key (kbd "<f2>") 'hs-toggle-hiding)
                             (global-set-key (kbd "<f3>") 'hs-hide-all)
                             (global-set-key (kbd "<f4>") 'toggle-truncate-lines)
                             (global-set-key (kbd "<f5>") 'start-debugging)
                             (global-set-key (kbd "<f6>") 'my-whitespace-cleanup)
                             (global-set-key (kbd "<f12>") 'imenu)
                             (global-set-key (kbd "M-m") 'pierre-compile)))

(load-theme 'pierre)
(global-hl-line-mode 1)

;; (use-package autumn-light-theme :ensure t)
;; (use-package faff-theme :ensure t)
;; (use-package forest-blue-theme :ensure t)
;; (use-package material-theme :ensure t)
;; (use-package nofrils-acme-theme :ensure t)
;; (use-package poet-theme :ensure t)
;; (use-package reykjavik-theme :ensure t)
;; (use-package subatomic-theme :ensure t)
;; (use-package toxi-theme :ensure t)
;; (use-package zweilight-theme :ensure t)

;; (use-package sublime-themes :ensure t)
;;   :init (progn
;;           ;; (load-theme 'junio)
;;           ;; (load-theme 'fogus)
;;           ;; (load-theme 'manoj-dark) (global-hl-line-mode 0)
;;           ;; (load-theme 'granger)
;;           ;; (load-theme 'wombat)
;;           ;; (set-cursor-color "violet")
;;           ))

(defun highlight-todos ()
   (font-lock-add-keywords nil
                           '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                             ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                             ("\\<\\(DIRTY\\):" 1 font-lock-warning-face t)                             
                             ("\\<\\(HACK\\):" 1 font-lock-warning-face t)
                             ("\\<\\(BUG\\):" 1 font-lock-warning-face t)
                             ("\\<\\(NOTE\\):" 1 font-lock-doc-face t)
                             )))
(add-hook 'prog-mode-hook 'highlight-todos)
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode)))
(add-hook 'prog-mode-hook (lambda () (hide-ifdef-mode)))

;; The original value is "\f\\|[      ]*$", so we add the bullets (-), (+), and (*).
;; There is no need for "^" as the regexp is matched at the beginning of line.
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

(setq hs-allow-nesting t)

(server-start)

(provide '.emacs)
;;; .emacs ends here
