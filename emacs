(setq column-number-mode t)
(set-cursor-color "#f88")
(show-paren-mode 1)
(toggle-truncate-lines nil)

;; code conventions
(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)
(setq-default c-basic-offset 3)

;; use ssh with tramp mode
(setq tramp-default-method "ssh")

;; auto-save locally with tramp
(setq tramp-auto-save-directory "~/emacs-tramp-autosaves")

;;;; load libs
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/emacs-nav")
(require 'nav)

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(if (file-exists-p "~/.emacs.d/php-mode.el")
    (load-file "~/.emacs.d/php-mode.el"))

(if (file-exists-p "~/.emacs.d/javascript.el")
    (load-file "~/.emacs.d/javascript.el"))

(if (file-exists-p "~/.emacs.d/android-mode.el")
    (load-file "~/.emacs.d/android-mode.el"))

(if (file-exists-p "~/.emacs.d/paredit.el")
    (load-file "~/.emacs.d/paredit.el"))

(if (file-exists-p "~/.emacs.d/highlight-parentheses.el")
    (load-file "~/.emacs.d/highlight-parentheses.el"))

(setq org-log-done 'time)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun hs-on ()
  (hs-minor-mode t))

(add-hook 'js-mode-hook 'hs-on)

(add-hook 'php-mode-hook 'hs-on)
(add-hook 'php-mode-hook 'turn-on-auto-fill)

(add-hook 'clojure-mode-hook 'hs-on)
(add-hook 'clojure-mode-hook 'turn-on-auto-fill)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)

(require 'whitespace)
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(global-whitespace-mode 1)
(setq whitespace-line nil)
(setq whitespace-style (delq 'newline-mark whitespace-style))

(require 'clojure-mode)
(tool-bar-mode 0)

(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)
