;; highlight current line
(global-hl-line-mode 1)

;; show matching paren
(show-paren-mode 1)

;; fix delete key on mac os x
(normal-erase-is-backspace-mode 1)

;; exec path on mac os x
(add-to-list 'exec-path "/usr/local/bin/")

;; do not truncate long lines
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

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(if (file-exists-p "~/.emacs.d/php-mode.el")
    (load-file "~/.emacs.d/php-mode.el"))

(if (file-exists-p "~/.emacs.d/javascript.el")
    (load-file "~/.emacs.d/javascript.el"))

(if (file-exists-p "~/.emacs.d/android-mode.el")
    (load-file "~/.emacs.d/android-mode.el"))

;; (setenv "PATH" (concat
;; 	"/usr/local/git/bin:~/Code/Android/sdk/tools:~/Code/Android/sdk/platform-tools/:/opt/local/bin/:"
;; 	(getenv "PATH")))

;; (setenv "SBCL_HOME" '"~/LISP-SBCL/lib/sbcl")

;; (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime/")
;; local common lisp implementation
;; (setq inferior-lisp-program "/opt/local/bin/sbcl")
;; (setq inferior-lisp-program "~/LISP-SBCL/bin/sbcl")
;; (setq slime-lisp-implementations `((sbcl ("~/LISP-SBCL/bin/sbcl"))))
;(require 'slime)
;(require 'slime-editing-commands)
;(slime-setup '(slime-repl slime-asdf slime-fancy slime-banner))


;; (setq load-path (cons "~/.emacs.d/org-7.5/lisp" load-path))
;; (setq load-path (cons "~/.emacs.d/org-7.5/contrib/lisp" load-path))
;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; (org-babel-do-load-languages 'org-babel-load-languages '(
;;                                                          (lisp . t)
;;                                                          (emacs-lisp . t)))

;; org-mode config
;;; add timestamp when item is marked 'done'
(setq org-log-done 'time)
