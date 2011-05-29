;; fix mac os x path
(setq exec-path (append exec-path '("/usr/local/bin/")))

(setq indent-tabs-mode nil)

(setq c-basic-offset 2)
(setq tab-width 2)

(add-to-list 'load-path "~/.emacs.d")
(require 'android-mode)

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(show-paren-mode 1)
(global-hl-line-mode -1)
(menu-bar-mode -1)
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

(setq tramp-default-method "ssh")
(setq auto-save-default nil)

(if (file-exists-p "~/.emacs.d/php-mode.el")
    (load-file "~/.emacs.d/php-mode.el"))


(if (file-exists-p "~/.emacs.d/javascript.el")
    (load-file "~/.emacs.d/javascript.el"))

(toggle-truncate-lines nil)

(setenv "PATH"
	(concat "/usr/local/git/bin:~/Code/Android/sdk/tools:~/Code/Android/sdk/platform-tools/:/opt/local/bin/:"
		(getenv "PATH")))


(setenv "SBCL_HOME" '"~/LISP-SBCL/lib/sbcl")

(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime/")
(setq inferior-lisp-program "~/LISP-SBCL/bin/sbcl")
(setq slime-lisp-implementations `((sbcl ("~/LISP-SBCL/bin/sbcl"))))
(require 'slime)
;(require 'slime-editing-commands)
(slime-setup '(slime-repl slime-asdf slime-fancy slime-banner))


(setq load-path (cons "~/.emacs.d/org-7.5/lisp" load-path))
(setq load-path (cons "~/.emacs.d/org-7.5/contrib/lisp" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(org-babel-do-load-languages 'org-babel-load-languages '(
                                                         (lisp . t)
                                                         (emacs-lisp . t)))