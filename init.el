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

;; M-m compile
;; In ibuffer use comma (',') to rotate through different sorting
;; shr-render-buffer to render HTML
;; M-x list-face-display and look for font-lock-X-face
;; hide-ifdef-toggle-shadowing
;; now I can grep, maybe find-grep, etc...
;; find-named-dired to find files in directory tree, if `find` is installed

;;;; TAGS

;; eshell, then  find . -regextype posix-extended -type f -regex ".*\.(c|cpp|h)" | c:/"Program Files"/Emacs/emacs-28.2/bin/etags.exe --declarations -
;; Then M-. to find with XREF, M-, to come back
;; M-? to find references
;; (tags-reset-tags-table) when needing to load a new table (across restarts) for a new codebase for example.

;; To use regexes in Emacs, look up Rx

;;;; ORG

;; Allows things like this in org mode
;; #+ATTR_ORG: :width 200
;; [[./images/typesofdevs.png]]
;;
;; Also add this anywhere in the buffer
;; #+STARTUP: inlineimages

;; C-c C-o to open link at point

;;;; Debugging elisp
;; Been using edebug

;;;; Debugging C
;; installing gdb with msys64 then pacman -s gdb
;; then M-x gdb
;; NOt working, don't want to debug


;;;; BOOKMARKS and REGISTERS
;; bookmarks: use to navigate to locations quickly across reboots
;; C-x r l to list
;; C-x r m <ret> to create

;; registers: use to copy paste same value multiple times (kills overwrite last copied)
;;; positions
;; C-x r <SPC> R to add value of point in R
;; C-x r j     R to jump to register R
;;; text content
;; C-x r s R Copy region into register R (‘copy-to-register’).
;; C-x r i R Insert text from register R (‘insert-register’).


(set-language-environment "UTF-8")

(setq path-to-msys64-bins "s:\\msys64\\usr\\bin")
(setenv "PATH" (format "%s;%s" path-to-msys64-bins (getenv "PATH")))
;; There should be no need to run this as exec-path would be set from PATH, but can validate with (mapc 'print exec-path)
;; Result: didn't work, setting explicitly.
(add-to-list 'exec-path path-to-msys64-bins)

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

(customize-set-variable 'package-gnupghome-dir "/c/Users/Pierre/AppData/Roaming/.emacs.d/elpa/gnupg")

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

(defun my-whitespace-cleanup ()
  (interactive)
  (whitespace-cleanup)
  (message "cleaned whitespace.."))

(defun pierre-locate-script (script-name fn)
  (interactive)
  (let ((script-directory (locate-dominating-file buffer-file-name script-name)))
    (if (null script-directory) (message "SCRIPT NOT FOUND! Starting from file %s" buffer-file-name)
      (let ((full-script-path (file-name-concat script-directory script-name)))
        (message "RUNNING %s %s" fn full-script-path)
        (funcall fn full-script-path)))))

(use-package cc-mode :init (progn
                             (customize-set-variable 'c-default-style "bsd")
                             (global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
                             (global-set-key (kbd "<f1>") 'open-init.el)
                             (global-set-key (kbd "<f2>") 'hs-toggle-hiding)
                             (global-set-key (kbd "<f3>") 'hs-hide-all)
                             (global-set-key (kbd "<f4>") 'toggle-truncate-lines)
                             (global-set-key (kbd "<f5>") (lambda () (interactive)(pierre-locate-script "start-debugging.bat" 'shell-command)))
                             (global-set-key (kbd "<f6>") 'my-whitespace-cleanup)
                             (global-set-key (kbd "M-m")  (lambda () (interactive)(pierre-locate-script "build.bat"           'compile)))))

(add-hook 'prog-mode-hook (lambda () (hs-minor-mode)))
(add-hook 'prog-mode-hook (lambda () (hide-ifdef-mode)))

;;;;
;; Theme and display related
;;;;

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

(add-to-list 'default-frame-alist '(font . "consolas-11" ))
(set-face-attribute 'default t :font "consolas-11" )

;; (defun set-screen-size ()
;;   (interactive)
;;   (when window-system
;;     (set-frame-size (selected-frame) 150 40)
;;     (set-frame-position (selected-frame) 20 0)))

(use-package autumn-light-theme :ensure t)
(use-package faff-theme :ensure t)
(use-package forest-blue-theme :ensure t)
(use-package material-theme :ensure t)
(use-package nofrils-acme-theme :ensure t)
(use-package poet-theme :ensure t)
(use-package reykjavik-theme :ensure t)
(use-package subatomic-theme :ensure t)
(use-package toxi-theme :ensure t)
(use-package zweilight-theme :ensure t)
(use-package sublime-themes :ensure t)

(defun theme-switcher ()
  (setq-local pierre-current-theme-index -1)
  (setq-local pierre-theme-candidates (custom-available-themes))
  (defun quick-try-theme (offset)
    (let* ((current-theme (nth pierre-current-theme-index pierre-theme-candidates))
           (new-index (min (max (+ pierre-current-theme-index offset) 0) (- (length pierre-theme-candidates) 1)))
           (new-theme (nth new-index pierre-theme-candidates)))
      (progn
        (disable-theme current-theme)
        (load-theme new-theme t nil)
        (setq pierre-current-theme-index new-index)
        (message "NEW THEME %s" new-theme))))

  (local-set-key (kbd "<f1>") (lambda () (interactive)(quick-try-theme -1)))
  (local-set-key (kbd "<f2>") (lambda () (interactive)(quick-try-theme 1)))
  (message "READY TO TRY SOME THEMES"))

;; Eval this to try it out (no need to uncomment)
;; (theme-switcher)

;; (load-theme 'poet-dark t nil)
;; (load-theme 'subatomic t nil)
;; (load-theme 'graham t nil)
;; (load-theme 'granger t nil)
;; (load-theme 'toxi t nil) (global-hl-line-mode 1)
(load-theme 'poet-dark t nil) (global-hl-line-mode 1);; Does not support resizing very well


;; (load-theme 'modus-vivendi t nil)
;; (load-theme 'junio)
;; (load-theme 'fogus)
;; (load-theme 'manoj-dark) (global-hl-line-mode 0)
;; (load-theme 'granger)
;; (load-theme 'wombat)
;; (load-theme 'pierre) (global-hl-line-mode 1)
;; (set-cursor-color "violet")
;; (global-hl-line-mode 0)
;; (global-hl-line-mode 1)

;; These were set for the pierre theme, I think
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(dired-directory ((t (:foreground "MediumSeaGreen"))))
;;  '(font-lock-comment-face ((t (:foreground "PaleGreen4" :slant italic))))
;;  '(font-lock-variable-name-face ((t (:foreground "#699978"))))
;;  '(hl-line ((t (:background "#003523" :extend t))))

;;;;
;; End of theme
;;;;


;;; MARK
(defface mmv-face
  '((t :background "maroon2" :foreground "white"))
  "Face used for showing the mark's position.")

(defvar-local mmv-mark-overlay nil
  "The overlay for showing the mark's position.")

(defvar-local mmv-is-mark-visible t
  "The overlay is visible only when this variable's value is t.")

(defun mmv-draw-mark (&rest _)
  "Make the mark's position stand out by means of a one-character-long overlay.
   If the value of variable `mmv-is-mark-visible' is nil, the mark will be
   invisible."
  (unless mmv-mark-overlay
    (setq mmv-mark-overlay (make-overlay 0 0 nil t))
    (overlay-put mmv-mark-overlay 'face 'mmv-face))
  (let ((mark-position (mark t)))
    (cond
     ((null mark-position) (delete-overlay mmv-mark-overlay))
     ((and (< mark-position (point-max))
           (not (eq ?\n (char-after mark-position))))
      (overlay-put mmv-mark-overlay 'after-string nil)
      (move-overlay mmv-mark-overlay mark-position (1+ mark-position)))
     (t
      ; This branch is called when the mark is at the end of a line or at the
      ; end of the buffer. We use a bit of trickery to avoid the higlight
      ; extending from the mark all the way to the right end of the frame.
      (overlay-put mmv-mark-overlay 'after-string
                   (propertize " " 'face (overlay-get mmv-mark-overlay 'face)))
      (move-overlay mmv-mark-overlay mark-position mark-position)))))

(add-hook 'pre-redisplay-functions #'mmv-draw-mark)

(defun mmv-toggle-mark-visibility ()
  "Toggles the mark's visiblity and redraws it (whether invisible or visible)."
  (interactive)
  (setq mmv-is-mark-visible (not mmv-is-mark-visible))
  (if mmv-is-mark-visible
      (set-face-attribute 'mmv-face nil :background "maroon2" :foreground "white")
    (set-face-attribute 'mmv-face nil :background 'unspecified :foreground 'unspecified))
  (mmv-draw-mark))

;;;; END OF MARK



;; The original value is "\f\\|[      ]*$", so we add the bullets (-), (+), and (*).
;; There is no need for "^" as the regexp is matched at the beginning of line.
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

(setq hs-allow-nesting t)

(setq org-image-actual-width nil)

(server-start)

(toggle-frame-fullscreen)

(provide '.emacs)
;;; .emacs ends here
