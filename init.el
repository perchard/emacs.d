;; store anything added via the customize interface in a separate file instead of polluting this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; add lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; machinery for installing required packages, borrowed from steve purcell
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el
(require 'init-elpa)

;; ripgrep (for projectile)
(require-package 'ripgrep)

;; project interaction library
(require-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; minibuffer completion
(require-package 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

;; prompt available keybinding completions
(require-package 'which-key)
(which-key-mode)

;; major mode for editing jsx files (React)
(require-package 'rjsx-mode)

;; mimics the effect of fill-column in visual-line-mode
(require-package 'visual-fill-column)
;; turn on visual-fill-column-mode when visual-line-mode is turned on
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
;; center in frame
(setq visual-fill-column-center-text t)

;; hide tool bar, scroll bar, and menu bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; transparent titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; set default font
(set-frame-font "Operator Mono 18")

;; don't display the splash screen (scratch will be displayed instead)
(setq inhibit-splash-screen t)

;; turn off audible bell
(setq ring-bell-function 'ignore)

;; use ibuffer instead of list-buffers
(define-key global-map [remap list-buffers] 'ibuffer)
;; group ibuffer buffers by git repo
(require-package 'ibuffer-vc)
(add-hook 'ibuffer-hook
  (lambda ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

;; save state between sessions
(desktop-save-mode 1)

;; auto close brackets
(electric-pair-mode 1)

;; indent css files with 2 spaces
(setq css-indent-offset 2)

;; set defaults
(setq-default
  indent-tabs-mode nil ;; spaces instead of tabs
  make-backup-files nil) ;; don't make backup files (they end with a tilde, ~)
