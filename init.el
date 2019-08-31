;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; store anything added via the customize interface in a separate file instead of polluting this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; add lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; machinery for installing required packages, borrowed from steve purcell
(require 'init-elpa)

;; adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; cursor
(setq-default cursor-type '(bar . 4))
(add-to-list 'default-frame-alist '(cursor-color . "#20BBFC"))

;; theme
(require-package 'doom-themes)
(load-theme 'doom-one-light t)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

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

;; typography
(set-frame-font "Operator Mono 18")
(setq-default line-spacing 8)
(global-prettify-symbols-mode 1)
(setq-default fill-column 80)

;; wrap and center text in text (including org) files
(require-package 'visual-fill-column)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(setq visual-fill-column-center-text t)

;; markdown support
(require-package 'markdown-mode)
(setq markdown-enable-wiki-links 1)
(setq markdown-link-space-sub-char " ")

;; browse/filter/edit directories of plan text (inspired by Notational Velocity)
(require-package 'deft)
(setq deft-extensions '("md" "txt" "org"))
(setq deft-use-filename-as-title t)
(setq deft-strip-summary-regexp
  (concat "\\(^Tags:.*$"
          "\\|^Title:.*$"
          "\\|^\\(?:^\\|[^\\]\\)\\(\\[\\[\\([^]|]+\\)\\(|\\([^]]+\\)\\)?\\]\\]\\)$" ;; [[WikiLinks]]
          "\\)"))
(setq deft-directory "~/Dropbox/zettelkasten")
(global-set-key [f8] 'deft)

;; hide tool bar, scroll bar, and menu bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; transparent titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; don't display the splash screen (scratch will be displayed instead)
(setq inhibit-startup-screen t)

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
  indent-tabs-mode nil
  make-backup-files nil)
