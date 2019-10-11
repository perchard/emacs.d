;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; store anything added via the customize interface in a separate file to avoid polluting this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set location (used by theme-changer)
(setq calendar-latitude 37.8716)
(setq calendar-longitude -122.273)
(setq calendar-location-name "Berkeley, CA")

;; add lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; machinery for installing required packages, borrowed from steve purcell
(require 'init-elpa)

;; set up $PATH
(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; cursor
(setq-default cursor-type '(bar . 4))
(add-to-list 'default-frame-alist '(cursor-color . "#20BBFC"))

;; set window (frame) startup size
(setq default-frame-alist '((left . -1) (width . 100) (fullscreen . fullheight)))

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
(add-to-list 'default-frame-alist '(font . "Operator Mono 18"))
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
(setq markdown-indent-on-enter 'indent-and-new-item)
(load "~/.emacs.d/site-lisp/adaptive-wrap-vp.el")
(add-hook 'markdown-mode-hook #'adaptive-wrap-prefix-vp-mode)

;; spell check
(add-hook 'text-mode-hook #'flyspell-mode)

;; sentences end with a single space
(setq sentence-end-double-space nil)

;; window management
(windmove-default-keybindings 'super)
(add-hook 'after-init-hook 'winner-mode)

;; browse/filter/edit directories of plain text (inspired by notational velocity)
(require-package 'deft)
(setq deft-extensions '("md" "txt" "org"))
(setq deft-use-filename-as-title t)
(setq deft-strip-summary-regexp
  (concat "\\(^Tags:.*$"
          "\\|^Title:.*$"
          "\\|^\\(?:^\\|[^\\]\\)\\(\\[\\[\\([^]|]+\\)\\(|\\([^]]+\\)\\)?\\]\\]\\)$" ;; [[WikiLinks]]
          "\\)"))
(setq deft-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Zettelkasten")
(global-set-key [f8] 'deft)

;; org mode setup
(define-key global-map (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-agenda-files
      '("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/inbox.org"
        "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/projects.org"
        "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/work.org"
        "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/consulting.org"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-capture-templates
      '(("t"
         "Todo [inbox]"
         entry
         (file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/inbox.org")
         "* TODO %?"
         :prepend t)))

;; clojure
(require-package 'clojure-mode)
(require-package 'cider)

;; hide tool bar, scroll bar, and menu bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; transparent titlebar
(require-package 'ns-auto-titlebar)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; set theme based on time
(require-package 'theme-changer)
(require 'theme-changer)
(require-package 'doom-themes)
(change-theme 'doom-one-light 'doom-one)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

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
(setq-default ibuffer-show-empty-filter-groups nil)

;; auto close brackets
(electric-pair-mode 1)

;; set defaults
(setq-default
  indent-tabs-mode nil
  make-backup-files nil)

(provide 'init)

;;; init.el ends here
