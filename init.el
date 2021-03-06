;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; store anything added via the customize interface in a separate file to avoid polluting this one
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set location (used by theme-changer)
(setq calendar-latitude 37.8716)
(setq calendar-longitude -122.273)
(setq calendar-location-name "Berkeley, CA")

;; add lisp/ and site-lisp/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

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
(which-key-setup-side-window-right)

;; typography
(add-to-list 'default-frame-alist '(font . "Operator Mono 16"))
(setq-default line-spacing 8)
(global-prettify-symbols-mode 1)
(setq-default fill-column 80)

;; wrap and center text in markdown-mode and org-mode
(require-package 'visual-fill-column)
(add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-fill-column-mode)
(add-hook 'org-mode-hook #'visual-fill-column-mode)
(setq markdown-command "multimarkdown")
(setq visual-fill-column-center-text t)

;; markdown support
(require-package 'markdown-mode)
(setq markdown-enable-wiki-links 1)
(setq markdown-link-space-sub-char " ")
(setq markdown-indent-on-enter 'indent-and-new-item)
(require 'adaptive-wrap-vp)
(add-hook 'markdown-mode-hook #'adaptive-wrap-prefix-vp-mode)

;; spell check
(add-hook 'text-mode-hook #'flyspell-mode)

;; sentences end with a single space
(setq sentence-end-double-space nil)

;; window management
(windmove-default-keybindings 'super)
(add-hook 'after-init-hook 'winner-mode)

;; zettelkasten
(defgroup zetteldeft nil
  "A zettelkasten on top of deft."
  :group 'deft
  :link '(url-link "https://efls.github.io/zetteldeft"))
(defcustom zetteldeft-id-format "%Y%m%d%H%M"
  "Format used when generating zetteldeft IDs."
  :type 'string
  :group 'zetteldeft)
(defcustom zetteldeft-id-regex "20[0-9]\\{10\\}"
  "The regular expression used to search for zetteldeft IDs."
  :type 'string
  :group 'zetteldeft)
(defcustom zetteldeft-title-prefix "title: "
  "Prefix string included when `zetteldeft--insert-title' is called."
  :type 'string
  :group 'zetteldeft)
(defcustom zetteldeft-title-suffix "\ntags: \n\n"
  "String inserted below title when `zetteldeft--insert-title' is called."
  :type 'string
  :group 'zetteldeft)
(setq deft-strip-summary-regexp "^title:.*$\\|^id:.*$\\|^tags:.*$")
(setq deft-new-file-format zetteldeft-id-format)
(setq deft-extensions '("md" "txt" "org"))
(setq deft-directory "~/Dropbox/zettelkasten")
(require 'zetteldeft)
(require-package 'deft)
(require-package 'avy)
(global-set-key (kbd "C-c d d") 'deft)
(global-set-key (kbd "C-c d D") 'zetteldeft-deft-new-search)
(global-set-key (kbd "C-c d R") 'deft-refresh)
(global-set-key (kbd "C-c d s") 'zetteldeft-search-at-point)
(global-set-key (kbd "C-c d c") 'zetteldeft-search-current-id)
(global-set-key (kbd "C-c d f") 'zetteldeft-follow-link)
(global-set-key (kbd "C-c d F") 'zetteldeft-avy-file-search-ace-window)
(global-set-key (kbd "C-c d l") 'zetteldeft-avy-link-search)
(global-set-key (kbd "C-c d t") 'zetteldeft-avy-tag-search)
(global-set-key (kbd "C-c d T") 'zetteldeft-tag-buffer)
(global-set-key (kbd "C-c d i") 'zetteldeft-find-file-id-insert)
(global-set-key (kbd "C-c d I") 'zetteldeft-find-file-full-title-insert)
(global-set-key (kbd "C-c d o") 'zetteldeft-find-file)
(global-set-key (kbd "C-c d n") 'zetteldeft-new-file)
(global-set-key (kbd "C-c d N") 'zetteldeft-new-file-and-link)
(global-set-key (kbd "C-c d r") 'zetteldeft-file-rename)
(global-set-key (kbd "C-c d x") 'zetteldeft-count-words)

;; org mode setup
(define-key global-map (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-agenda-files
      '("~/Dropbox/gtd/inbox.org"
        "~/Dropbox/gtd/backlog.org"
        "~/Dropbox/gtd/to-read.org"
        "~/Dropbox/gtd/to-watch.org"
        "~/Dropbox/gtd/shopstyle.org"
        "~/Dropbox/gtd/tickler.org"
        "~/Dropbox/gtd/someday.org"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-capture-templates
      '(("t"
         "Todo [inbox]"
         entry
         (file "~/Dropbox/gtd/inbox.org")
         "* TODO %?"
         :prepend t)))
(setq org-tags-column 0)

;; clojure
(require-package 'clojure-mode)
(require-package 'cider)

;; magit
(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; ripgrep
(require-package 'rg)
(rg-enable-default-bindings)

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
