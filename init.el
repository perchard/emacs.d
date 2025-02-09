;; bootstrap package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; hide startup screen
(setq inhibit-startup-screen t)

;; hide toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; hide scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set transparent title bar
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; set typography
(add-to-list 'default-frame-alist '(font . "Berkeley Mono-16"))
(setq-default line-spacing 0.2)

;; set theme
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-monokai-pro t)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

;; configure backups
(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq backup-by-copying t)

;; save desktop configuration
(desktop-save-mode t)

;; auto close brackets
(electric-pair-mode 1)

;; use ibuffer instead of list-buffers
(define-key global-map [remap list-buffers] 'ibuffer)

;; use incremental completions and selection narrowing
(straight-use-package 'helm)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; display key bindings following currently entered incomplete command
(straight-use-package 'which-key)
(which-key-mode)
(which-key-setup-side-window-right)

;; a major mode for editing Markdown-formatted text
(straight-use-package 'markdown-mode)

;; a simple LLM client for Emacs
(straight-use-package 'gptel)

;; tree-based file navigation
(straight-use-package 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; in-buffer completion
(straight-use-package 'company)
(setq company-backends '(company-capf
                         company-keywords))
(add-hook 'after-init-hook 'global-company-mode)

;; git porcelain
(straight-use-package 'magit)

;; treesitter sources
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))		  
