;; TODOs
;; treesitter
;; tramp (-container?)
;; wgrep
;; dir-locals
;; other existing customs
;; corfu


;;
;; Top-level configuration for `package`, `use-package`, and `auto-compile`
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package auto-compile
  :ensure t
  :pin melpa-stable
  :custom
  ;; Always prefer newer bytecode
  (load-prefer-newer t)
  :config
  (auto-compile-on-load-mode))

;;
;; Use use-package for emacs configurations
;;
(use-package emacs
  :custom
  ;; Indispensable top level key bindings for macos
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super)

  ;; Sideline custom.el
  (custom-file (concat user-emacs-directory "custom.el"))

  ;; No need for the startup screen
  (inhibit-startup-screen t)

  ;; Make `TAB` smarter
  (tab-always-indent 'complete)

  ;; Bell-ring is annoying
  (ring-bell-function 'ignore)

  ;; Always add a trailing newline to files
  (require-final-newline t)

  :config
  ;; Toolbar is a waste of space
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

  ;; Blinking cursor is silly
  (blink-cursor-mode -1)

  ;; Include more metadata in the modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)

  ;; We want this for magit
  (global-unset-key (kbd "s-m"))

  ;; Don't require typing out `yes` or `no`; accept `y` or `n`.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Always start the initial frame large
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  ;; Delete the selection on any keypress
  (delete-selection-mode t)

  ;; If files change outside emacs, automatically reload them
  (global-auto-revert-mode t)

  ;; Inconsolata-16 as the default
  ;; TODO: Can this go into the `solarized` use-pacakge below?
  (add-to-list 'default-frame-alist '(font . "Inconsolata-16")))

;;
;; Get some theme in there
;;
(use-package solarized
  :ensure solarized-theme
  :pin melpa-stable
  :defer t
  :init
  (load-theme 'solarized-dark t))


;;
;; Remember things with recentf, savehist, and saveplace
;;
(use-package recentf
  :ensure t
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 1000)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  :config
  (recentf-mode t))

(use-package savehist
  :ensure t
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" user-emacs-directory))
  :config
  (savehist-mode t))

(use-package saveplace
  :ensure t
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  :config
  (setq-default save-place t))


;;
;; Load MOVEC and which-key
;;
(use-package vertico
  :ensure t
  :pin melpa-stable
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :pin melpa-stable
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :pin melpa-stable
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; ("C-x p b" . consult-projectile-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
)

(use-package consult-dir
  :ensure t
  :pin melpa-stable)

(use-package marginalia
  :ensure t
  :pin melpa-stable
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :pin melpa-stable
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-." . embark-act)
   ("C->" . embark-dwim)
   ("s-]" . embark-collect)
   ("s-}" . embark-export)
   ("C-h B" . embark-bindings))
)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :pin melpa-stable
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :ensure t
  :pin melpa-stable
  :config
  (which-key-mode))


;;
;; Completion - Let's Try Corfu
;;
(use-package corfu
  :ensure t
  :pin melpa-stable
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("<escape>" . corfu-reset)
        ("C-g" . corfu-quit)
        ("s-SPC" . corfu-insert-separator)
        ("C-p" . corfu-previous)
        ))


;;
;; Load magit, projectile, diff-hl, flycheck, etc. as key programming configs
;;

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("s-m m" . magit-status)
	 ("s-m j" . magit-dispatch)
	 ("s-m k" . magit-file-dispatch)
	 ("s-m l" . magit-log-buffer)
	 ("s-m b" . magit-blame)))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (projectile-mode t)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)))

(use-package diff-hl
  :ensure t
  :pin melpa-stable
  :after (magit)
  :config
  (global-diff-hl-mode t)
  ;; TODO hook:?
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package consult-flycheck
  :ensure t
  :pin melpa-stable)


;;
;; Navigation in space and time (tramp, windows, undo, etc.)
;;
(use-package undo-tree
  :ensure t
  :pin gnu
  :custom
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-history"))))
  (undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode t))

(use-package ace-window
  :ensure t
  :pin melpa-stable
  :config
  ;; TODO bind:?
  (global-set-key [remap other-window] 'ace-window))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator "|"))

(use-package tramp
  :ensure t
  :custom
  (tramp-default-method "ssh")
  :config
  ;; TODO: custom:?
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;;
;; General editing
;;

(use-package volatile-highlights
  :ensure t
  :pin melpa-stable
  :config
  (volatile-highlights-mode t))

(use-package anzu
  :ensure t
  :pin melpa-stable
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode 1))

(use-package whitespace
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :custom
  (whitespace-line-column 80)
  (whitespace-style '(face tabs empty trailing lines-tail)))

;;
;; Programming modes, tree-sitter, LSP
;;
;; (use-package treesit-auto
;;   :ensure t
;;   :pin melpa-stable
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package lsp-mode
  :ensure t
  :pin melpa-stable
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-completion-provider :none) ;; Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook ((c-mode . lsp)
	 (go-mode . lsp)
	 (c++-mode . lsp)
	 (rust-mode . lsp)
	 (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :pin melpa-stable
  :commands lsp-ui-mode)

;;
;; Load sidelined custom.el
;;
(when (file-exists-p custom-file)
  (load custom-file))


;;
;; Start the server
;;
(server-start)
