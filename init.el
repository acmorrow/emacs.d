;; TODOs
;; - tramp (-container?) and consult(-dir?) integration
;; - dir-locals - not in this file somehow
;; - backup and temp files
;; - cape?
;; - supersave
;; - flyspell


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

  ;; Teach treesit where to get the relevant modules
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")))

  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode)
     (go-mode . go-ts-mode)
     (html-mode . html-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (rust-mode . rust-ts-mode)
     (toml-mode . toml-ts-mode)))

  :config
  ;; Toolbar is a waste of space
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

  ;; Blinking cursor is silly
  (blink-cursor-mode -1)

  ;; Include more metadata in the modeline
  (line-number-mode +1)
  (column-number-mode +1)
  (size-indication-mode +1)

  ;; We want this for magit
  (global-unset-key (kbd "s-m"))

  ;; Don't require typing out `yes` or `no`; accept `y` or `n`.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Always start the initial frame large
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  ;; Delete the selection on any keypress
  (delete-selection-mode +1)

  ;; If files change outside emacs, automatically reload them
  (global-auto-revert-mode +1)

  ;; global line numbers
  (global-display-line-numbers-mode +1)

  ;; Inconsolata-16 as the default
  ;; TODO: Can this go into the `solarized` use-pacakge below?
  (add-to-list 'default-frame-alist '(font . "Inconsolata-16")))


;;
;; Get some theme in there, and other UI stuff
;;
(use-package solarized
  :ensure solarized-theme
  :pin melpa-stable
  :defer t
  :init
  (load-theme 'solarized-dark t))

(use-package diff-hl
  :ensure t
  :pin melpa-stable
  :after (magit)
  :config
  (global-diff-hl-mode +1)
  ;; TODO hook:?
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package hl-line
  :ensure t
  :config
  (global-hl-line-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator "|"))

(use-package diminish
  :ensure t
  :pin melpa-stable)

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
  (recentf-mode +1))

(use-package savehist
  :ensure t
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" user-emacs-directory))
  :config
  (savehist-mode +1))

(use-package saveplace
  :ensure t
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  :config
  (save-place-mode +1))


;;
;; Load MOVEC and which-key
;;
(use-package vertico
  :ensure t
  :pin melpa-stable
  :init
  (vertico-mode +1))

(use-package orderless
  :ensure t
  :pin melpa-stable
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :pin melpa-stable
  :after (projectile)
  :custom
  (consult-narrow-key "<")
  (consult-project-function (lambda (_) (projectile-project-root)))
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
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
  :pin melpa-stable
  :custom
  (consult-dir-project-list-function 'consult-dir-projectile-dirs))

(use-package marginalia
  :ensure t
  :pin melpa-stable
  :init
  (marginalia-mode +1))

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
  (which-key-mode +1))


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

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))


;;
;; Load magit, projectile, diff-hl, flycheck, etc., compile as key programming configs
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
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package consult-flycheck
  :ensure t
  :pin melpa-stable
  :after (consult)
)

(use-package compile
  :ensure t
  :custom
  (compilation-max-output-line-length nil)
  (compilation-skip-threshold 0))

(use-package smartparens-mode
  :ensure smartparens
  :pin melpa-stable
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))


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
  (global-undo-tree-mode +1))

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

(use-package tramp
  :ensure t
  :custom
  (tramp-default-method "ssh")
  (tramp-show-ad-hoc-proxies t)
  (enable-remote-dir-locals t)
  (tramp-use-ssh-controlmaster-options nil)
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
  (volatile-highlights-mode +1))

(use-package anzu
  :ensure t
  :pin melpa-stable
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package whitespace
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :custom
  (whitespace-line-column nil)
  (whitespace-style '(face tabs empty trailing lines-tail)))

(use-package wgrep
  :ensure t
  :pin melpa-stable)


;;
;; Programming modes, tree-sitter, LSP
;;
(use-package lsp-mode
  :ensure t
  ;; :pin melpa-stable
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-completion-provider :none) ;; Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook ((c-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (c-or-c++-mode . lsp-deferred)
         (c-or-c++-ts-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (go-ts.mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  ;; :pin melpa-stable
  :commands lsp-ui-mode)

;;
;; Other configuration
;;

;; For reading Mastering Emacs
(use-package ereader
  :ensure t
  :pin melpa)


;;
;; Load sidelined custom.el
;;
(when (file-exists-p custom-file)
  (load custom-file))


;;
;; Start the server
;;
(server-start)
