;; TODOs
;; - tramp (-container?) and consult(-dir?) integration
;; - dir-locals - not in this file somehow
;; - cape?
;; - supersave
;; - C-a C-a beginning of line beginning of statement fix

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

(use-package use-package
  :ensure nil  ;; built-in
  :custom
  (use-package-always-ensure t))

(use-package auto-compile
  :pin melpa-stable
  :demand
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode))


;;
;; Use use-package for emacs configurations
;;
(use-package emacs
  :ensure nil
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

  ;; Keep backup files in temp
  (backup-directory-alist '((".*" . ,temporary-file-directory)))

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
  :pin melpa-stable
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode +1))

(use-package hl-line
  :ensure nil  ;; built-in
  :config
  (global-hl-line-mode +1))

(use-package uniquify
  :ensure nil  ;; built-in
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator "|"))

(use-package diminish
  :pin melpa-stable)


;;
;; Remember things with recentf, savehist, and saveplace
;;
(use-package recentf
  :ensure nil  ;; built-in
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 1000)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  :config
  (recentf-mode +1))

(use-package savehist
  :ensure nil  ;; built-in
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" user-emacs-directory))
  :config
  (savehist-mode +1))

(use-package saveplace
  :ensure nil  ;; built-in
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  :config
  (save-place-mode +1))


;;
;; Load MOVEC and which-key
;;
(use-package vertico
  :pin melpa-stable
  :init
  (vertico-mode +1))

(use-package orderless
  :pin melpa-stable
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :pin melpa-stable
  :after (projectile vertico)
  :custom
  (consult-narrow-key "<")
  (consult-project-function (lambda (_) (projectile-project-root)))
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
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
  :pin melpa  ;; stable is too old
  :after (vertico)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file-command))
  :custom
  (consult-dir-project-list-function 'consult-dir-projectile-dirs)
  :init
  (defun my/consult-dir--tramp-container-hosts ()
      (cl-loop for (id name) in (tramp-container--completion-function tramp-docker-program)
           collect (format "/docker:%s" name)))

  (defvar my/consult-dir--source-tramp-container
    `(:name     "Docker"
      :narrow   ?d
      :category file
      :face     consult-file
      :history  file-name-history
      :items    ,#'my/consult-dir--tramp-container-hosts)
    "Docker candidate source for `consult-dir'.")

  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'my/consult-dir--source-tramp-container t))

(use-package marginalia
  :pin melpa-stable
  :init
  (marginalia-mode +1))

(use-package embark
  :pin melpa-stable
  :bind
  (("C-." . embark-act)
   ("C->" . embark-dwim)
   ("s-]" . embark-collect)
   ("s-}" . embark-export)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :pin melpa-stable
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :pin melpa-stable
  :diminish
  :config
  (which-key-mode +1))


;;
;; Completion - Let's Try Corfu
;;
(use-package corfu
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
  :pin melpa-stable
  :bind (("s-m m" . magit-status)
         ("s-m j" . magit-dispatch)
         ("s-m k" . magit-file-dispatch)
         ("s-m l" . magit-log-buffer)
         ("s-m b" . magit-blame)))

(use-package projectile
  :pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :custom
  (projectile-per-project-compilation-buffer t))

(use-package flycheck
  :pin melpa-stable
  :diminish
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package consult-flycheck
  :pin melpa-stable
  :after (consult)
)

(use-package flyspell
  :ensure nil  ;; built-in
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package flyspell-correct
  :pin melpa-stable
  :after flyspell
  :defer t)

(use-package consult-flyspell
  :after (consult)
  :config
  (setq consult-flyspell-select-function (lambda () (flyspell-correct-at-point) (consult-flyspell))
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

(use-package compile
  :ensure nil  ;; built-in
  :custom
  (compilation-max-output-line-length nil)
  (compilation-skip-threshold 0))

(use-package smartparens
  :ensure t
  :pin melpa-stable
  :diminish smartparens-mode
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

(use-package eldoc
  :ensure nil  ;; built-in
  :diminish eldoc-mode)


;;
;; Navigation in space and time (tramp, windows, undo, etc.)
;;
(use-package undo-tree
  :pin gnu
  :diminish
  :custom
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-history"))))
  (undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode +1))

(use-package ace-window
  :pin melpa-stable
  :config
  (setq aw-dispatch-always t)
  :bind
  ([remap other-window] . ace-window))

(use-package windmove
  :ensure nil  ;; built-in
  :config
  (windmove-default-keybindings))

(use-package tramp
  :ensure nil  ;; built-in
  :custom
  (tramp-default-method "ssh")
  (tramp-show-ad-hoc-proxies t)
  (enable-remote-dir-locals t)
  (tramp-use-ssh-controlmaster-options nil)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


(use-package avy
  :pin melpa-stable
  :ensure t
  :custom
  (avy-enter-times-out nil)
  :bind
  (("M-g c" . avy-goto-char-timer)
   ("M-g l" . avy-goto-line)
   :map isearch-mode-map
   ("M-s a" . avy-isearch))
)

;;
;; General editing
;;
(use-package guru-mode
  :pin melpa-stable
  :diminish
  :config
  (guru-global-mode +1))

(use-package volatile-highlights
  :pin melpa-stable
  :diminish
  :config
  (volatile-highlights-mode +1))

(use-package anzu
  :pin melpa-stable
  :diminish
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package whitespace
  :ensure nil  ;; built-in
  :diminish
  :init
  ;; TODO: Why doesn't this seem to work when done with :hook before-save?
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :custom
  (whitespace-line-column nil)
  (whitespace-style '(face tabs empty trailing)))

(use-package wgrep
  :pin melpa-stable)


;;
;; Programming modes, tree-sitter, LSP
;;
(use-package treesit-auto
  :pin melpa-stable
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
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
  ;; :pin melpa-stable
  :commands lsp-ui-mode)


;;
;; Other configuration
;;
(use-package ereader
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
