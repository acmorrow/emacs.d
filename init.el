;;; -*- lexical-binding: t -*-

;; TODO: dir-locals - not in this file somehow
;; TODO: treesit directory no-littering
;; TODO: dap mode for C++ and Rust
;; TODO: flyspell correct integration via corfu, don't overwrite C-. anywhere conflicts with embark.
;; TODO: activities or burly?
;; TODO: embark-prefix-help-command without needing to go through C-h.
;; TODO: embark-act for LSP and similar
;; TODO: org mode setup

;;
;; Very early setup, enough to ensure we can avoid littering even through
;; bringing up package/use-package/no-littering
;;
(defvar no-littering-etc-directory (expand-file-name ".cache/etc/" user-emacs-directory))
(defvar no-littering-var-directory (expand-file-name ".cache/var/" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa/" no-littering-var-directory))

;;
;; Top-level configuration for `package`, `use-package`, and `auto-compile`
;;
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archive-priorities '("melpa-stable" . 10) t)
(add-to-list 'package-archive-priorities '("melpa" . 1) t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")
(use-package use-package)

(use-package no-littering
  :demand
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (no-littering-theme-backups))

(use-package auto-compile
  :demand
  :init
  (setq load-prefer-newer t)
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

  ;; Sideline custom.el per no-littering
  (custom-file (no-littering-expand-etc-file-name "custom.el"))

  ;; No need for the startup screen
  (inhibit-startup-screen t)

  ;; Make `TAB` smarter
  (tab-always-indent 'complete)

  ;; Bell-ring is annoying
  (ring-bell-function 'ignore)

  ;; Always add a trailing newline to files
  (require-final-newline t)

  ;; Configure buffer uniqueness
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator "|")

  (custom-unlispify-menu-entries nil)
  (custom-unlispify-tag-names nil)

  ;; Go to where things already are
  (display-buffer-base-action '(display-buffer-reuse-window (reusable-frames . visible)))

  ;; Use pop-to-buffer for emacsclient buffers so they don't replace the current window
  (server-window 'pop-to-buffer)

  :config
  ;; Toolbar/scrollbar is a waste of space
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

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
  (add-to-list 'default-frame-alist '(font . "Inconsolata-16"))

  ;; On macOS, this key is used to cycle windows of the current
  ;; app. So, arrange to let it do so for our frames as well.
  :bind (("M-`" . ns-next-frame)
         ("M-/" . completion-at-point)))


;;
;; Get some theme in there, and other UI stuff
;;
(use-package solarized
  :ensure solarized-theme
  :defer t
  :init
  (load-theme 'solarized-dark t))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode +1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package diminish)


;;
;; Remember things with recentf, savehist, and saveplace
;;
(use-package recentf
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 1000)
  :config
  (recentf-mode +1))

(use-package savehist
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  :config
  (savehist-mode +1))

(use-package saveplace
  :config
  (save-place-mode +1))


;;
;; Load MOVEC and which-key, and other means
;; of finding things
;;
(use-package vertico
  :init
  (vertico-mode +1)
  (vertico-multiform-mode +1)
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  :custom
  (enable-recursive-minibuffers t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :after (projectile vertico)
  :custom
  (consult-narrow-key "<")
  (consult-project-function (lambda (_) (projectile-project-root)))
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)
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
         ("M-r" . consult-history)))

(use-package consult-dir
  :pin melpa  ;; stable is too old
  :after (vertico)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
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

(use-package consult-projectile
  :pin melpa
  :after (consult projectile)
  :bind
  (:map projectile-mode-map
        ("s-p f" . consult-projectile)))

(use-package marginalia
  :init
  (marginalia-mode +1))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C->" . embark-dwim)
   ("s-]" . embark-collect)
   ("s-}" . embark-export)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; From https://karthinks.com/software/avy-can-do-anything/
  (with-eval-after-load 'avy
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))
  :custom
  (embark-indicators '(
    embark-minimal-indicator  ; default is embark-mixed-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))
  (embark-prompter 'embark-completing-read-prompter)
  (embark-cycle-key ".")
  (embark-help-key "?"))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :diminish
  :config
  (which-key-mode +1))

(use-package rg
  :ensure-system-package rg)


;;
;; Completion - Let's Try Corfu and Cape
;;
(use-package corfu
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
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package cape
  :demand
  :bind ("C-M-/" . cape-prefix-map)
  :config
  (define-key cape-prefix-map (kbd "y") 'consult-yasnippet)
  (define-key cape-prefix-map (kbd "Y") 'yas-insert-snippet)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        #'cape-keyword nil t))))


;;
;; Load magit, projectile, fly[check,spell], compile, etc. as key programming configs.
;;
(use-package magit
  :custom
  (magit-list-refs-sortby "-creatordate")
  (magit-diff-refine-hunk 'all)
  (magit-log-show-refname-after-summary t)
  :bind (("s-m m" . magit-status)
         ("s-m j" . magit-dispatch)
         ("s-m k" . magit-file-dispatch)
         ("s-m l" . magit-log-buffer)
         ("s-m b" . magit-blame)))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :custom
  (projectile-per-project-compilation-buffer t)
  (compilation-save-buffers-predicate
   (lambda ()
     (and (buffer-file-name)
          (projectile-project-p)
          (projectile-file-exists-p (buffer-file-name))))))

(use-package flycheck
  :diminish
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package consult-flycheck
  :after (consult)
)

(use-package flyspell
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package flyspell-correct
  :after flyspell
  :defer t)

(use-package consult-flyspell
  :pin melpa
  :after (consult)
  :config
  (setq consult-flyspell-select-function (lambda () (flyspell-correct-at-point) (consult-flyspell))
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

(use-package compile
  :custom
  (compilation-max-output-line-length nil)
  (compilation-skip-threshold 0))

(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  :custom
  (sp-base-key-bindings 'sp))

(use-package eldoc
  :diminish eldoc-mode)


;;
;; Navigation in space and time (tramp, windows, undo, etc.)
;;
(use-package undo-tree
  :pin gnu
  :diminish
  :custom
  (undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode +1))

(use-package ace-window
  :demand
  :init
  ;; From https://karthinks.com/software/emacs-window-management-almanac/#aw-select-the-completing-read-for-emacs-windows
  (defun my/ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command
invoked immediately after this command (ignoring reading from the
minibuffer). Creates a new window before displaying the
buffer. When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  (defun my/ace-window-one-command ()
  (interactive)
  (let ((win (aw-select " ACE")))
    (when (windowp win)
      (with-selected-window win
        (let* ((command (key-binding
                         (read-key-sequence
                          (format "Run in %s..." (buffer-name)))))
               (this-command command))
          (call-interactively command))))))
  :bind
  ([remap other-window] . ace-window)
  ("C-x 4 o" . 'my/ace-window-prefix)
  ("C-x O" . 'my/ace-window-one-command)
  :custom
  (switch-to-buffer-obey-display-actions t)
  (aw-dispatch-always t)
  (ace-window-display-mode t)
  )

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-show-ad-hoc-proxies t)
  (enable-remote-dir-locals t)
  (tramp-use-scp-direct-remote-copying t)
  (tramp-use-connection-share nil)
  (tramp-copy-size-limit (* 1024 1024))
  :config
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package avy
  :config
  ;; From https://karthinks.com/software/emacs-window-management-almanac/
  (define-advice pop-global-mark (:around (pgm) use-display-buffer)
    "Make `pop-to-buffer' jump buffers via `display-buffer'."
    (cl-letf (((symbol-function 'switch-to-buffer)
               #'pop-to-buffer))
      (funcall pgm)))
  :custom
  (avy-enter-times-out nil)
  (avy-all-windows 'all-frames)
  :bind
  (("M-g c" . avy-goto-char-timer)
   ("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   :map isearch-mode-map
   ("M-s a" . avy-isearch)))


;;
;; General editing
;;
(use-package guru-mode
  :diminish
  :config
  (guru-global-mode +1))

(use-package volatile-highlights
  :diminish
  :config
  (volatile-highlights-mode +1))

(use-package anzu
  :diminish
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package whitespace
  :diminish
  :init
  ;; TODO: Why doesn't this seem to work when done with :hook before-save?
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :custom
  (whitespace-line-column nil)
  (whitespace-style '(face tabs empty trailing)))

(use-package wgrep)

(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("C-c C-p" . wdired-change-to-wdired-mode)))

(use-package wdired
  :ensure nil
  :custom
  ;; Allow editing perms bits in wdired
  (wdired-allow-to-change-permissions t)
)

(use-package atomic-chrome
  :demand t
  :custom
  (atomic-chrome-extension-type-list '(atomic-chrome))
  (atomic-chrome-buffer-open-style 'frame)
  :config (atomic-chrome-start-server))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)))


;;
;; Programming modes, tree-sitter, LSP
;;
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; https://www.reddit.com/r/emacs/comments/1ewrjrm/help_get_proper_syntax_highlight_on_ctsmode/
  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'rust
                :ts-mode 'rust-ts-mode
                :remap 'rust-mode
                :url "https://github.com/tree-sitter/tree-sitter-rust"
                :revision "v0.23.3"
                :ext "\\.rs\\'"))
  ;; https://github.com/renzmann/treesit-auto/issues/76
  (setq major-mode-remap-alist
        (treesit-auto--build-major-mode-remap-alist))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package rust-mode
  :demand
  :pin melpa
  :custom
  (rust-mode-treesitter-derive t)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package rustic
  :pin melpa
  :after (rust-mode))

(use-package go-mode
  :hook (go-ts-mode . (lambda ()
                        (whitespace-toggle-options '(tabs))
                        (setq tab-width 2)))
  :custom
  (go-ts-mode-indent-offset 2))

(use-package markdown-mode
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "markdown"))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package lsp-mode
  :pin melpa
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-completion-provider :none) ;; Corfu!
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.01)
  ;; Having the inlays on all the time is distracting. Instead, toggle them on
  ;; interactively with `M-x lsp-inline-hints-mode` when you want them, then make
  ;; them go away by running it again.
  ;; TODO: Add a toggle for this under the LSP command keymap.
  ;; (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         (c++-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c-or-c++-mode . lsp-deferred)
         (c-or-c++-ts-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (cmake-mode . lsp-deferred)
         (cmake-ts-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :pin melpa
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable nil))

(use-package consult-lsp
  :pin melpa
  :after lsp-mode
  :commands consult-lsp-symbols
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package auth-source
  :pin melpa
  :ensure t)

(use-package auth-source-1password
  :pin melpa
  :ensure t
  :ensure-system-package op
  :config
  (auth-source-1password-enable))

(use-package gptel
  :pin melpa
  :config
  (setq gptel-model 'claude-sonnet-4-5-20250929)
  (defun my/get-anthropic-api-key ()
    "Retrieve Anthropic API key from auth-source (1Password)."
    (let ((key (auth-source-pick-first-password
                :host "anthropic.com"
                :user "password")))
      (if key
          key
        (user-error "Anthropic API key not found in auth-source"))))

  (setq gptel-backend (gptel-make-anthropic "Claude"
                         :stream t
                         :key (my/get-anthropic-api-key))))


;; Prereq for claude-code-ide
(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat" :rev :newest)
  :demand
  :bind (:map eat-semi-char-mode-map
              ;; Unbind M-` so it falls through to global binding (ns-next-frame)
              ("M-`" . nil)))

;; vterm - alternative terminal backend for claude-code-ide
(use-package vterm
  :ensure t
  :pin melpa
  :bind (:map vterm-mode-map
              ;; Unbind M-` so it falls through to global binding (ns-next-frame)
              ("M-`" . nil)))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  ;; :ensure-system-package claude-code
  :bind ("s-c" . claude-code-ide-menu)
  :custom

  (claude-code-ide-cli-extra-flags "--model claude-sonnet-4-5-20250929")

  ;; Terminal backend
  ;; (claude-code-ide-terminal-backend 'eat)  ; Commented out - trying vterm instead
  (claude-code-ide-terminal-backend 'vterm)

  ;; Window configuration
  (claude-code-ide-use-side-window nil)
  (claude-code-ide-focus-on-open t)

  ;; Diff integration with ediff
  (claude-code-ide-use-ide-diff t)
  (claude-code-ide-focus-claude-after-ediff t)

  (claude-code-ide-diagnostics-backend 'flycheck)
  (claude-code-ide-debug-mode nil)
  (claude-code-ide-chat-auto-scroll t)

  :config
  (setenv "ANTHROPIC_API_KEY" (my/get-anthropic-api-key))

  ;; Enable Emacs MCP tools for deep integration
  (claude-code-ide-emacs-tools-setup)

  ;; Custom MCP tool for LSP formatting
  (defun my/claude-lsp-format-buffer (file-path)
    "Format the specified file using LSP formatting.
FILE-PATH must be an absolute path to the file to format."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((target-buffer (or (find-buffer-visiting file-path)
                               (find-file-noselect file-path))))
        (if (not target-buffer)
            (format "Error: Could not open file: %s" file-path)
          (with-current-buffer target-buffer
            (if (not (bound-and-true-p lsp-mode))
                (format "Error: LSP mode not active in buffer for file: %s" file-path)
              (condition-case err
                  (progn
                    (lsp-format-buffer)
                    (save-buffer)
                    (format "Successfully formatted and saved: %s" (buffer-file-name)))
                (error (format "Error formatting %s: %s"
                              file-path
                              (error-message-string err))))))))))

  (claude-code-ide-make-tool
   :function #'my/claude-lsp-format-buffer
   :name "lsp_format_buffer"
   :description "Format a specific file using LSP formatting. Requires an absolute file path."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to format.")))

  ;; Custom MCP tools for reading dir-locals
  (defun my/claude-read-dir-locals (file-path)
    "Read effective dir-local variables for FILE-PATH.
Opens FILE-PATH and returns buffer-local-variables as a Lisp form."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let ((buffer (find-file-noselect file-path)))
            (unwind-protect
                (with-current-buffer buffer
                  (format "%S" (buffer-local-variables)))
              (kill-buffer buffer)))
        (error (format "Error reading dir-locals for %s: %s"
                      file-path
                      (error-message-string err))))))

  (defun my/claude-read-project-dir-locals (file-path)
    "Read effective dir-local variables for the project containing FILE-PATH.
Finds the project root and delegates to `my/claude-read-dir-locals`."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((default-directory (file-name-directory file-path))
                 (project-root (or (projectile-project-root)
                                  (when-let ((proj (project-current)))
                                    (project-root proj))
                                  default-directory))
                 ;; Use a dummy file in the project root
                 (probe-file (expand-file-name ".dir-locals-probe" project-root)))
            (my/claude-read-dir-locals probe-file))
        (error (format "Error reading project dir-locals: %s" (error-message-string err))))))

  (claude-code-ide-make-tool
   :function #'my/claude-read-dir-locals
   :name "read_dir_locals"
   :description "Read buffer-local variables for a specific file path. Opens the file and returns buffer-local-variables as a Lisp form."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file or directory to read buffer-local variables for.")))

  (claude-code-ide-make-tool
   :function #'my/claude-read-project-dir-locals
   :name "read_project_dir_locals"
   :description "Read buffer-local variables for the project root containing a file. Finds the project root via projectile/project.el, then returns buffer-local-variables as a Lisp form."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to any file in the project. Project root will be determined automatically.")))

  ;; Custom MCP tools for projectile task management (split architecture)
  (require 'seq)  ; For seq-take-last in task output limiting

  ;; Tool 1: Start a projectile task (non-blocking)
  (defun my/claude-projectile-task-start (task-type command file-path)
    "Start a projectile task (compile, test, configure, install, package, run).
Returns the compilation buffer name for later querying.

TASK-TYPE is one of: compile, test, configure, install, package, run.
COMMAND is the shell command to execute (required).
FILE-PATH is used to determine which project to operate on."
    (claude-code-ide-mcp-server-with-session-context nil
      ;; Validate projectile-per-project-compilation-buffer is set
      (if (not projectile-per-project-compilation-buffer)
          "Error: projectile-per-project-compilation-buffer must be t for safe parallel compilation. Add (setq projectile-per-project-compilation-buffer t) to your Emacs config."
        ;; Determine project from file-path
        (let* ((default-directory (file-name-directory file-path))
               (project-root (projectile-project-root)))
          (if (not project-root)
              (format "Error: %s is not in a projectile project" file-path)
            ;; Determine the task function and command map
            (let* ((task-info (pcase task-type
                               ("compile" (cons #'projectile-compile-project projectile-compilation-cmd-map))
                               ("test" (cons #'projectile-test-project projectile-test-cmd-map))
                               ("configure" (cons #'projectile-configure-project projectile-configure-cmd-map))
                               ("install" (cons #'projectile-install-project projectile-install-cmd-map))
                               ("package" (cons #'projectile-package-project projectile-package-cmd-map))
                               ("run" (cons #'projectile-run-project projectile-run-cmd-map))
                               (_ nil)))
                   (task-function (car task-info))
                   (command-map (cdr task-info))
                   (compilation-read-command nil) ;; Disable prompting
                   (compilation-dir (projectile-compilation-dir)))
              (if (not task-function)
                  (format "Error: Unknown task-type '%s'. Must be one of: compile, test, configure, install, package, run" task-type)
                ;; Cache the command in projectile's map
                (when command
                  (puthash compilation-dir command command-map))
                ;; Compute the buffer name deterministically (respects per-project setting)
                (let ((buffer-name (projectile-compilation-buffer-name "compilation")))
                  ;; Call the projectile task function (non-blocking)
                  (funcall task-function nil)
                  ;; Return the buffer name for later querying
                  (format "Started %s in buffer: %s" task-type buffer-name)))))))))

  ;; Tool 2: Wait for projectile task completion and get size info
  (defun my/claude-projectile-task-wait (buffer-name)
    "Check if compilation is finished and return size info when done.

BUFFER-NAME is the name of the compilation buffer to check.

Returns 'running' if still executing, or 'finished' with output size (lines and chars)
when complete. Use this to poll for completion and decide whether to use head/tail
limiting when calling projectile_task_query."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (get-buffer buffer-name)))
        (if (not buf)
            (format "Error: Buffer not found: %s" buffer-name)
          (with-current-buffer buf
            (if (memq buf compilation-in-progress)
                (format "Status: running")
              ;; Compilation finished - return size info
              (let* ((line-count (count-lines (point-min) (point-max)))
                     (char-count (- (point-max) (point-min))))
                (format "Status: finished\n\nOutput size:\n  Lines: %d\n  Characters: %d"
                        line-count char-count))))))))

  ;; Tool 3: Query projectile task output (call after task-wait says finished)
  (defun my/claude-projectile-task-query (buffer-name &optional head-lines tail-lines)
    "Retrieve output from a finished compilation buffer.

BUFFER-NAME is the name of the compilation buffer to query.
Optional HEAD-LINES limits output to first N lines.
Optional TAIL-LINES limits output to last N lines.

This should only be called after projectile_task_wait indicates the task is finished.
Returns the compilation output, optionally limited by head-lines or tail-lines."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (get-buffer buffer-name)))
        (if (not buf)
            (format "Error: Buffer not found: %s" buffer-name)
          (with-current-buffer buf
            (let* ((full-output (buffer-substring-no-properties (point-min) (point-max)))
                   (lines (split-string full-output "\n")))
              (cond
               ;; Limit to first N lines
               (head-lines
                (string-join (seq-take lines head-lines) "\n"))
               ;; Limit to last N lines (use nbutlast or seq-drop since seq-take-last doesn't exist)
               (tail-lines
                (let ((drop-count (max 0 (- (length lines) tail-lines))))
                  (string-join (seq-drop lines drop-count) "\n")))
               ;; Return full output
               (t full-output))))))))

  ;; Tool 4: Kill a running projectile task
  (defun my/claude-projectile-task-kill (buffer-name)
    "Kill a running compilation in the specified buffer.
BUFFER-NAME is the name of the compilation buffer to kill.
Returns a status message."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (get-buffer buffer-name)))
        (if (not buf)
            (format "Error: Buffer not found: %s" buffer-name)
          (with-current-buffer buf
            (if (not (memq buf compilation-in-progress))
                (format "No compilation running in buffer: %s" buffer-name)
              ;; Use compilation-mode's built-in kill function
              (kill-compilation)
              (format "Killed compilation in buffer: %s" buffer-name)))))))

  ;; Register the projectile task MCP tools
  (claude-code-ide-make-tool
   :function #'my/claude-projectile-task-start
   :name "projectile_task_start"
   :description "Start a projectile task (compile, test, configure, install, package, run) for a project. Non-blocking - returns immediately with the compilation buffer name. Use projectile_task_wait to poll for completion, then projectile_task_query to retrieve output. Requires projectile-per-project-compilation-buffer to be enabled."
   :args '((:name "task_type"
            :type string
            :description "The type of projectile task to run: compile, test, configure, install, package, or run")
           (:name "command"
            :type string
            :description "The shell command to execute for this task.")
           (:name "file_path"
            :type string
            :description "Absolute path to a file in the project (used to determine which project to operate on).")))

  (claude-code-ide-make-tool
   :function #'my/claude-projectile-task-wait
   :name "projectile_task_wait"
   :description "Poll for projectile task completion and get output size. Returns 'running' if still executing, or 'finished' with line/character count when done. Use this to poll after projectile_task_start, then use the size info to decide whether to retrieve full output or use head/tail limiting with projectile_task_query."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to check (returned by projectile_task_start).")))

  (claude-code-ide-make-tool
   :function #'my/claude-projectile-task-query
   :name "projectile_task_query"
   :description "Retrieve compilation output from a finished task. Should only be called after projectile_task_wait indicates the task is finished. Returns full output by default, or limited output if head_lines or tail_lines is specified."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to query (returned by projectile_task_start).")
           (:name "head_lines"
            :type number
            :description "Limit output to first N lines (like 'head -n'). Recommended for checking errors at start of output."
            :optional t)
           (:name "tail_lines"
            :type number
            :description "Limit output to last N lines (like 'tail -n'). Recommended for checking summary/final errors."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'my/claude-projectile-task-kill
   :name "projectile_task_kill"
   :description "Kill a running compilation in the specified buffer. Equivalent to pressing C-c C-k in the compilation buffer."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the compilation buffer to kill.")))

  ;; Custom MCP tools for Emacs introspection
  (defun my/claude-emacs-describe (name type)
    "Describe an Emacs symbol/mode/package.
NAME is the symbol name as a string.
TYPE is one of: function, variable, mode, package, symbol."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (let* ((symbol (intern name))
                 (temp-buf (generate-new-buffer " *temp-help*")))
            (unwind-protect
                (save-window-excursion
                  (cl-letf (((symbol-function 'help-buffer)
                             (lambda () temp-buf)))
                    (pcase type
                      ("function" (describe-function symbol))
                      ("variable" (describe-variable symbol))
                      ("mode" (describe-function symbol))
                      ("package" (describe-package symbol))
                      ("symbol" (describe-symbol symbol))
                      (_ (error "Unknown type '%s'. Must be one of: function, variable, mode, package, symbol" type))))
                  (with-current-buffer temp-buf
                    (buffer-string)))
              (when (buffer-live-p temp-buf)
                (kill-buffer temp-buf))))
        (error (format "Error describing %s: %s" name (error-message-string err))))))

  (defun my/claude-emacs-apropos (pattern)
    "Search for all Emacs symbols matching PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (let ((show-all t))
              (apropos pattern show-all)
              ;; Capture content from *Apropos* buffer
              (with-current-buffer "*Apropos*"
                (prog1 (buffer-string)
                  (kill-buffer)))))
        (error (format "Error running apropos: %s" (error-message-string err))))))

  (defun my/claude-emacs-apropos-command (pattern)
    "Search for interactive Emacs commands matching PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (apropos-command pattern t)
            ;; Capture content from *Apropos* buffer
            (with-current-buffer "*Apropos*"
              (prog1 (buffer-string)
                (kill-buffer))))
        (error (format "Error running apropos-command: %s" (error-message-string err))))))

  (defun my/claude-emacs-apropos-documentation (pattern)
    "Search Emacs documentation for PATTERN."
    (claude-code-ide-mcp-server-with-session-context nil
      (condition-case err
          (save-window-excursion
            (apropos-documentation pattern)
            ;; Capture content from *Apropos* buffer
            (with-current-buffer "*Apropos*"
              (prog1 (buffer-string)
                (kill-buffer))))
        (error (format "Error running apropos-documentation: %s" (error-message-string err))))))

  (claude-code-ide-make-tool
   :function #'my/claude-emacs-describe
   :name "emacs_describe"
   :description "Get documentation for an Emacs symbol. Returns docstring, current value (for variables), arguments (for functions), and other metadata from the running Emacs session."
   :args '((:name "name"
            :type string
            :description "The name of the symbol to describe (e.g., 'projectile-compile-project', 'lsp-mode').")
           (:name "type"
            :type string
            :description "The type of thing to describe: function, variable, mode, package, or symbol. Use 'symbol' for a unified view of all aspects.")))

  (claude-code-ide-make-tool
   :function #'my/claude-emacs-apropos
   :name "emacs_apropos"
   :description "Search for all Emacs symbols (functions, variables, faces, etc.) matching a pattern. Use for broad exploration."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match symbol names.")))

  (claude-code-ide-make-tool
   :function #'my/claude-emacs-apropos-command
   :name "emacs_apropos_command"
   :description "Search for interactive Emacs commands (callable via M-x) matching a pattern. More focused than emacs_apropos - only returns commands users can invoke."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match command names.")))

  (claude-code-ide-make-tool
   :function #'my/claude-emacs-apropos-documentation
   :name "emacs_apropos_documentation"
   :description "Search Emacs documentation text for a pattern. Finds functions/variables whose docstrings contain the pattern. Use for concept-based search (e.g., 'buffer naming', 'code formatting')."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regexp) to match in documentation text.")))

  ;; lsp-describe-thing-at-point wrapper (returns hover info as string)
  (defun my/claude-lsp-describe-thing-at-point (file-path line column)
    "Get LSP hover information at FILE-PATH:LINE:COLUMN.
Returns formatted hover text including type signature and documentation.
LINE is 1-based, COLUMN is 0-based (Emacs conventions)."
    (if (not file-path)
        (error "file_path parameter is required")
      (claude-code-ide-mcp-server-with-session-context nil
        (let ((target-buffer (or (find-buffer-visiting file-path)
                                 (find-file-noselect file-path))))
          (with-current-buffer target-buffer
            (condition-case err
                (save-excursion
                  ;; Position at the specified location
                  (goto-line line)
                  (move-to-column column)
                  ;; Get hover contents from LSP
                  (let ((contents (-some->> (lsp--text-document-position-params)
                                    (lsp--make-request "textDocument/hover")
                                    (lsp--send-request)
                                    (lsp:hover-contents))))
                    (if (and contents (not (equal contents "")))
                        ;; Render the hover content as text (same as lsp--display-contents does)
                        (mapconcat 'string-trim-right
                                   (split-string (lsp--render-on-hover-content contents t) "\n")
                                   "\n")
                      (format "No hover information at %s:%d:%d" file-path line column))))
              (error
               (format "Error getting hover info at %s:%d:%d: %s"
                       file-path line column (error-message-string err)))))))))

  (claude-code-ide-make-tool
   :function #'my/claude-lsp-describe-thing-at-point
   :name "lsp_describe_thing_at_point"
   :description "Get LSP hover information (type signature and documentation) at a specific location. Returns formatted text with type, parameters, and docstring."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file.")
           (:name "line"
            :type number
            :description "Line number (1-based).")
           (:name "column"
            :type number
            :description "Column number (0-based).")))

)


;;
;; Snippets
;;
(use-package yasnippet
  :diminish yas-minor-mod
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet
  :pin melpa
  :after (consult yasnippet))


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
