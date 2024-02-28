;;
;; Top-level configuration for `package` and `use-package`
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))


;;
;; Use use-package for emacs configurations
;;
(use-package emacs
  :config

  ;; Indispensable top level key bindings for macos
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)

  ;; We want this for magit
  (global-unset-key (kbd "s-m"))

  ;; Sideline custom.el
  (setq custom-file (concat user-emacs-directory "custom.el")))


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
;; Remember things with recentf and savehist
;;
(use-package recentf
  :ensure t
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-mode t))

(use-package savehist
  :ensure t
  :config
  (setq savehist-additional-variables
      '(search-ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-mode +1))


;;
;; Load vertico, consult, marginalia and which-key
;;
(use-package vertico
  :ensure t
  :pin melpa-stable
  :init
  (vertico-mode))

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

(use-package marginalia
  :ensure t
  :pin melpa-stable
  :init
  (marginalia-mode))

(use-package which-key
  :ensure t
  :pin melpa-stable
  :config
  (which-key-mode))


;;
;; Load magit and projectile
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


;;
;; Load sidelined custom.el
;;
(when (file-exists-p custom-file)
  (load custom-file))
