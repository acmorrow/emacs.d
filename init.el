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
;; Load vertico, consult, and which-key
;;
(use-package vertico
  :ensure t
  :pin melpa-stable
  :init
  (vertico-mode))

(use-package consult
  :ensure t
  :pin melpa-stable)

(use-package which-key
  :ensure t
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
