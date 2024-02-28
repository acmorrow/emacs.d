;;
;; Top-level configuration for `package`
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;
;; Use use-package for emacs configurations
;;
(use-package emacs
  :config

  ;; Indispensable top level key bindings for macos
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)

  ;; Sideline custom.el
  (setq custom-file (concat user-emacs-directory "custom.el")))


;;
;; Load sidelined custom.el
;;
(when (file-exists-p custom-file)
  (load custom-file))
