;;
;; Indispensable top level key bindings for macos
;;
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)


;;
;; Top-level configuration for `package`
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;
;; Sideline custom.el
;;
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
