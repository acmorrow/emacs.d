;;
;; Indispensable top level key bindings for macos
;;
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)


;;
;; Sideline custom.el
;;
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
