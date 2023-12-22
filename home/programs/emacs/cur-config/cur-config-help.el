;;; cur-config-help.el --- help configurations

;;; Commentary:

;;; Code:
;; ===which-key===
(use-package which-key
  :ensure t
  :demand t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 3))

;; ===helpful===
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(provide 'cur-config-help)
;;; cur-config-help.el ends here
