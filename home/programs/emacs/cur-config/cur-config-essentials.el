;;; cur-config-essentials.el --- essentials for emacs

;;; Commentary:

;;; Code:
;; ===Parens===
(use-package paren
  :ensure nil
  :demand t
  :custom
  (show-paren-delay 0 "No delay for paren highlighting"))

;; ===Line Numbers===
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)
;; Disable line numbers for terminal modes
(dolist (mode '(term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'cur-config-essentials)
;;; cur-config-essentials.el ends here
