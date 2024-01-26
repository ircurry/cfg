;;; cur-config-c.el --- configurations for working with c code

;;; Commentary:

;;; Code:
;; ===CCLS===
(use-package ccls
  ;:ensure t
  :hook
  ((c-mode c++-mode) . lsp-mode)
  ((c-mode c++-mode) .
   (lambda () (require 'ccls))))

;; ===Basic Settings===
(push '(c-mode . "linux") c-default-style)
(push '(c++-mode . "linux") c-default-style)

(provide 'cur-config-c)
;;; cur-config-c.el ends here
