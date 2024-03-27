;;; cur-config-c.el --- configuration for c

;;; Commentary:

;;; Code:
;; ===C Tree-Sitter Mode===
(use-package c-ts-mode
  :mode
  ("\\.c\\'" . c-ts-mode)
  ("\\.h\\'" . c-ts-mode)
  :custom
  ;; TODO: make this only append if the a-list does not have the desiered value
  (c-default-style (append '((c-ts-mode . "linux")) c-default-style) "default style for c programs is linux")
  :hook
  (c-ts-mode . lsp-deferred))

(provide 'cur-config-c)
;;; cur-config-c.el ends here
