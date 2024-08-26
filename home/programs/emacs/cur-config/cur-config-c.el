;; ===C Tree-Sitter Mode===
(use-package c-ts-mode
  :after (cc-mode)
  :mode
  ("\\.c\\'" . c-ts-mode)
  ("\\.h\\'" . c-ts-mode)
  :custom
  (c-default-style '((c-ts-mode . "linux")
                     (java-mode . "java")
                     (awk-mode  . "awk")
                     (other     . "gnu"))
		   "default style for c programs is linux")
  :hook
  (c-ts-mode . (lambda () (require 'ccls) (lsp-deferred))))

;; ===CCLS Mode===
(use-package ccls
  :after (cc-mode c-ts-mode))

(provide 'cur-config-c)
