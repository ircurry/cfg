;; ===Go Tree-Sitter Mode===
(use-package go-ts-mode
  :mode "\\.go\\'"
  :custom
  (go-ts-mode-indent-offset 4 "Set the indentation to 4")
  :hook (go-ts-mode . lsp-deferred))

(provide 'cur-config-go)
