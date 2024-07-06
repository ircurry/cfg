;; ===Go Tree-Sitter Mode===
(use-package go-ts-mode
  :mode
  ("\\.go\\'" . go-ts-mode)
  ("go\\.mod\\'" . go-mod-ts-mode)
  :custom
  (go-ts-mode-indent-offset 4 "Set the indentation to 4")
  :hook
  (go-ts-mode . lsp-deferred)
  (go-ts-mode . (lambda () (setq tab-width 4))))

(provide 'cur-config-go)
