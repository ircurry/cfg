;; ===Rust-Mode===
(use-package rustic
  :after (lsp-mode)
  :hook (rustic . lsp-deferred))

(provide 'cur-config-rust)
