;; ===nix-mode===
(use-package nix-mode
  :hook
  ((nix-mode) . lsp-deferred))

(provide 'cur-config-nix)
