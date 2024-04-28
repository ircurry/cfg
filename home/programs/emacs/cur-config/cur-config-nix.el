;; ===nix-mode===
(use-package nix-mode
  :hook
  ((nix-mode) . lsp-deferred))
  ;:ensure t)

(provide 'cur-config-nix)
