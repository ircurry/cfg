;; ===nix-mode===
(use-package nix-mode
  :hook
  (nix-mode . lsp-deferred)) ;; So that envrc mode will work

(use-package nix-mode
  :after lsp-mode
  :custom
  (lsp-disabled-clients '((nix-mode . nix-nil)) "disable nil so that nixd will be used as lsp-server")
  (setq lsp-nix-nixd-server-path "nixd" "set nixd binary path to be use from current $PATH"))

(provide 'cur-config-nix)
