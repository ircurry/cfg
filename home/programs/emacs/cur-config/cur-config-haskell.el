;; ===Haskell-Mode===
(use-package haskell-mode)

;; ===LSP-Haskell===
(use-package lsp-haskell
  :hook
  ((haskell-mode) . lsp-deferred))

;; ===Company-GHCI===
(use-package company-ghci
  :after (company)
  :custom (company-ghc-show-info t)
  :config
  (add-to-list company-backends 'company-ghci))

(provide 'cur-config-haskell)
