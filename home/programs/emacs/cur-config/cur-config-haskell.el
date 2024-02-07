;;; cur-config-haskell.el --- haskell language configurations

;;; Commentary:

;;; Code:
;; ===Haskell-Mode===
(use-package haskell-mode)

;; ===LSP-Haskell===
(use-package lsp-haskell
  :hook
  ((haskell-mode) . lsp-mode)
  ((haskell.mode) .
   (lambda () (require 'lsp-haskell))))

;; ===Company-GHCI===
(use-package company-ghci
  :after (company)
  :custom (company-ghc-show-info t)
  :config
  (push 'company-ghci company-backends))

(provide 'cur-config-haskell)
;;; cur-config-haskell.el ends here
