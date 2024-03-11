;;; cur-config-nix.el --- working with nix in emacs

;;; Commentary:

;;; Code:
;; ===nix-mode===
(use-package nix-mode
  :hook
  ((nix-mode) . lsp-deferred))
  ;:ensure t)

(provide 'cur-config-nix)
;;; cur-config-nix.el ends here
