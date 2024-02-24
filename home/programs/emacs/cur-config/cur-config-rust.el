;;; cur-config-rust.el --- rust language configurations

;;; Commentary:

;;; Code:
;; ===Rust-Mode===
(use-package rustic
  :after (lsp-mode)
  :hook (rustic . lsp-deferred))

(provide 'cur-config-rust)
;;; cur-config-rust.el ends here
