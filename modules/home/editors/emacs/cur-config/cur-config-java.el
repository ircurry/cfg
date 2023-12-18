;;; cur-config-java.el --- configuration for java

;;; Commentary:
;; My configuration for java in Emacs.
;; Currently does not work on my machine because jdtls
;; is not available in the void repos.  When it does I
;; will remove the comment in front of requiring this
;; package.

;;; Code:
;; ===java-mode===
(use-package java-mode
  :commands (java-mode))

;; ===lsp-java===
(use-package lsp-java
  :ensure t
  :after (lsp-mode)
  :hook
  (java-mode . lsp-mode))

(provide 'cur-config-java)
;;; cur-config-java.el ends here
