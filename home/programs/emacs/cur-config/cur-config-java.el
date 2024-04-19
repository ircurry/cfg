;;; cur-config-java.el --- configuration for java

;;; Commentary:

;;; Code:

;; ===Java Tree-Sitter Mode===
(use-package java-ts-mode
  :mode "\\.java\\'")

;; ===lsp-java===
(use-package lsp-java
  :after (lsp-mode cc-mode)
  :init
  :hook
  (envrc-mode . (lambda ()
                  (when (equal major-mode 'java-ts-mode)
                    (setq lsp-java-server-install-dir (concat (getenv "JDTLS_PATH") "/share/java/jdtls/")))))
  (java-ts-mode . lsp-deferred)
  :config
  (defun lsp-java--ls-command ()
    (let ((jdtls-path (getenv "JDTLS_PATH"))
          (jdtls-exec-options (list
                               "-configuration"
                               (concat (getenv "HOME") "/.jdtls/config_linux")
                               "-data"
                               (concat (getenv "HOME") "/.jdtls/java-workspace"))))
      (message (concat jdtls-path "/share/java/"))
      (append (list (concat jdtls-path "/bin/jdtls")) jdtls-exec-options))))

(provide 'cur-config-java)
;;; cur-config-java.el ends here
