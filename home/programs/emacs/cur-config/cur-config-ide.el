;;; cur-config-ide.el --- configuration to make emacs ide like

;;; Commentary:
;; The packages and configurations required to turn Emacs into an ide.
;; Primarily this means an lsp client, a completion system, a syntax
;; checker, and a dap client (yet to be included/configured).

;;; Code:
;; ===LSP Mode===
(use-package lsp-mode
  :ensure t
  :demand t
  :hook
  (lsp-mode  . lsp-enable-which-key-integration)
  :custom (lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp)
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 3 1024 1024))
  (setq lsp-idle-delay 0.500)
  (setq lsp-lens-enable nil)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode))

;; ===Company Mode===
(use-package company
  :ensure t
  :hook
  (prog-mode . company-mode)
  (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-common-or-cycle)
         ("<return>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
  ;;(company-tng-configure-default))

;; ===Flycheck===
(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode)
  (lsp-mode  . flycheck-mode))

;; ===Treemacs==
(use-package treemacs
  :ensure t
  :bind
  (:map prog-mode-map
        ("C-c C-t" . treemacs-select-window))
  (:map cur/sub-leader-keymap
	("C-t" . treemacs-select-window))
  :config
  (treemacs-follow-mode))

;; ===xref-ivy===
(use-package ivy-xref
  :ensure t
  :after (ivy)
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; ===Magit===
(use-package magit
  :ensure t
  :bind (:map cur/sub-leader-keymap
	 ("C-M-g" . magit))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (transient-default-level 5 "Allowing for commit signing"))

;; ===project.el===
(use-package projectile
  :ensure t
  :bind (:map cur/leader-keymap
              ("p C-p" . projectile-switch-project)
              ("p C-d" . projectile-find-dir)
              ("p C-f" . projectile-find-file)
              ("p C-c" . projectile-compile-project)
              ("p C-b" . projectile-switch-to-buffer)
              ("p b"   . projectile-ibuffer)
              ("p C-k" . projectile-kill-buffers)
              ("p d"   . projectile-dired)))

;; ===Zoxide===
(use-package zoxide
  :ensure t
  :bind (:map cur/leader-keymap
	 ("z" . zoxide-find-file)))

(provide 'cur-config-ide)
;;; cur-config-ide.el ends here
