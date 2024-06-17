;; ===LSP Mode===
(use-package lsp-mode
  :demand t
  :hook
  (lsp-mode  . lsp-enable-which-key-integration)
  :custom
  (lsp-keymap-prefix "C-c C-M-l" "lsp mode keymap")
  :bind (:map lsp-mode-map
              ("C-c C-a" . lsp-execute-code-action)  ; code actions
              ("C-c C-e" . lsp-treemacs-errors-list) ; treemacs error list
              ("C-c f"   . lsp-find-references)      ; find references
              ("C-c r"   . lsp-find-definition))     ; find definitions
  :config
  (lsp-deferred)
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 3 1024 1024))
  (setq lsp-idle-delay 0.500)
  (setq lsp-lens-enable nil)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after (lsp-mode)
  :custom
  (lsp-ui-doc-enable nil "lsp-ui doc disabled by default")
  (lsp-ui-doc-show-with-cursor t "lsp-ui doc follows cursor")
  (lsp-ui-doc-show-with-mouse t "lsp-ui doc follows mouse")
  (lsp-ui-doc-position 'at-point "lsp-ui doc shows at cursor")
  :bind (:map lsp-ui-mode-map
              ("C-c C-f" . lsp-ui-peek-find-references)  ; find references ui
              ("C-c C-r" . lsp-ui-peek-find-definitions) ; find definitions ui
              ("C-c C-d" . lsp-ui-doc-mode) ; toggle doc mode
              :map lsp-ui-peek-mode-map
              ("ESC" . lsp-ui-peek--abort)             ; toggle doc mode
              ("g"   . lsp-ui-peek--abort)             ; toggle doc mode
              ("j"   . lsp-ui-peek--select-next)       ; toggle doc mode
              ("k"   . lsp-ui-peek--select-prev)       ; toggle doc mode
              ("C-j" . lsp-ui-peek--select-next-file)  ; toggle doc mode
              ("C-k" . lsp-ui-peek--select-prev-file)) ; toggle doc mode
  :hook
  (lsp-mode . lsp-ui-mode))

;; ===Company Mode===
(use-package company
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
  :hook
  (prog-mode . flycheck-mode)
  (lsp-mode  . flycheck-mode))

;; ===Treemacs==
(use-package treemacs
  :bind
  (:map cur/sub-leader-keymap
        ("C-t" . treemacs-select-window))
  :config
  (treemacs-follow-mode))

;; ===Magit===
(use-package magit
  :bind (:map cur/sub-leader-keymap
              ("C-M-g" . magit))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (transient-default-level 5 "Allowing for commit signing"))

;; ===Projectile===
(use-package projectile
  :after (rg)
  :bind (:map cur/projectile-map
              ("C-p"   . projectile-switch-project)
              ("C-a"   . projectile-add-known-project)
              ("C-d"   . projectile-dired)
              ("d"     . projectile-find-dir)
              ("C-f"   . projectile-find-file)
              ("C-r"   . consult-ripgrep)
              ("C-c"   . projectile-compile-project)
              ("C-b"   . consult-project-buffer)
              ("C-l"   . projectile-ibuffer)
              ("C-k"   . projectile-kill-buffers)
              ("C-v"   . projectile-vc)
              ("C-e"   . projectile-run-eshell))
  :config
  (projectile-mode 1))

;; Need ripgrep wrapper for `projectile-ripgrep'
(use-package rg)

(use-package just-mode)

(use-package justl
  :bind (:map cur/projectile-map
              ("C-j" . justl)))

(provide 'cur-config-ide)

(use-package cur-tmux
  :hook
  (projectile-after-switch-project . cur-tmux-switch-add-project-window))
