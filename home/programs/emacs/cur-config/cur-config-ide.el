(use-package emacs
  :demand t
  :custom
  (display-fill-column-indicator-column 81))

(use-package emacs
  :disabled t
  :demand t
  :hook (prog-mode . display-fill-column-indicator-mode))

;; ===LSP Mode===
(use-package lsp-mode
  :disabled t
  :demand t
  :hook
  (lsp-mode  . lsp-enable-which-key-integration)
  :custom
  (lsp-keymap-prefix "C-c C-M-l" "lsp mode keymap")
  (lsp-file-watch-threshold 1750)
  (lsp-headerline-breadcrumb-enable nil)
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
  :disabled t
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

(use-package eglot
  :ensure nil
  :bind ( :map eglot-mode-map
	  ("C-c C-a" . eglot-code-actions))
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package eldoc
  :after (eglot)
  :bind ( :map eglot-mode-map
	  ("C-c C-d" . eldoc-doc-buffer)))

(use-package eldoc
  :hook (prog-mode . eldoc-mode))

;; ===Company Mode===
(use-package company
  :disabled t
  :hook
  (prog-mode . company-mode)
  :bind ( :map company-active-map
          ("<tab>" . company-complete-common-or-cycle)
          ("<return>" . company-complete-selection)
	  :map company-mode-map
	  ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company
  :disabled t
  :after (lsp-mode)
  :hook (lsp-mode . company-mode))

;; ===Flycheck===
(use-package flycheck
  :after (prog-mode)
  :hook
  (prog-mode . flycheck-mode))

(use-package flycheck
  :after (lsp-mode)
  :hook
  (lsp-mode . flycheck-mode))

;; ===Magit===
(use-package magit
  :defer 2
  :bind ( :map cur/sub-leader-keymap
          ("C-v" . magit)
	  :map project-prefix-map
          ("C-v" . nil)
          ("v" . magit-project-status)
          ("V" . project-vc-dir))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (transient-default-level 5 "Allowing for commit signing")
  :config
  (setq magit-bind-magit-project-status nil))

(use-package project ; Demand project after magit so that magit-project-status works
  :after (magit)
  :demand t)

(use-package project
  :bind ( :map project-prefix-map
          ("d"   . project-dired)
          ("D"   . project-find-dir)
          ("C-b" . nil)
          ("l"   . project-list-buffers))
  :custom
  (project-buffers-viewer #'project-list-buffers-ibuffer)
  (project-switch-use-entire-map t))

(use-package rg)

;; ===rainbow-delimiters===
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

;; ===Paredit===
(use-package paredit
  :hook ((emacs-lisp-mode lisp-interaction-mode scheme-mode) . paredit-mode))

;; ===Java Tree-Sitter Mode===
(use-package java-ts-mode
  :mode "\\.java\\'"
  :custom (java-ts-mode-indent-offset 8))

(use-package eglot
  :ensure nil
  :after (java-ts-mode)
  :hook (java-ts-mode . eglot-ensure))

;; ===lsp-java===
(use-package lsp-java
  :disabled t
  :after (java-ts-mode lsp-mode cc-mode envrc)
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

(use-package just-mode)

;; ===YAML===
(use-package yaml-mode
  :commands (yaml-mode))

;; ===nix-mode===
(use-package nix-mode)

(use-package eglot
  :ensure nil
  :after (nix-mode)
  :hook (nix-mode . eglot-ensure))

(use-package lsp-mode
  :disabled t
  :after (nix-mode)
  :hook
  (nix-mode . lsp-deferred) ;; So that envrc mode will work
  :custom
  (lsp-disabled-clients '((nix-mode . nix-nil)) "disable nil so that nixd will be used as lsp-server")
  (lsp-nix-nixd-server-path "nixd" "set nixd binary path to be use from current $PATH"))

;; ===Rust-Mode===
(use-package rustic)

(use-package eglot
  :ensure nil
  :after (rustic)
  :hook (rustic-mode . eglot-ensure))

(use-package lsp-mode
  :disabled t
  :after (rustic)
  :hook (rustic-mode . lsp-deferred))

;; ===Haskell-Mode===
(use-package haskell-mode)

(use-package eglot
  :ensure nil
  :after (haskell-mode)
  :hook (haskell-mode . eglot-ensure))

(use-package lsp-mode
  :disabled t
  :after (haskell-mode)
  :hook (haskell-mode . lsp-deferred))

;; ===LSP-Haskell===
(use-package lsp-haskell
  :disabled t
  :after (haskell-mode lsp-mode)
  :demand t)

;; ===Company-GHCI===
(use-package company-ghci
  :after (company haskell-mode)
  :demand t
  :custom (company-ghc-show-info t)
  :config
  (push 'company-ghci company-backends))

;; ===Tuareg===
(use-package tuareg)

;; ===Utop===
(use-package utop
  :commands (utop utop-mode)
  :config
  (advice-add 'utop :around 'inheritenv-apply))

(use-package merlin
  :after (tuareg)
  :hook (tuareg-mode . merlin-mode))

(use-package merlin-company
  :after (company merlin)
  :demand t)

;; ===C Tree-Sitter Mode===
(use-package c-ts-mode
  :after (cc-mode)
  :mode
  ("\\.c\\'" . c-ts-mode)
  ("\\.h\\'" . c-ts-mode)
  :custom
  (c-default-style '((c-ts-mode . "linux")
                     (java-mode . "java")
                     (awk-mode  . "awk")
                     (other     . "gnu"))
		   "default style for c programs is linux"))

(use-package eglot
  :ensure nil
  :after (c-ts-mode)
  :hook (c-ts-mode . eglot-ensure))

(use-package lsp-mode
  :disabled t
  :after (c-ts-mode)
  :hook (c-ts-mode . lsp-deferred))

;; ===CCLS Mode===
(use-package ccls
  :disabled t
  :after (cc-mode c-ts-mode lsp-mode)
  :demand t)

;; ===Zig Mode===
(use-package zig-mode)

(use-package eglot
  :ensure nil
  :after (zig-mode)
  :hook (zig-mode . eglot-ensure))

(use-package lsp-mode
  :disabled t
  :after (zig-mode)
  :hook (zig-mode . lsp-deferred))

;; ===Go Tree-Sitter Mode===
(use-package go-ts-mode
  :mode
  ("\\.go\\'" . go-ts-mode)
  ("go\\.mod\\'" . go-mod-ts-mode)
  :custom
  (go-ts-mode-indent-offset 4 "Set the indentation to 4")
  :hook
  (go-ts-mode . (lambda () (setq tab-width 4))))

(use-package eglot
  :ensure nil
  :after (go-ts-mode)
  :hook (go-ts-mode . eglot-ensure))

(use-package lsp-mode
  :disabled t
  :after (go-ts-mode)
  :hook (go-ts-mode . lsp-deferred))

(provide 'cur-config-ide)
