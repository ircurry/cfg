(use-package emacs
  :demand t
  :custom
  (display-fill-column-indicator-column 81))

(use-package emacs
  :disabled t
  :demand t
  :hook (prog-mode . display-fill-column-indicator-mode))

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

;; ===Flycheck===
(use-package flycheck
  :after (prog-mode)
  :hook
  (prog-mode . flycheck-mode))

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

;; ===YAML===
(use-package yaml-mode
  :commands (yaml-mode))

;; ===nix-mode===
(use-package nix-mode)

(use-package eglot
  :ensure nil
  :after (nix-mode)
  :hook (nix-mode . eglot-ensure))

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

(provide 'cur-config-ide)
