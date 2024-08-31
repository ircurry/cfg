;; ===Vterm===
(use-package vterm
  :bind ( :map cur/sub-leader-keymap
          ("C-S-t" . vterm))
  :custom
  (vterm-shell "fish")
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

(use-package cur-vterm
  :bind ( :map vterm-mode-map
          ("C-c C-RET"      . cur-vterm-enter-password)
          ("C-c C-<return>" . cur-vterm-enter-password)
          :map project-prefix-map
          ("T" . cur-vterm-project-other-window)))

;; ===Eat===
(use-package eat
  :hook (eat-exec . (lambda (&rest _) (eat-line-mode)))
  :bind ( :map eat-mode-map
	  ("C-c C-RET" . eat-send-password)
	  ("C-c C-<return>" . eat-send-password)
	  :map project-prefix-map
	  ("t" . eat-project)
	  :map cur/sub-leader-keymap
	  ("C-t" . eat))
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-directory-tracking t))

(use-package eat
  :after eshell
  :custom
  (eshell-visual-commands nil "nil because using `eat-eshell-mode'")
  (eshell-visual-subcommands nil "nil because using `eat-eshell-mode'")
  :config
  (eat-eshell-mode 1))

;; ===Eshell===
(use-package eshell
  :bind ( :map cur/sub-leader-keymap
          ("C-e" . eshell)))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package cur-eshell
  :after eshell
  :custom
  (eshell-prompt-regexp cur-eshell-prompt-regexp "Regex for custom eshell prompt")
  (eshell-prompt-function 'cur-eshell-prompt "Set custom prompt for eshell"))

;; ===Zoxide===
(use-package zoxide)

(provide 'cur-config-shell)
