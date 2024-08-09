;; ===Vterm===
(use-package vterm
  :bind ( :map cur/sub-leader-keymap
          ("C-t" . vterm))
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
          ("t" . cur-vterm-project-other-window)))

;; ===Eat===
(use-package eat
  :config
  (eat-eshell-mode 1))

;; ===Eshell===
(use-package eshell
  :bind ( :map cur/sub-leader-keymap
          ("C-e" . eshell)))

(use-package cur-eshell
  :custom
  (eshell-prompt-regexp cur-eshell-prompt-regexp "Regex for custom eshell prompt")
  (eshell-prompt-function 'cur-eshell-prompt "Set custom prompt for eshell"))

;; ===Zoxide===
(use-package zoxide)

(provide 'cur-config-shell)
