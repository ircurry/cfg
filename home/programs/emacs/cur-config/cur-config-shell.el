;; ===Vterm===
(use-package vterm
  :ensure nil
  :commands (vterm cur/meow-vterm-other-window cur/meow-vterm)
  :bind (:map vterm-mode-map
         ("C-c C-k" . cur/vterm-kill)
         :map cur/sub-leader-keymap
         ("C-RET" . cur/meow-vterm-other-window)
         ("RET"   . cur/meow-vterm))
  :init
  (defun cur/meow-vterm ()
    (interactive)
    (vterm)
    (meow-insert-mode))
  (defun cur/meow-vterm-other-window ()
    (interactive)
    (vterm-other-window)
    (meow-insert-mode))
  (defun cur/vterm-kill ()
    (interactive)
    (when (equal (buffer-name) "*vterm*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer-and-window))))
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

;; ===Eat===
(use-package eat
  :config
  (eat-eshell-mode 1))

;; ===Eshell===
(use-package eshell)

(use-package cur-eshell
  :custom
  (eshell-prompt-regexp cur-eshell-prompt-regexp "Regex for custom eshell prompt")
  (eshell-prompt-function 'cur-eshell-prompt "Set custom prompt for eshell"))

;; ===Zoxide===
(use-package zoxide)

(provide 'cur-config-shell)
