(use-package pcomplete
  :ensure nil
  :custom (pcomplete-termination-string ""))

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
  :after (vterm)
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
	  ("C-c C-<return>" . eat-send-password))
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-directory-tracking t))

(use-package eat
  :after meow
  :hook
  (eat--char-mode . (lambda (&rest _)
		      (if eat--char-mode
			  (meow-mode -1)
			(unless meow-mode
			  (meow-mode +1))))))

(use-package eat
  :after eshell
  :demand t
  :hook (eat-eshell-exec . (lambda (&rest _) (eat-eshell-emacs-mode)))
  :bind ( :map eat-eshell-emacs-mode-map
	  ("C-c C-RET" . eat-send-password)
	  ("C-c C-<return>" . eat-send-password)
	  :map eat-eshell-semi-char-mode-map
	  ("C-c C-RET" . eat-send-password)
	  ("C-c C-<return>" . eat-send-password))
  :custom
  (eshell-visual-commands nil "nil because using `eat-eshell-mode'")
  (eshell-visual-subcommands nil "nil because using `eat-eshell-mode'")
  :config
  (eat-eshell-mode 1))

(use-package corfu
  :after (eat)
  :hook (eat-mode . (lambda (&rest _)
		      (setq-local corfu-auto nil)
		      (setq-local corfu-quit-at-boundary nil)
		      (corfu-mode +1))))

;; ===Eshell===
(use-package eshell
  :bind ( :map cur/sub-leader-keymap
          ("C-e" . eshell)))

(use-package corfu
  :after (eshell)
  :hook
  (eshell-mode . (lambda (&rest _)
		   (setq-local corfu-auto nil)
		   (setq-local corfu-quit-at-boundary nil)
		   (corfu-mode +1))))

(use-package eshell-syntax-highlighting
  :after eshell
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package cur-eshell
  :after (eshell)
  :demand t
  :custom
  (eshell-prompt-function 'cur-eshell-prompt "Set custom prompt for eshell")
  :config
  (setopt eshell-prompt-regexp cur-eshell-prompt-regexp))

;; ===MisTTY===
(use-package mistty
  :bind ( :map project-prefix-map
	  ("t" . mistty-in-project)
	  :map cur/sub-leader-keymap
	  ("C-t" . mistty))
  :custom (mistty-shell-command "fish"))

(use-package cur-mistty
  :after (mistty)
  :bind ( :map mistty-mode-map
	  ("C-c C-RET" . cur-mistty-send-password)
	  ("C-c C-<return>" . cur-mistty-send-password)))

(provide 'cur-config-shell)
