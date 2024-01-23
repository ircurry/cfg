;;; cur-config-help.el --- help configurations

;;; Commentary:

;;; Code:
;; ===which-key===
(use-package which-key
  :ensure t
  :demand t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 3))

;; ===helpful===
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; ===Info===
(use-package info
  :config
  (meow-define-state info
    "meow state for interacting with Info"
    :lighter "INFO"
    :keymap meow-dired-keymap)
  (meow-define-keys 'info
    '(":" . meow-page-up)
    '("?" . meow-page-down)
    '("," . beginning-of-buffer)
    '("." . end-of-buffer)
    ; '("a" . )
    ; '("b" . )
    ; '("c" . Info-follow-reference)
    ; '("d" . )
    ; '("e" . )
    '("f" . Info-menu)
    '("g" . keyboard-quit)
    '("h" . Info-backward-node)
    '("H" . meow-left)
    '("i" . Info-index)
    '("j" . meow-next)
    '("k" . meow-prev)
    '("l" . Info-forward-node)
    '("L" . meow-right)
    '("m" . meow-join)
    '("n" . Info-next)
    ; '("o" . )
    '("p" . Info-prev)
    '("q" . meow-goto-line)
    '("r" . Info-follow-reference)
    ; '("s" . )
    '("t" . Info-toc)
    '("u" . Info-up)
    '("v" . meow-visit)
    ; '("w" . )
    ; '("x" . )
    '("y" . Info-copy-current-node-name)
    ; '("z" . Info-goto-node)
    '("RET" . Info-follow-nearest-node)
    '("SPC" . meow-keypad)
    '("TAB" . Info-next-reference)
    '("<backtab>" . Info-prev-reference)
    '("<escape>" . keyboard-quit))
  (add-hook 'Info-mode-hook #'meow-info-mode))

(provide 'cur-config-help)
;;; cur-config-help.el ends here
