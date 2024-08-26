;; ===which-key===
(use-package which-key
  :demand t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.0001))

;; ===Info===
(use-package info
  :config
  (setq meow-info-keymap (make-keymap))
  (meow-define-state info
    "meow state for interacting with Info"
    :lighter "INFO"
    :keymap meow-info-keymap)
  (meow-define-keys 'info
    '(":" . meow-page-up)
    '("?" . meow-page-down)
    '("," . beginning-of-buffer)
    '("." . end-of-buffer)
    ;;  '("a" . )
    ;;  '("b" . )
    ;;  '("c" . Info-follow-reference)
    ;;  '("d" . )
    ;;  '("e" . )
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
    ;;  '("o" . )
    '("p" . Info-prev)
    '("q" . meow-goto-line)
    '("r" . Info-follow-reference)
    ;;  '("s" . )
    '("t" . Info-toc)
    '("u" . Info-up)
    '("v" . meow-visit)
    ;;  '("w" . )
    ;;  '("x" . )
    '("y" . Info-copy-current-node-name)
    '("z" . info-display-manual)
    '("RET" . Info-follow-nearest-node)
    '("SPC" . meow-keypad)
    '("TAB" . Info-next-reference)
    '("<backtab>" . Info-prev-reference)
    '("<escape>" . keyboard-quit))
  (add-hook 'Info-mode-hook #'meow-info-mode))

(provide 'cur-config-help)
