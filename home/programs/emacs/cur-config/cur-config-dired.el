;;; cur-config-dired.el --- configurations for dired

;;; Commentary:

;;; Code:
;; ===Dired===
(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map cur/sub-leader-keymap
	 ("C-d" . dired))
  :config
  (setq meow-dired-keymap (make-keymap))
  (meow-define-state dired
    "meow state for interacting with dired"
    :lighter "DIRED"
    :keymap meow-dired-keymap)
  (meow-define-keys 'dired
    '("d" . dired-flag-file-deletion)
    '("h" . dired-up-directory)
    '("j" . meow-next)
    '("k" . meow-prev)
    '("l" . dired-find-file)
    '("m" . dired-mark)
    '("q" . meow-goto-line)
    '("s" . dired-do-flagged-delete)
    '("v" . meow-visit)
    '("x" . dired-mark)
    '("SPC" . meow-keypad)
    '("<escape>" . ignore))
  (add-hook 'dired-mode-hook #'meow-dired-mode))

(provide 'cur-config-dired)
;;; cur-config-dired.el ends here
