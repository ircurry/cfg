;;; cur-config-shell.el --- accessing the shell in emacs

;;; Commentary:

;;; Code:
;; ===Compile Commands===
(use-package compile
  :bind (:map cur/leader-keymap
	 ("RET" . compile)))

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

(provide 'cur-config-shell)
;;; cur-config-shell.el ends here
