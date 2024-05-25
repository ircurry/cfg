(use-package cur-window
  :config
  (setq window-sides-slots
        '(1 1 1 1))
  (setq display-buffer-alist
        '(("\\*Org Src.*"
           (display-buffer-same-window))
          ("\\*Help.*"
           (display-buffer-reuse-window
            display-buffer-below-selected)
           (body-function . select-window))
          ("\\*Occur\\*"
           (display-buffer-reuse-window
            display-buffer-below-selected)
           (body-function . cur-window-select-fit-to-size))
          ((or  (derived-mode . eshell-mode)
                (derived-mode . vterm-mode)
                (derived-mode . eat-mode)
                (derived-mode . justl-mode)
                "justl - .*"
                "\\*\\(vterm\\|eshell.*\\)\\*")
           (display-buffer-in-side-window)
           (side . top)
           (slot . 0)
           (body-function . cur-window-select-fit-to-size)))))

;; ===Window Functions===
(defun cur/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun cur/split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun cur/other-window-reverse ()
  (interactive)
  (other-window -1))

(provide 'cur-config-window)
