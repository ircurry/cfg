(use-package cur-window
  :config
  (setq window-sides-slots
        '(1 1 1 1))
  (setq display-buffer-alist
        '(("\\`\\*Async Shell Command.*\\'"
           (display-buffer-no-window))
          ("\\*Org Src.*"
           (display-buffer-same-window))
          ((or (derived-mode . compilation-mode)
               (derived-mode . geiser-mode)
               (derived-mode . grep-mode)
               (derived-mode . help-mode)
               (derived-mode . Info-mode)
               (derived-mode . rg-mode)
               (derived-mode . woman-mode))
           (cur-window-display-buffer-below-or-pop)
           (body-function . cur-window-select-fit-to-size))
	  ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ((or (derived-mode . occur-mode)
               (derived-mode . xref--xref-buffer-mode))
           (display-buffer-reuse-window
            display-buffer-below-selected)
           (dedicated . t)
           (body-function . cur-window-select-fit-to-size))
          ((or (derived-mode . justl-mode)
               "justl - .*")
           (display-buffer-reuse-window
            display-buffer-at-bottom)
           (dedicated . t)
           (window-height . 0.25))
	  ((or "\\*eshell .*"
               "\\*.*-eshell*"
               "\\*.*-eat\\*")
           (display-buffer-reuse-window
            display-buffer-at-bottom)
           (window-height . 0.25))
          ((or (derived-mode . vterm-mode)
               "\\*vterm.*\\*"
               "\\*.*-vterm\\*")
           (display-buffer-reuse-mode-window
            display-buffer-same-window))
	  (".*"
	   (display-buffer-same-window)))))

(provide 'cur-config-window)
