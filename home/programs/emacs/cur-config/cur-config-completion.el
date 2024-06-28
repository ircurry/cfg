(use-package minibuffer
  :config
  (setq completion-styles '(basic substring orderless))
  (setq completion-category-overrides
        '((file      (styles . (basic partial-completion orderless)))
          (kill-ring (styles . (emacs22 orderless))))))

(use-package orderless
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil))
  :config
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp)))

(use-package vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package vertico-posframe
  :config
  (vertico-posframe-mode 1))

(use-package consult
  :demand t
  :bind (("C-x b" . consult-buffer)
         ("M-g i" . consult-imenu)
         ("M-y"   . consult-yank-pop)))

(provide 'cur-config-completion)
