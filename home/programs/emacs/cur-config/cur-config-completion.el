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
  :init
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :demand t
  :bind (("C-x b"           . consult-buffer)
         ("M-g i"           . consult-imenu)
         ("M-y"             . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map cur/projectile-map
         ("C-r"   . consult-ripgrep)
         ("C-b"   . consult-project-buffer)
         :map goto-map
         ("m" . consult-mark)
         ("M" . consult-global-mark)
         ("o" . consult-outline))
  :custom
  (consult-preview-allowed-hooks '(global-font-lock-mode
                                   save-place-find-file-hook
                                   ;; Dired
                                   dired-hide-details-mode
                                   hl-line-mode
                                   nerd-icons-dired-mode))
  :config
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

(provide 'cur-config-completion)
