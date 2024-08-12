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
  :bind (("C-x b"               . consult-buffer)
         ("M-g i"               . consult-imenu)
         ("M-y"                 . consult-yank-pop)
         ([remap goto-line]     . consult-goto-line)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap rg-project]    . consult-ripgrep)
         ([remap rg]            . consult-ripgrep)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         :map consult-narrow-map
         ("?" . consult-narrow-help)
         :map goto-map
         ("m" . consult-mark)
         ("M" . consult-global-mark)
         ("o" . consult-outline)
         :map search-map
         ("/" . consult-line)
         ("?" . consult-line-multi))
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

(use-package consult
  :after (org)
  :bind ( :map org-mode-map
          ([remap consult-imenu] . consult-org-heading)))

(use-package cur-consult
  :after (consult)
  :custom
  (cur-override-theme-load-function #'cur-consult-theme))

(provide 'cur-config-completion)

(use-package emabark
  :bind ( :map global-map
          ("C-." . embark-act)
          :map minibuffer-local-map
          ("C-." . embark-act)
          :map dired-mode-map
          ("'"  . embark-act)
          ("\"" . embark-dwim)))
