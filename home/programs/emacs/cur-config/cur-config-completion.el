(use-package emacs
  :demand t
  :ensure nil
  :custom
  (minibuffer-follows-selected-frame nil "Display the minibuffer on a per-frame basis"))

(use-package minibuffer
  :demand t
  :config
  (setq completion-styles '(basic substring orderless))
  (setq completion-category-overrides
        '((file      (styles . (basic partial-completion orderless)))
          (kill-ring (styles . (emacs22 orderless))))))

(use-package orderless
  :demand t
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil))
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp)))

(use-package vertico
  :defer t
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind ( :map vertico-map
	  ("M-e" . vertico-next-group)
	  ("M-a" . vertico-previous-group))
  :init
  (vertico-mode 1))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode +1))

(use-package consult
  :defer t
  :bind (("C-x b"               . consult-buffer)
         ("M-g i"               . consult-imenu)
         ("M-y"                 . consult-yank-pop)
         ([remap goto-line]     . consult-goto-line)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap rg-project]    . consult-ripgrep)
         ([remap rg]            . consult-ripgrep)
         :map consult-narrow-map
         ("?" . consult-narrow-help)
         :map goto-map
         ("m" . consult-mark)
         ("M" . consult-global-mark)
         ("o" . consult-outline)
         :map search-map
         ("l" . consult-line)
         ("L" . consult-line-multi)
	 :map cur/register-map
	 ("C-s" . consult-register-store)
	 ("C-l" . consult-register-load)
	 ("C-v" . consult-register))
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
  :after project
  :defer t
  :bind ( :map project-prefix-map
	  ("b" . consult-project-buffer)))

(use-package consult
  :if (executable-find "rg")
  :defer t
  :bind ( :map search-map
	  ("i" . consult-ripgrep)))

(use-package consult
  :unless (executable-find "rg")
  :defer t
  :bind ( :map search-map
	  ("i" . consult-grep)))

(use-package consult
  :after (org)
  :defer t
  :bind ( :map org-mode-map
          ([remap consult-imenu] . consult-org-heading)))

(use-package cur-consult
  :after (consult)
  :demand t
  :custom
  (cur-override-theme-load-function #'cur-consult-theme))

(use-package consult-dir
  :defer t
  :bind ( :map global-map
	  ("C-x C-d" . consult-dir)
	  :map minibuffer-local-completion-map
	  ("C-x C-d" . consult-dir)
	  ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-dir
  :after vertico
  :defer t
  :bind ( :map vertico-map
	  ("C-x C-d" . consult-dir)
	  ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-dir
  :after meow
  :defer t
  :bind ( :map cur/sub-leader-keymap
	  ("C-d" . consult-dir)))

(use-package emabark
  :defer t
  :bind ( :map global-map
          ("C-." . embark-act)
          :map minibuffer-local-map
          ("C-." . embark-act)
          :map dired-mode-map
          ("'"  . embark-act)
          ("\"" . embark-dwim)))

(use-package corfu
  :defer t
  :bind
  (:map corfu-map
	("TAB"       . corfu-next)
	("<tab>"     . corfu-next)
	("S-TAB"     . corfu-previous)
	("<backtab>" . corfu-previous)
	("<escape>"  . corfu-quit) ; note: cannot bind "ESC" because it causes errors
	("M-SPC"     . corfu-insert-separator))
  :custom
  (corfu-auto t)
  (corfu-preview-current 'insert)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 3)
  (corfu-max-width 40)
  (corfu-popupinfo-delay '(2.0 . 1.0))
  (tab-always-indent 'complete)
  :hook
  (corfu-mode . corfu-popupinfo-mode))

(provide 'cur-config-completion)
