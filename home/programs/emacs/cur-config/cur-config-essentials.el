;; ===Parens===
(use-package paren
  :ensure nil
  :demand t
  :custom
  (show-paren-delay 0 "No delay for paren highlighting"))

;; ===Line Numbers===
(use-package emacs
  :custom
  (display-line-numbers-type t)
  :config
  (column-number-mode 1)
  (global-display-line-numbers-mode 1)
  ;; Disable line numbers for terminal modes
  (dolist (mode '(bookmark-bmenu-mode-hook
                  org-mode-hook
                  dired-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  eat-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode -1)))))

;; ===Bookmarks===
(use-package bookmark
  :commands (bookmark-set
             bookmark-set-no-overwrite
             bookmark-jump
             bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-save-flag 1))

(use-package recentf
  :hook (emacs-startup . recentf-mode))

(use-package isearch
  :ensure nil
  :demand t
  :custom
  (isearch-wrap-pause 'no-ding)
  (isearch-repeat-on-direction-change t)
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(use-package grep
  :ensure nil
  :commands (grep lgrep rgrep)
  :custom
  (grep-save-buffers 'ask)
  (grep-template "grep <X> <C> -nH --null -e <R> <F>")
  :bind ( :map search-map
	  ("d" . lgrep)
	  ("r" . rgrep)
	  :map grep-mode-map
	  ("M-e" . compilation-next-error)
	  ("M-a" . compilation-previous-error)))

(use-package proced
  :ensure nil
  :if (eq system-type 'gnu/linux)
  :commands (proced)
  :custom
  (proced-auto-update-flag t)
  (proced-enable-color-flag t)
  (proced-auto-update-interval 2)
  (proced-descend t)
  (proced-filter 'user))

(use-package server
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(use-package tmr
  :defer t)

;; ===which-key===
(use-package which-key
  :demand t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))

(provide 'cur-config-essentials)
