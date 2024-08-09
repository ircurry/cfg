;; ===Parens===
(use-package paren
  :ensure nil
  :demand t
  :custom
  (show-paren-delay 0 "No delay for paren highlighting"))

;; ===Line Numbers===
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)
;; Disable line numbers for terminal modes
(dolist (mode '(bookmark-bmenu-mode-hook
                org-mode-hook
                dired-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                eat-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
  :demand t
  :custom
  (isearch-wrap-pause 'no-ding)
  (isearch-repeat-on-direction-change t)
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(use-package proced
  :ensure nil
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

(provide 'cur-config-essentials)
