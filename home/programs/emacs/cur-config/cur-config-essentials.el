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
  :bind ( :map isearch-mode-map
	  ("M-s l" . isearch-toggle-lax-whitespace))
  :custom
  (isearch-wrap-pause 'no-ding)
  (isearch-repeat-on-direction-change t)
  (isearch-lazy-count t)
  (isearch-lax-whitespace t)
  (search-whitespace-regexp ".*?")
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  :config
  (setq isearch-regexp-lax-whitespace nil)) ; not a custom variable for some reason

(use-package emacs
  :custom (list-matching-lines-jump-to-current-line nil)
  :bind ( :map occur-mode-map
	  ("RET" . occur-mode-goto-occurrence-other-window)
	  :map search-map
	  ("o" . occur)))

(use-package grep
  :ensure nil
  :commands (grep lgrep rgrep)
  :custom
  (grep-save-buffers 'ask)
  :bind ( :map search-map
	  ("d" . lgrep)
	  ("r" . rgrep)
	  :map grep-mode-map
	  ("M-e" . compilation-next-file)
	  ("M-a" . compilation-previous-file))
  :config
  (let ((rg-found (executable-find "rg")))
    (setopt grep-template (if rg-found
			      "rg --no-heading -nH --null -e <R> <F>"
			      "grep <X> <C> -nH --null -e <R> <F>"))
    (setopt xref-search-program (if rg-found 'ripgrep 'grep))))

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
