;; ===Default Font===
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono Nerd Font-11"))

;; ===Default Opacity===
(add-to-list 'default-frame-alist
             '(alpha-background . 85))

;; ===Themes Path===
(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes"))

;; ===Theme Library===
(use-package cur-theme
  :bind ( :map cur/toggle-map
          ("C-t" . cur-override-theme-load-theme))
  :custom
  (cur-override-theme-overrides
   '((doom-gruvbox (secondary-selection :background "#504945"))
     (doom-flatwhite (haskell-operator-face :background (doom-color 'bg)
                                            :foreground (doom-color 'fg))
                     (haskell-type-face :background (doom-color 'fw-teal-blend)
                                        :foreground (doom-color 'fw-teal-text))
                     (haskell-constructor-face :background (doom-color 'fw-orange-blend)
                                               :foreground (doom-color 'fw-orange-text))))))

;; ===Doom Emacs Themes===
(use-package doom-themes
  :demand t
  :config
  (cur-override-theme-load-theme 'doom-tomorrow-night))

;; ===Autothemer===
(use-package autothemer)

;; ===Catppuccin Theme===
(use-package catppuccin-theme
  :after (autothemer cur-theme))
;; :config
;; (cur-override-theme-load-theme 'catppuccin))

;; ===Text Scaling===
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("f" nil "finished" :exit t)
  ("RET" nil "finished" :exit t))
(define-key cur/toggle-map (kbd "C-s") 'hydra-text-scale/body)

;; ===Ef-Themes===
(use-package ef-themes)
;; :config
;; (cur/load-theme 'ef-tritanopia-dark))

;; ===Ibuffer Icons===
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ===Spacious Padding===
(use-package spacious-padding
  :disabled t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 8
	   :header-line-width 0
	   :mode-line-width 0
	   :tab-width 0
	   :right-divider-width 18
	   :scroll-bar-width 0
	   :fringe-width 0))
  (setq spacious-padding-subtle-mode-line nil)
  (spacious-padding-mode 1))

(use-package doom-modeline)
  ;; :hook (emacs-startup . doom-modeline-mode))

(use-package cur-mode-line
  :custom
  (mode-line-right-align-edge 'right-fringe)
  :config
  (setq-default mode-line-end-spaces
		'(""
		  cur-mode-line-flycheck-indicator
		  (:eval (when (cur-mode-line-flycheck-display-p) "  "))
		  cur-mode-line-eat-indicator
		  (:eval (when (cur-mode-line-eat-display-p) "  "))
		  cur-mode-line-major-mode-indicator
		  (:eval (cur-mode-line--end-space-dwim 0))))
  (setq-default mode-line-format
		'("%e"
		  mode-line-client
                  cur-mode-line-kmacro-indicator
                  cur-mode-line-narrowed-indicator
                  "  "
                  cur-mode-line-buffer-status-indicator
		  (:eval (when (cur-mode-line-meow-display-p) "  "))
                  cur-mode-line-meow-state-indicator
                  "  "
		  cur-mode-line-buffer-name-indicator
		  (:eval (when (mode-line-window-selected-p) "  "))
		  cur-mode-line-postion-indicator
		  cur-mode-line-right-align
		  mode-line-end-spaces)))

(provide 'cur-config-faces)
