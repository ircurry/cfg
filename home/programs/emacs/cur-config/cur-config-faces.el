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
     (doom-flatwhite (haskell-operator-face :background "#f7f3ee"
                                            :foreground "#605a52")
                     (haskell-type-face :background "#d2ebe3"
                                        :foreground "#465953")
                     (haskell-constructor-face :background "#f7e0c3"
                                               :foreground "#5b5143")
		     (completions-common-part :background "#dde4f2"
					      :foreground "#7382a0"
					      :weight 'bold)
		     (org-dispatcher-highlight :background "#f7e0c3"
					       :foreground "#957f5f")
		     (help-key-binding :background "#f7f3ee"
				       :foreground "#7382a0")
		     (fill-column-indicator :foreground "#b9a992")))))

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
