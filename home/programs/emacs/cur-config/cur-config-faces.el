;; ===Default Font===

(use-package fontaine
  :demand t
  :bind ( :map cur/toggle-map
	  ("C-s" . fontaine-set-preset))
  :custom
  (fontaine-presets '((regular)
		      (extra-small
		       :default-height 100)
		      (small
		       :default-height 107)
		      (large
		       :default-height 140)
		      (t
		       :default-family "JetBrainsMono Nerd Font"
		       :default-weight regular
		       :default-height 110

		       :fixed-pitch-family nil
		       :fixed-pitch-weight nil
		       :fixed-pitch-height 1.0

		       :fixed-pitch-serif-family nil
		       :fixed-pitch-serif-weight nil
		       :fixed-pitch-serif-height 1.0

		       :mode-line-active-height 1.0
		       :mode-line-inactive-height 1.0)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

;; ===Default Opacity===
(add-to-list 'default-frame-alist
             '(alpha-background . 85))

(use-package emacs
  :preface
  (defun cur/toggle-frame-opacity ()
    (interactive)
    (let ((current-alpha (frame-parameter nil 'alpha-background)))
      (cond ((eq current-alpha 100)
	     (set-frame-parameter nil 'alpha-background 85))
	    (t
	     (set-frame-parameter nil 'alpha-background 100)))))
  :bind ( :map cur/toggle-map
	  ("C-o" . cur/toggle-frame-opacity)))

;; ===Themes Path===
(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes"))

;; ===Theme Library===
(use-package cur-theme
  :bind ( :map cur/toggle-map
          ("C-t" . cur-override-theme-load-theme))
  :custom
  (cur-override-theme-overrides
   '((catppuccin (company-preview :foreground (catppuccin-color 'overlay0)
				  :background (if (eq catppuccin-flavor 'latte)
						  (catppuccin-darken (catppuccin-color 'base) 12)
						(catppuccin-lighten (catppuccin-color 'base) 17)))
		 (cur-mode-line-meow-state :background (catppuccin-color 'lavender)
					   :foreground (catppuccin-color 'base))
		 (cur-mode-line-major-mode-active :foreground (catppuccin-color 'blue) :weight 'bold)
		 (flycheck-info :underline (list :style 'wave :color (catppuccin-color 'green)))
		 (flycheck-warning :underline (list :style 'wave :color (catppuccin-color 'yellow)))
		 (flycheck-error :underline (list :style 'wave :color (catppuccin-color 'red))))
     (doom-gruvbox (secondary-selection :background "#504945")
		   (cur-mode-line-major-mode-active :foreground "#83a598"
						    :weight 'bold))
     (doom-oceanic-next (cur-mode-line-major-mode-active :foreground "#6699CC"
							 :weight 'bold))
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
		     (fill-column-indicator :foreground "#b9a992")
		     (org-block :foreground "#93836c"
				:background "#f1ece4")))))

;; ===Doom Emacs Themes===
(use-package doom-themes
  :demand t)
  ;; :config
  ;; (cur-override-theme-load-theme 'doom-flatwhite))

;; ===Autothemer===
(use-package autothemer)

;; ===Catppuccin Theme===
(use-package catppuccin-theme
  :after (autothemer cur-theme))

(cur-override-theme-load-theme 'catppuccin)

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
  (cur-mode-line-exclude-narrow-inidcator '(Info-mode))
  :config
  (setq-default mode-line-end-spaces
		'(""
		  cur-mode-line-flycheck-indicator
		  (:eval (when (cur-mode-line-flycheck-display-p) "  "))
		  cur-mode-line-eat-indicator
		  (:eval (when (cur-mode-line-eat-display-p) "  "))
		  cur-mode-line-eat-eshell-indicator
		  (:eval (when (cur-mode-line-eat-eshell-display-p) "  "))
		  cur-mode-line-major-mode-indicator
		  cur-mode-line-end-padding))
  (setq-default mode-line-format
		'("%e"
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
