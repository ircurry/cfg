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
  :demand t
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
                                                         :weight 'bold)
                        (elpher-gemini-heading1 :foreground "#ec5f67" :height 1.8 :inherit 'bold)
                        (elpher-gemini-heading2 :foreground "#6699cc" :height 1.4 :inherit 'bold)
                        (elpher-gemini-heading3 :foreground "#c594c5" :height 1.2 :inherit 'bold)
			(elpher-gemini-preformatted :foreground "#65737e" :inhert 'fixed-pitch)
			(hl-line :background "#343D46"))
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
                                :background "#f1ece4"))
     (doom-nord (mode-line :background "#3b4252")
                (mode-line-inactive :foreground "#88c0d0"
                                    :background "#3b4252")
                (hl-line :background "#3b4252")
                (font-lock-comment-face :foreground "#4c566a")
                (org-block-begin-line :foreground "#d8dee9"
                                      :background "#3b4252"
                                      :inherit 'org-block
                                      :extend t)
                (org-block :background "#2e3440")
                (dired-broken-symlink :foreground "#ebcb8b"
                                      :background "#bf616a")
                (meow-beacon-fake-selection :foreground "#d8dee9"
                                            :weight 'bold)
                (secondary-selection :background "#3b4252")
                (vertical-border :foreground "#3b4252"
                                 :background "#3b4252")
                (help-key-binding :foreground "#88c0d0"
                                  :background "#2e3440"
                                  :box '(:line-width 1 :color "#4c566a")
                                  :inherit 'fixed-pitch)
                (corfu-default :foreground "#eceff4"
                               :background "#3b4252")
                (corfu-current :foreground "#eceff4"
                               :background "#434c5e")
                (aw-leading-char-face :foreground "#bf616a" :height 1.3)
                (aw-background-face :foreground "#4c566a"))
     (doom-tomorrow-night (mode-line-inactive :background "#0f1011")))))

;; ===Doom Emacs Themes===
(use-package doom-themes)

;; ===Autothemer===
(use-package autothemer)

;; ===Catppuccin Theme===
(use-package catppuccin-theme
  :after (autothemer cur-theme))

(cur-override-theme-load-theme 'catppuccin)

;; ===Ef-Themes===
(use-package ef-themes)

;; ===Ibuffer Icons===
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;; Code:

(use-package cur-mode-line
  :demand t
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

(use-package page-break-lines)

(provide 'cur-config-faces)
