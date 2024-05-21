;; ===Default Font===
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono Nerd Font-11"))

;; ===Themes Path===
(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes"))

;; ===Doom Emacs Themes===
(use-package doom-themes
  :demand t
  :init
  (defun cur/theme-override ()
    "Change faces depending on what the value of `custom-enabled-themes' is."
    (cond ((member 'doom-gruvbox custom-enabled-themes)
           (set-face-attribute 'secondary-selection nil
                               :background "#504945"))
          ((member 'doom-flatwhite custom-enabled-themes)
           (set-face-attribute 'haskell-operator-face nil
                               :background (doom-color 'bg)
                               :foreground (doom-color 'fg))
           (set-face-attribute 'haskell-type-face nil
                               :background (doom-color 'fw-teal-blend)
                               :foreground (doom-color 'fw-teal-text))
           (set-face-attribute 'haskell-constructor-face nil
                               :background (doom-color 'fw-orange-blend)
                               :foreground (doom-color 'fw-orange-text)))
          (t t)))
  (defun cur/load-theme (theme)
    "Load THEME, disabling all other currently enabled themes. Then
check for overrides with `cur/theme-override'."
    (interactive
     (list
      (intern (completing-read "Cur Custom Themes: "
                               (mapcar #'symbol-name
                                       (custom-available-themes))))))
    (condition-case nil
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme theme t)
          (cur/theme-override))
      (error "Problem loading theme %s" theme)))
  :bind (:map cur/toggle-map
              ("C-t" . cur/load-theme)))

;; ===Autothemer===
(use-package autothemer)

;; ===Catppuccin Theme===
(use-package catppuccin-theme
  :after (autothemer))

;; ===Text Scaling===
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("f" nil "finished" :exit t)
  ("RET" nil "finished" :exit t))
(define-key cur/toggle-map (kbd "C-s") 'hydra-text-scale/body)

;; ===Ef-Themes===
(use-package ef-themes
  :config
  (cur/load-theme 'ef-tritanopia-dark))

;; ===Ibuffer Icons===
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ===Spacious Padding===
(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 8
          :header-line-width 0
          :mode-line-width 0
          :tab-width 0
          :right-divider-width 18
          :scroll-bar-width 0
          :fringe-width 0))
  (setq spacious-padding-subtle-mode-line nil)
  (spacious-padding-mode 1))

(provide 'cur-config-faces)
