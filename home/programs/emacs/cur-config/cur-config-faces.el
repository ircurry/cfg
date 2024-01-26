;;; cur-config-faces.el --- faces configuration

;;; Commentary:

;;; Code:
;; ===Default Font===
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono Nerd Font-11"))
;; (add-to-list 'default-frame-alist
;;              '(font . "Iosevka Term-11"))

;; Themes Path
(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes"))

;; ===Doom Emacs Themes===
(use-package doom-themes
  ;:ensure t
  :demand t
  :init
  (defun cur/meow-theme-tweaker ()
    (interactive)
    (counsel-load-theme)
    (cond ((equal custom-enabled-themes '(doom-gruvbox))
           (set-face-attribute 'secondary-selection nil :background "#504945"))
          (t t)))
  :bind (:map cur/leader-keymap
	 ("t C-t" . cur/meow-theme-tweaker))
  :config
  (load-theme 'doom-nord-aurora t))

;; ===Autothemer===
(use-package autothemer
  ;:ensure t
  )

;; ===Catppuccin Theme===
(use-package catppuccin-theme
  ;:ensure t
  )

;; (use-package ewal
;;   :ensure t)
;; 
;; (use-package ewal-doom-themes
;;   :ensure t)

;; ===Text Scaling===
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("f" nil "finished" :exit t)
  ("RET" nil "finished" :exit t))
(define-key cur/leader-keymap (kbd "t C-s") 'hydra-text-scale/body)

(provide 'cur-config-faces)
;;; cur-config-faces.el ends here
