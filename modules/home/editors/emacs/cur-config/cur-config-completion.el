;;; cur-config-completion.el --- configurations emacs completion

;;; Commentary:
;; This file does not contain code completion.  Rather, this
;; is my configuration for Emacs' completion system and
;; the main interfaces that use them.  For code completion
;; please go see `cur-config-ide.el' and company mode.

;;; Code:
;; ===Counsel==
(use-package counsel
  :ensure t
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; ===Swiper===
(use-package swiper
  :ensure t
  :commands (swiper))

;; ===Ivy===
(use-package ivy
  :ensure t
  :demand t
  :after (counsel)
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :after (counsel ivy)
  :config
  (ivy-rich-mode 1))

(provide 'cur-config-completion)
;;; cur-config-completion.el ends here
