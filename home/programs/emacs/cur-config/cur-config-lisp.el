;; ===rainbow-delimiters===
(use-package rainbow-delimiters
  ;:ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ===Paredit===
(use-package paredit
  :config
  (paredit-mode 1))

;; ===Geiser===

(provide 'cur-config-lisp)