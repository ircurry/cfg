;; ===Tuareg===
(use-package tuareg
  :hook (tuareg-mode . merlin-mode)
  :defer t)

;; ===Utop===
(use-package utop
  :defer t)

(use-package merlin
  :defer)

(use-package merlin-company
  :after (merlin))

(provide 'cur-config-ocaml)
