;; ===Tuareg===
(use-package tuareg
  :hook (tuareg-mode . merlin-mode)
  :defer t)

;; ===Utop===
;; (use-package utop
;;   :commands (utop utop-mode)
;;   :config
;;   (advice-add 'utop :around 'inheritenv-apply))

(use-package merlin
  :defer)

(use-package merlin-company
  :after (merlin))

(provide 'cur-config-ocaml)
