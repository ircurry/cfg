;; ===Gemini-Mode===
(use-package gemini-mode
  :defer t)

;; ===Org Gemini Exporter==
(use-package ox-gemini
  :defer t)

;; ===Elpher Gemini/Gopher Client===
(use-package elpher
  :defer t
  :custom
  (elpher-default-url-type "gemini"))

(provide 'cur-config-smol-net)
