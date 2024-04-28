;; ===Gemini-Mode===
(use-package gemini-mode)

;; ===Org Gemini Exporter==
(use-package ox-gemini)

;; ===Elpher Gemini/Gopher Client===
(use-package elpher
  :custom
  (elpher-default-url-type "gemini"))

(provide 'cur-config-smol-net)
