;;; cur-config-markup.el --- configurations for comman markup languages
;;; Commentary:
;;; Code:

;; ===YAML===
(use-package yaml-mode
  :ensure t
  :commands (yaml-mode))

;; ===EWW/Yuck Configuration===
(use-package yuck-mode
  :ensure t
  :commands (yuck-mode))

(provide 'cur-config-markup)
;;; cur-config-markup.el ends here