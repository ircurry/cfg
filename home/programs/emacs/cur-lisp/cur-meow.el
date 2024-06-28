;;; cur-meow.el --- Extending Meow's Capabilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'meow)

(defun cur-meow-toggle-temp-normal-motion ()
  "Toggle normal and motion mode.
If in neither of the two states, return nil."
  (interactive)
  (cond ((meow-normal-mode-p) (call-interactively 'meow-motion-mode))
        ((meow-motion-mode-p) (call-interactively 'meow-normal-mode))
        (t nil)))

(provide 'cur-meow)
;;; cur-meow.el ends here
