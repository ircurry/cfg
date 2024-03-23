;;; cur-config-windows.el --- emacs windows configuration

;;; Commentary:

;;; Code:
;; ===Window Functions===
(defun cur/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun cur/split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun cur/other-window-reverse ()
  (interactive)
  (other-window -1))

(provide 'cur-config-windows)
;;; cur-config-windows.el ends here
