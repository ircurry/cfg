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

;; ===Window Keybindings===
(define-key cur/leader-keymap (kbd "j") 'other-window)
(define-key cur/leader-keymap (kbd "k") 'cur/other-window-reverse)
(define-key cur/leader-keymap (kbd "1") 'delete-other-windows)
(define-key cur/leader-keymap (kbd "2") 'cur/split-and-follow-horizontally)
(define-key cur/leader-keymap (kbd "3") 'cur/split-and-follow-vertically)
(define-key cur/leader-keymap (kbd "0") 'delete-window)

(provide 'cur-config-windows)
;;; cur-config-windows.el ends here
