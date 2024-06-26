;;; cur-vterm.el --- Customization and helper functions for vterm

;;; Commentary:

;;; Code:

(require 'vterm)

(defun cur-vterm-enter-password ()
  (declare (interactive-only t))
  (interactive)
  (unless (equal major-mode 'vterm-mode)
    (user-error "Not in a VTerm Terminal"))
  (vterm-insert (read-passwd "Password: "))
  (vterm-send-return))

(provide 'cur-vterm)
;;; cur-vterm.el ends here
