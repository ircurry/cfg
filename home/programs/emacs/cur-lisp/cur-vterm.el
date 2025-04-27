;;; cur-vterm.el --- Customization and helper functions for vterm

;;; Commentary:

;;; Code:

(require 'vterm)
(require 'auth-source)

(defun cur-vterm-enter-password ()
  (declare (interactive-only t))
  (interactive)
  (unless (equal major-mode 'vterm-mode)
    (user-error "Not in a VTerm Terminal"))
  (vterm-insert (read-passwd "Password: "))
  (vterm-send-return))

(defun cur-vterm-project (&optional other-window)
  (interactive)
  (if-let* ((current-project (project-current))
            (default-directory (project-root current-project))
            (buffer-name (format "*%s-%s*" (project-name current-project) "vterm")))
      (if (buffer-live-p (get-buffer buffer-name))
          (if other-window
              (switch-to-buffer-other-window buffer-name)
            (switch-to-buffer buffer-name))
        (if other-window
            (vterm-other-window buffer-name)
          (vterm buffer-name)))
    (user-error "Cannot create vterm buffer in %s" (cdr current-project))))

(defun cur-vterm-project-other-window ()
  (interactive)
  (cur-vterm-project t))

(provide 'cur-vterm)
;;; cur-vterm.el ends here
