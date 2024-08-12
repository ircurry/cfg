;;; cur-consult-el --- Extending and Customizing Consult -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'consult)

(defgroup cur-consult ()
  "Integrate Consult and Projectile."
  :group 'consult
  :prefix "cur-consult-")

(defun cur-consult-theme (theme)
  "Disable current themes and enable THEME from `consult-themes'.

If THEME is a list of symbols, go through and enable each theme in reverse
order.  The command supports previewing the currently selected theme."
  (interactive
   (list
    (let* ((regexp (consult--regexp-filter
                    (mapcar (lambda (x) (if (stringp x) x (format "\\`%s\\'" x)))
                            consult-themes)))
           (avail-themes (seq-filter
                          (lambda (x) (string-match-p regexp (symbol-name x)))
                          (cons 'default (custom-available-themes))))
           (saved-theme (if (< (length custom-enabled-themes) 2)
                            (car custom-enabled-themes)
                          custom-enabled-themes)))
      (consult--read
       (mapcar #'symbol-name avail-themes)
       :prompt "Theme: "
       :require-match t
       :category 'theme
       :history 'consult--theme-history
       :lookup (lambda (selected &rest _)
                 (setq selected (and selected (intern-soft selected)))
                 (or (and selected (car (memq selected avail-themes)))
                     saved-theme))
       :state (lambda (action theme)
                (pcase action
                  ('return (cur-consult-theme (or theme saved-theme)))
                  ((and 'preview (guard theme)) (cur-consult-theme theme))))
       :default (symbol-name (or (if (and saved-theme (listp saved-theme))
                                     (car (last saved-theme))
                                   saved-theme)
                                 'default))))))
  (when (or (eq theme 'default)
            (when (listp theme)
              (member 'default theme)))
    (setq theme nil))
  (cond ((symbolp theme)
         (unless (eq theme (car custom-enabled-themes))
           (mapc #'disable-theme custom-enabled-themes)
           (when theme
             (if (custom-theme-p theme)
                 (enable-theme theme)
               (load-theme theme :no-confirm)))))
        ((listp theme)
         (unless (equal theme custom-enabled-themes)
           (let ((themes (reverse theme)))
             (mapc #'disable-theme custom-enabled-themes)
             (when themes
               (mapc (lambda (theme)
                       (if (custom-theme-p theme)
                           (enable-theme theme)
                         (load-theme theme :no-confirm)))
                     themes)))))))

(provide 'cur-consult)
;;; cur-consult.el ends here
