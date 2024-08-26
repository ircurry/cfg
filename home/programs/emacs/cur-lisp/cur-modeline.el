;;; cur/modeline.el --- My custom modeline -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This is my custom modeline.  This is here bascially because I don't want to
;; use doom modeline.

;;; Code:
(defgroup cur-modeline nil
  "My custom modeline that tries to be minimal."
  :group 'mode-line)

(defgroup cur-modeline-faces nil
  "The faces for my custom modeline."
  :group 'cur-modeline)

(defun cur-mode-line/padding ()
  "Function to return padding so `mode-line-end-spaces' will be right aligned."
  (let ((r-length (length (format-mode-line mode-line-end-spaces))))
    (propertize " "
                'display `(space :align-to (- right ,r-length)))))

(setq-default mode-line-format
              '("%e"
                (:eval
                 (when (and (mode-line-window-selected-p) defining-kbd-macro)
                   (propertize "  KMACRO  " 'face 'font-lock-string-face)))
                (:eval
                 (when (and (mode-line-window-selected-p)
                            (buffer-narrowed-p)
                            (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
                   (propertize "  NARROWED  " 'face 'font-lock-constant-face)))
                (:eval
                 (when (and (member 'meow features) (mode-line-window-selected-p))
                   (propertize (format "  %s  " (upcase (symbol-name meow--current-state)))
                               'face 'highlight)))
                "  "
                (:eval
                 (when (mode-line-window-selected-p)
                   (cond (buffer-read-only
                          (propertize "RO" 'face 'shadow))
                         ((buffer-modified-p)
                          (propertize "**" 'face 'shadow))
                         (t
                          (propertize "RW" 'face 'shadow)))))
                "  "
                (:eval
                 (propertize (format "%s" (buffer-name)) 'face 'bold))
                "  "
                (:eval
                 (if (mode-line-window-selected-p)
                     (propertize (capitalize (symbol-name major-mode)) 'face 'warning)
                   (propertize (capitalize (symbol-name major-mode)) 'face 'shadow)))
                (:eval (cur-mode-line/padding))))

(provide 'cur-modeline)
;;; cur-modeline.el ends here
