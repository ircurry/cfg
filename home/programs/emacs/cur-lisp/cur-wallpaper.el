;;; cur-wallpaper.el --- Setting wallpaper through dired -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dired)
(require 'image-dired)

(defun cur-wallpaper-set-wallpaper (arg)
  "Set wallpaper as ARG."
  (interactive (list (image-dired-original-file-name)))
  (call-process-shell-command
   (concat "swww img -t any " arg " &")))

(defun cur-wallpaper-set-wallpaper-dired (arg)
  "Set wallpaper as ARG."
  (interactive (list (dired-get-filename)))
  (call-process-shell-command
   (concat "swww img -t any " arg " &")))

(provide 'cur-wallpaper)
;;; cur-wallpaper.el ends here
