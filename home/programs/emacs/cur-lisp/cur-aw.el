;;; cur-aw.el --- The extensions for ace-window -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ace-window)

(defgroup cur-aw ()
  "Base functions and variables for my Emacs configuration."
  :group 'windows)

(defun cur-aw--switch-buffer ()
  "Call `consult-buffer' if it is bound.
Call `switch-to-buffer' if it is not."
  (cond ((fboundp 'consult-buffer)
	 (call-interactively 'consult-buffer))
	(t
	 (call-interactively 'switch-to-buffer))))

(defun cur-aw-switch-buffer-in-window (window)
  "Switch to buffer in WINDOW.
Prefer the use of `consult-buffer'."
  (aw-switch-to-window window)
  (cur-aw--switch-buffer))

(provide 'cur-aw)
;;; cur-aw.el ends here
