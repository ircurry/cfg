;;; cur-popper.el --- Extending Popper's capabilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'popper)

(defgroup cur-popper ()
  "Base functions and variables for my Emacs configuration."
  :group 'popper)

(defcustom cur-popper-select-conditions '("\\*eat\\*" (derived-mode . eat-mode))
  "A list of `buffer-match-p' conditions that determine if a popped buffer is focused."
  :type '(repeat (choice (cons (choice (const derived-mode) (const major-mode)) symbol)
			 string))
  :group 'cur-popper)

(defcustom cur-popper-select-window 'dwim
  "Whether to select a popped buffer with `cur-popper-dispaly-buffer-dwim'."
  :type '(choice (const :tag "Always" t)
		 (const :tag "Sometimes" dwim)
		 (const :tag "Never" nil))
  :group 'cur-popper)

(defun cur-popper-display-buffer-dwim (buffer &optional alist)
  "Display BUFFER at the bottom of the frame and conditonally select the window.
ALIST is passed to `dispaly-buffer-at-bottom'."
  (let* ((nalist (append alist `((window-height . ,popper-window-height))))
	 (window (display-buffer-at-bottom buffer nalist)))
    (pcase cur-popper-select-window
      (`nil window)
      (`dwim (if (buffer-match-p `(or . ,cur-popper-select-conditions) (window-buffer window))
		 (select-window window)
	       window))
      (_ (select-window window)))))

(provide 'cur-popper)
;;; cur-popper.el ends here
