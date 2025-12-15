;;; cur-window.el --- Customization and helper functions for window management -*- lexical-binding: t -*-

;;; Commentary:
;; Custom functions and variables for managing windows.  In particular, setting `display-buffer-alist'
;; so that windows are just placed how I want.
;;
;; Much of this was shamelessly stolen from Protesilaos Stavrou, who greatly inspired much of my config.
;; Please see https://protesilaos.com/emacs/dotemacs#h:35b8a0a5-c447-4301-a404-bc274596238d for his window
;; management section of his Emacs config.

;;; Code:

(defun cur-window--window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

(defun cur-window--three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

(defun cur-window--get-display-buffer-below-or-pop ()
  "Return list of functions for `cur-window-display-buffer-below-or-pop'."
  (list
   #'display-buffer-reuse-mode-window
   (if (or (cur-window--window-small-p)
           (cur-window--three-or-more-windows-p))
       #'display-buffer-below-selected
     #'display-buffer-pop-up-window)))

(defun cur-window-display-buffer-below-or-pop (&rest args)
  "Display buffer below current window or pop a new window.
The criterion for choosing to display the buffer below the
current one is a non-nil return value for
`prot-common-window-small-p'.

Apply ARGS expected by the underlying `display-buffer' functions.

This as the action function in a `display-buffer-alist' entry."
  (let ((functions (cur-window--get-display-buffer-below-or-pop)))
    (catch 'success
      (dolist (fn functions)
        (when (apply fn args)
          (throw 'success fn))))))

(defvar cur-window-window-sizes
  '( :max-height (lambda () (floor (frame-height) 3))
     :min-height 10
     :max-width (lambda () (floor (frame-width) 3))
     :min-width 20)
  "This is a property list of max and min values for window sizes.
This value is used by `cur-window-select-fit-to-size' to set the
size of the window.")

(defun cur-window--get-window-size (key)
  "Use KEY to extract the value from `cur-window-window-sizes'.
If the value is a number return it, if it is a function, call
it and return it's value."
  (when-let ((value (plist-get cur-window-window-sizes key)))
    (cond
     ((functionp value)
      (funcall value))
     ((numberp value)
      value)
     (t
      (error "The value of %s is not a number or function" key)))))

(defun cur-window-select-fit-to-size (window)
  "Select WINDOW and fit it to the buffer.
The minimum and maximum height and width is determined by
`cur-window-window-sizes'."
  (select-window window)
  (fit-window-to-buffer
   window
   (cur-window--get-window-size :max-height)
   (cur-window--get-window-size :min-height)
   (cur-window--get-window-size :max-width)
   (cur-window--get-window-size :min-width)))

(defvar cur-window-same-window-modes-list
  '()
  "List of major modes that should have their window reused.
The function `cur-window-display-buffer-below-or-pop' uses this variable to
determine if it will create a new window or reuse the current one.")

(defun cur-window--get-display-buffer-same-window-or-below ()
  "Return a list of functions for `cur-window-display-buffer-same-window-or-below'."
  (list
   #'display-buffer-reuse-window
   (if (memq major-mode cur-window-same-window-modes-list)
       #'display-buffer-same-window
     #'display-buffer-below-selected)))

(defun cur-window-display-buffer-same-window-or-below (&rest args)
  "Display buffer either reusing the current window or bellow."
  (let ((functions (cur-window--get-display-buffer-same-window-or-below)))
    (catch 'success
      (dolist (fn functions)
        (when (apply fn args)
          (throw 'success fn))))))

(provide 'cur-window)
;;; cur-window.el ends here
