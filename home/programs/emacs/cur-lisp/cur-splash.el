;;; cur-spash --- A relatviely small and customizable splash screen for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;; Setting up the Customization Group

(defgroup cur-splash ()
  "An extensible splash screen for Emacs."
  :group 'environment
  :prefix "cur-splash-")

;;;;; Text Insertion Functions

(defvar-local cur-splash-center-text-p t
  "Boolean that controls if text insertion is centered.")

(cl-defun cur-splash-append-newline-optional ()
  "Append newline to `current-buffer' if last character isn't already a newline."
  (goto-char (point-max))
  (let ((last-char (char-after (1- (point-max)))))
    (if (not (eql ?\n last-char))
	(insert ?\n))))

(cl-defun cur-splash--center-padding-amount (length)
  "Return number of spaces to center a string of LENGTH based on `fill-column'.
If LENGTH is odd it rounds it up to the nearest even integer.  The function
assumes the value of `fill-column' is the correct value of the current window."
  (let ((length length))
    (when (cl-oddp fill-column)
      (setq length (1+ length)))
    (let* ((odd-width-right-shift (if (cl-oddp fill-column) 1 0))
	   (half-width (/ fill-column 2))
	   (half-string (- (/ length 2) odd-width-right-shift)))
      (if (< half-string half-width) (- half-width half-string) 0))))

(defun cur-splash--with-padding (string n)
  "Return STRING with N padding prepended to it."
  (concat (make-string n ?\s) string))

(cl-defun cur-splash-append-text (string &optional (newline t))
  "Append STRING to a the `current-buffer'.
NEWLINE determines if an optional newline is added with
`cur-splash-append-newline-optional'.  If variable `cur-splash-center-text-p' is
non-nil the string will be centered by treating the text as a whole and adding
enough padding to center the longest line, keeping the relative distance between
characters."
  (goto-char (point-max))
  (let* ((string string)
	 (string-lines (string-lines string nil t))
	 (padding 0)
	 (longest-line-length 0))
    (when cur-splash-center-text-p
      (dolist (line string-lines)
	(when (length> line longest-line-length)
	  (setq longest-line-length (length line))))
      (setq padding (cur-splash--center-padding-amount longest-line-length)))
    (insert (apply 'concat (mapcar (lambda (string)
				     (cur-splash--with-padding string padding))
				   string-lines)))
    (when newline (cur-splash-append-newline-optional))))

;;;;; `cur-splash-header' Functions and Variables

(defcustom cur-splash-header-contents "Greetings"
  "The contents \\='cur-splash-header will return.
It can be either a string or nil.  If it is nil no header will be inserted into
the splash screen buffer."
  :type '(choice (const nil) string)
  :group 'cur-splash)

(defun cur-splash-header ()
  "Return a string that will be inserted into the splash buffer.
This string is from \\='cur-splash-header-contents."
  (cond ((not cur-splash-header-contents) (list ""))
	((stringp cur-splash-header-contents) (list cur-splash-header-contents))))

;;;;; `cur-splash-subheader' Functions and Variables

(defcustom cur-splash-subheader-functions
  '(cur-splash-subheader-os cur-splash-subheader-init-time)
  "."
  :type '(repeat function)
  :group 'cur-splash-mode)

;;;;; `cur-splash-main' Functions and Variables

(cl-defstruct (cur-splash-menu-title
	       (:conc-name cur-splash-menu-title-)
	       (:constructor cur-splash-menu-title-make
			     (title-string &key face center))
	       (:copier cur-splash-menu-title-copy))
  "The title info for a menu."
  (title-string "")
  (face 'cur-splash-menu-title-face)
  (center t))

(cl-defstruct (cur-splash-menu-item
	       (:conc-name cur-splash-menu-item-)
	       (:constructor cur-splash-menu-item-make
			     (command &key face keymap display-binding command-string center))
	       (:copier nil))
  "The title info for a menu."
  (command nil)
  (face 'cur-splash-menu-title-face)
  (keymap nil)
  (display-binding t)
  (command-string nil)
  (center t))

(cl-defun cur-splash-menu-title-string (title &optional (face 'cur-splash-menu-title-face))
  "Propertize the string TITLE with FACE."
  (propertize title 'face face))

(cl-defun cur-splash-linear-menu-title-insert (title &key (center cur-splash-center-text-p) (width fill-column))
  "Insert menu title TITLE.
CENTER and WIDTH will override `cur-splash-center-text-p' and `fill-column'
respectively."
  (let ((fill-column width)
	(cur-splash-center-text-p center))
    (cur-splash-append-text title)))

(cl-defun cur-splash--menu-item-keybinding)

(cl-defun cur-splash-menu-item-string
    (command &key
	     (keymap nil)
	     (title nil)
	     (display-binding t)
	     (comand-string nil)
	     (face 'cur-splash-menu-item-face))
  "Display COMMAND as menu item.")

(cl-defun cur-splash-insert-menu (menu &optional))

(cur-splash-menu
 '(("Org Commands" :center nil :face my-face)
   (command)))

(cur-splash-menu
 :function-output (function-symbol :center t :face face-symbol)
 :text (text :center t :face face-symbol)
 :menu-grid
 (:column
  (:menu ("Commands" :center t :face face-symbol)
	 (command       :keymap key-map :title "." :display-binding t :command-string nil :face face-symbol)
	 (other-command :keymap key-map :title "." :display-binding t :command-string nil :face face-symbol))
  (:menu ("Org Commands" :center t :face face-symbol)
	 (org-agenda    :keymap key-map :title "." :display-binding t :command-string nil :face face-symbol)
	 (other-command :keymap key-map :title "." :display-binding t :command-string nil :face face-symbol))
  :column
  (:menu ("Commands" :center t :face face-symbol)
	 (command       :keymap key-map :title "." :display-binding t :command-string nil :face face-symbol)
	 (other-command :keymap key-map :title "." :display-binding t :command-string nil :face face-symbol))
  (:menu ("Org Commands" :center t)
	 (org-agenda    :keymap key-map :title "." :display-binding t :command-string nil :face face-symbol)
	 (other-command :keymap key-map :title "." :display-binding t :command-string nil :face face-symbol))))

(defun cur-splash-subheader ()
  "Return  of strings to be inserted into."
  (mapcar #'funcall cur-splash-subheader))

;;;;; `cur-splash-mode'

(defvar cur-splash-mode-map
  ())

(define-derived-mode cur-splash-mode fundamental-mode "Splash Screen"
  "Major mode for simple splash screen."
  :group 'cur-splash-mode
  (setq-local buffer-read-only t
	      inhibit-read-only nil))

;;;; Spawning the Splash Screen

(defcustom cur-splash-text-functions
  '(cur-splash-header cur-splash-subheader cur-splash-main cur-splash-footer)
  "."
  :type '(repeat function)
  :group 'cur-splash)
