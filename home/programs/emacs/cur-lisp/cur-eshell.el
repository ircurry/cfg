;;; cur-eshell.el --- My exensions of the Emacs Shell -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'eshell)
(require 'files)
(require 'dired)

(defun cur-eshell-prompt ()
  "A minimal and colourful prompt for `eshell'.
Set `eshell-prompt-function' to this function to enable."
  (let* ((red     (face-foreground 'ansi-color-red))
         (green   (face-foreground 'ansi-color-green))
         (yellow  (face-foreground 'ansi-color-yellow))
         (blue    (face-foreground 'ansi-color-blue))
         (magenta (face-foreground 'ansi-color-magenta))
         (cyan    (face-foreground 'ansi-color-cyan))
         (white   (face-foreground 'ansi-color-white)))
    (concat
     (propertize "["                   'face `(:weight bold :foreground ,red))
     (propertize (eshell/whoami)       'face `(:weight bold :foreground ,yellow))
     (propertize "@"                   'face `(:weight bold :foreground ,green))
     (propertize (system-name)         'face `(:weight bold :foreground ,blue))
     " "
     (propertize (concat (abbreviate-file-name (eshell/pwd))) 'face `(:weight bold :foreground ,magenta))
     (propertize "]"                   'face `(:weight bold :foreground ,red))
     (propertize "λ "                  'face `(:weight bold :foreground ,white)))))

(defun eshell/i (fn)
  "Call FN interactively.
FN should be a string."
  (let ((user-fn (intern-soft fn)))
    (cond ((and user-fn (commandp user-fn))
	   (call-interactively user-fn))
	  (user-fn
	   (user-error "Function is not an interactive function"))
	  (t
	   (user-error "There was no function with that name")))))

(defun eshell/ff (&optional file)
  "Eshell alias to open FILE.
Will call `find-file' interactively if no file is specified."
  (cond (file
         (find-file file))
        (t
         (call-interactively 'find-file))))

(defun eshell/lf (&optional dir)
  "Eshell alias to open `dired' at DIR.
Will call `dired' on current directory if no directory is specified."
  (cond (dir
         (dired dir))
        (t
         (dired "."))))

(defvar cur-eshell-prompt-regexp
  "^\\[.*?@.*? [/~].*\\]?[#$λ] "
  "Regex for the `cur-eshell-prompt' prompt.")

(provide 'cur-eshell)
;;; cur-base.el ends here
