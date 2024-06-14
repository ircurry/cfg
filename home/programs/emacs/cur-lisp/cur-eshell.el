;;; cur-eshell.el --- My exensions of the Emacs Shell

;;; Commentary:

;;; Code:

(require 'eshell)
(require 'files)
(require 'dired)

(defun cur-eshell-prompt ()
  "A minimal and colourful prompt for `eshell'.
Set `eshell-prompt-function' to this function to enable."
  (concat
   (propertize "[" 'face 'ansi-color-red)
   (propertize (eshell/whoami) 'face 'ansi-color-yellow)
   (propertize "@" 'face 'ansi-color-green)
   (propertize (system-name) 'face 'ansi-color-blue)
   " "
   (propertize (concat (eshell/pwd)) 'face 'ansi-color-magenta)
   (propertize "]" 'face 'ansi-color-red)
   (propertize "$ " 'face 'bold)))

(defun eshell/ff (&optional file)
  "Eshell alias to open FILE.
Will call `find-file' interactively if no file is specified."
  (cond (file
         (find-file file))
        (t
         (call-interactively 'find-file))))

(defun eshell/dir (&optional dir)
  "Eshell alias to open `dired' at DIR.
Will call `dired' on current directory if no directory is specified."
  (cond (dir
         (dired dir))
        (t
         (dired "."))))

(defvar cur-eshell-prompt-regexp
  "^\\[[^]]*\\]\\[?[[:digit:]]*\\]?[#$λ] "
  "Regex for the `cur-eshell-prompt' prompt.")

(provide 'cur-eshell)
;;; cur-base.el ends here
