;;; cur-theme.el --- Helper functions for theming -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun cur-theme-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

(defun cur-theme-blend (color1 color2 alpha)
  "Blend COLOR1 with COLOR2 with ALPHA.
Both COLOR1 and COLOR2 must be strings in hexidecimal form, prepended
with a \"#\"."
  (if (and (string-prefix-p "#" color2) (string-prefix-p "#" color2))
      (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
             (cl-loop for it    in (cur-theme-name-to-rgb color1)
                      for other in (cur-theme-name-to-rgb color2)
                      collect (+ (* alpha it) (* other (- 1 alpha)))))
    (error "No \"#\" prefix for \"%s\" and \"%s\"" color2 color2)))

(defun cur-theme-lighten (color alpha)
  "Blend COLOR with \"#FFFFFF\" and ALPHA."
  (cur-theme-blend color "#FFFFFF" (- 1 alpha)))

(defun cur-theme-darken (color alpha)
  "Blend COLOR with \"#000000\" and ALPHA."
  (cur-theme-blend color "#000000" (- 1 alpha)))

(defmacro cur-theme-def (theme-name doc default-colors custom-colors faces)
  "Generate custom theme named THEME-NAME.
DOC is the documentation string.  DEFAULT-COLORS is a list of lists,
where the first element is a symbol and the second is a string.
The names should corespond to the default-colors schema.  CUSTOM-COLORS
is the same as DEFAULT-COLORS but with no schema.  FACES is a list
of faces."
  `(let* (,@default-colors
          ,@custom-colors)
     (deftheme ',theme-name
       ,doc)
     (custom-theme-set-faces
      ,theme-name
      ,@(cur-theme--expand-faces faces))))

(defun cur-theme--expand-faces (face-defs)
  "Take FACE-DEFS and return a list of faces that can be used by `custom-theme-set-faces'."
  (mapcar
   (lambda (face-def)custom
     (let* ((face (car face-def))
            (def  (cdr face-def)))
       `(list ',face ,`(list (list t (list ,@def))))))
   face-defs))

(provide 'cur-theme)
;;; cur-theme.el ends here
