;;; cur-theme.el --- Helper functions for theming -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

(defgroup cur-theme nil
  "A themeing library for creating simple and extensible themes."
  :group 'faces)

(defgroup cur-override-theme nil
  "A theme that will conditionally override aspects of your current theme."
  :group 'cur-theme)

(defvar cur-theme--default-face-defs
  '(())
  "The default definitions for theme faces.")

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

(defvar cur-theme--schema-names
  '(base00
    base01
    base02))

(defun cur-theme--check-default-schema (colors)
  "Take COLORS and check if it has all of the default schema values defined.
The names of the default schema are in `cur-theme--schema-names'.
Note, this will not check recursively defined colors.  For example, something
like ((thing1 thing2) (thing2 thing1)) will be considered valid."
  (let ((color-names (mapcar (lambda (x) (car x)) colors)))
    (dolist (color-name cur-theme--schema-names)
      (unless (member color-name color-names)
        (error "Color %s is not defined in theme" color-name)))
    (dolist (color colors)
      (unless (equal (length color) 2)
        (error "Color %s has several definitions" (car color)))
      (let ((name (car color))
            (value (car (last color))))
        (cond ((stringp value)
               (unless (equal (length value) 7)
                 (error "Color %s's value is too long: %s" name value))
               (unless (string-match-p (rx "#" (repeat 6 (any "A-F" "a-f" "0-9"))) value)
                 (error "Color %s does not have a proper hex value: %s" name value)))
              ((symbolp value)
               (unless (member value color-names)
                 (error "Color %s references a nondefined color %s" name value)))
              (t
               (error "Color %s has nonsensical definition %s" name value)))))))

(defun cur-theme--expand-faces (face-defs)
  "Take FACE-DEFS and return faces that can be used by `custom-theme-set-faces'."
  (mapcar
   (lambda (face-def)
     (let* ((face (car face-def))
            (def  (cdr face-def)))
       `(list ',face ,`(list (list t (list ,@def))))))
   face-defs))

(defun cur-theme--get-face (face-name face-defs)
  "Return face-def with FACE-NAME in FACE-DEFS.
FACE-NAME should be a symbol."
  (cl-loop for face-def in face-defs
           when (equal face-name (car face-def))
           return face-def))

(defun cur-theme--merge-faces (prev-list new-list)
  "Take PREV-LIST of face defs and overlay NEW-LIST on top."
  (let (overridden)
    (setq overridden prev-list)
    (dolist (face-override new-list)
      (let ((face (car face-override))
            (definition (cdr face-override)))
        (if (assoc face overridden)
            (setcdr (cur-theme--get-face face overridden) definition)
          (push face-override overridden))))
    overridden))

(defun cur-theme--option-plist-get (plist prop default)
  "Get PROP from PLIST if PLIST has PROP, otherwise returning DEFAUlT."
  (if (plist-member plist prop)
      (plist-get plist prop)
    default))

(defmacro cur-theme-def (theme-name doc options-plist colors &rest faces)
  "Generate custom theme named THEME-NAME.
DOC is the documentation string.
OPTIONS-PLIST is an plist of optionional parameters that change the behavior of
different parts of the macro.  They are as follows:
* :merge - When t, merge FACES with `cur-theme--default-face-defs'.  When nil,
  do not perform any merging.  Defaults to t.
* :color-check - When set to t, check COLORS for conforming to schema.  When
  nil preform no checks.  Defaults to t.

COLORS is a list of lists, where the first element is a symbol and the second is
a string that is the hex value of the color or the name of another color in the
list.  COLORS must define all names in `cur-theme--schema-names' are defined.
OPTIONS-PLIST will affect this behavior.

FACES is a list of faces in the form (FACE PROPERTIES).  For example, (bold
:background bg :foreground fg :weight \\='bold).  OPTIONS-PLIST will affect
this behavior."
  (declare (indent 1))
  (let* ((merge (cur-theme--option-plist-get options-plist :merge t))
         (color-check (cur-theme--option-plist-get options-plist :color-check t)))
    (cond (color-check
           (cur-theme--check-default-schema colors))
          (t (when (eq merge t)
               (warn "Theme defined with no default schema but still merges with default faces defs which require the default schema"))))

    `(let* (,@colors)
       (deftheme ,theme-name
         ,doc)
       ,(unless (and (equal (length faces) 1)
                     (equal (car faces) nil))
          `(custom-theme-set-faces
            ',theme-name
            ,@(if merge
                 (cur-theme--expand-faces
                  (cur-theme--merge-faces cur-theme--default-face-defs faces))
               (cur-theme--expand-faces faces)))))))

(defcustom cur-override-theme-overrides
  '((doom-gruvbox (secondary-selection :background "#504945")))
  "The overrides for cur-override-theme."
  :group 'cur-override-theme
  :type '(alist :key-type symbol :value-type sexp))

(defcustom cur-override-theme-theme-priority
  'single
  "The priority for which theme has its overrides applied.
`first' means apply the override of the first theme if it has one.  `last' means
apply the override of the last theme if it has one.  `single' means expect only
a single theme to be enabled and signal a user error if multiple are enabled."
  :group 'cur-override-theme
  :type '(choice (const :tag "Use First Enabled Theme" first)
                 (const :tag "Use Last Enabled Theme" last)
                 (const :tag "Expect a Single Enabled Theme" single)))

(defun cur-theme--get-overrides (theme)
  "Return the overrides for THEME from `cur-override-theme-overrides'."
  (cdr (assoc theme cur-override-theme-overrides)))

(defmacro cur-override-theme-def ()
  ""
  (let ((overrides
         (cond ((eq cur-override-theme-theme-priority 'first)
                (cur-theme--get-overrides (car (last custom-enabled-themes))))
               ((eq cur-override-theme-theme-priority 'last)
                (cur-theme--get-overrides (car custom-enabled-themes)))
               ((eq cur-override-theme-theme-priority 'single)
                (when (> (length custom-enabled-themes) 1)
                  (user-error "More than one theme enabled.
Please disable all but one theme or change the value of `cur-override-theme-theme-priority'"))
                (cur-theme--get-overrides (car custom-enabled-themes))))))
    `(cur-theme-def cur-override
       "A theme to override defintions another theme."
       ( :merge nil
         :color-check nil)
       nil
       ,@ (if overrides
              overrides
            '(nil)))))

(defun cur-theme-load-theme (theme)
  "Load THEME, disabling all other currently enabled themes."
  (interactive
   (list
    (intern (completing-read "Custom Themes: "
                             (mapcar #'symbol-name
                                     (custom-available-themes))))))
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme theme t))
    (error "Problem loading theme %s" theme)))

(defcustom cur-override-theme-load-function
  #'cur-theme-load-theme
  "The function to set a theme or themes."
  :group 'cur-override-theme
  :type '(function))

(defun cur-override-theme-load-theme ()
  "Load a theme and then load the `cur-override' theme.
The function to load a theme can be changed with
`cur-override-theme-load-function'."
  (interactive)
  (call-interactively cur-override-theme-load-function)
  (load-theme 'cur-override t))

(provide 'cur-theme)
;;; cur-theme.el ends here
