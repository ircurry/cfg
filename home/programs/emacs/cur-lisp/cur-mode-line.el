;;; cur-mode-line.el --- My custom mode-line -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;;; Custom Groups

(defgroup cur-mode-line nil
  "My custom mode-line that tries to be minimal."
  :group 'mode-line)

(defgroup cur-mode-line-faces nil
  "The faces for my custom mode-line."
  :group 'cur-mode-line)

(defface cur-mode-line-active
  '((t :inherit mode-line-active))
  "Face for active mode-line."
  :group 'cur-mode-line-faces)

(defface cur-mode-line-inactive
  '((t :inherit mode-line-inactive))
  "Face for inactive mode-line."
  :group 'cur-mode-line-faces)

(unless (boundp 'mode-line-right-align-edge)
  (defcustom mode-line-right-align-edge 'right-fringe
    "Where mode-line should align to.
This will be defined in Emacs 30."
    :type '(choice (const right-margin)
                   (const right-fringe)
                   (const window))
    :group 'mode-line))

;;;;; Modeline Padding For Right Alignment

(defun cur-mode-line-right-align-space (arg)
  "Return a space right alligned to before where ARG should begin.
ARG should be a list parseable by `format-mode-line'."
  (let* ((end-string (format-mode-line arg))
	 (end-string-length (length end-string))
	 (end-length (if (> end-string-length 0)
			 (progn
			   (add-face-text-property
			    0 end-string-length 'mode-line t end-string)
			   (string-pixel-width end-string))
		       0)))
    (propertize " " 'display
		(if (and (display-graphic-p)
			 (not (eq mode-line-right-align-edge 'window)))
		    `(space :align-to (- ,mode-line-right-align-edge
					 (,end-length)))
		  `(space :align-to
			  (,(- (window-pixel-width)
			       (window-scroll-bar-width)
			       (window-right-divider-width)
			       (* (or (car (window-margins)) 0)
				  (frame-char-width))
			       (or (cadr (window-fringes)) 0)
			       (pcase mode-line-right-align-edge
				 ('right-margin (or (cdr (window-margins)) 0))
				 (_ 0))
			       end-length)))))))

(defvar cur-mode-line-right-align
  '(:eval (cur-mode-line-right-align-space mode-line-end-spaces))
  "Puts a space right alligned to before where `mode-line-end-spaces' should begin.
This should ALWAYS come before `mode-line-end-spaces' otherwise the
spacing will be messed up.")

(defun cur-mode-line-end-space-conditionally (arg)
  "Return a list of space characters based on `mode-line-right-align-edge' and ARG.
If `mode-line-right-align-edge' is \\='right-align or \\='window, return a single
space character plus ARG addtional space characters.  Otherwise, just return ARG
space characters."
  (pcase mode-line-right-align-edge
    ((or 'right-margin 'window)
     (let ((spaces " "))
       (dotimes (_ arg spaces)
	 (setq spaces (concat spaces " ")))))
    (_
     (let (spaces )
       (dotimes (_ arg spaces)
	 (setq spaces (concat spaces " ")))))))

(defvar cur-mode-line-end-padding
  '(:eval (cur-mode-line-end-space-conditionally 0))
  "This is a value used to add some padding for `mode-line-end-spaces'.
This is supposed to go at the end of `mode-line-end-spaces'.
To create add more padding you can use the `cur-mode-line-end-space-dwim'
 function and input the amount of extra space you would like at the end.")

;;;;; Kmacro Indicator

(defface cur-mode-line-kmacro
  '((t :inherit font-lock-string-face))
  "Face for when KMacros are being defined."
  :group 'cur-mode-line-faces)

(defun cur-mode-line--kmacro ()
  "Return a propertized string if defining a kmacro."
  (when (and (mode-line-window-selected-p) defining-kbd-macro)
    (propertize "  KMACRO  " 'face 'cur-mode-line-kmacro)))

(defvar cur-mode-line-kmacro-indicator
  '(:eval (cur-mode-line--kmacro))
  "Indicator for KMacro definitions.")

;;;;; Narrowed Indicator

(defface cur-mode-line-narrowed
  '((t :inherit font-lock-constant-face))
  "Face for current window is narrowed."
  :group 'cur-mode-line-faces)

(defcustom cur-mode-line-exclude-narrow-inidcator '()
  "Which modes to exclude from showing the narrow indicator."
  :type '(repeat symbol)
  :group 'cur-mode-line)

(defun cur-mode-line--narrowed ()
  "Return a propertized string if current window is narrowed."
  (when (and (mode-line-window-selected-p)
             (buffer-narrowed-p)
             (not (apply #'derived-mode-p cur-mode-line-exclude-narrow-inidcator)))
    (propertize "  NARROWED  " 'face 'cur-mode-line-narrowed)))

(defvar cur-mode-line-narrowed-indicator
  '(:eval (cur-mode-line--narrowed))
  "Indicator for window narrowing.")

;;;;; Meow Indicator

(defface cur-mode-line-meow-state
  '((t :inherit highlight))
  "Face for showing meow state."
  :group 'cur-mode-line-faces)

(defun cur-mode-line-meow-display-p ()
  "Return whether or not meow related indicators should be displayed."
  (and (member 'meow features)
       (mode-line-window-selected-p)
       meow-mode))

(defun cur-mode-line--meow ()
  "Return a propertized string of the current meow state."
  (when (cur-mode-line-meow-display-p)
    (propertize (format "  %s  " (substring-no-properties
				  (upcase (symbol-name meow--current-state))
				  0 3))
		'face 'cur-mode-line-meow-state)))

(defvar cur-mode-line-meow-state-indicator
  '(:eval (cur-mode-line--meow))
  "Indicator for current meow state.")

;;;;; Buffer Status

(defface cur-mode-line-buffer-status
  '((t :inherit bold))
  "Face for the file indicator."
  :group 'cur-mode-line-faces)

(defun cur-mode-line--buffer-status-indicator ()
  "Return a propertized string for file indicator."
  ;; TODO: make these clickable
  (propertize
   (concat "%z" (mode-line-eol-desc) "%1*" "%1+" "%@")
   'face 'cur-mode-line-buffer-status))

(defvar cur-mode-line-buffer-status-indicator
  '(:eval (cur-mode-line--buffer-status-indicator))
  "Indicator for file coding system, editing status, and remote status.")

;;;;; Buffer Name

(defface cur-mode-line-buffer-name
  '((t ()))
  "Face for the buffer name indicator."
  :group 'cur-mode-line-faces)

(defun cur-mode-line--buffer-name ()
  "Return a propertized string of the current buffer's name."
  (propertize (format "%s" (buffer-name)) 'face 'cur-mode-line-buffer-name))

(defvar cur-mode-line-buffer-name-indicator
  '(:eval (cur-mode-line--buffer-name))
  "Indicator for current buffer name.")

;;;;; Major Mode

(defface cur-mode-line-major-mode-active
  '((t :inherit (mode-line-emphasis bold)))
  "Face for major mode indicator when active."
  :group 'cur-mode-line-faces)

(defface cur-mode-line-major-mode-inactive
  '((t :inherit bold))
  "Face for major mode indicator when inactive."
  :group 'cur-mode-line-faces)

(defun cur-mode-line--major-mode ()
  "Return a propertized string of current `major-mode'."
  (let ((face (if (mode-line-window-selected-p)
		  'cur-mode-line-major-mode-active
		'cur-mode-line-major-mode-inactive)))
    (propertize
     (capitalize
      (string-replace
       "-" " " (string-replace
		"-mode" "" (symbol-name major-mode))))
     'face face)))

(defvar cur-mode-line-major-mode-indicator
  '(:eval (cur-mode-line--major-mode))
  "Indicator for current buffer's major mode.")

;;;;; Eat

(defface cur-mode-line-eat
  '((t :inherit (bold eat-shell-prompt-annotation-success)))
  "Face for position when active."
  :group 'cur-mode-line-faces)

(defun cur-mode-line-eat-display-p ()
  "Return whether or not eat related indicators should be displayed."
  (and (member 'eat features)
       (mode-line-window-selected-p)
       (eq major-mode 'eat-mode)))

(defun cur-mode-line--eat ()
  "Return propertized string of current eat mode."
  (when (cur-mode-line-eat-display-p)
   (propertize
    (cond (eat--line-mode "(Line Mode)")
	  ((or eat--char-mode eat--eshell-char-mode) "(Char Mode)")
	  ((or eat--semi-char-mode eat--eshell-semi-char-mode)
	   "(Semi-Char Mode)")
	  (t "(Emacs Mode)"))
    'face 'cur-mode-line-eat)))

(defvar cur-mode-line-eat-indicator
  '(:eval (cur-mode-line--eat))
  "Indicator for Eat's current mode.")

;;;;; Position

(defface cur-mode-line-postion
  '((t ()))
  "Face for position when active."
  :group 'cur-mode-line-faces)

(defun cur-mode-line--position ()
  "Return a propertized string with the position of point in current buffer."
  (when (mode-line-window-selected-p)
    (concat "(%p" (when column-number-mode ", %c") ")")))

(defvar cur-mode-line-postion-indicator
  '(:eval (cur-mode-line--position))
  "Indicator for current buffer's position.")

;;;;; Flycheck

(defun cur-mode-line-flycheck-display-p ()
  "Whether or not to display flycheck indicator."
  (and (member 'flycheck features)
       flycheck-mode
       (mode-line-window-selected-p)))

(defun cur-mode-line--flycheck-num-errors (level face)
  "Return a string of number of errors in LEVEL with FACE applied conditionally."
  (when (cur-mode-line-flycheck-display-p)
    (let* ((error-nums (or (cdr (assoc level (flycheck-count-errors flycheck-current-errors))) 0))
	   (icon (pcase level
		   ('info "󰋽")
		   ('warning "")
		   ('error "")))
	   (string (format "%s %s" icon error-nums)))
      (if flycheck-mode-line-color
	  (propertize string 'face face)
	string))))

(cur-mode-line--flycheck-num-errors 'info 'cur-mode-line-flycheck-info)
(cur-mode-line--flycheck-num-errors 'warning 'cur-mode-line-flycheck-warning)
(cur-mode-line--flycheck-num-errors 'error 'cur-mode-line-flycheck-error)

(defface cur-mode-line-flycheck-info
  '((t :inherit (flycheck-error-list-info mode-line)))
  "Face for flycheck info indicator."
  :group 'cur-mode-line-faces)

(defface cur-mode-line-flycheck-warning
  '((t :inherit (flycheck-error-list-warning mode-line)))
  "Face for flycheck info indicator."
  :group 'cur-mode-line-faces)

(defface cur-mode-line-flycheck-error
  '((t :inherit (flycheck-error-list-error mode-line)))
  "Face for flycheck info indicator."
  :group 'cur-mode-line-faces)

(defvar cur-mode-line-flycheck-indicator
  '((:eval (cur-mode-line--flycheck-num-errors 'error 'cur-mode-line-flycheck-error))
    " "
    (:eval (cur-mode-line--flycheck-num-errors 'warning 'cur-mode-line-flycheck-warning))
    " "
    (:eval (cur-mode-line--flycheck-num-errors 'info 'cur-mode-line-flycheck-info)))
  "Indicator for flycheck errors.")

(format-mode-line cur-mode-line-flycheck-indicator)

;;;;; Add Variables To Risky
(dolist (var '(cur-mode-line-right-align
	       cur-mode-line-end-padding
	       cur-mode-line-kmacro-indicator
	       cur-mode-line-narrowed-indicator
	       cur-mode-line-meow-state-indicator
	       cur-mode-line-buffer-status-indicator
	       cur-mode-line-buffer-name-indicator
	       cur-mode-line-major-mode-indicator
	       cur-mode-line-postion-indicator
	       cur-mode-line-flycheck-indicator
	       cur-mode-line-eat-indicator))
  (put var 'risky-local-variable t))

(provide 'cur-mode-line)
;;; cur-mode-line.el ends here
