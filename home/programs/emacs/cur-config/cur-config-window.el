(use-package cur-window
  :config
  (setq window-sides-slots
        '(1 1 1 1))
  (setq display-buffer-alist
        '(("\\`\\*Async Shell Command.*\\'"
           (display-buffer-no-window))
          ("\\*Org Src.*"
           (display-buffer-same-window))
          ((or (derived-mode . compilation-mode)
               (derived-mode . geiser-mode)
               (derived-mode . grep-mode)
               (derived-mode . help-mode)
               (derived-mode . Info-mode)
               (derived-mode . rg-mode)
               (derived-mode . woman-mode))
           (cur-window-display-buffer-below-or-pop)
           (body-function . cur-window-select-fit-to-size))
	  ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer))
	  ("\\*Register Preview\\*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . fit-window-to-buffer)
	   (window-parameters . ((mode-line-format . none))))
          ((or (derived-mode . occur-mode)
               (derived-mode . xref--xref-buffer-mode))
           (display-buffer-reuse-window
            display-buffer-below-selected)
           (dedicated . t)
           (body-function . cur-window-select-fit-to-size))
          ((or (derived-mode . justl-mode)
               "justl - .*")
           (display-buffer-reuse-window
            display-buffer-at-bottom)
           (dedicated . t)
           (window-height . 0.25))
	  ((or "\\*eshell .*"
               "\\*.*-eshell*"
               "\\*.*-eat\\*")
           (display-buffer-reuse-window
            display-buffer-at-bottom)
           (window-height . 0.25))
          ((or (derived-mode . vterm-mode)
               "\\*vterm.*\\*"
               "\\*.*-vterm\\*")
           (display-buffer-reuse-mode-window
            display-buffer-same-window))
	  ((or (derived-mode . reb-mode)
	       (derived-mode . reb-lisp-mode)
	       "\\*RE-Builder\\*")
	   (display-buffer-reuse-mode-window
	    display-buffer-at-bottom)
	   (window-height . 0.25))
	  (".*"
	   (display-buffer-same-window)))))

(use-package ace-window
  :custom
  (aw-keys '(?n ?h ?j ?k ?l ?i ?u ?y ?r ?e))
  (aw-dispatch-always t)
  :config
  (setq aw-dispatch-alist '((?d delete-window)
			    (?D aw-delete-window "Delete Window")
			    (?m aw-swap-window "Swap Window")
			    (?w aw-flip-window)
			    (?b cur-aw-switch-buffer-in-window "Select Buffer")
			    (?s aw-split-window-horz "Split Horizonally")
			    (?v aw-split-window-vert "Split Vertically")
			    (?o delete-other-windows)
			    (?O delete-other-windows "Delete Other Windows")
			    (?? aw-show-dispatch-help))))

(use-package cur-aw
  :after ace-window)

(use-package popper
  :demand t
  :bind ( :map cur/toggle-map
	  ("w" . popper-toggle-type))
  :config
  (setopt popper-reference-buffers '("\\*eat\\*" "\\*.*-eat\\*$" eat-mode
				     "\\*eshell\\*" "\\*.*-eshell\\*$" eshell-mode
				     compilation-mode
				     occur-mode
				     geiser-mode
				     grep-mode
				     rg-mode
				     xref--xref-buffer-mode
				     reb-mode
				     reb-lisp-mode)))

(use-package popper
  :after project
  :demand t
  :config
  (setopt popper-group-function #'popper-group-by-project))

(use-package popper
  :if (not (locate-library "cur-popper.el"))
  :config
  (setopt popper-display-function #'display-buffer-below-selected)
  (progn
    (popper-mode -1)
    (popper-mode +1)))

(use-package cur-popper
  :after (popper)
  :config
  (setopt popper-display-function #'cur-popper-display-buffer-dwim)
  (setopt popper-window-height #'cur-popper-fit-window-height)
  (setopt cur-popper-select-conditions '("\\*eat\\*" "\\*.*-eat\\*" (major-mode . eat-mode)
					 "\\*eshell\\*" "\\*.*-eshell\\*" (major-mode . eshell-mode)
					 "\\*Occur\\*" (major-mode . occur)
					 (major-mode . compilation-mode)
					 (major-mode . grep-mode)
					 (major-mode . rg-mode)
					 (major-mode . xref--xref-buffer-mode)
					 (major-mode . reb-mode)
					 (major-mode . reb-lisp-mode)))
  (progn
    (popper-mode -1)
    (popper-mode +1)))

(use-package emacs
  :after repeat
  :preface
  (defvar cur/scroll-up-repeat-map
    (let ((map (make-sparse-keymap)))
      map)
    "The keymap to repeat scrolling up.")
  (defvar cur/scroll-down-repeat-map
    (let ((map (make-sparse-keymap)))
      map)
    "The keymap to repeat scrolling down.")
  :bind ( :repeat-map cur/scroll-down-repeat-map
	  ("v" . scroll-down-command)
	  :repeat-map cur/scroll-up-repeat-map
	  ("v" . scroll-up-command)))

(provide 'cur-config-window)
