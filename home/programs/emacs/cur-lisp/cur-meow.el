;;; cur-meow.el --- Extending Meow's Capabilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'meow)
(eval-when-compile (require 'cl-lib))

;;; Customization
(defgroup cur-meow ()
  "Options related to extensions for the meow package."
  :group 'meow)

;;; Normal-Motion Toggle
(defun cur-meow-toggle-temp-normal-motion ()
  "Toggle normal and motion mode.
If in neither of the two states, return nil."
  (interactive)
  (cond ((meow-normal-mode-p) (call-interactively 'meow-motion-mode))
        ((meow-motion-mode-p) (call-interactively 'meow-normal-mode))
        (t nil)))

;;; Minisearch (`meow' integrated incremental search)
(defvar cur-meow--highlight-overlays nil
  "Highlight overlays.")

(defun cur-meow--valid-regexp-p (regexp)
  "Test REGEXP to see if it is a valid regexp."
  (condition-case nil
      (string-match-p regexp "")
    (error nil)
    (:success t)))

(defun cur-meow--highlight-cleanup ()
  "Delete all overlays in `cur-meow--highlight-overlays'."
  (while cur-meow--highlight-overlays
    (delete-overlay (car cur-meow--highlight-overlays))
    (setq cur-meow--highlight-overlays (cdr cur-meow--highlight-overlays))))

(cl-defun cur-meow--search-from-point
    (&key (search "")
	  (start-point (point))
	  (reverse nil)
	  (force nil))
  "Search for and get next or previous occurrence of SEARCH.
START-POINT is the point to start the search from.  If REVERSE is non nil search
backwards with `re-search-backward' instead of the default `re-search-forward'.
FORCE will run run the regexp search without checking if it is a valid regexp."
  (let ((search-fun (if reverse #'re-search-backward #'re-search-forward)))
    (unless (or (string-empty-p search)
		(and (not force) (not (cur-meow--valid-regexp-p search))))
      (save-mark-and-excursion
	(save-match-data
	  (goto-char start-point)
	  (when (or (apply search-fun search nil t 1 '())
		    (save-mark-and-excursion
		      (goto-char (if reverse (point-max) (point-min)))
		      (apply search-fun search nil t 1 '())))
	    (cons t (match-data))))))))

(cl-defun cur-meow--goto-highlight
    (&key (search "")
	  (reverse nil)
	  (start-point (if reverse (point-max) (point-min))))
  "Highlight and goto next or previous instance of SEARCH.
SEARCH should be a regexp string.  REVERSE causes the search to be backwards if
it is non nil.  START-POINT is the point at which the search should start.  If
no point is specified it defaults to `point-max' if reverse in non nil and
`point-min' if point is nil."
  (when (null executing-kbd-macro)
    (let* ((current-point (point))
	   (matching (cur-meow--search-from-point :search search
						  :start-point start-point
						  :reverse reverse))
	   (matched (car matching))
	   (data (cdr matching)))
      (cond
       (matched (cur-meow--highlight-cleanup)
		(let* ((beg (marker-position (car data)))
		       (end (marker-position (cadr data)))
		       (point (if reverse beg end)))
		  (cur-meow--highlight-cleanup)
		  (goto-char point)
		  (let ((overlay (make-overlay beg end)))
		    (push overlay cur-meow--highlight-overlays)
		    (overlay-put overlay 'priority 1000)
		    (overlay-put overlay 'face 'meow-search-highlight)
		    (overlay-put overlay 'window (selected-window)))))
       ((string-empty-p search)
	(cur-meow--highlight-cleanup)
	(goto-char start-point))
       (t (dolist (overlay cur-meow--highlight-overlays)
	    (overlay-put overlay 'face nil))
	  (goto-char current-point))))))

(cl-defun cur-meow--minibuffer-search-setup
    (&key (reverse nil)
	  (sel meow--selection)
	  (hist meow--selection-history)
	  (start-point (point))
	  (buffer (current-buffer)))
  "Set up the minibuffer to incrementally search BUFFER.

If REVERSE is non nil, searches will happen backwards unless SEARCH-FUN is set.
The beginnning of the search will be where the point is instead the end.

SEL is a meow selection, typically from `meow--selection'.  It will be restored
when exiting the minibuffer.

HIST is the meow selection history, typically from `meow--selection-history'.
It will be restored when exiting the minibuffer.

START-POINT is the point at which the search should start.  By default this is
the current point."
  (let* ((unwind (make-symbol "cur/search-unwind-function"))
	 (after-change (make-symbol "cur/after-change-hook-function")))
    (fset after-change
	  (lambda (_beg _end _len)
	    (let ((string (minibuffer-contents))
		  (case-fold-search nil))
	      (with-minibuffer-selected-window
		(with-current-buffer buffer
		  (cur-meow--goto-highlight :search string
					    :start-point start-point
					    :reverse reverse))))))
    (fset unwind
	  (lambda ()
	    (remove-hook 'after-change-functions after-change t)
	    (remove-hook 'minibuffer-exit-hook unwind t)
	    ;; Restore point and mark
	    (with-minibuffer-selected-window
	      (with-current-buffer buffer (goto-char start-point)))
	    (cur-meow--highlight-cleanup)
	    (with-minibuffer-selected-window
	      (with-current-buffer buffer
		(setq meow--selection-history hist)
		(when sel (meow--select-without-history sel))))))
    (lambda ()
      (add-hook 'minibuffer-exit-hook unwind nil t)
      (add-hook 'after-change-functions after-change nil t)
      (with-minibuffer-selected-window
	(with-current-buffer buffer
	  (when sel (meow--cancel-selection)))))))

(defvar cur-meow-mini-search-history nil
  "History of regexp searches by `cur-meow-mini-search'.")

(defun cur-meow-mini-search (arg)
  "Search for regexp incrementally and select it like `meow-search'.
Giving ARG with reverse the direction of the search."
  (interactive "P")
  (let* ((case-fold-search nil)
	 (reverse (xor (meow--with-negative-argument-p arg) (meow--direction-backward-p)))
	 (start-point (point))
	 (search (minibuffer-with-setup-hook
		     (cur-meow--minibuffer-search-setup :reverse reverse
							:start-point start-point)
		   (read-from-minibuffer "Search: " nil nil
					 nil cur-meow-mini-search-history
					 "")))
	 (matching (cur-meow--search-from-point :search search
						:start-point start-point
						:reverse reverse))
	 (matched (car matching)))
    (if matched
	(let* ((data (cdr matching))
	       (marker-beg (car data))
	       (marker-end (cadr data))
	       (beg (if reverse (marker-position marker-end) (marker-position marker-beg)))
	       (end (if reverse (marker-position marker-beg) (marker-position marker-end))))
	  (add-to-history 'regexp-search-ring search regexp-search-ring-max)
	  (thread-first
            (meow--make-selection '(select . visit) beg end)
            (meow--select))
	  (if reverse
              (message "Reverse search: %s" search)
            (message "Search: %s" search))
          (meow--ensure-visible))
      (message "Searching %s failed" search))
    (meow--highlight-regexp-in-buffer search)))

(provide 'cur-meow)
;;; cur-meow.el ends here
