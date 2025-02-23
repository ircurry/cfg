;;; cur-search.el --- My wrappers around isearch -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun cur-search-ifr (direction)
  "Begin `isearch-forward-regexp' or `isearch-backward-regexp' based on DIRECTION.
DIRECTION should be a number.  If DIRECTION is negative,
`isearch-backward-regexp' is called.  Otherwise, `isearch-forward-regexp' is
called."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (< 0 direction)
	(call-interactively #'isearch-forward-regexp)
      (call-interactively #'isearch-backward-regexp))))

(defun cur-search-ibr (direction)
  "The opposite of `cur-search-ifr'.
If DIRECTION is negative, call `isearch-forward-regexp' and vice versa."
  (interactive "p")
  (cur-search-ifr (- direction)))

(provide 'cur-search)
;;; cur-search.el ends here
