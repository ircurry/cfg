;;; cur-config-elisp.el --- configurations for working with elisp

;;; Commentary:

;;; Code:

;; ===rainbow-delimiters===
(use-package rainbow-delimiters
  ;:ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ===Helpful Emacs-Lisp Functions===
(defun cur/alist-add-or-replace (pair alist)
  "Function to add or replace a PAIR to ALIST and return ALIST.

This does not set the variable to be a new value."
  (let* ((pair-key     (car pair))
	 (alist-result (assoc pair-key alist)))
    (if alist-result
	(let ((new-alist (assoc-delete-all pair-key alist)))
	  (push pair new-alist))
      (push pair alist))))

(defun cur/alist-add-if-pair-not-exist (pair alist)
  "Function to add a PAIR to ALIST if PAIR is not in ALIST and return ALIST.

This does not set the variable to be a new value."
  (let* ((pair-key     (car pair))
	 (pair-value   (cdr pair))
	 (alist-result (assoc pair-key alist)))
    (if (and alist-result (equal pair-value alist-result))
	nil
      (push pair alist))))

(defun cur/alist-add-if-key-not-exist (pair alist)
  "Function to add a PAIR to ALIST if the key in PAIR is not in ALIST.
Returns ALIST.

This does not set the variable to be a new value."
  (let* ((pair-key     (car pair))
	 (alist-result (assoc pair-key alist)))
    (if alist-result
	nil
      (push pair alist))))

(provide 'cur-config-elisp)
;;; cur-config-elisp.el ends here
