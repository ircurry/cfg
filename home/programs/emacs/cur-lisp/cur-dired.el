;;; cur-dired.el --- My extensions to dired -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun cur-dired-maybe-insert-subdir-or-find-file (&optional other-window)
  (interactive "P")
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(if other-window
	    (dired-other-window file)
	  (dired-maybe-insert-subdir file nil t))
      (if other-window
	  (find-file-other-window file)
	(dired--find-possibly-alternative-file file)))))

;;;###autoload
(defun cur-dired-find-file-dwim (&optional other-window)
  (interactive "P")
  (let* ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (when other-window
	      (progn
		(dired-other-window file)	
		(dired-goto-file file)))
	    (when (and (cdr dired-subdir-alist)
		       (dired-goto-subdir file))
	      (when dired-trivial-filenames
		(dired-goto-next-nontrivial-file))
	      (run-hooks 'dired-initial-position-hook)
	      t)
	    (dired--find-possibly-alternative-file file))
      (progn
	(if other-window
	    (dired-find-file-other-window file)
	  (dired--find-possibly-alternative-file file))))))

(provide 'cur-dired)
;;; cur-dired.el ends here
