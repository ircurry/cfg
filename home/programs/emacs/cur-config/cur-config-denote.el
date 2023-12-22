;;; cur-config-denote.el --- configurations for managing my zettelkasten

;;; Commentary:

;;; Code:
;; ===Denote===
(use-package denote
  :ensure t
  
  ;; ===Defining Helper Fucntions===
  :init
  (defun denote-all ()
    "Create note while prompting for every field.
This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(title keywords template subdirectory file-type date)."
    (declare (interactive-only t))
    (interactive)
    (let ((denote-prompts '(file-type title keywords subdirectory)))
      (call-interactively #'denote)))
  (defun denote-subdir-and-template ()
    "The same as `denote-subdirectory' but with template included.
This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(title keywords template subdirectory)."
    (declare (interactive-only t))
    (interactive)
    (let ((denote-prompts '(file-type title keywords subdirectory)))
      (call-interactively #'denote)))
  
  ;; ===Settings===
  :custom
  (denote-directory "~/denote" "Denote directory on my System")
  (denote-infer-keywords t "I want denote to infer keywords")
  (denote-sort-keywords t "Keywords should be sorted in files")
  (denote-known-keywords '("zknstruct" "zknexcpt" "zknunfin" "zknbib" "zknindex" "zknbuffer")
			 "zettelkasten specific keywords")
  (denote-prompts '(title keywords template) "Order of prompts")
  (denote-allow-multi-word-keywords nil "No multi-word keywords")
  (denote-date-format nil "Default date format")
  (denote-org-link-format "[[denote:%s][%s]]" "Use the title as the link name")
  
  ;; ===Bindings===
  :bind (:map cur/leader-keymap
	 ("d C-d" . denote)
	 ("d d" . denote-all)
	 ("d M-d" . denote-subdir-and-template)
	 ("d C-l" . denote-link-or-create)
	 ("d C-f" . denote-link-find-file)
	 ("d C-b" . denote-link-find-backlink)
	 ("d C-a" . denote-link-add-missing-links)
	 ("d a" . denote-link-add-links)
	 ("d C-r" . denote-dired-rename-marked-files)
	 ("d r" . denote-dired-rename-marked-files-using-front-matter)
	 ("d C-t" . denote-keywords-add)
	 ("d t" . denote-keywords-remove)
	 ("d C-n" . denote-rename-file)
	 ("d n" . denote-rename-file-using-front-matter))

  ;; ===Hooks===
  :hook
  (dired-mode . denote-dired-mode)
  
  :config
  ;; ===Templates===
  (setq denote-templates
	`((default . "\n\n\nCite:  ")
          (bibliography . ,(concat "* Citation\n"
                           "#+begin_src bibtex\n"
                           "  \n"
                           "#+end_src\n"
                           "\n\n"
                           "* Outline\n"
                           "\n\n"
                           "* Notes\n"
                           "\n\n"
                           "* Continuation\n"
                           "\n\n")))))

(provide 'cur-config-denote)
;;; cur-config-denote.el ends here
