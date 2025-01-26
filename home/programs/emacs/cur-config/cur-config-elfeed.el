;;; cur-config-elfeed.el --- Links for elfeed -*- lexical-binding: t -*-

(use-package elfeed
  :defer t
  :bind ( :map elfeed-show-mode-map
	  ("w" . elfeed-show-visit)
	  ("e" . eww)
	  :map elfeed-search-mode-map
	  ("w" . elfeed-search-browse-url)
	  ("e" . eww))
  :config
  (setopt elfeed-search-filter "@2-weeks-ago")
  (setopt elfeed-db-directory (concat (abbreviate-file-name (expand-file-name user-emacs-directory)) "elfeed-db")))

(let ((cur-links-path (concat (abbreviate-file-name (expand-file-name user-emacs-directory)) "cur-elfeed-links")))
  (use-package elfeed
    :if (file-readable-p cur-links-path)
    :config
    (setopt elfeed-feeds (car (read-from-string (with-temp-buffer
						  (insert-file-contents cur-links-path)
						  (buffer-substring-no-properties (point-min) (point-max))))))))

(use-package elfeed-tube
  :after elfeed
  :bind ( :map elfeed-show-mode-map
	  ("F" . elfeed-tube-fetch)
	  ([remap save-buffer] . elfeed-tube-save)
	  :map elfeed-search-mode-map
	  ("F" . elfeed-tube-fetch)
	  ([remap save-buffer] . elfeed-tube-save))
  :config
  (setopt elfeed-tube-auto-fetch-p t
	  elfeed-tube-auto-save-p nil)
  (elfeed-tube-setup))

(use-package elfeed-tube-mpv
  :after (elfeed-tube elfeed)
  :bind ( :map elfeed-show-mode-map
	  ("v" . elfeed-tube-mpv)
	  :map elfeed-search-mode-map
	  ("v" . elfeed-tube-mpv)))

(provide 'cur-config-elfeed)
