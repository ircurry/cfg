;; ===Gemini-Mode===
(use-package gemini-mode
  :defer t)

;; ===Org Gemini Exporter==
(use-package ox-gemini
  :after org
  :defer t
  :commands (org-gemini-export-to-file org-gemini-export-to-buffer))

;; ===Elpher Gemini/Gopher Client===
(use-package elpher
  :defer t
  :custom
  (elpher-default-url-type "gemini"))

;;; cur-config-elfeed.el --- Links for elfeed -*- lexical-binding: t -*-
(use-package elfeed
  :defer t
  :commands (elfeed)
  :bind ( :map elfeed-show-mode-map
	  ("w" . elfeed-show-visit)
	  ("e" . eww)
	  ("i" . imenu)
	  :map elfeed-search-mode-map
	  ("w" . elfeed-search-browse-url)
	  ("e" . eww))
  :config
  (setopt elfeed-search-filter "@2-weeks-ago")
  (setopt elfeed-db-directory (concat (abbreviate-file-name (expand-file-name user-emacs-directory)) "elfeed-db")))

(use-package consult
  :after elfeed
  :defer t
  :bind ( :map elfeed-show-mode-map
	  ([remap imenu] . consult-imenu)))

(use-package elfeed
  :if (file-readable-p (locate-user-emacs-file "cur-elfeed-links"))
  :defer t
  :config
  (setopt elfeed-feeds (car (read-from-string (with-temp-buffer
						(insert-file-contents (locate-user-emacs-file "cur-elfeed-links"))
						(buffer-substring-no-properties (point-min) (point-max)))))))

(use-package elfeed-tube
  :after elfeed
  :demand t
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
  :demand t
  :bind ( :map elfeed-show-mode-map
	  ("v"       . elfeed-tube-mpv)
	  ("C-c C-f" . elfeed-tube-mpv-follow-mode)
	  ("C-c C-w" . elfeed-tube-mpv-where)
	  :map elfeed-search-mode-map
	  ("v" . elfeed-tube-mpv)))

(provide 'cur-config-elfeed)

(use-package cur-yt
  :defer t
  :commands (cur-yt-play-video))

(use-package cur-yt
  :after (elfeed elfeed-tube)
  :defer t
  :bind ( :map elfeed-show-mode-map
	  ("V" . cur-yt-play-video)
	  :map elfeed-search-mode-map
	  ("V" . cur-yt-play-video)))

(use-package elcord
  :defer t
  :custom
  (elcord-boring-buffers-regexp-list '("^ " "\\\\*Messages\\\\*"
				       "\\\\*Help\\\\*" "\\\\*elpher\\\\*"
				       "\\\\*Org Src .*\\\\*"
				       "\\\\*Occur\\\\*"
				       "\\\\*Embark Collect .*\\\\*")))

(provide 'cur-config-net)
