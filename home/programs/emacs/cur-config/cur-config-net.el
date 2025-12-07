;; ===Gemini-Mode===
(use-package gemini-mode)

;; ===Org Gemini Exporter==
(use-package ox-gemini
  :after org
  :commands (org-gemini-export-to-file org-gemini-export-to-buffer))

;; ===Elpher Gemini/Gopher Client===
(use-package elpher
  :custom
  (elpher-default-url-type "gemini"))

;;; cur-config-elfeed.el --- Links for elfeed -*- lexical-binding: t -*-
(use-package elfeed
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
  :bind ( :map elfeed-show-mode-map
	  ([remap imenu] . consult-imenu)))

(use-package elfeed
  :if (file-readable-p (locate-user-emacs-file "cur-elfeed-links"))
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
	  ("V"       . elfeed-tube-mpv)
	  ("C-c C-f" . elfeed-tube-mpv-follow-mode)
	  ("C-c C-w" . elfeed-tube-mpv-where)
	  :map elfeed-search-mode-map
	  ("V" . elfeed-tube-mpv)))

(provide 'cur-config-elfeed)

(use-package yeetube
  :bind ( :map yeetube-mode-map
	  ("s"   . yeetube-search)
	  ("V" . yeetube-play))
  :config
  (setf yeetube-invidious-instances '("youtube.com")))

(use-package hl-line-mode
  :after (yeetube)
  :hook (yeetube-mode . hl-line-mode))

(use-package cur-yt
  :after (yeetube)
  :demand t
  :bind ( :map yeetube-mode-map
	  ("v" . cur-yt-play-video))
  :custom (yeetube-play-function #'cur-yt-yeetube-play-resolution))

(use-package cur-yt
  :commands (cur-yt-play-video))

(use-package cur-yt
  :after (elfeed elfeed-tube)
  :bind ( :map elfeed-show-mode-map
	  ("f" . cur-yt-freetube-video)
	  ("v" . cur-yt-play-video)
	  :map elfeed-search-mode-map
	  ("f" . cur-yt-freetube-video)
	  ("v" . cur-yt-play-video)))

(use-package elcord
  :custom
  (elcord-boring-buffers-regexp-list '("^ " "\\\\*Messages\\\\*"
				       "\\\\*Help\\\\*" "\\\\*elpher\\\\*"
				       "\\\\*Org Src .*\\\\*"
				       "\\\\*Occur\\\\*"
				       "\\\\*Embark Collect .*\\\\*")))

(provide 'cur-config-net)
